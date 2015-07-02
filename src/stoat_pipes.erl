-module(stoat_pipes).

-export([transform/1]).

-import(erl_syntax, [type/1]).
-include_lib("stoat.hrl").


-record(step, {wrappers=[], src, mod, line, bind=[]}).
-record(s, {wrappers=[], mod, steps=[], exprs}).


transform ({Input, Steps, Line}=In) -> 
	proc_steps(Steps, #s{exprs=[Input]}).

proc_steps ([], #s{steps=Steps}=S) -> 
	compose_call(S#s{steps=lists:reverse(Steps)});

% NOTE THAT WE CURRENTLY ONLY ALLOW ONE WRAPPER. THIS MAY CHANGE, 
% BUT THAT IS NOT A GOOD REASON FOR IT TO BE WRAPPED IN A SILLY LIST NOW.
proc_steps ([{{'|+', _}, Wrapper, [], _L}|T], Acc) ->
	proc_steps(T, Acc#s{wrappers=[Wrapper]});
	
proc_steps ([{{'|-',_}, {_,_,ToRemove}, [], _L}|T], #s{wrappers=[{_,_,ToRemove}]}=Acc) ->
	proc_steps(T, Acc#s{wrappers=[]});
proc_steps ([{{'|-', _}, _, _, _}|T], Acc) ->
	proc_steps(T, Acc);
	
proc_steps ([{{'|<', _}, {call, L, Mod, [Pos]}, [], _L}|T], Acc) ->
	proc_steps(T, Acc#s{mod={Mod, Pos}});
	
proc_steps ([{{'|<', _}, {atom, L, Mod}, [], _L}|T], Acc) ->
	proc_steps(T, Acc#s{mod={{atom, L, Mod}, 'default_|<_pos'(Mod)}});
	
proc_steps ([{{'|:', L}, Call, B, _L}|T], #s{steps=Steps, wrappers=W, mod=M}=Acc) ->
	proc_steps(T, Acc#s{steps=[
		#step{src={mod, M, Call}, line=L, wrappers=W, bind=B}|Steps]});
		
proc_steps ([{'?[', Clauses, Bind, L}|T], #s{steps=Steps}=Acc) ->
	Stmt = {'case', L, {var, L, '_'}, Clauses},
	proc_steps(T, Acc#s{steps=[#step{src=Stmt}|Steps]});
	
proc_steps ([{'?{', Y, N, L}|T], #s{steps=Steps}=Acc) ->
	Stmt = {'if', L, [{clause, L, [], [[{var, L, '_'}]], [Y]},
		            {clause, L, [], [[{atom, L, true}]], [N]}]},
	proc_steps(T, Acc#s{steps=[#step{src=Stmt}|Steps]});
	
proc_steps ([{{'|>', L}, F, Bind, _L}|T], #s{steps=Steps, wrappers=W}=Acc) ->	
	proc_steps(T, Acc#s{steps=[#step{wrappers=W, src=F, line=L, bind=Bind}|Steps]});
	
proc_steps ([{{'|)', L}, F, Bind, _L}|T], #s{steps=Steps}=Acc) ->
	proc_steps(T, Acc#s{steps=[#step{src=F, line=L, bind=Bind}|Steps]});
		
proc_steps ([{{'|/', L}, F, [], _L}|T], #s{steps=Steps}=Acc) ->
	proc_steps(T, Acc#s{steps=[#step{src={tap,F}, line=L}|Steps]}).

compose_call (#s{steps=[], exprs=Exprs}) -> lists:reverse(Exprs);

% TODO. check for steps that are a fun, apply them in place. won't currently work with wrappers
% X |> double |> fun(X)->X+1 end |> triple.
% triple( fun(X)->X+1 end( double( X ) ) ).
% V1 = double(X) + 1, triple(V).
% compose_call (#s{steps[#step{src={'fun',_,_}}|T], wrappers=[]}, exprs=[Input|Texp]) ->

compose_call (#s{steps=[H|T], exprs=[Input|Texp]}=S) -> 
	Call = do_call(H, Input),
	compose_call(S#s{steps=T, exprs=case H#step.bind of
		[] -> [Call|Texp];
		[{Var, Line}|_] = Binding ->
			Assignment = do_match(Line, lists:reverse([Call|Binding])), %{match, Line, Var, Call},
			[Var, Assignment|Texp]
	end}).
	
do_match (_, [Val])      ->  check_binding(Val);
do_match (Line, [Val|T]) ->  
	{match, Line, check_binding(Val), do_match(Line, T)}.

check_binding ({{var, _, _}=Val, _}) -> Val;
check_binding (Val) -> Val.
	
do_call (#step{src={tap, F}, line=L}, Input) ->
	Arg = {var, L, 'X__'},
	Fun = {'fun', L, {clauses, [{clause, L, [Arg], [], [{call, L, F, [Arg]}, Arg]}]}}, 
	{call, L, Fun, [Input]};
	
do_call (#step{src={mod, {M, Pos}, Call}, line=L}=Step, Input) ->
	case Call of
		{atom, _, _}=F -> 
			do_call(Step#step{src={remote, L, M, F}}, Input);
		{call, _L, F, Args} ->
			Pivot = case Pos of
				{integer, _, N} -> N;
				N when is_integer(N), N >= 0 -> N;
				N when is_integer(N) -> length(Args)+N+1;
				{op,_,'-',{integer,_,N}} -> length(Args)-N+1
			end,
			{Bef, Aft} = lists:split(Pivot, Args),
			do_call(Step#step{src={
				call, L, {remote, L, M, F}, Bef++[{var, L, '_'}]++Aft}}, Input)
	end;
do_call (#step{src={remote, _,_,_}=Op, line=L, wrappers=[]}, Input) ->
	{call, L, Op, [Input]};
do_call (#step{src={atom, _,_}=Op, line=L, wrappers=[]}, Input) ->
	{call, L, Op, [Input]};
do_call (#step{src={var, _,_}=Op, line=L, wrappers=[]}, Input) ->
	case stoat_cuts:replace_underscore(Input, Op) of
		{true, Expr1} -> Expr1;
		_             -> {call, L, Op, [Input]}
	end;
do_call (#step{src={'fun', _,_}=Op, line=L, wrappers=[]}, Input) ->
	{call, L, Op, [Input]};
do_call (#step{src=Expr, line=L, wrappers=[]}, Input) ->
	case stoat_cuts:replace_underscore(Input, Expr) of
		{true, Expr1} ->  Expr1;
		Other             ->  {call, L, Expr, [Input]}
	end;
do_call (#step{src=Src, wrappers=[W], line=L}, Input) ->
	case stoat_cuts:replace_underscore(Input, Src) of
		{true, Src1} ->  {call, L, W, [Src1, Input]};
		_            ->  {call, L, W, [wrap_op(Src), Input]}
	end.

wrap_op ({remote, L, M, F}=Call) ->
	Arg = {var, L, 'X__'},
	{'fun', L, {clauses, [{clause, L, [Arg], [], [{call, L, Call, [Arg]}]}]}};
wrap_op ({atom, _, _}=Op)   ->   Op;
wrap_op ({'fun', _, _}=Op)  ->   Op;
wrap_op ({var, _, _}=Op)    ->   Op;
wrap_op (Op)                ->   Op.

'default_|<_pos' (lists) -> -1;
'default_|<_pos' (maps) -> -1;
'default_|<_pos' (_) -> -1.








