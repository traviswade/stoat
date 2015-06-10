-module(stoat_pipes).

-export([transform/1]).

-import(erl_syntax, [type/1]).

-record(step, {wrappers=[], src, mod, line, bind=[]}).
-record(s, {wrappers=[], mod, steps=[], exprs}).

-define(p, error_logger:info_msg).

transform ({Input, Steps, Line}=In) -> proc_steps(Steps, #s{exprs=[Input]}).
	
% '|>' '|+' '|-' '|)' '|/' '|m' '|:' '~'
% |{->}

proc_steps ([], #s{steps=Steps}=S) -> 
	compose_call(S#s{steps=lists:reverse(Steps)});

% NOTE THAT WE CURRENTLY ONLY ALLOW ONE WRAPPER. THIS MAY CHANGE, AND DO_CALL WILL NEED TO BE UPDATED
proc_steps ([{{'|+', _}, Wrapper, [], _L}|T], Acc) ->
	proc_steps(T, Acc#s{wrappers=[Wrapper]});
	
proc_steps ([{{'|-',_}, {_,_,ToRemove}, [], _L}|T], #s{wrappers=[{_,_,ToRemove}]}=Acc) ->
	proc_steps(T, Acc#s{wrappers=[]});
proc_steps ([{{'|-', _}, _, _, _}|T], Acc) ->
	proc_steps(T, Acc);
	
proc_steps ([{{'|m', _}, Mod, [], _L}|T], Acc) ->
	proc_steps(T, Acc#s{mod=Mod});
	
proc_steps ([{{'|>', L}, F, Bind, _L}|T], #s{steps=Steps, wrappers=W}=Acc) ->	
	proc_steps(T, Acc#s{steps=[#step{wrappers=W, src=F, line=L, bind=Bind}|Steps]});
	
proc_steps ([{{'|)', L}, F, Bind, _L}|T], #s{steps=Steps}=Acc) ->
	proc_steps(T, Acc#s{steps=[#step{src=F, wrappers=[], line=L, bind=Bind}|Steps]});
		
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
	Arg = {var, L, 'Arg'},
	Fun = {'fun', L, {clauses, [{clause, L, [Arg], [], [{call, L, F, [Arg]}, Arg]}]}},
	{call, L, Fun, [Input]};
	
do_call (#step{src=F, line=L, wrappers=[]}, Input) ->
	{call, L, F, [Input]};
	
do_call (#step{src=F, wrappers=[W], line=L}, Input) ->
	{call, L, W, [wrap_op(F), Input]}.
	
%  {remote,28,{atom,28,m},{atom,28,f1}}
% {'fun',0,
  % {clauses,
  %     [{clause,0,
  %          [{var,29,'X'}],
  %          [],
  %          [{call,29,
  %               {remote,29,{atom,29,m},{atom,29,f2}},
  %               [{var,29,'X'}]}]}]}}
wrap_op ({remote, L, M, F}=Call) ->
	Arg = {var, L, 'Arg__'},
	{'fun', L, {clauses, [{clause, L, [Arg], [], [{call, L, Call, [Arg]}]}]}};
wrap_op (Op) ->
	error_logger:info_msg("wrapping op: ~p~n", [Op]), Op.








