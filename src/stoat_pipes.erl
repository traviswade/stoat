-module(stoat_pipes).

-export([transform/1]).

-import(erl_syntax, [type/1]).

-record(step, {wrappers=[], src, mod, line}).
-record(s, {wrappers=[], mod, steps=[], input}).

-define(p(F, D), error_logger:info_msg(F, D)).

transform ({Input, Steps, Line}=In) -> 
	?p("processing pipe: ~p~n", [In]),
	Steps1 = proc_steps(Steps, #s{input=Input}),
	?p("bound: ~p~n", [Steps1]),
	?p("which equals : ~p~n", [erl_prettypr:format(Steps1)]),
	{atom, 1, c}.
	
% '|>' '|+' '|-' '|)' '|/' '|m' '|:' '~'
% todo : steps will be a nested array of chunks. we'll need another record field of the expressions so far
proc_steps ([], #s{steps=Steps}=S) -> 
	compose_call(S#s{steps=lists:reverse(Steps)});

% NOTE THAT WE ONLY ACTUALLY ALLOW ONE WRAPPER. THIS MAY CHANGE, AND DO_CALL WILL NEED TO BE UPDATED
proc_steps ([{{'|+', _}, Wrapper, [], _L}|T], Acc) ->
	proc_steps(T, Acc#s{wrappers=[Wrapper]});
	
proc_steps ([{{'|-',_}, {_,_,ToRemove}, [], _L}|T], #s{wrappers=[{_,_,ToRemove}]}=Acc) ->
	proc_steps(T, Acc#s{wrappers=[]});
proc_steps ([{{'|-', _}, _, _, _}|T], Acc) ->
	proc_steps(T, Acc);
	
proc_steps ([{{'|m', _}, Mod, [], _L}|T], Acc) ->
	proc_steps(T, Acc#s{mod=Mod});
	
proc_steps ([{{'|>', L}, F, [], _L}|T], #s{steps=Steps, wrappers=W}=Acc) ->	
	proc_steps(T, Acc#s{steps=[#step{wrappers=W, src=F, line=L}|Steps]});
	
proc_steps ([{{'|)', L}, F, [], _L}|T], #s{steps=Steps}=Acc) ->
	proc_steps(T, Acc#s{steps=[#step{src=F, wrappers=[], line=L}|Steps]});
	
proc_steps ([{{'|/', L}, F, [], _L}|T], #s{steps=Steps}=Acc) ->
	proc_steps(T, Acc#s{steps=[#step{src={tap,F}, line=L}|Steps]}).

compose_call (#s{steps=[], input=Input}) -> Input;
compose_call (#s{steps=[H|T], input=Input}=S) -> 
	compose_call(S#s{steps=T, input=do_call(H, Input)}).
	
do_call (#step{src={tap, F}, line=L}, Input) ->
	Arg = {var, L, 'Arg'},
	Fun = {'fun', L, {clauses, [{clause, L, [Arg], [], [{call, L, F, [Arg]}, Arg]}]}},
	{call, L, Fun, [Input]};

do_call (#step{src=F, line=L, wrappers=[]}, Input) ->
	{call, L, F, [Input]};
	
do_call (#step{src=F, wrappers=[W], line=L}, Input) ->
	{call, L, W, [F, Input]}.

