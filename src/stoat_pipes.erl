-module(stoat_pipes).

-export([transform/1]).

-import(erl_syntax, [type/1]).

-record(step, {wrappers=[], src, mod}).
-record(s, {wrappers=[], mod, steps=[]}).

-define(p(F, D), error_logger:info_msg(F, D)).

transform ({Input, Steps, Line}=In) -> 
	?p("processing pipe: ~p~n", [In]),
	Steps1 = do_steps(Steps),
	?p("bound: ~p~n", [Steps1]),
	{atom, 1, c}.
	
init_steps (Raw) -> [#step{src=Step} || Step <- Raw].
	
do_steps (Steps) -> do_steps(Steps, #s{}).
% 	?p("steps: ~p~n", [Steps]),
% 	add_wrappers(Steps, {[], []}).

 % '|>' '|+' '|-' '|)' '|/' '|m' '|:' '~'

do_steps ([], #s{steps=Steps}) -> lists:reverse(Steps);

do_steps ([{{'|+', _}, Wrapper, [], _Line}|T], #s{wrappers=W}=Acc) ->
	do_steps(T, Acc#s{wrappers=[Wrapper|W]});
	
do_steps ([{{'|-', _}, Wrapper, [], _Line}|T], #s{wrappers=W}=Acc) ->
	do_steps(T, Acc#s{wrappers=W--[Wrapper]});
	
do_steps ([{{'|m', _}, Mod, [], _Line}|T], Acc) ->
	do_steps(T, Acc#s{mod=Mod});
	
do_steps ([{{'|>', _}, F, [], _Line}|T], #s{steps=Steps, wrappers=W}=Acc) ->	
	do_steps(T, Acc#s{steps=[#step{wrappers=W, src=F}|Steps]});
	
do_steps ([{{'|)', _}, F, [], _Line}|T], #s{steps=Steps}=Acc) ->
	do_steps(T, Acc#s{steps=[#step{src=F}|Steps]});
	
do_steps ([{{'|/', _}, F, [], _Line}|T], #s{steps=Steps}=Acc) ->
	do_steps(T, Acc#s{steps=[#step{src={tap,F}}|Steps]}).

% add_wrappers ([], {_, AccS}) -> lists:reverse(AccS);
% add_wrappers ([#step{src={{'|+',_},W,[],_Line}}|T], {AccW, AccS}) ->
% 	add_wrappers(T, {[B|AccB], AccS});
% add_wrappers ([#step{src={{'|-',_},W,[],_Line}}|T], {AccW, AccS}) ->
% 	add_wrappers(T, {AccW--[W], AccS});
% add_wrappers ([S|T], {AccW, AccS}) ->
% 	add_wrappers(T, {AccW, [S#step{binders=AccW}|AccS]}).
% 


