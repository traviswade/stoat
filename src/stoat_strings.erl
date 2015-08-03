
-module(stoat_strings).

-export([interpolate/1]).
-import(lists, [sublist/3]).

-include_lib("stoat.hrl").

interpolate ({sstring, L, S}) -> 
	case re:run(S, "#{.*}", [global, ungreedy]) of
		nomatch -> 
			{string, L, S};
		{match, Matches} ->
			% ?p("replacements: ~p~n", [do_repl(Matches, L, S, {0, []})]),
			% {string, L, S}
			do_repl(Matches, L, S, {0, []})
	end.
	
	
do_repl ([], L, S, {Prev, Acc}) -> 
	finish_repl(L, Acc, {string, L, sublist(S, Prev+1, length(S))});
do_repl ([[{Start,Len}]|T], L, S, {Prev, Acc}) ->
	do_repl(T, L, S,
		{Start+Len, 
			[{eval, sublist(S, Start+3, Len-3)}, 
			 sublist(S, Prev+1, Start-Prev) | Acc]}).
			
finish_repl (L, [], Acc)            ->  Acc;
finish_repl (L, [{eval, H}|T], Acc) ->  
	Repl = {call, L,
		{remote,1,{atom,1,stoat_util},{atom,1,to_l}},
		[stoat_util:str2expr(H)]},
	finish_repl(L, T, {op, L, '++', Repl, Acc});
finish_repl (L, [Str|T], Acc)       ->  
	finish_repl(L, T, {op, L, '++', {string, L, Str}, Acc}).
			



