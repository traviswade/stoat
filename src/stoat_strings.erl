
-module(stoat_strings).

-export([interpolate/1]).
-import(lists, [sublist/3]).

-include_lib("stoat.hrl").

interpolate ({sstring, L, S}) -> 
	case re:run(S, "#{.*}", [global, ungreedy]) of
		nomatch -> 
			?p("no replacements to make : ~p~n", [S]),
			{string, L, S};
		{match, Matches} ->
			?p("would be doing some replacement:  ~p (~p)~n", [S, Matches]),
			?p("replacements: ~p~n", [make_replacements(Matches, L, S, {0, []})]),
			{string, L, S}
	end.
	
	
make_replacements ([], L, S, {Prev, Acc}) -> 
	% finish him
	lists:reverse([{string, L, sublist(S, Prev+1, length(S))}|Acc]);
make_replacements ([[{Start,Len}]|T], L, S, {Prev, Acc}) ->
	make_replacements(T, L, S,
		{Start+Len, 
			[{eval, L, sublist(S, Start+3, Len-3)}, 
			 {string, L, sublist(S, Prev+1, Start-Prev)} | Acc]}).


