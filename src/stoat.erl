
-module(stoat).

-compile(export_all).

-include_lib("stoat.hrl").


show (Str) ->
	?p("parsing: ~p~n", [Str]),
	{ok, Tokens, _} = stoat_lex:string(Str),
	?p("tokens: ~p~n", [Tokens]),
	{ok, Parsed} =  stoat_parse:parse(Tokens),
	?p("parsed: ~p~n", [Parsed]),
	Parsed.