-module(stoat_util).

-export([str2expr/1, erlstr2expr/1, no_lines/1, form2erl/1, line/1, write_file/2]).

str2expr (Str) ->
	{ok, Toks, _} = stoat_lex:string(Str ++ "."),
	{ok, Expr} = stoat_parse:parse_expr(Toks),
	Expr.

erlstr2expr (Str) ->
	{ok, Toks, _} = erl_scan:string(Str ++ "."),
	{ok, [Expr]} = erl_parse:parse_exprs(Toks),
	Expr.
	
form2erl (F) -> try
		erl_prettypr:format(F)
	catch _:_ -> {error, badform, F} end.
	
line ({_, L, _}) -> L;
line (_) -> 0.
	
	
% without_lines (Forms) whe
no_lines (Forms) when is_list(Forms) -> [no_lines(F) || F <- Forms];
no_lines ({clauses, Cs})             -> {clauses, no_lines(Cs)};
no_lines ({Type, _Line, A})          -> {Type, 0, no_lines(A)};
no_lines ({Type, _Line, A, B})       -> {Type, 0, no_lines(A), no_lines(B)};
no_lines ({Type, _Line, A, B, C})    -> {Type, 0, no_lines(A), no_lines(B), no_lines(C)};
no_lines (Other)                     -> Other.

write_file (Path, Content) ->
	{ok, F} = file:open(iolist_to_binary(Path), [write]),
	file:write(F, Content),
	file:close(F).
