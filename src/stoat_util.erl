-module(stoat_util).

-export([str2expr/1, erlstr2expr/1, no_lines/1]) .

str2expr (Str) ->
	{ok, Toks, _} = stoat_lex:string(Str ++ "."),
	{ok, Expr} = stoat_parse:parse_expr(Toks),
	Expr.

erlstr2expr (Str) ->
	{ok, Toks, _} = erl_scan:string(Str ++ "."),
	{ok, [Expr]} = erl_parse:parse_exprs(Toks),
	Expr.
	
	
% without_lines (Forms) whe
no_lines (Forms) when is_list(Forms) -> [no_lines(F) || F <- Forms];
no_lines ({clauses, Cs})             -> {clauses, no_lines(Cs)};
no_lines ({Type, _Line, A})          -> {Type, 0, no_lines(A)};
no_lines ({Type, _Line, A, B})       -> {Type, 0, no_lines(A), no_lines(B)};
no_lines ({Type, _Line, A, B, C})    -> {Type, 0, no_lines(A), no_lines(B), no_lines(C)};
no_lines (Other)                     -> Other.
