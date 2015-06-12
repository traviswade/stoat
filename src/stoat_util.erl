-module(stoat_util).

-export([str2expr/1, erlstr2expr/1]) .

str2expr (Str) ->
	{ok, Toks, _} = stoat_lex:string(Str ++ "."),
	{ok, Expr} = stoat_parse:parse_expr(Toks),
	Expr.

erlstr2expr (Str) ->
	{ok, Toks, _} = erl_scan:string(Str ++ "."),
	{ok, [Expr]} = erl_parse:parse_exprs(Toks),
	Expr.