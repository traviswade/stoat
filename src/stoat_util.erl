-module(stoat_util).

-export([with_file/2, to_l/1, find_file/2,
	str2expr/1, erlstr2expr/1, no_lines/1, 
	form2erl/1, line/1, ebin_dir/1]).
	
-include_lib("stoat.hrl").

with_file (Fnam, Fun) ->
	{ok, F} = file:open(Fnam, [write]),
	Res = Fun(fun(Output)->file:write(F, Output) end),
	file:close(F),
	Res.
	
to_l (L) when is_list(L) -> L;
to_l (B) when is_binary(B) -> binary_to_list(B);
to_l (A) when is_atom(A) -> atom_to_list(A);
to_l (I) when is_integer(I) -> integer_to_list(I);
to_l (X) -> lists:flatten(io_lib:format("~p~n", [X])).

find_file (Name, Dir) ->
	{ok, Files} = file:list_dir(Dir),
	find_file(Files, Name, Dir).
	
find_file ([], _, _)            -> error;
find_file ([Name|_], Name, Dir) -> filename:join([Dir, Name]);
find_file ([F|T], Name, Dir)    ->
	Path = filename:join([Dir, F]),
	case filelib:is_dir(Path) of
		true ->
			case find_file(Name, Path) of
				error -> find_file(T, Name, Dir);
				Found  -> Found
			end;
		_ ->
			find_file(T, Name, Dir)
	end.

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
	
ebin_dir (SrcPath) ->
	PathEls = filename:split(filename:dirname(SrcPath)),
	filename:join([case El of "src" -> "ebin"; Other -> Other end || El <- PathEls]).
	
line ({_, L, _}) -> L;
line (_) -> 0.
	
	
% without_lines (Forms) whe
no_lines (Forms) when is_list(Forms) -> [no_lines(F) || F <- Forms];
no_lines ({clauses, Cs})             -> {clauses, no_lines(Cs)};
no_lines ({Type, _Line, A})          -> {Type, 0, no_lines(A)};
no_lines ({Type, _Line, A, B})       -> {Type, 0, no_lines(A), no_lines(B)};
no_lines ({Type, _Line, A, B, C})    -> {Type, 0, no_lines(A), no_lines(B), no_lines(C)};
no_lines (Other)                     -> Other.
