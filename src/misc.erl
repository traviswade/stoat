-module(misc).

-export([with_file/2, to_l/1, find_file/2]).


with_file (Fnam, Fun) ->
	{ok, F} = file:open(Fnam, [write]),
	Res = Fun(fun(Output)->file:write(F, Output) end),
	file:close(F),
	Res.
	
to_l (B) when is_binary(B) -> binary_to_list(B);
to_l (A) when is_atom(A) -> atom_to_list(A);
to_l (I) when is_integer(I) -> integer_to_list(I);
to_l (X) -> X.

find_file (Name, Dir) ->
	{ok, Files} = file:list_dir(Dir),
	find_file(Files, Name, Dir).
	
find_file ([], _, _)            -> error;
find_file ([Name|_], Name, Dir) -> filename:join([Dir, Name]);
find_file ([F|T], Name, Dir)    ->
	Path = filename:join([Dir, F]),
	case filelib:is_dir(Path) of
		true ->
			case find_file(Name, Dir) of
				error -> find_file(T, Name, Dir);
				Found  -> Found
			end;
		_ ->
			find_file(T, Name, Dir)
	end.
	