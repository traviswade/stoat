-module(misc).

-export([with_file/2]).


with_file (Fnam, Fun) ->
	{ok, F} = file:open(Fnam, [write]),
	Res = Fun(fun(Output)->file:write(F, Output) end),
	file:close(F),
	Res.