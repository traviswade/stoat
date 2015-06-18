-module(funs).

anonymous () ->
	lists:map(fun(N) -> N + 1 end, [1, 2, 3]).
	
multiclause () ->
	fun ({struct, L}) -> L;
		(L)           -> L
	end.