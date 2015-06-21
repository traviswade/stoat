-module(funs).

anonymous () ->
	lists:map(fun(N) -> N + 1 end, [1, 2, 3]).
	
anoncuts () -> [
	fun (X__) -> X__ end,
	fun () -> 5 end,
	fun (X__) -> X__ + 3 end].
	
multiclause () ->
	fun ({struct, L}) -> L;
		(L)           -> L
	end.