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
	
trailer () -> m:f(1, fun(X) -> X + 1 end).
	
trailer1 () -> m:f(fun(X) -> X + 1 end, 1).
	
anon_pipe () -> fun (X__) -> f(g(X__)) end.

trailer_pipe () -> m:f(1, fun(X__) -> f(g(X__)) end).

trailer_pipe2 () ->
	m:f(fun(X__) -> f(g(X__)) end, 1).
	
somefuns (ModuleName) ->
	[
	fun local/1,
	fun remote:f1/1,
	fun ModuleName:f1/1].