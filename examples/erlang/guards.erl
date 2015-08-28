-module(guards).

f1 (X, Y) when is_integer(X), is_integer(Y), Y > X -> ok.

f2 () -> fun(X) when is_integer(X) -> X + 1 end.

f3 () -> fun(X) when X > 4 -> X + 1; (_) -> 4 end.

f4 (X__) when is_integer(X__) -> X__ + 1.