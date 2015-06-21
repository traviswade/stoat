-module(basic).
-compile(export_all).
-export([add/1]).

const () -> k.

add (X) -> X + 1.
add1 (X__) -> X__ + 1.

to_list (X) when is_list(X) -> X;
to_list (X) when is_binary(X) -> binary_to_list(X);
to_list (X) when is_integer(X) -> integer_to_list(X).


order (X, Y) when Y < X -> {Y, X};
order (X, Y)            -> {X, Y}.