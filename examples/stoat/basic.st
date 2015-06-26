

-export([add/1]).

const () -> k.

add (X) -> X + 1.
add1 -|> _ + 1.

to_list 
(X::list)    -> X;
(X::binary)  -> binary_to_list(X);
(X::integer) -> integer_to_list(X).

order
(X, Y::(_<X)) -> {Y, X};
(X, Y)        -> {X, Y}.


