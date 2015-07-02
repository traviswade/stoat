-module(basic).

-export([add/1]).

const () -> k.

add (X) -> X + 1.
add1 (X__) -> X__ + 1.

strings1 () -> "abc".
strings2 () -> "abc" "def".
strings3 () -> "abc" ++ "def".
strings4 () -> [$a|"bc"].

bin1 () -> <<"abc">>.

maps1 () -> #{a=>1}.
maps2 (M) -> M#{b=>2}.

to_list (X) when is_list(X) -> X;
to_list (X) when is_binary(X) -> binary_to_list(X);
to_list (X) when is_integer(X) -> integer_to_list(X).

order (X, Y) when Y < X -> {Y, X};
order (X, Y)            -> {X, Y}.
 
-record(usr, {name, email}).
name (#usr{name=N}) -> N.

noassign ({_}=A) -> 1.
