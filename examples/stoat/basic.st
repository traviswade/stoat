%-
look, a
MULTILINE
comment!
-%

-export([add/1]).

const () -> k.

add (X) -> X + 1.
add1 -|> _ + 1.

strings1 () -> "abc".
strings2 () -> "abc" "def".
strings3 () -> "abc" ++ "def".
strings4 () -> [$a|"bc"].

bin1 () -> <<"abc">>.

trycatch1 () -> try x+1 catch _:_ -> badarg end.
trycatch2 () ->
	try x+1 of
		A::integer -> {ok, A};
		_ -> error
	catch error:badarith -> error
	end.

maps1 () -> #{a=>1}.
maps2 (M) -> M#{b=>2}.

sendrec () ->
	F = {|receive N::integer -> log({int, N}); _ -> log(other) end},
	Pid = spawn(F),
	Pid ! 2.

to_list (X::list)    -> X;
	(X::binary)  -> binary_to_list(X);
	(X::integer) -> integer_to_list(X).

order (X, Y::(_<X)) -> {Y, X};
	(X, Y)        -> {X, Y}.

-record(usr, {name, email}).
name (#usr{name=N}) -> N.
name1 (U) -> U#usr.name.


noassign ({_}=A) -> 1.
