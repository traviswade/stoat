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

trycatch1 () -> try x+1 catch _:_ -> badarg end.
trycatch2 () ->
	try x+1 of
		A when is_integer(A) -> {ok, A};
		_ -> error
	catch error:badarith -> error
	end.
		
maps1 () -> #{a=>1}.
maps2 (M) -> M#{b=>2}.

sendrec () ->
	F = fun() -> receive N when is_integer(N) -> log({int, N}); _ -> log(other) end end,
	Pid = spawn(F),
	Pid ! 2.
	
to_list (X) when is_list(X) -> X;
to_list (X) when is_binary(X) -> binary_to_list(X);
to_list (X) when is_integer(X) -> integer_to_list(X).

order (X, Y) when Y < X -> {Y, X};
order (X, Y)            -> {X, Y}.
 
-record(usr, {name, email}).
name (#usr{name=N}) -> N.

noassign ({_}=A) -> 1.
