%-
look, a
MULTI. LINE.
comment!
-%

% exports work just like erlang.
-export([add/1]).

% and functions can be written in the same way
const () -> k.

add (X) -> X + 1.

% for single-clause, single-argument functions whose body is a pipe call
% there is no need to include the argument list or '->' 
add1 |> _ + 1.

% pretty much like Erlang (plus interpolation -- see stringinterp.st)
strings1 () -> "abc".
strings2 () -> "abc" "def".
strings3 () -> "abc" ++ "def".
strings4 () -> [$a|"bc"].

% binary
bin1 () -> <<"abc">>.

% try catch is likely to change in the future
trycatch1 () -> try x+1 catch _:_ -> badarg end.
trycatch2 () ->
	try x+1 of
		A::integer -> {ok, A};
		_ -> error
	catch error:badarith -> error
	end.

% maps are the same as r17.5 -- still missing some syntax.
maps1 () -> #{a=>1}.
maps2 (M) -> M#{b=>2}.

sendrec () ->
	F = {|receive N::integer -> log({int, N}); _ -> log(other) end},
	Pid = spawn(F),
	Pid ! 2.

% see guards examples for the argument type-looking syntax
to_list (X::list)    -> X;
	(X::binary)  -> binary_to_list(X);
	(X::integer) -> integer_to_list(X).

order (X, Y::(_<X)) -> {Y, X};
	(X, Y)        -> {X, Y}.

-record(usr, {name, email}).
name (#usr{name=N}) -> N.
name1 (U) -> U#usr.name.


noassign ({_}=A) -> 1.
