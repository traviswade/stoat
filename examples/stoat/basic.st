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


% function calls. we can do them like normal:
funcall1 (X) -> f(X).

% and for single-argument functions, we can omit the parentheses.
funcall2 (X) -> f X.

% but watchout: function application has a high precedence:
funcall3 (X) -> f X + 1.

% to fix this you can either put the argument in parentheses or use the
% the backward pipe operator, which has a low precedence
funcall4 (X) -> f <| X + 1.


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
-def(a(X),[X]).
-record(usr, {name=abc, email}).
name (#usr{name=N}) -> N.
name1 (U) -> U#usr.name.


noassign ({_}=A) -> 1.
