
% macros look pretty much like in Erlang. Use the 
% attributes 'def' and 'incl' to define and include.
% Macros are currently evaluated at parse time.
% Which is weird but has not much consequence for
% simple macros.

-def(a, 1).
-def(wrap(A), {ok, A}).
-def(appl1(A, B), A(B)).
-def(appl2(A, B), B(A)).

-incl(common).

use1 () -> ?a.

wrap () -> ?wrap(1).

appl1 () -> ?appl1({|X | X+1}, 1).
appl2 () -> ?appl2(1, {|X | X+1}).

remote1 () -> ?somethingcommon.
remote2 (X) -> ?commonf(X).
