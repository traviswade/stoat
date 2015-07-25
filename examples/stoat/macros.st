
-def(a, 1).
-def(wrap(A), {ok, A}).
-def(appl1(A, B), A(B)).
-def(appl2(A, B), B(A)).

use1 () -> ?a.

wrap () -> ?wrap(1).

appl1 () -> ?appl1({X | X+1}, 1).
appl2 () -> ?appl2(1, {X | X+1}).