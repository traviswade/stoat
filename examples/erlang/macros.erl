-module(macros).
-define(a, 1).
-define(wrap(A), {ok, A}).
-define(appl1(A, B), A(B)).
-define(appl2(A, B), B(A)).
-include_lib("common.hrl").

use1 () -> ?a.

wrap () -> ?wrap(1).

appl1 () -> ?appl1(fun(X)->X+1 end, 1 ).
appl2 () -> ?appl2(1, fun(X)->X+1 end). 

remote1 () -> ?somethingcommon.
remote2 (X) -> ?commonf(X).
