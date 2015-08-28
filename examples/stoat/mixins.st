
-export([some_internal/0]).

% include all the functions (and exports) from modules in the current path with these names.
-mixin(mixin1).
-mixin(mixin2).

some_internal () -> ok.

% we can override here -- our clauses come first, followed by those of the mixins, in reverse
% order of inclusion (see mixins.erl)
overriding(intargmod) -> -1.
