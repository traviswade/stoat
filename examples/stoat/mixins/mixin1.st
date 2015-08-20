
-export([frommixin1/0]).
-export([frommixin1a/1]).

frommixin1 () -> ok.
frommixin1a (_) -> ok.

f1 (_) -> default.

overriding(inmix1) -> -2.
