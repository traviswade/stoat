
-module(mixins).

some_internal () -> ok.

frommixin1 () -> ok.
frommixin1a (bad) -> notok;
frommixin1a (_) -> ok.
frommixin2 () -> ok.

overriding(intargmod) -> -1;
overriding(inmix2) -> -3;
overriding(inmix1) -> -2.

f1 (_) -> default.

% f1 (_) -> default.