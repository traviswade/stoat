-module(cuts).

f1 (X__) -> X__.

f2 (X__) -> X__ + 1.

f3 (X__) -> f1(X__) + 1 + X__.

f4 (X__) -> {X__, X__}.

f5 (X__) -> X__().

f6 (X__) -> #{a => X__}.

f7 (X__) -> #{X__ => 1}.

f8 (X__) -> X__#{a => 1}.

f9 (X__) -> X__ + X__.

f10 (X__) -> X__(X__).

f11 (X__) -> [X__|[X__]].

f12 (X__) -> #usr{login=X__, email=X__}.

f13 (X__) -> X__#usr{login= <<"admin">>}.

f14 (X__) -> X__#usr.name.

% f1b (X__) -> X__.

% f4b (X__) -> 