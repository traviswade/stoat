

f1 (X::integer, Y::(integer, _>X)) -> ok.

f2 () -> {X::integer| X+1}.

f3 () -> {X::(_>4) | X+1; _ | 4}.