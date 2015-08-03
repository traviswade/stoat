-module(stringinterp).

f1 () -> "abc #{dontinterpolate} def".

f2 () -> "nothingtointerpolate".

f3 () -> "abc " ++ stoat_util:to_l(1) ++ " def " ++ stoat_util:to_l(a) ++ " ghi " ++ stoat_util:to_l(<<"x">>) ++ "".
