
% Use tick marks instead of quotes. '#{}' inside that
% interpolates enclosed code as plain Stoat.

f1 () -> "abc #{dontinterpolate} def".

f2 () -> `nothingtointerpolate`.

f3 () -> `abc #{1} def #{a} ghi #{<<"x">>}`.
