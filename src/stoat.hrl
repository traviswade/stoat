
% -define(p(Fmt, Args), error_logger:info_msg(Fmt++"~n", [Args])).
% -define(p(Arg), ?p(Arg, [])).
-define(p, error_logger:info_msg).

-define(is(X), fun(MaybeX)->MaybeX=:=X end).