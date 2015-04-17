
-define(p(Arg, Fmt), error_logger:info_msg(Arg++"~n", [Fmt])).
-define(p(Arg), ?p(Arg, [])).