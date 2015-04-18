-module(stoat_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stoat.hrl").


print (Arg) -> print(Arg, []).
print (Arg, Fmt) -> error_logger:info_msg(Arg++"~n", Fmt).

a_test () ->
	stoat:show("a () = 1 + 2."),
	stoat:show("a () = 1 + 2; () = 2 + 1."),
	stoat:show("a () = 1 + 2; a () = 2 + 1.").
	% a = 1.
