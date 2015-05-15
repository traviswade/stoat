-module(stoat_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stoat.hrl").


print (Arg) -> print(Arg, []).
print (Arg, Fmt) -> error_logger:info_msg(Arg++"~n", Fmt).

% a_test () ->
% 	% stoat:show("a () = 1 + 2."),
% 	% stoat:show("a () = (1 + 2) * 3; () = 2 + 1."),
% 	% stoat:show("a () = 1 + 2; a () = 2 + 1."),
% 	% stoat:show("a (X) = \"A\\\"\", X."),
% 	% stoat:show("a () = A andalso B."),
% 	% stoat:show("a () = f(x)."),
% 	% stoat:show("a () = {1, 2}."),
% 	% stoat:show("a () = [A||A<-B]."),
% 	% stoat:show("a () = fn a:b/1."),
% 	% stoat:show("a () = fn () -> 1 end."),
% 	% stoat:show("a () = &() -> 1."),
% 	% stoat:show("a () = fn (a, b) -> 2 end."),
% 	stoat:show("b () = &(a) -> 1.").
	
file_test () -> ?assertEqual(ok, stoat:file_to_erl("../test/one.stoat")).
	

