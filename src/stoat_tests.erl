-module(stoat_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stoat.hrl").




file_test () -> ?assertEqual(ok, stoat:file_to_erl("../test/one.stoat")).
	

