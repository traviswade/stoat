-module(stoat_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stoat.hrl").

-define(relpth, "../").

% file_test () -> 
% 	?assertEqual(ok, stoat:file_to_erl("../test/pipes.stoat")).
% 	% ?assertEqual(ok, stoat:file_to_erl("../test/pipes.stoat")).
	
examples_test () ->
	{ok, Fils} = file:list_dir(?relpth++"examples/stoat/"),
	[test_example(hd(string:tokens(F, "."))) || F <- Fils].

test_example (Fil) ->
	error_logger:info_msg("processing file: ~p~n", [Fil]),
	{ok, Stoat} = stoat:parse_file(?relpth ++ "examples/stoat/" ++ Fil ++ ".st"),
	{ok, Erl} = epp:parse_file(?relpth ++ "examples/erlang/" ++ Fil ++ ".erl", []),
	Fs = [stoat_util:no_lines(F) || {function, _,_,_,_}=F <- Stoat],
	Fe = [stoat_util:no_lines(F) || {function, _,_,_,_}=F <- Erl],
	% ?p("ok:: ST:~p~n~nERL:~p~n~n", [Stoat, Erl]),
	lists:foreach(fun({A, B}) -> 
			case A of B -> ok; _ ->
				error_logger:info_msg("~p IS NOT~n ~p~n", [A, B]) end,
			?assertEqual(A, B) 
		end, lists:zip(Fs, Fe)).
