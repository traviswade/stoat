-module(stoat_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stoat.hrl").

-define(relpth, "../").

% file_test () -> 
% 	?assertEqual(ok, stoat:file_to_erl("../test/pipes.stoat")).
% 	% ?assertEqual(ok, stoat:file_to_erl("../test/pipes.stoat")).
	
examples_test () ->
	{ok, Fils} = file:list_dir(?relpth++"examples/stoat/"),
	[test_example(F) || [F, "st"] <- [string:tokens(F, ".") || F <- Fils]].

test_example (Fil) ->
	error_logger:info_msg("processing file: ~p~n", [Fil]),
	{ok, Stoat} = stoat:parse_file(?relpth ++ "examples/stoat/" ++ Fil ++ ".st"),
	{ok, Erl} = epp:parse_file(?relpth ++ "examples/erlang/" ++ Fil ++ ".erl", []),
	Fs = sort_by_name([{F, stoat_util:form2erl(F)} || {function, _,_,_,_}=F <- Stoat]),
	Fe = sort_by_name([{F, stoat_util:form2erl(F)} || {function, _,_,_,_}=F <- Erl]),
	length(Fs) =:= length(Fe),
	lists:foreach(fun({{Fa, A}, {Fb, B}}) -> 
			% ?p("testing: ~p~n~p~n~n---~n", [A, B]),
			case A of B -> ok; _ ->
					error_logger:info_msg("~p ~nIS NOT~n ~p~n", [A, B]),
					error_logger:info_msg("~p ~nIS NOT~n ~p~n", [Fa, Fb])
				end,
			?assertEqual(A, B) 
		end, lists:zip(Fs, Fe)).

sort_by_name (Funs) ->
	lists:sort(
		fun ({A,_}, {B,_}) -> {element(3,A), element(4, A)} >= {element(3, B), element(4, B)} end, 
		Funs).