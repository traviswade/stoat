-module(stoat_cuts).

-export([transform/1]).

-define(p(X), error_logger:info_msg("~p~n", X)).


transform ({call, L, Op, Args}=In) ->
	?p({"transforming", L, Op, Args}),
	case check_args(Args) of
		{[], _} -> 
			In;
		{Missing, Args1} ->
			?p({cutting, {'fun', L, {clauses, [{clause, L, Missing, [], [{call, L, Op, Args1}]}]}}}),
			{'fun', L, {clauses, [{clause, L, Missing, [], [{call, L, Op, Args1}]}]}}
	end.
		
check_args (Args) -> check_args(Args, {[], []}).

check_args ([], {Missing, Args1}) -> 
	{lists:reverse(Missing), lists:reverse(Args1)};
	
check_args ([{var, L, '_'}|T], {Missing, Args1}) ->
	DummyArg = {var, L, list_to_atom("Arg__" ++ integer_to_list(length(Missing)))},
	check_args(T, {[DummyArg|Missing], [DummyArg|Args1]});
	
check_args ([Arg|T], {Missing, Args1}) ->
	check_args(T, {Missing, [Arg|Args1]}).
