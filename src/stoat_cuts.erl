-module(stoat_cuts).

-export([transform/1]).

-export([find_cut/1, find_var/1]).

-define(p(X), error_logger:info_msg("~p~n", X)).

transform ({call, L, Op, Args}=In) ->
	case check_args(Args) of
		{[], _} -> 
			In;
		{Missing, Args1} ->
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
	
find_cut (Expr) -> ok.

% find the (first-ish) variable in an argument
% for example to use in a cut
find_var ({var, _, V}) -> V;
find_var ({cons, _, H, T}) ->
	case is_var(H) of {true, V} -> V;_ -> find_var(T)
	end;
find_var ({record, _, _, Fields}) -> find_var(Fields);
find_var ([{record_field, _, _, F}|T]) -> 
	case is_var(F) of {true, V} -> V; _ -> find_var(T)
	end;
find_var ({tuple, _, Elements}) -> find_var(Elements);
find_var ([H|T]) ->
	case is_var(H) of {true, V} -> V; _ -> find_var(T)
	end.
	
is_var ({var, _, V}) when is_atom(V) -> 
	case atom_to_list(V) of
		"_"++_ -> false;
		_ -> {true, V}
	end.

