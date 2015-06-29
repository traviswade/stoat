-module(stoat_cuts).

% -export([transform/1]).

-export([find_var/1, replace_underscore/2, replace_var/3, expr2fun/1]).


% find the (first-ish) variable in an argument
% for example to use in a cut, and return the (atom) variale name
find_var ({var, _, V}=Var) -> 
	case is_var(Var) of {true, Var} -> Var; _ -> false
	end;
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
	
is_var ({var, _, V}=Var) when is_atom(V) -> 
	case atom_to_list(V) of
		"_"++_ -> false;
		_ -> {true, Var}
	end.
	
expr2fun (Expr) ->
	L = stoat_util:line(Expr),
	Var = {var, L, 'X__'},
	case replace_var(Var, Expr, fun is__/1) of
		{true, Expr1} ->
			{'fun', L, {clauses, [{clause, L, [Var], [], [Expr1]}]}};
		_ ->
			{'fun', L, {clauses, [{clause, L, [], [], [Expr]}]}}
	end.
	
replace_underscore(Var, Node) -> replace_var(Var, Node, fun is__/1).

replace_var (Var, {cons, Line, H, T}=A, F) ->
	case replace_var(Var, H, F) of
		{true, H1} -> {true, {cons, Line, H1, T}};
		_ -> case replace_var(Var, T, F) of
			{true, T1} -> {true, {cons, Line, H, T1}};
			_ -> {false, A}
		end
	end;
replace_var (Var, {tuple, Line, Elems}=T, F) ->
	case replace_var(Var, Elems, F) of
		{true, Elems1} -> {true, {tuple, Line, Elems1}};
		_ -> {false, T}
	end;
replace_var (Var, {record_field, Line, Field, Val}=A, F) ->
	case replace_var(Var, Val, F) of
		{true, Val1} -> {true, {record_field, Line, Field, Val1}};
		_ -> {false, A}
	end;
replace_var (Var, [H|T], F) ->
	case replace_var(Var, H, F) of
		{true, H1} -> {true, [H1|T]};
		_ -> case replace_var(Var, T, F) of
			{true, T1} -> {true, [H|T1]};
			_ -> {false, [H|T]}
		end
	end;
replace_var (Var, {record, Line, Record, Fields}=A, F) ->
	case replace_var(Var, Fields, F) of
		{true, Fields1} -> {true, {record, Line, Record, Fields1}};
		_ -> {false, A}
	end;
replace_var (Var, {op, Line, Op, X, Y}=A, F) ->
	case replace_var(Var, X, F) of 
		{true, X1} -> {true, {op, Line, Op, X1, Y}};
		_ -> case replace_var(Var, Y, F) of
			{true, Y1} -> {true, {op, Line, Op, X, Y1}};
			_ -> {false, A}
		end
	end;
replace_var (Var, {var, Line, A}, F) ->
	case F(A) of 
		true -> {true, Var}; % TODO: replace line in var with Line
		_ -> {false, {var, Line, A}}
	end;
replace_var (Var, {call, L, Op, Args}=Call, F) ->
	case replace_var(Var, Op, F) of
		{true, Op1} -> {true, {call, L, Op1, Args}};
		_ -> case replace_var(Var, Args, F) of
			{true, Args1} -> {true, {call, L, Op, Args1}};
			_ -> {false, Call}
		end
	end;
replace_var (_, X, _) -> {false, X}.

is__ (A) when is_atom(A) -> is__(atom_to_list(A));
is__ ("_"++_) -> true;
is__ (_) -> false.

-include_lib("eunit/include/eunit.hrl").
find_var_test () ->
	F = fun(Str) -> {var, _, A} = find_var(stoat_util:str2expr(Str)), A end,
	?assertEqual('A', F("A")),
	?assertEqual('A', F("[A|_]")),
	?assertEqual('A', F("[_|A]")),
	?assertEqual('A', F("#r{a=A}")),
	?assertEqual('A', F("{_, A}")).
	
replace_var_test () ->
	F1 = fun(Str) -> stoat_util:str2expr(Str) end,
	F2 = fun(Str) -> replace_underscore({var, 1, 'A'}, F1(Str)) end,
	?assertEqual({true, F1("A>1")}, F2("_>1")),
	?assertEqual({false, F1("A>1")}, F2("A>1")).

