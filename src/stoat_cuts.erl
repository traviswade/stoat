-module(stoat_cuts).

% -export([transform/1]).

-export([find_var/1, replace_underscore/2, replace_var/3, expr2fun/1, maybe_expr2fun/1]).
-include_lib("stoat.hrl").


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
	
maybe_expr2fun (Expr) ->
	L = stoat_util:line(Expr),
	Var = {var, L, 'X__'},
	case replace_var(Var, Expr, fun is__/1) of
		{true, Expr1} ->
			{'fun', L, {clauses, [{clause, L, [Var], [], [Expr1]}]}};
		_ -> Expr
	end.
	
expr2fun (Expr) ->
	L = stoat_util:line(Expr),
	Var = {var, L, 'X__'},
	case replace_var(Var, Expr, fun isat/1) of
		{true, Expr1} ->
			{'fun', L, {clauses, [{clause, L, [Expr1], [], [Var]}]}};
		_ -> case replace_var(Var, Expr, fun is__/1) of
			{true, Expr1} ->
				{'fun', L, {clauses, [{clause, L, [Var], [], [Expr1]}]}};
			_ ->
				{'fun', L, {clauses, [{clause, L, [], [], [Expr]}]}}
		end
	end.
	
% TODO: GET RID OF THIS once we replace immediately applied funs.
replace_underscore(Var, Node) -> 
	case replace_var(Var, Node, fun isat/1) of
		{true, Expr1} -> {true, Expr1};
		_ ->  replace_var(Var, Node, fun is__/1)
	end.

-define(replraw(Item), element(2, replace_var(Var, Item, F))).
replace_var (Var, {cons, Line, H, T}=A, F) ->
	case replace_var(Var, H, F) of
		{true, H1} -> {true, {cons, Line, H1, ?replraw(T)}};
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
		{true, Val1} -> {true, {record_field, Line, ?replraw(Field), Val1}};
		_ -> case replace_var(Var, Field, F) of
				{true, Field1} -> {true, {record_field, Line, Field1, Val}};
				_ -> {false, A}
			end
	end;
replace_var (Var, {record_field, Line, RecordVar, RecordName, Field}=R, F) ->
	case replace_var(Var, RecordVar, F) of
		{true, RecordVar1} -> {true, {record_field, Line, RecordVar1, RecordName, Field}};
		_ ->
			case replace_var(Var, Field, F) of
				{true, Field1} -> {true, {record_field, Line, RecordVar, RecordName, Field1}};
				_ -> {false, R}
			end
	end;
	
replace_var (Var, L, F) when is_list(L) ->
	{Vals, Replaced} = lists:unzip(
		[replace_var(Var, Item, F) || Item <- L]),
	case lists:member(true, Vals) of
		true -> {true, Replaced}; _ -> {false, L} 
	end;
replace_var (Var, {'case', L, Head, Clauses}=C, F) ->
	case replace_var(Var, Head, F) of
		{true, Head1} -> {true, {'case', L, Head1, Clauses}};
		_ -> {false, C}
	end;
replace_var (Var, {'if', L, [{clause, L, [], Cond, Y}, Else]}=If, F) ->
	case replace_var(Var, Cond, F) of
		{true, Cond1} -> {true, {'if', L, [{clause, L, [], Cond1, Y}, Else]}};
		_ -> {false, If}
	end;
replace_var (Var, {record, Line, Head, Record, Fields}=Rec, F) ->
	case replace_var(Var, Head, F) of
		{true, Head1} ->
			{true, {record, Line, Head1, ?replraw(Record), ?replraw(Fields)}};
		_ -> case replace_var(Var, Record, F) of
				{true, Record1} -> {true, {record, Line, Head, Record1, ?replraw(Fields)}};
				_ -> case replace_var(Var, Fields, F) of
						{true, Fields1} -> {true, {record, Line, Head, Record, Fields1}};
					_	 -> {false, Rec}
					end
			end
	end;
replace_var (Var, {bin, Line, Elements}=Bin, F) ->
	case replace_var(Var, Elements, F) of
		{true, Elements1} ->
			{true, {bin, Line, Elements1}};
		_ -> {false, Bin}
	end;
replace_var (Var, {bin_element, Line, Targ, default, [binary]}=El, F) ->
	case replace_var(Var, Targ, F) of
		{true, Targ1} -> {true, {bin_element, Line, Targ1, default, [binary]}};
		_ -> {false, El}
	end;

replace_var (Var, {record, Line, Record, Fields}=A, F) ->
	case replace_var(Var, Fields, F) of
		{true, Fields1} -> {true, {record, Line, Record, Fields1}};
		_ -> {false, A}
	end;
replace_var (Var, {map, Line, Head, Fields}=M, F) ->
	case replace_var(Var, Head, F) of
		{true, Head1} -> {true, {map, Line, Head1, ?replraw(Fields)}};
		_ -> case replace_var(Var, Fields, F) of
			{true, Fields1} -> {true, {map, Line, Head, Fields1}};
			_ -> {false, M}
		end
	end;
replace_var (Var, {map,Line,Fields}=M, F) ->
	case replace_var(Var, Fields, F) of
		{true, Fields1} -> {true, {map, Line, Fields1}};
		_ -> {false, M}
	end;
replace_var (Var, {map_field_assoc,Line,K,V}=M, F) ->
	case replace_var(Var, K, F) of
		{true, K1} -> {true, {map_field_assoc, Line, K1, ?replraw(V)}};
		_ -> case replace_var(Var, V, F) of
			{true, V1} -> {true, {map_field_assoc, Line, K, V1}};
			_ -> {false, M}
		end
	end;
replace_var (Var, {op, Line, Op, X, Y}=A, F) ->
	case replace_var(Var, X, F) of 
		{true, X1} -> {true, {op, Line, Op, X1, ?replraw(Y)}};
		_ -> case replace_var(Var, Y, F) of
			{true, Y1} -> {true, {op, Line, Op, X, Y1}};
			_ -> {false, A}
		end
	end;
replace_var (Var, {var, Line, A}, F) ->
	case F(A) of 
		true -> {true, Var};
		_ -> {false, {var, Line, A}}
	end;
replace_var (Var, {call, L, Op, Args}=Call, F) ->
	case replace_var(Var, Op, F) of
		{true, Op1} -> {true, {call, L, Op1, ?replraw(Args)}};
		_ -> case replace_var(Var, Args, F) of
			{true, Args1} -> {true, {call, L, Op, Args1}};
			_ -> {false, Call}
		end
	end;
replace_var (_, X, _) -> {false, X}.

is__ (V) -> V=='_'.

isat (V) -> V == '@'.

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

