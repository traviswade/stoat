-module(stoat_inline_fun).
-include_lib("stoat.hrl").

% inline single-clause, single-expression funs that are immediately evaluated.
% we generate a lot of these and the erlang compiler doesn't seem to convert them.
% 
% -export([transform/1]).
% -import(erl_syntax, [type/1]).
% 
% transform (Forms) ->
% 	erl_syntax:revert_forms(do_transforms(erl_syntax:form_list(Forms))).
% 	
% do_transforms (Tree) -> 
% 	inline_funs(case erl_syntax:subtrees(Tree) of
% 		[] -> Tree;
% 		L -> 
% 			erl_syntax:update_tree(Tree, [[do_transforms(T) || T <- Group] || Group <- L])
% 	end).
% 	
% inline_funs (Node) -> 
% 	case type(Node) of
% 		application ->
% 			Op = erl_syntax:application_operator(Node),
% 			case type(Op) of
% 				fun_expr ->
% 					case erl_syntax:fun_expr_arity(Op) of
% 						1 ->
% 							Cl = erl_syntax:fun_expr_clauses(Op),
% 							MaxExprs = lists:max([length(erl_syntax:clause_body(C)) || C <- Cl]),
% 							case {length(Cl), MaxExprs} of
% 								{1, 1} ->
% 									[Var] = erl_syntax:application_arguments(Node),
% 									Ptns = erl_syntax:clause_patterns(hd(Cl)),
% 									% ?p("ONE CLAUSE (simple inline): ~p~n", [Ptns]),
% 									Node;
% 								{_, 1} ->
% 									% ?p("MORE THAN ONE (case clause)~p~n", [Cl]),
% 									Node;
% 								_ ->
% 									% ?p("More than one expr (un-inlineable): ~p~n", [Cl]),
% 									Node
% 							end;
% 						_ -> Node
% 					end;
% 				_ -> Node
% 			end;
% 		_ ->  Node
% 	end.
	