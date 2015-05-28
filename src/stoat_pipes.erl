-module(stoat_pipes).

-export([process_forms/1, proc_pipe/1]).

-import(erl_syntax, [type/1]).

proc_pipe (In) -> 
	error_logger:info_msg("processing pipe: ~p~n", [In]),
	{atom, 1, c}.

process_forms (Forms) ->
	erl_syntax:revert_forms(do_transforms(erl_syntax:form_list(Forms))).
	
do_transforms (Tree) -> 
	transform(case erl_syntax:subtrees(Tree) of
		[] -> Tree;
		L ->
			Subtrees = [[do_transforms(T) || T<-Group] || Group<-L],
			erl_syntax:update_tree(Tree, Subtrees)
	end).

transform (Node) -> 
	case type(Node) of
		infix_expr ->
			case erl_syntax:operator_name(erl_syntax:infix_expr_operator(Node)) of
				'|>' -> do_application(Node);
				_    -> Node
			end;
		_ -> Node
	end.
	
do_application (Node) ->
	[Val, _Op, F] = lists:append(erl_syntax:subtrees(Node)),
	erl_syntax:application(F, [Val]).
