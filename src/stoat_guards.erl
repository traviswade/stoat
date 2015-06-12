
-module(stoat_guards).

-export([compose_guards/1]).

% guards is always empty for now. we will probably never use
% them in that position so we could just clean them out.
compose_guards (GuardedArgs) ->
	compose_guards (GuardedArgs, {[], []}).
	
compose_guards ([], {Args, Guards}) -> {lists:reverse(Args), lists:reverse(Guards)};
compose_guards ([{Arg, GuardSpecs}|T], {AccArgs, AccGuards}) ->
	compose_guards (T, {[Arg|AccArgs], [proc_guard(Arg, G) || G <- GuardSpecs] ++ AccGuards}).
	% compose_guards(T, {[Arg|AccArgs], []}).
	
proc_guard (Arg, {atom, Line, Atom}) ->
	F = list_to_atom("is_" ++ atom_to_list(Atom)),
	{call,Line,{atom, Line, F},[{var,Line,stoat_cuts:find_var(Arg)}]};
proc_guard (Arg, Expr) ->
	case stoat_cuts:replace_underscore(Arg, Expr) of
		{true, Expr1} -> Expr1;
		_ -> throw("bad guard expression")
	end.