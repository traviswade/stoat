-module(stoat_macros).

-export([register_module/2, register_macro/2, expand_macro/1, handle_incl/1]).
-include_lib("stoat.hrl").

% Handle BASIC (about the same as erlang) macros.
% This module is a JOKE and it will be completely re-written soon.
% It was slapped together to provide a bare minimum of requirements so that 
% we can start using the language. It is NOT A PREPROCESSOR, but runs along with
% and gets called from the regular parser, because that was a lot easier than recreating epp. 
% It keeps a little bit of STATE in the PROCESS DICTIONARY, because I don't see a good way to make code
% running in the context of a yecc compile aware of the macro values otherwise.
% (We could at least set up a proper set of gen_servers though.) 
% This is certain to cause problems.
% Anyway this all requires stoat_macros:register_module
% to be called at the beginning of a module (because there isn't a straightforward
% way to get the module info from inside the parser -- probably because we shouldn't 
% be doing that sort of thing in the parser!!), so it assumes that a whole module is
% getting compiled in the same process and that separate modules are either in different
% processes or are done sequentially.


register_module (Path, Mod) ->
	put(mod, Mod),
	put(path, filename:dirname(Path)),
	put(libs, []),
	put(macros, #{}).

register_macro ({atom, _L, K}, V) ->
	put(macros, maps:put(K, V, get(macros)));
register_macro ({call, _L, {atom, _, K}, Args}, V) ->
	put(macros, maps:put({K, length(Args)}, {V, Args}, get(macros))).
	
expand_macro ({atom, _L, K}) ->
	get_macro(K);
expand_macro ({call, _L, {atom, _, K}, Args}) ->
	{Expr, OrigArgs} = get_macro({K, length(Args)}),
	lists:foldl(
		fun ({Arg, {var, _, Orig}}, Acc) -> 
			element(2, stoat_cuts:replace_var(Arg, Acc, ?is(Orig))) end, 
		Expr, 
		lists:zip(Args, OrigArgs)).
			
get_macro (K) -> 
	try 
		maps:get(K, get(macros))
	catch _:_ ->
		throw(badmacro)
	end.
	
handle_incl (F) ->
	case maps:find(F, get(macros)) of
		error ->
			F1 = misc:to_l(F) ++ ".ht",
			?p("running macro: ~p (~p)~n", [F, misc:find_file(misc:to_l(F1), get(path))]);
		{ok, _} ->
			?p("already ran macro ~p~n", F)
		end.
	
	
	
	
	
	