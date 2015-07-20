-module(stoat_macros).

-export([register_module/1, register_macro/2, expand_macro/1]).
-include_lib("stoat.hrl").

% Handle BASIC (about the same as erlang) macros.
% This module is a JOKE and it will be completely re-written soon.
% It was slapped together to provide a bare minimum of requirements so that 
% we can start using the language.
% It is NOT A PREPROCESSOR, but runs along with
% and gets called from the regular parser. It keeps a little bit of STATE
% in a PROCESS DICTIONARY and lots in ets tables. It requires register_module
% to be called at the beginning of a module (because there isn't a straightforward
% way to get the module info from inside the parser -- probably because we shouldn't 
% be doing that sort of thing in the parser!!), so it assumes that a whole module is
% getting compiled in the same process and that separate modules are either in different
% processes or are done sequentially.


register_module (Mod) ->
	put(mod, Mod),
	put(libs, []),
	put(macros, #{}).

register_macro ({atom, _L, K}, V) ->
	?p("setting macro: ~p TO ~p (~p)~n", [K, V, get(mod)]),
	put(macros, maps:put(K, V, get(macros))).
% register_macro ({call, _L, {atom, _, K}, Args}, V) ->
% 	?p("setting call macro: ~p~n", [K]),
% 	put(macros, maps:put({K, length(Args)}, V, get(macros))).


	
expand_macro ({atom, _L, K}) ->
	get_macro(K).
% expand_macro ({call, _L, {atom, _, K}, Args}) ->
% 	Expr = get_macro({K, length(Args)}),
% 	lists:reduce(
% 		fun (Arg, {N, Acc}) -> 
% 			{N+1, stoat_cuts:replace_var(Arg, Acc, ?is(varname(N)))} end, 
% 		Expr, 
% 		{1, Args}).
		
varname (N) -> list_to_atom("__macro_var_"++integer_to_list(N)).
	
get_macro (K) -> 
	try 
		?p("getting macro ~p from ~p (~p)~n", [K, get(macros), get(mod)]),
		maps:get(K, get(macros))
	catch _:_ ->
		throw(badmacro)
	end.
	
	
	
	
	