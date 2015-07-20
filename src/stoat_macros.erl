-module(stoat_macros).

-export([register_module/1, register_macro/2, expand_macro/1]).
-include_lib("stoat.hrl").

% Handle basic (about the same as erlang) macros.
% This module is a JOKE and it will be completely re-written soon.
% It was slapped together to provide a bare minimum of requirements so that 
% we can start using the language.
% It is not a preprocessor, but runs along with
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
	
expand_macro ({atom, _L, K}) ->
	?p("trying to expand macro : ~p~n", [K]),
	try
		maps:get(K, get(macros))
	catch _:_ ->
		% throw(badmacro)
		{integer, 1, 1}
	end.