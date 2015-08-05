-module(stoat_macros).

-export([register_module/2, register_macro/2, expand_macro/1, handle_incl/1]).
-include_lib("stoat.hrl").

% Handle BASIC (about the same as erlang) macros.
% This module is a JOKE and it will be completely re-written soon.
% It was slapped together to provide a bare minimum of requirements so that 
% we can start using the language. It is NOT A PREPROCESSOR, but runs along with
% and gets called from the regular parser, because that was a lot easier than trying to recreate epp. 
% It keeps a little bit of STATE in the PROCESS DICTIONARY, because I don't see a good way to make code
% running in the context of a yecc compile aware of the macro values otherwise.
% (We could at least set up a proper set of gen_servers though.) 
% This is certain to cause problems. Like for example it can never be parallelized.
% Anyway this all requires stoat_macros:register_module
% to be called at the beginning of a module (because there isn't a straightforward
% way to get the module info from inside the parser -- probably because we shouldn't 
% be doing that sort of thing in the parser!!), so it assumes that a whole module is
% getting compiled in the same process and that separate modules are either in different
% processes or are done sequentially.

-record(state, {mod, path, libs, macros}).
-define(update_state(Expr), update_state(fun(S)->Expr end)).

update_state (F) -> put(macro_state, F(get_state())).
get_state () -> case get(macro_state) of 
	#state{}=S -> S; 
	_ -> #state{} 
end.
get_macros () -> (get_state())#state.macros.

register_module (Path, Mod) ->
	?update_state(S#state{mod=Mod, path=filename:dirname(Path), libs=[], macros=#{}}).

register_macro ({atom, _L, K}, V) ->
	?update_state(S#state{macros=maps:put(K, V, S#state.macros)});
register_macro ({call, _L, {atom, _, K}, Args}, V) ->
	?update_state(S#state{macros=maps:put({K, length(Args)}, {V, Args}, S#state.macros)}).
	
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
	S = get_state(),
	try 
		maps:get(K, S#state.macros)
	catch error:bad_key ->
		find_macro(K, S#state.libs)
	end.
find_macro (_, [])    -> throw(badmacro);
find_macro (K, [H|T]) -> 
	try
		[{H, M}] = ets:lookup(macros, H),
		case maps:find(K, M) of
			error -> find_macro(K, T);
			{ok, V} -> V
		end
	catch error:badarg -> throw(badinclude)
	end.
	
ensure_compiled (Incl) ->
	catch ets:new(macros, [set, named_table, public]),
	case ets:lookup(macros, Incl) of
		[_] -> ok;
		[] ->
			Fil = stoat_util:find_file(stoat_util:to_l(Incl)++".ht", (get_state())#state.path),
			CachedState = get_state(),
			stoat:parse_file(Fil),
			catch ets:insert(macros, {Incl, get_macros()}),
			?update_state(CachedState),
			ok
		end.
				
handle_incl (Incl) ->
	ensure_compiled(Incl),
	?update_state(S#state{libs=[Incl, S#state.libs]}).
	
	
	
	
	
	
	