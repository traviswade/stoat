-module(stoat_macros).

-export([register_module/2, register_macro/2, expand_macro/1, handle_incl/1, register_record/4, transform/2]).
-include_lib("stoat.hrl").

% Handle BASIC (about the same as erlang) macros and do lots of nice things with records.
% This macro and import handling is a JOKE and it will be completely re-written soon.
% It was slapped together to provide a bare minimum of requirements so that 
% we can start using the language. It is NOT A PREPROCESSOR, but runs along with
% and gets called from the regular parser, because that was a lot easier than trying to recreate epp. 
% It keeps a little bit of STATE in the PROCESS DICTIONARY, because I don't see a good way to make code
% running in the context of a yecc compile aware of the macro values otherwise.
% This is certain to cause problems. Like for example it can never be parallelized.
% Anyway this all requires stoat_macros:register_module
% to be called at the beginning of a module (because there isn't a straightforward
% way to get the module info from inside the parser -- probably because we shouldn't 
% be doing this sort of thing in the parser), so it assumes that a whole module is
% getting compiled in the same process and that separate modules are either in different
% processes or are done sequentially.

-record(state, {mod, path, libs, macros, records}).
-define(update_state(Expr), update_state(fun(S)->Expr end)).

update_state (F) -> put(macro_state, F(get_state())).
get_state () -> case get(macro_state) of 
	#state{}=S -> S; 
	_ -> #state{} 
end.
get_macros () -> (get_state())#state.macros.
get_records () -> (get_state())#state.records.

register_module (Path, Mod) ->
	?update_state(S#state{mod=Mod, path=filename:dirname(Path), libs=[], macros=#{}, records=[]}).

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
	catch ets:new(records, [set, named_table, public]),
	case ets:lookup(macros, Incl) of
		[_] -> ok;
		[] ->
			Fil = stoat_util:find_file(stoat_util:to_l(Incl)++".ht", (get_state())#state.path),
			CachedState = get_state(),
			stoat:parse_file(Fil),
			catch ets:insert(macros, {Incl, get_macros()}),
			catch ets:insert(records, {Incl, get_records()}),
			?update_state(CachedState),
			ok
		end.
		
register_record (RecordName, {tuple, L, Fields}=RT, Opts, Raw) ->
	?update_state(S#state{records=[Raw|S#state.records]}),
	Arg = {var, L, 'X__'},	
	register_macro({call, L, {atom, L, list_to_atom(atom_to_list(RecordName)++"_to_list")}, [Arg]}, 
		record_to_list(Fields, RecordName, Arg, L)),
	register_macro({call, L, {atom, L, list_to_atom("list_to_"++atom_to_list(RecordName))}, [Arg]}, 
		list_to_record(Fields, RecordName, Arg, L)),
	register_macro({call, L, {atom, L, list_to_atom(atom_to_list(RecordName)++"_to_map")}, [Arg]},
		record_to_map(Fields, RecordName, Arg, L)),
	register_macro({call, L, {atom, L, list_to_atom("map_to_"++atom_to_list(RecordName))}, [Arg]}, 
		map_to_record(Fields, RecordName, Arg, L)).
	
record_to_list([], _, _, L) -> {nil, L};
record_to_list ([H|T], RecordName, Arg, L) -> 
	F = record_field_name(H),
	{cons, L, 
		{tuple, L, [F, {record_field, L, Arg, RecordName, F}]}, 
		record_to_list(T, RecordName, Arg, L)}.
		
list_to_record (Fields, RecordName, Arg, L) ->
	{record, L, RecordName, lists:foldl(fun ({match, L, Field, Default}, Acc) ->		
				[{record_field, L, Field, {call, L,
				      {remote,L,{atom,L,proplists},{atom,L,get_value}},
				      [Field, Arg, Default]}}|Acc];
			(Field, Acc) ->
				[{record_field, L, Field, {call, L,
				      {remote,L,{atom,L,proplists},{atom,L,get_value}},
				      [Field, Arg]}}|Acc]
			end,
			[], lists:reverse(Fields))}.
			
record_to_map (Fields, RecordName, Arg, L) ->
	{map, L, 
		[{map_field_assoc, L, record_field_name(F), 
			{record_field, L, Arg, RecordName, record_field_name(F)}} || F <- Fields]}.
			
map_to_record (Fields, RecordName, Arg, L) ->
	{record, L, RecordName, lists:foldl(fun ({match, L, Field, Default}, Acc) ->		
				[{record_field, L, Field, {call, L,
				      {remote,L,{atom,L,maps},{atom,L,get}},
				      [Field, Arg, Default]}}|Acc];
			(Field, Acc) ->
				[{record_field, L, Field, {call, L,
				      {remote,L,{atom,L,maps},{atom,L,get}},
				      [Field, Arg, {atom, L, undefined}]}}|Acc]
			end,
			[], lists:reverse(Fields))}.

	
record_field_name ({match, _L, Field, _Default}) -> Field;
record_field_name (Field) -> Field.

% This gets called during parsing, from stoat_parse.yrl. Make sure
% the included file has been parsed (and its mixins cached) and mark it as included			
handle_incl (Incl) ->
	ensure_compiled(Incl),
	?update_state(S#state{libs=[Incl, S#state.libs]}).

% And this is done after parsing is complete. Just prepend all the records from included files.
transform (Forms, _Opts) ->
	lists:foldl(fun (Lib, Acc) -> ets:lookup(records, Lib) ++ Acc end,
		Forms, (get_state())#state.libs).
	
	
	
	
	
	
	