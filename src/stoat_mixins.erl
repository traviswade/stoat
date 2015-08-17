
-module(stoat_mixins).

-export([transform/2]).

-include_lib("stoat.hrl").

-record(mixin, {exports=[], functions=#{}}).

% [{attribute,2,export,[{some_internal,0}]},
%                      {attribute,mixin,mixin1},
%                      {function,7,some_internal,0,
%                                [{clause,7,[],[],[{atom,7,ok}]}]}]


transform (Forms, Opts) ->
	ensure_processed(Forms, Opts),
	Forms.
	
ensure_processed ([], _) -> ok;
ensure_processed ([{attribute, mixin, Mod}|T], Opts) ->
	proc_mixin(Mod, Opts),
	ensure_processed(T, Opts);
ensure_processed ([H|T], Opts) -> ensure_processed(T, Opts).
	
proc_mixin (Mod, Opts) ->
	Path = filename:dirname(maps:get(path, Opts)),
	catch ets:new(mixins, [set, named_table, public]),
	case ets:lookup(mixins, Mod) of
		[_] -> ok;
		[] ->
			Fil = stoat_util:find_file(stoat_util:to_l(Mod)++".st", Path),
			{ok, Forms} = stoat:parse_file(Fil),
			catch ets:insert(mixins, {Mod, forms_to_mixin(Forms, #mixin{})})
	end.
	
forms_to_mixin ([], Acc) -> Acc;
forms_to_mixin ([{attribute, _, export, Exp1}|T], #mixin{exports=Exp0}=M) ->
	forms_to_mixin(T, M#mixin{exports=Exp0 ++ Exp1});
forms_to_mixin ([{function, _, Fnam, Arity, Clauses}|T], #mixin{functions=F}=M) ->
	forms_to_mixin(T, M#mixin{functions=maps:put({Fnam, Arity}, Clauses, F)});
forms_to_mixin ([_|T], M) ->
	forms_to_mixin(T, M).

% proc_forms ([{attribute, mixin, Mod}|T], #state{}=S) % TODO: 