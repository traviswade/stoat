
-module(stoat_mixins).

-export([transform/2]).

-include_lib("stoat.hrl").

-record(mixin, {exports=[], functions=#{}}).

% [{attribute,2,export,[{some_internal,0}]},
%                      {attribute,mixin,mixin1},
%                      {function,7,some_internal,0,
%                                [{clause,7,[],[],[{atom,7,ok}]}]}]


transform (Forms, Path) ->
	% ?p("transforming forms: ~p~n", [forms_to_mixin(Forms, #mixin{})]),
	ensure_processed(Forms, Path),
	Forms.
	
ensure_processed ([], _) -> ok;
ensure_processed ([{attribute, mixin, Mod}|T], Path) ->
	proc_mixin(Mod, Path),
	ensure_processed(T, Path);
ensure_processed ([H|T], Path) -> ensure_processed(T, Path).
	
proc_mixin (Mod, Path) ->
	catch ets:new(mixins, [set, named_table, public]),
	case ets:lookup(mixins, Mod) of
		[_] -> ok;
		[] ->
			?p("looking for file ~p In ~p~n", [stoat_util:to_l(Mod)++".st", Path]),
			Fil = stoat_util:find_file(stoat_util:to_l(Mod)++".st", Path),
			{ok, Forms} = stoat:parse_file(Path),
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