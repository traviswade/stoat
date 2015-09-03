-module(stoat_mixins).

-export([transform/2]).

-include_lib("stoat.hrl").

% rearranged forms for easy lookup
-record(mixin, {exports=[], functions=#{}}).

transform (Forms, Opts) ->
	mix_in_mixins(
		gather_mixins(Forms, Opts, #mixin{}), 
		Forms, 
		forms_to_mixin(Forms), 
		[]).
	 

% exports. group all the exports together and place them where the FIRST export occurred.
% (and discard any further export statements)
mix_in_mixins (#mixin{exports=ignore}=Mixins, [{attribute, _, export, _}|T], Formatted, AccForms) ->
	mix_in_mixins(Mixins, T, Formatted, AccForms);
mix_in_mixins (#mixin{exports=Exp}=Mixins, 
	[{attribute, L, export, _}|T], #mixin{exports=ExpOrig}=Formatted, AccForms) ->
	mix_in_mixins(
		Mixins#mixin{exports=ignore}, 
		T, 
		Formatted, 
		[{attribute, L, export, lists:usort(Exp++ExpOrig)}|AccForms]);

% a function. add any mixin clauses and remove it from the mixin set
mix_in_mixins (#mixin{functions=Funs}=Mixins, [{function, L, FNam, Arity, Clauses}=F|T], Formatted, AccForms) ->
	case maps:find({FNam, Arity}, Funs) of
		{ok, Clauses1} ->
			mix_in_mixins(
				Mixins#mixin{functions=maps:without([{FNam, Arity}], Funs)},
				T,
				Formatted,
				[{function, L, FNam, Arity, Clauses++Clauses1}|AccForms]);
		error -> mix_in_mixins(Mixins, T, Formatted, [F|AccForms])
	end;
		
% anything else in the input forms. just add it as is.
mix_in_mixins (Mixins, [Other|T], Formatted, AccForms) ->
	mix_in_mixins(Mixins, T, Formatted, [Other|AccForms]);
	
% input forms are exhausted. add in the rest of the mixed in functions
mix_in_mixins (#mixin{functions=Funs}=Mixins, [], Formatted, AccForms) ->
	lists:reverse(maps:fold(
			fun({FNam, Arity}, Cs, Acc) -> [{function, 0, FNam, Arity, Cs}|Acc] end,
			AccForms,
			Funs)).

% create one super mixin to fold into the actual module.
gather_mixins ([], _, Acc) -> Acc;
gather_mixins ([{attribute, mixin, Mod}|T], Opts, #mixin{exports=AccExp, functions=AccFuns}) ->
	#mixin{exports=Exp, functions=Funs} = get_mixin(Mod, Opts),
	Funs1 = maps:fold(fun(K, V, Acc) ->
			case maps:find(K, Acc) of
				{ok, V1} ->  maps:update(K, V++V1, Acc);
				error    -> maps:put(K, V, Acc)
			end
		end,
		AccFuns, Funs),
	gather_mixins(T, Opts, #mixin{exports=lists:usort(AccExp++Exp), functions=Funs1});
gather_mixins ([_|T], Opts, Acc) -> gather_mixins(T, Opts, Acc).
	
get_mixin (Mod, Opts) ->
	Path = filename:dirname(maps:get(path, Opts)),
	catch ets:new(mixins, [set, named_table, public]),
	case ets:lookup(mixins, Mod) of
		[{Mod, Mixin}] -> Mixin;
		[] ->
			Fil = stoat_util:find_file(stoat_util:to_l(Mod)++".st", Path),
			{ok, Forms} = stoat:parse_file(Fil),
			Mixin = forms_to_mixin(Forms),
			catch ets:insert(mixins, {Mod, Mixin}),
			Mixin
	end.
	
forms_to_mixin (Forms) -> forms_to_mixin(Forms, #mixin{}).
forms_to_mixin ([], Acc) -> Acc;
forms_to_mixin ([{attribute, _, export, Exp1}|T], #mixin{exports=Exp0}=M) ->
	forms_to_mixin(T, M#mixin{exports=Exp0 ++ Exp1});
forms_to_mixin ([{function, _, Fnam, Arity, Clauses}|T], #mixin{functions=F}=M) ->
	forms_to_mixin(T, M#mixin{functions=maps:put({Fnam, Arity}, Clauses, F)});
forms_to_mixin ([_|T], M) ->
	forms_to_mixin(T, M).
	