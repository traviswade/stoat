-module(stoat_mixins).

-export([transform/2]).

-include_lib("stoat.hrl").

% rearranged forms for easy lookup
-record(mixin, {exports=[], functions=#{}}).

% [{attribute,2,export,[{some_internal,0}]},
%                      {attribute,mixin,mixin1},
%                      {function,7,some_internal,0,
%                                [{clause,7,[],[],[{atom,7,ok}]}]}]

% [{mixin1,{mixin,[{frommixin1,1},{frommixin1a,1}],
%                                     #{{frommixin1,0} => [{clause,5,[],[],[{atom,5,ok}]}],
%                                       {frommixin1a,0} => [{clause,6,[],[],[{atom,6,ok}]}]}}}]
% 

transform (Forms, Opts) ->
	mix_in_mixins(
		gather_mixins(Forms, Opts, #mixin{}), 
		Forms, 
		forms_to_mixins(Forms), 
		[]).
	 
% fin.
mix_in_mixins (#{functions=#{}}, [], _, AccForms) -> lists:reverse(AccForms);

% exports. group all the exports together and place them where the FIRST export occurred.
mix_in_mixins (#{exports=[_|_]=Exp}=Mixins, [{attribute, L, export, _}|T], #{exports=ExpOrig}=Formatted, AccForms) ->
	mix_in_mixins(
		Mixins#mixin{exports=[]}, 
		T, 
		Formatted, 
		[{attribute, L, export, lists:usort(Exp++ExpOrig)}|AccForms]);
% (and discard any further export statements)
mix_in_mixins (Mixins, [{attribute, _, export, _}|T], Formatted, AccForms) ->
	mix_in_mixins(Mixins, T, Formatted, AccForms);
	
% a function. add any mixin clauses and remove it from the mixin set
mix_in_mixins (#mixin{functions=Funs}=Mixins, [{function, L, FNam, Arity, Clauses}=F|T], Formatted, AccForms) ->
	case maps:find({FNam, Arity}, Funs) of
		{ok, Clauses1} ->
			mix_in_mixins(
				Mixins#mixin{functions=maps:without([{FNam, Arity}], Funs)},
				T,
				Formatted,
				[{function, L, FNam, Arity, Clauses1++Clauses}|AccForms]);
		error -> mix_in_mixins(Mixins, T, Formatted, [F|AccForms]);
		
% anything else in the input forms. just add it as is.
mix_in_mixins (Mixins, [Other|T], Formatted, AccForms) ->
	mix_in_mixins(Mixins, T, Formatted, [Other|AccForms]);
	
% input forms are exhausted. add in the rest of the mixed in functions
mix_in_mixins (#mixins{functions=Funs}=Mixins, [], Formatted, AccForms) ->
	mix_in_mixins(
		Mixins#{functions=#{}}, 
		[], 
		Formatted, 
		maps:fold(
			fun({FNam, Arity}, Cs, Acc) -> [{function, 0, Fnam, Arity, Cs}|Acc] end,
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
	