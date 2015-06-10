
-module(stoat).

-compile(export_all).

-include_lib("stoat.hrl").


getline () ->
	    case io:request({get_until, unicode, '-->>', scanny, tokens, [1]}) of
		{ok, Toks, EndPos} ->
			error_logger:error_msg("something: ~p~n", [Toks]),
		    case stoat_parse:parse(Toks) of
		    	{ok,Exprs} -> {ok,Exprs,EndPos};
		    	{error,E} -> {error,E,EndPos}
		     end;
		Other ->
			error_logger:error_msg("other: ~p~n", [Other]),
		    Other
	    end.

show (Str) -> 
	?p("parsing: ~p~n", [Str]),
	{ok, Tokens, _} = stoat_lex:string(Str),
	?p("tokens: ~p~n", [Tokens]),
	{ok, Parsed} =  stoat_parse:parse(Tokens),
	?p("parsed: ~p~n", [Parsed]),
	Parsed.
	
-define(br, "\n\n").
file_to_erl (Path) ->
	{ok, Forms} = parse_file(Path),
	Rootname = filename:rootname(Path),	
	misc:with_file(Rootname++".forms", fun (W) ->
		[W(io_lib:format("~p~n~n", [F])) || F <- Forms] end),
	Forms1 = proc_forms(Forms),
	misc:with_file(Rootname++"1.forms", fun (W) ->
		[W(io_lib:format("~p~n~n", [F])) || F <- Forms1] end),
	misc:with_file(Rootname++".erl", fun (W) ->
		[W(erl_prettypr:format(F) ++ ?br) || F <- Forms1] end),
	ok.
	
parse_file (Path) ->
	{ok, Bin} = file:read_file(Path),
	Str = binary_to_list(Bin),
	{ok, Toks, _} = stoat_lex:string(Str),
	% error_logger:info_msg("tokens: ~p~n", [Toks]),
	{ok, file_attrs(Path) ++  parse_toks(Toks)}.
	
parse_toks (Toks) -> parse_toks(Toks, {[], []}).
parse_toks ([], {[], AccForms}) -> parse_forms(AccForms);
parse_toks ([{dot, _}=Dot|T], {AccForm, AccForms}) ->
	parse_toks(T, {[], [lists:reverse([Dot|AccForm])|AccForms]});
parse_toks ([H|T], {AccForm, AccForms}) ->
	parse_toks(T, {[H|AccForm], AccForms}).
	
parse_forms (ReversedForms) -> parse_forms(ReversedForms, []).
parse_forms ([], Acc) -> Acc;
parse_forms ([H|T], Acc) -> 
	{ok, Form} = stoat_parse:parse(H),
	parse_forms(T, [Form|Acc]).
	
file_attrs (Path) ->
	[ %{attribute,1,file,{Path,1}},
	     {attribute,2,module,list_to_atom(filename:rootname(filename:basename(Path)))},
		{attribute, 3, compile, export_all}].
	
show_filex (Fnam) ->
	{ok, Bin} = file:read_file(Fnam),
	show(binary_to_list(Bin)).
	
str2expr (Str) ->
	{ok, Toks, _} = erl_scan:string(Str ++ "."),
	{ok, [Expr]} = erl_parse:parse_exprs(Toks),
	Expr.
	
%%%%%%%%%%%%%%% 
proc_forms (Forms) -> Forms.
	% lists:foldl(fun(Mod, Acc) -> Mod:process_forms(Acc) end, Forms, [
	% 	stoat_pipes]).
		