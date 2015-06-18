
-module(stoat).

-compile(export_all).

-include_lib("stoat.hrl").


get_expr () -> getline([], fun stoat_parse:parse_expr/1).
get_forms () -> getline([], fun stoat_parse:parse/1).
getline (Acc, F) ->
	    case io:request({get_until, unicode, '-->>', stoat_lex, token, [1]}) of
			{ok, {dot, Pos}, _} ->
				Toks = lists:reverse([{dot, Pos}|Acc]),
				F(Toks);
			{ok, Tok, _} ->
				getline([Tok|Acc], F);
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
	% ?p("toks: ~p~n", [Toks]),
	% error_logger:info_msg("tokens: ~p~n", [Toks]),
	
	{ok, file_attrs(Path) ++  parse_toks(Toks)}.
	
parse_toks (Toks) -> parse_toks(Toks, {[], []}).
parse_toks ([], {[], AccForms}) -> parse_forms(AccForms);
parse_toks ([{dot, _}=Dot|T], {AccForm, AccForms}) ->
	parse_toks(T, {[], [lists:reverse([Dot|AccForm])|AccForms]});
parse_toks ([H|T], {AccForm, AccForms}) ->
	% ?p("parsing form: ~p~n~p~n~p~n888888888888888888~n~n", [H, AccForm, AccForms]),
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
	
	
%%%%%%%%%%%%%%% 
proc_forms (Forms) -> Forms.
	% lists:foldl(fun(Mod, Acc) -> Mod:process_forms(Acc) end, Forms, [
	% 	stoat_pipes]).
		