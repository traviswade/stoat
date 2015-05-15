
-module(stoat).

-compile(export_all).

-include_lib("stoat.hrl").


show (Str) ->
	?p("parsing: ~p~n", [Str]),
	{ok, Tokens, _} = stoat_lex:string(Str),
	?p("tokens: ~p~n", [Tokens]),
	{ok, Parsed} =  stoat_parse:parse(Tokens),
	?p("parsed: ~p~n", [Parsed]),
	Parsed.
	
file_to_erl (Path) ->
	file_to_erl(Path, filename:rootname(Path) ++ ".erl").
file_to_erl (Path, OutPath) ->
	{ok, Forms} = parse_file(Path),
	Erl = [erl_prettypr:format(F) || F <- Forms],
	file:write_file(OutPath, string:join(Erl, "\n\n")).
	
parse_file (Path) ->
	{ok, Bin} = file:read_file(Path),
	Str = binary_to_list(Bin),
	{ok, Toks, _} = stoat_lex:string(Str),
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
	
% show_file (Fnam) ->
% 	{ok, F} = file:open(Fnam, [read]),
% 	Tokens = loop(F, []),
% 	file:close(F),
% 	?p("tokens: ~p~n", Tokens),
% 	{ok, Parsed} = stoat_parse:parse(Tokens),
% 	?p("parsed: ~p~n", [Parsed]),
% 	Parsed.
% 	
% loop (F, Acc) ->
% 	case io:request(F, {get_until, prompt, stoat_lex, token, [1]}) of
% 		{ok, Tok, EndLine} ->
% 			?p("got token: ~p~n", [Tok]),
% 			loop(F, Acc++[Tok]);
% 		{error, token} ->
% 			exit(scanning_error);
% 		{eof, _} ->
% 			Acc
% 	end.