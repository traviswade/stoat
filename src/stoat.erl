
-module(stoat).

-compile(export_all).

-include_lib("stoat.hrl").

cli () -> stoatcli:start().

% get_expr () -> getline([], fun stoat_parse:parse_expr/1).
% get_forms () -> getline([], fun stoat_parse:parse/1).
% getline (Acc, F) ->
% 	    case io:request({get_until, unicode, '-->>', stoat_lex, token, [1]}) of
% 			{ok, {dot, Pos}, _} ->
% 				Toks = lists:reverse([{dot, Pos}|Acc]),
% 				F(Toks);
% 			{ok, Tok, _} ->
% 				getline([Tok|Acc], F);
% 		Other ->
% 			error_logger:error_msg("other: ~p~n", [Other]),
% 		    Other
% 	    end.


	
-define(br, "\n\n").
file_to_erl (Path) ->
	{ok, Forms} = parse_file(Path),
	Rootname = filename:rootname(Path),	
	stoat_util:with_file(Rootname++".forms", fun (W) ->
		[W(io_lib:format("~p~n~n", [F])) || F <- Forms] end),
	Forms1 = proc_forms(Forms, #{path=>Path}),
	stoat_util:with_file(Rootname++"1.forms", fun (W) ->
		[W(io_lib:format("~p~n~n", [F])) || F <- Forms1] end),
	stoat_util:with_file(Rootname++".erl", fun (W) ->
		[W(erl_prettypr:format(F) ++ ?br) || F <- Forms1] end),
	ok.
	
-define(default_opts, #{outpath => "./"}). 
-define(suffix, ".st").
-define(beam, ".beam").
compile (Path) -> compile(Path, ?default_opts).
compile (Path, Opts) ->
	{ok, Forms} = parse_file(Path),
	{ok, Mod, Bin} = compile:forms(Forms),
	Outpath = filename:join([maps:get(outpath, Opts), atom_to_list(Mod)++?beam]),
	file:write_file(Outpath, Bin).

parse_file (Path) ->
	{ok, Bin} = file:read_file(Path),
	Str = binary_to_list(Bin),
	{ok, Toks, Eof} = stoat_lex:string(Str),
	% THIS SHOULD NOT BE REQUIRED HERE (?)
	stoat_macros:register_module(Path, path2module(Path)),
	Opts = #{path=>Path},
	{ok, file_attrs(Path) ++  proc_forms(parse_toks(Toks), Opts) ++ [{eof, Eof}]}.
	
parse_toks (Toks) -> parse_toks(Toks, {[], []}).
parse_toks ([], {[], AccForms}) -> parse_forms(AccForms);
parse_toks ([{dot, _}=Dot|T], {AccForm, AccForms}) ->
	parse_toks(T, {[], [lists:reverse([Dot|AccForm])|AccForms]});
parse_toks ([H|T], {AccForm, AccForms}) ->
	% ?p("parsing form: ~p~n~p~n~p~n~n~n", [H, AccForm, AccForms]),
	parse_toks(T, {[H|AccForm], AccForms}).
	
parse_forms (ReversedForms) -> parse_forms(lists:reverse(ReversedForms), []).
parse_forms ([], Acc) -> lists:reverse(Acc);
parse_forms ([H|T], Acc) -> 
	% ?p("trying to parse form: ~p~n", [H]),
	{ok, Form} = stoat_parse:parse(H),
	parse_forms(T, [Form|Acc]).
	
default_opts () -> #{path=>none}.
	
path2module (Path) -> list_to_atom(filename:rootname(filename:basename(Path))).
	
file_attrs (Path) ->
	[ {attribute,1,file,{Path,1}},
	     {attribute,2,module, path2module(Path)}
		% {attribute, 3, compile, export_all}
		].
	
show_filex (Fnam) ->
	{ok, Bin} = file:read_file(Fnam),
	show(binary_to_list(Bin)).
	
show (Str) -> 
	?p("parsing: ~p~n", [Str]),
	{ok, Tokens, _} = stoat_lex:string(Str),
	?p("tokens: ~p~n", [Tokens]),
	{ok, Parsed} =  stoat_parse:parse(Tokens),
	?p("parsed: ~p~n", [Parsed]),
	Parsed.
	
version () -> "0.1".
	
%%%%%%%%%%%%%%% 
proc_forms (Forms, Opts) ->
	lists:foldl(fun(Mod, Acc) -> Mod:transform(Acc, maps:put(print, true, Opts)) end, Forms, [
		stoat_mixins]).
		