
-module(stoatcli).

-include_lib("stoat.hrl").


start () -> getline().

getline () ->
	getline([], fun (Cmd) ->
			case stoat_parse:parse_expr(Cmd) of
				{ok, Expr} ->
					?p("good job: ~p~n", [Expr]);
				Err -> ?p("try again: ~p~n", [Err])
			end,
			getline() 
		end).

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