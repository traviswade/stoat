-module(stoat_rebar).

-export([compile/2]).

compile(Config, _AppFile) ->
	Targ = filename:join(rebar_utils:get_cwd(), "src"),
	case true andalso filelib:is_dir(Targ) of
		true ->
			io:format("~~ Compiling stoat files: ~p -~n", [Targ]),
			filelib:fold_files(Targ, "\\.st", true, fun(Path, _Acc) -> compile_file(Path) end, []);
		_ -> io:format("skipping directory: ~p~n", [Targ])
	end,
	ok.
	
% TODO: JUST COMPILING EVERY TIME! USE MAKE OR CHECK TIMESTAMPS
compile_file (Path) ->
	Outpath = stoat_util:ebin_dir(Path),
	filelib:ensure_dir(filename:join([Outpath, "out.ebin"])),
	stoat:compile(Path, #{outpath => Outpath}).