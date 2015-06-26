-module(stoat_rebar).

-export([pre_compile/2]).

-spec pre_compile(Config::rebar_config:config(), AppFile::file:filename()) -> 'ok'.
pre_compile(Config, _AppFile) ->
	io:format(",hello: ~p", ["S-T-O-A-T"]).