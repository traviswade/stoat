-module(stoat_macros).

-export([register/2]).
-include_lib("stoat.hrl").


register (K, V) ->
	?p("registering macro: ~p (~p) ~n", [K, V]).
