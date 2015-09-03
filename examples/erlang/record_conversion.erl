
-module(record_conversion).

-record(usr, {name=bob, email}).

to_proplist (X__) -> [{name, X__#usr.name}, {email, X__#usr.email}].

list_to_usr (X__) -> #usr{name=proplists:get_value(name, X__, bob), email=proplists:get_value(email, X__)}.

to_map (X__) -> #{name=>X__#usr.name, email=>X__#usr.email}.

map_to_usr (X__) -> #usr{name=maps:get(name, X__, bob), email=maps:get(email, X__, undefined)}.
