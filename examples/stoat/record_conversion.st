


-record(usr, {name=bob, email}).

to_proplist |> ?usr_to_list(_).

list_to_usr |> ?list_to_usr(_).

to_map |> ?usr_to_map(_).

map_to_usr |> ?map_to_usr(_).
