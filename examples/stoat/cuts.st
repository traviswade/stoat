
% An expression with an underscore on the right side of a
% pipe expression works like a function replacing that position
% with its argument.
% Cuts in this position are ALWAYS inlined (see erlang equivalents).

f1 |> _.

f2 |> _ + 1.

f3 |> f1(_) + 1 + _.

f4 |> {_, _}.

f5 |> _().

f6 |> #{a=>_}.

f7 |> #{_=>1}.

f8 |> _#{a=>1}.

f9 |> _ + _.

f10 |> _(_).

f11 |> [_|[_]].

f12 |> #usr{login=_, email=_}.

f13 |> _#usr{login= <<"admin">>}.

f14 |> _#usr.name.

f15 |> _ == 1.

f16 |> <<"X", _/binary>>.

% f1b -|> @.
