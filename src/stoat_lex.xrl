
Definitions.

D = [0-9]
S = [\s\t\n]
H = [0-9a-fA-F]


Rules.


[A-Z_][0-9a-zA-Z_]* : {token, {var, TokenLine, list_to_atom(TokenChars)}}.

"([^"\\]|\\.)*"      : {token, {string, TokenLine, chars(strip(TokenChars, TokenLen))}}.
`([^`\\]|\\.)*`      : {token, {sstring, TokenLine, chars(strip(TokenChars, TokenLen))}}.



\|\+              : {token, {list_to_atom(TokenChars), TokenLine}}.
\|-               : {token, {list_to_atom(TokenChars), TokenLine}}.
\|>               : {token, {list_to_atom(TokenChars), TokenLine}}.
<\|               : {token, {list_to_atom(TokenChars), TokenLine}}.
\|\)              : {token, {list_to_atom(TokenChars), TokenLine}}.
\|/               : {token, {list_to_atom(TokenChars), TokenLine}}.
\|<               : {token, {list_to_atom(TokenChars), TokenLine}}.
\|:               : {token, {list_to_atom(TokenChars), TokenLine}}.
\|{               : {token, {list_to_atom(TokenChars), TokenLine}}.
\?\[              : {token, {list_to_atom(TokenChars), TokenLine}}.
\?\{              : {token, {list_to_atom(TokenChars), TokenLine}}.
@                 : {token, {list_to_atom(TokenChars), TokenLine}}.
{\|               : {token, {list_to_atom(TokenChars), TokenLine}}.

\?                : {token, {list_to_atom(TokenChars), TokenLine}}.

->                : {token, {list_to_atom(TokenChars), TokenLine}}.

::                : {token, {list_to_atom(TokenChars), TokenLine}}.
;;                : {token, {'end', TokenLine}}.

<-                : {token, {list_to_atom(TokenChars), TokenLine}}.
=>                : {token, {list_to_atom(TokenChars), TokenLine}}.
:=                : {token, {list_to_atom(TokenChars), TokenLine}}.
<<                : {token, {list_to_atom(TokenChars), TokenLine}}.
>>                : {token, {list_to_atom(TokenChars), TokenLine}}.
\|\|              : {token, {list_to_atom(TokenChars), TokenLine}}.
[\(\){}\[\];,\|]  : {token, {list_to_atom(TokenChars), TokenLine}}.
\+\+              : {token, {list_to_atom(TokenChars), TokenLine}}.
==                : {token, {list_to_atom(TokenChars), TokenLine}}.
=:=               : {token, {list_to_atom(TokenChars), TokenLine}}.
=/=               : {token, {list_to_atom(TokenChars), TokenLine}}.
>=                : {token, {list_to_atom(TokenChars), TokenLine}}.
=<                : {token, {list_to_atom(TokenChars), TokenLine}}.
[+\-*/=:\&<>#~\!] : {token, {list_to_atom(TokenChars), TokenLine}}.

\%.*\n            : skip_token.
\%-([^\%\\]|\\.)*-\% : skip_token.

\.{               : {token, {list_to_atom(TokenChars), TokenLine}}.
\.{S}             : {end_token, {dot, TokenLine}}.
\.                : {token, {list_to_atom(TokenChars), TokenLine}}.

andalso           : {token, {list_to_atom(TokenChars), TokenLine}}.
orelse            : {token, {list_to_atom(TokenChars), TokenLine}}.
fn                : {token, {list_to_atom(TokenChars), TokenLine}}.
end               : {token, {list_to_atom(TokenChars), TokenLine}}.
try               : {token, {list_to_atom(TokenChars), TokenLine}}.
catch             : {token, {list_to_atom(TokenChars), TokenLine}}.
of                : {token, {list_to_atom(TokenChars), TokenLine}}.
after             : {token, {list_to_atom(TokenChars), TokenLine}}.
receive           : {token, {list_to_atom(TokenChars), TokenLine}}.


[a-z][0-9a-zA-Z_]*  : {token, {atom, TokenLine, list_to_atom(TokenChars)}}.

\$.              : {token, {char, TokenLine, lists:nth(2, TokenChars)}}.

{D}+  : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
(\+|-)?[0-9]+\.[0-9]+((E|e)(\+|-)?[0-9]+)? : {token, {float, TokenLine, list_to_float(TokenChars)}}.

{S}+  : skip_token.

Erlang code.

strip (TokenChars, TokenLen) -> lists:sublist(TokenChars, 2, TokenLen-2).


%% COMPLETELY LIFTED FROM rvirding/lfe !

base1([C|Cs], Base, SoFar) when C >= $0, C =< $9, C < Base + $0 ->
    Next = SoFar * Base + (C - $0),
    base1(Cs, Base, Next);
base1([C|Cs], Base, SoFar) when C >= $a, C =< $z, C < Base + $a - 10 ->
    Next = SoFar * Base + (C - $a + 10),
    base1(Cs, Base, Next);
base1([C|Cs], Base, SoFar) when C >= $A, C =< $Z, C < Base + $A - 10 ->
    Next = SoFar * Base + (C - $A + 10),
    base1(Cs, Base, Next);
base1([C|Cs], _Base, SoFar) -> {SoFar,[C|Cs]};
base1([], _Base, N) -> {N,[]}.

chars([$\\,$x,C|Cs0]) ->
    case hex_char(C) of
        true ->
            case base1([C|Cs0], 16, 0) of
                {N,[$;|Cs1]} -> [N|chars(Cs1)];
                _Other -> [escape_char($x)|chars([C|Cs0])]
            end;
        false -> [escape_char($x)|chars([C|Cs0])]
    end;
chars([$\\,C|Cs]) -> [escape_char(C)|chars(Cs)];
chars([C|Cs]) -> [C|chars(Cs)];
chars([]) -> [].

hex_char(C) when C >= $0, C =< $9 -> true;
hex_char(C) when C >= $a, C =< $f -> true;
hex_char(C) when C >= $A, C =< $F -> true;
hex_char(_) -> false.

escape_char($b) -> $\b;                %\b = BS
escape_char($t) -> $\t;                %\t = TAB
escape_char($n) -> $\n;                %\n = LF
escape_char($v) -> $\v;                %\v = VT
escape_char($f) -> $\f;                %\f = FF
escape_char($r) -> $\r;                %\r = CR
escape_char($e) -> $\e;                %\e = ESC
escape_char($s) -> $\s;                %\s = SPC
escape_char($d) -> $\d;                %\d = DEL
escape_char(C) -> C.