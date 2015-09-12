
Definitions.

D = [0-9]
S = [\s\t\n]


Rules.


[A-Z_][0-9a-zA-Z_]* : {token, {var, TokenLine, list_to_atom(TokenChars)}}.

"([^"\\]|\\.)*"      : {token, {string, TokenLine, strip(TokenChars, TokenLen)}}.
`([^`\\]|\\.)*`      : {token, {sstring, TokenLine, strip(TokenChars, TokenLen)}}.



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