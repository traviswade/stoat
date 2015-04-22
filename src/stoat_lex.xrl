
Definitions.

D = [0-9]
S = [\s\t]


Rules.



[A-Z_][0-9a-zA-Z_]* : {token, {var, TokenLine, TokenChars}}.

\.          : {token, {dot, TokenLine}}.
->          : {token, {list_to_atom(TokenChars), TokenLine}}.
<-          : {token, {list_to_atom(TokenChars), TokenLine}}.
|>          : {token, {list_to_atom(TokenChars), TokenLine}}.
[\(\){};,]  : {token, {list_to_atom(TokenChars), TokenLine}}.
[+\-*/=]    : {token, {list_to_atom(TokenChars), TokenLine}}.


andalso     : {token, {list_to_atom(TokenChars), TokenLine}}.
orelse      : {token, {list_to_atom(TokenChars), TokenLine}}.
[a-z][0-9a-zA-Z_]*  : {token, {atom, TokenLine, TokenChars}}.

"[0-9a-zA-Z_]*"     : {token, {string, TokenLine, strip(TokenChars, TokenLen)}}.

{D}+  : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
(\+|-)?[0-9]+\.[0-9]+((E|e)(\+|-)?[0-9]+)? : {token, {float, TokenLine, list_to_float(TokenChars)}}.

{S}+  : skip_token.

Erlang code.

strip (TokenChars, TokenLen) -> lists:sublist(TokenChars, 2, TokenLen-2).