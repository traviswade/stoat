

Nonterminals
form
expr exprs expr_100 expr_150 expr_160 expr_200 expr_300 expr_400
expr_500 expr_600 expr_700 expr_800 expr_max
function function_clauses function_clause
argument_list
clause_args clause_guard clause_body
guard
atomic
add_op mult_op.

Terminals

'(' ')' ',' '->' '{' '}' '[' ']' '<-' ';'
'*' '/' 'div' 'rem' 'band' 'and'
'+' '-' 'bor' 'bxor' 'bsl' 'bsr' 'or' 'xor'
'when'
integer float atom var
dot.

Rootsymbol form.

form -> function dot : '$1'.

function -> function_clauses : build_function('$1').

function_clauses -> function_clause : ['$1'].
function_clauses -> function_clause ';' function_clauses : ['$1'|'$3'].

function_clause -> atom clause_args clause_guard clause_body : 
	{clause, ?line('$1'), element(3, '$1'), '$2', $3, '$4'}.
	
clause_args -> argument_list : element(1, '$1').

argument_list -> '(' ')' : {[],?line('$1')}.
% argument_list -> '(' exprs ')' : {'$2',?line('$1')}.

% clause_guard -> 'when' guard : '$2'.
clause_guard -> '$empty' : [].

clause_body -> '->' exprs: '$2'.

exprs -> expr : ['$1'].
% exprs -> expr ',' exprs : ['$1' | '$3'].

% expr -> 'catch' expr : {'catch',?line('$1'),'$2'}.
% expr -> expr_100 : '$1'.
expr -> integer '+' integer : ?mkop2('$1', '$2', '$3').

% expr_100 -> expr_150 '=' expr_100 : {match,?line('$2'),'$1','$3'}.
% expr_100 -> expr_150 '!' expr_100 : ?mkop2('$1', '$2', '$3').
expr_100 -> expr_150 : '$1'.

% expr_150 -> expr_160 'orelse' expr_150 : ?mkop2('$1', '$2', '$3').
expr_150 -> expr_160 : '$1'.

% expr_160 -> expr_200 'andalso' expr_160 : ?mkop2('$1', '$2', '$3').
expr_160 -> expr_200 : '$1'.

% expr_200 -> expr_300 comp_op expr_300 : ?mkop2('$1', '$2', '$3').
expr_200 -> expr_300 : '$1'.

% expr_300 -> expr_400 list_op expr_300 : ?mkop2('$1', '$2', '$3').
expr_300 -> expr_400 : '$1'.

expr_400 -> expr_400 add_op expr_500 : ?mkop2('$1', '$2', '$3').
expr_400 -> expr_500 : '$1'.

expr_500 -> expr_500 mult_op expr_600 : ?mkop2('$1', '$2', '$3').
expr_500 -> expr_600 : '$1'.

% expr_600 -> prefix_op expr_700 : ?mkop1('$1', '$2').
% expr_600 -> map_expr : '$1'.
expr_600 -> expr_700 : '$1'.

% expr_700 -> function_call : '$1'.
% expr_700 -> record_expr : '$1'.
expr_700 -> expr_800 : '$1'.

% expr_800 -> expr_max ':' expr_max : {remote,?line('$2'),'$1','$3'}.
expr_800 -> expr_max : '$1'.

expr_max -> var : '$1'.
expr_max -> atomic : '$1'.
% expr_max -> list : '$1'.
% expr_max -> binary : '$1'.
% expr_max -> list_comprehension : '$1'.
% expr_max -> binary_comprehension : '$1'.
% expr_max -> tuple : '$1'.
%%expr_max -> struct : '$1'.
expr_max -> '(' expr ')' : '$2'.
% expr_max -> 'begin' exprs 'end' : {block,?line('$1'),'$2'}.
% expr_max -> if_expr : '$1'.
% expr_max -> case_expr : '$1'.
% expr_max -> receive_expr : '$1'.
% expr_max -> fun_expr : '$1'.
% expr_max -> try_expr : '$1'.

% atomic -> char : '$1'.
atomic -> integer : '$1'.
atomic -> float : '$1'.
atomic -> atom : '$1'.
% atomic -> strings : '$1'.


mult_op -> '/' : '$1'.
mult_op -> '*' : '$1'.
mult_op -> 'div' : '$1'.
mult_op -> 'rem' : '$1'.
mult_op -> 'band' : '$1'.
mult_op -> 'and' : '$1'.

add_op -> '+' : '$1'.
add_op -> '-' : '$1'.
add_op -> 'bor' : '$1'.
add_op -> 'bxor' : '$1'.
add_op -> 'bsl' : '$1'.
add_op -> 'bsr' : '$1'.
add_op -> 'or' : '$1'.
add_op -> 'xor' : '$1'.

Erlang code.

-define(line(Tup), element(2, Tup)).
-define(mkop2(L, OpPos, R),
	io:format("trying to make op2: ~p ~p ~p ~n", [L, OpPos, R]),
        begin
            {Op,Pos} = OpPos,
            {op,Pos,Op,L,R}
        end).


build_function(Cs) ->
	io:format("'trying to build function', ~p~n", [Cs]),
	C1 = hd(Cs),
    Name = element(3, C1),
    Arity = length(element(4, C1)),
    {function, ?line(C1), Name, Arity, check_clauses(Cs, Name, Arity)}.

check_clauses(Cs, Name, Arity) ->
     mapl(fun ({clause,L,N,As,G,B}) when N =:= Name, length(As) =:= Arity ->
		 {clause,L,As,G,B};
	     ({clause,L,_N,_As,_G,_B}) ->
		 ret_err(L, "head mismatch") end, Cs).
		
ret_err(L, S) ->
    {location,Location} = get_attribute(L, location),
    return_error(Location, S).

get_attribute(L, Name) ->
    erl_scan:attributes_info(L, Name).

mapl(F, [H|T]) ->
	V = F(H),
	[V | mapl(F,T)];
mapl(_, []) ->
	[].
