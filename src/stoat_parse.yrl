

Nonterminals
form
expr
function function_clauses function_clause
argument_list
clause_args clause_guard clause_body
expr_400 expr_500
guard
add_op.

Terminals

'(' ')' ',' '->' '{' '}' '[' ']' '<-' ';'
'+' '-'
'when'
integer float atom var
dot.

Rootsymbol form.

form -> function dot : '$1'.

function -> function_clauses : build_function('$1').

function_clauses -> function_clause : ['$1'].
function_clauses -> function_clause ';' function_clauses : ['$1'|'$3'].

function_clause -> atom '<-' '->' integer : 
	{clause, ?line('$1'), element(3, '$1'), ['$2'], '$3', '$4'}.

function_clause -> atom clause_args clause_guard clause_body :
	{clause, ?line('$1'), element(3, '$1'),'$2','$3','$4'}.
	
clause_args -> argument_list : element(1, '$1').

argument_list -> '<-' : $1.

% clause_guard -> 'when' guard : '$2'.
clause_guard -> '$empty' : [].

clause_body -> '->' integer: '$2'.

expr_400 -> expr_400 add_op expr_500 : ?mkop2('$1', '$2', '$3').
expr_400 -> expr_500 : '$1'.

expr_500 -> integer : $1.

% guard -> integer : $1.


add_op -> '+' : '$1'.
add_op -> '-' : '$1'.



Erlang code.

-define(line(Tup), element(2, Tup)).
-define(mkop2(L, OpPos, R),
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
