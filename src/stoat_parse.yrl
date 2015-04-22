

Nonterminals
form
expr exprs expr_100 expr_150 expr_160 expr_200 expr_300 expr_400
expr_500 expr_600 expr_700 expr_800 expr_max
function function_clauses function_clause function_name
argument_list
clause_args clause_guard clause_body
guard
atomic strings list tail 
binary bin_elements bin_element bit_size_expr 
bit_expr opt_bit_size_expr opt_bit_type_list bit_type bit_type_list
add_op mult_op prefix_op list_op comp_op.

Terminals

'(' ')' ',' '->' '{' '}' '[' ']' '<-' ';' '|' '<<' '>>' ':' '!'
'==' '=:=' '=/=' '<' '>' '>=' '=<' '/='
'++' '--'
'*' '/' 'div' 'rem' 'band' 'and'
'andalso' 'orelse'
'+' '-' 'bor' 'bxor' 'bsl' 'bsr' 'or' 'xor' 'bnot' 'not'
'='
'when'
char integer float atom string var

dot.

Rootsymbol form.

form -> function dot : '$1'.

function -> function_clauses : build_function('$1').

function_clauses -> function_clause : ['$1'].
function_clauses -> function_clause ';' function_clauses : ['$1'|'$3'].

function_clause -> atom clause_args clause_guard clause_body : 
	{clause, ?line('$1'), element(3, '$1'), '$2', $3, '$4'}.
% will fail in build_function in first position
function_clause -> clause_args clause_guard clause_body :
	show({noname_clause, ?line(hd('$3')), noname, '$1', '$2', '$3'}).
	
	
clause_args -> argument_list : element(1, '$1').

argument_list -> '(' ')' : {[],?line('$1')}.
argument_list -> '(' exprs ')' : {'$2',?line('$1')}.

clause_guard -> 'when' guard : '$2'.
clause_guard -> '$empty' : [].

clause_body -> '=' exprs: '$2'.

exprs -> expr : ['$1'].
exprs -> expr ',' exprs : ['$1' | '$3'].

guard -> exprs : ['$1'].
guard -> exprs ';' guard : ['$1'|'$3'].

% expr -> 'catch' expr : {'catch',?line('$1'),'$2'}.
expr -> expr_100 : '$1'.

% no assignment!
% expr_100 -> expr_150 '=' expr_100 : {match,?line('$2'),'$1','$3'}.
expr_100 -> expr_150 '!' expr_100 : ?mkop2('$1', '$2', '$3').
expr_100 -> expr_150 : '$1'.

expr_150 -> expr_160 'orelse' expr_150 : ?mkop2('$1', '$2', '$3').
expr_150 -> expr_160 : '$1'.

expr_160 -> expr_200 'andalso' expr_160 : ?mkop2('$1', '$2', '$3').
expr_160 -> expr_200 : '$1'.

expr_200 -> expr_300 comp_op expr_300 : ?mkop2('$1', '$2', '$3').
expr_200 -> expr_300 : '$1'.

expr_300 -> expr_400 list_op expr_300 : ?mkop2('$1', '$2', '$3').
expr_300 -> expr_400 : '$1'.

expr_400 -> expr_400 add_op expr_500 : ?mkop2('$1', '$2', '$3').
expr_400 -> expr_500 : '$1'.

expr_500 -> expr_500 mult_op expr_600 : ?mkop2('$1', '$2', '$3').
expr_500 -> expr_600 : '$1'.

expr_600 -> prefix_op expr_700 : ?mkop1('$1', '$2').
% expr_600 -> map_expr : '$1'.
expr_600 -> expr_700 : '$1'.

% expr_700 -> function_call : '$1'.
% expr_700 -> record_expr : '$1'.
expr_700 -> expr_800 : '$1'.

% expr_800 -> expr_max ':' expr_max : {remote,?line('$2'),'$1','$3'}.
expr_800 -> expr_max : '$1'.

expr_max -> var : '$1'.
expr_max -> atomic : '$1'.
expr_max -> list : '$1'.
expr_max -> binary : '$1'.
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

list -> '[' ']' : {nil,?line('$1')}.
list -> '[' expr tail : {cons,?line('$1'),'$2','$3'}.

tail -> ']' : {nil,?line('$1')}.
tail -> '|' expr ']' : '$2'.
tail -> ',' expr tail : {cons,?line('$2'),'$2','$3'}.

binary -> '<<' '>>' : {bin,?line('$1'),[]}.
binary -> '<<' bin_elements '>>' : {bin,?line('$1'),'$2'}.

bin_elements -> bin_element : ['$1'].
bin_elements -> bin_element ',' bin_elements : ['$1'|'$3'].

bin_element -> bit_expr opt_bit_size_expr opt_bit_type_list :
	{bin_element,?line('$1'),'$1','$2','$3'}.

bit_expr -> prefix_op expr_max : ?mkop1('$1', '$2').
bit_expr -> expr_max : '$1'.

opt_bit_size_expr -> ':' bit_size_expr : '$2'.
opt_bit_size_expr -> '$empty' : default.

opt_bit_type_list -> '/' bit_type_list : '$2'.
opt_bit_type_list -> '$empty' : default.

bit_type_list -> bit_type '-' bit_type_list : ['$1' | '$3'].
bit_type_list -> bit_type : ['$1'].

bit_type -> atom             : element(3,'$1').
bit_type -> atom ':' integer : { element(3,'$1'), element(3,'$3') }.

bit_size_expr -> expr_max : '$1'.

% atomic -> char : '$1'.
atomic -> integer : '$1'.
atomic -> float : '$1'.
atomic -> atom : '$1'.
atomic -> strings : '$1'.

% what is this for?
strings -> string : '$1'.
strings -> string strings :
	{string,?line('$1'),element(3, '$1') ++ element(3, '$2')}.

prefix_op -> '+' : '$1'.
prefix_op -> '-' : '$1'.
prefix_op -> 'bnot' : '$1'.
prefix_op -> 'not' : '$1'.

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

list_op -> '++' : '$1'.
list_op -> '--' : '$1'.

comp_op -> '==' : '$1'.
comp_op -> '/=' : '$1'.
comp_op -> '=<' : '$1'.
comp_op -> '<' : '$1'.
comp_op -> '>=' : '$1'.
comp_op -> '>' : '$1'.
comp_op -> '=:=' : '$1'.
comp_op -> '=/=' : '$1'.

Erlang code.

-define(line(Tup), element(2, Tup)).
-define(mkop1(OpPos, A),
        begin
            {Op,Pos} = OpPos,
            {op,Pos,Op,A}
        end).
-define(mkop2(L, OpPos, R),
        begin
            {Op,Pos} = OpPos,
            {op,Pos,Op,L,R}
        end).
-define(p, error_logger:info_msg).


build_function(Cs) ->
	C1 = hd(Cs),
    Name = element(3, C1),
    Arity = length(element(4, C1)),
    {function, ?line(C1), Name, Arity, check_clauses(Cs, Name, Arity)}.

check_clauses(Cs, Name, Arity) -> [check_clause(C, Name, Arity) || C <- Cs].
check_clause ({clause, L, Name, As, G, B}, Name, Arity) when length(As) =:= Arity ->
		{clause, L, As, G, B};
check_clause ({noname_clause, L, _, As, G, B}, Name, Arity) when length(As) =:= Arity ->
		{clause, L, As, G, B};
check_clause ({clause,L,_N,_As,_G,_B}, _, _) ->
		ret_err(L, "head mismatch").
		
ret_err(L, S) ->
    {location,Location} = get_attribute(L, location),
    return_error(Location, S).

get_attribute(L, Name) ->
    erl_scan:attributes_info(L, Name).
	
show (Item) ->
	?p("show: ~p", [Item]),
	Item.
