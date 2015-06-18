

Nonterminals
form
expr exprs expr_100 expr_150 expr_160 expr_180 expr_200 expr_300 expr_400
expr_500 expr_600 expr_700 expr_800 expr_max
list_comprehension lc_expr lc_exprs
binary_comprehension
record_expr record_fields record_tuple record_field
fun_expr fun_clause fun_clauses fun_clause_body
fun_argument_list
short_fun_clause
atom_or_var integer_or_var
function function_clauses function_clause function_name
argument_list arg_exprs arg_expr arg_guards
clause_args clause_guard clause_body
guard
function_call call_argument_list
tuple
atomic strings list tail 
binary bin_elements bin_element bit_size_expr 
bit_expr opt_bit_size_expr opt_bit_type_list bit_type bit_type_list
add_op mult_op prefix_op list_op comp_op 
pipe pipe_op pipe_call pipe_calls pipe_bindings pipe_binding.

Terminals
char integer float atom string var
'(' ')' ',' '->' '{' '}' '[' ']' '<-' ';' '|' '<<' '>>' ':' '!'
'<=' '||' '=>' '&' '#' '.' 
'|>' '|+' '|-' '|)' '|/' '|m' '|:' '~' '|{'
'::'
'==' '=:=' '=/=' '<' '>' '>=' '=<' '/='
'++' '--'
'*' '/' 'div' 'rem' 'band' 'and' 'fn' 'end'
'andalso' 'orelse'
'+' '-' 'bor' 'bxor' 'bsl' 'bsr' 'or' 'xor' 'bnot' 'not'
'='
'when'
dot.

Rootsymbol form.

form -> function dot : '$1'.

function -> function_clauses : build_function('$1').


% fun_clause -> fun_argument_list clause_guard fun_clause_body :
% 	{Args,Pos} = '$1',
% 	% note the empty clause guard is completely ignored for now
% 	{Args1, Guards} = stoat_guards:compose_guards(Args),
% 	{clause,Pos,'fun',Args1, Guards,'$3'}.

function_clauses -> function_clause : ['$1'].
function_clauses -> function_clause ';' function_clauses : ['$1'|'$3'].

function_clause -> atom clause_args clause_guard clause_body : 
	{Args, Guards} = stoat_guards:compose_guards('$2'),

	% TODO : why do the guards need to be wrapped in a list?
	% are they different from fun guards?
	
	{clause, ?line('$1'), ?tokch('$1'), Args, [[G]||G<-Guards], '$4'}.
% will fail in build_function in first position
function_clause -> clause_args clause_guard clause_body :
	{Args, Guards} = stoat_guards:compose_guards('$1'),
	{noname_clause, ?line(hd('$3')), noname, Args, [[G]||G<-Guards], '$3'}.
	
	
function_clause -> atom '-' pipe_calls : 
	L = ?line('$1'),
	Arg = {var, L, 'X__'},
	Expr = stoat_pipes:transform({Arg, '$3', L}),
	% flattening again. TODO: take care of this in the transform!
	{clause, ?line('$1'), ?tokch('$1'), [Arg], [], lists:flatten(Expr)}.
	
	
clause_args -> argument_list : element(1, '$1').



clause_guard -> 'when' guard : '$2'.
clause_guard -> '$empty' : [].

clause_body -> '->' exprs: '$2'.



% expr -> 'catch' expr : {'catch',?line('$1'),'$2'}.
expr -> expr_100 : '$1'.


% no assignment!
% expr_100 -> expr_150 '=' expr_100 : {match,?line('$2'),'$1','$3'}.
expr_100 -> expr_150 '!' expr_100 : ?mkop2('$1', '$2', '$3').
expr_100 -> expr_150 : '$1'.

expr_150 -> expr_160 'orelse' expr_150 : ?mkop2('$1', '$2', '$3').
expr_150 -> expr_160 : '$1'.

expr_160 -> expr_200 'andalso' expr_160 : ?mkop2('$1', '$2', '$3').
expr_160 -> expr_180 : '$1'.

expr_180 -> pipe : stoat_pipes:transform('$1').
expr_180 -> expr_200 : '$1'.

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
% expr_600 -> expr_650 : '$1'.
expr_600 -> expr_700 : '$1'.

% expr_650 -> pipe : stoat_pipes:transform('$1').
% expr_650 -> expr_700 : '$1'.

expr_700 -> function_call : '$1'.
expr_700 -> record_expr : '$1'.
expr_700 -> expr_800 : '$1'.

expr_800 -> expr_max ':' expr_max : {remote,?line('$2'),'$1','$3'}.
expr_800 -> expr_max : '$1'.

expr_max -> var : '$1'.
expr_max -> atomic : '$1'.
expr_max -> list : '$1'.
expr_max -> binary : '$1'.
expr_max -> list_comprehension : '$1'.
expr_max -> binary_comprehension : '$1'.
expr_max -> tuple : '$1'.
expr_max -> '(' expr ')' : '$2'.
% expr_max -> 'begin' exprs 'end' : {block,?line('$1'),'$2'}.
% expr_max -> if_expr : '$1'.
% expr_max -> case_expr : '$1'.
% expr_max -> receive_expr : '$1'.
expr_max -> fun_expr : '$1'.
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

bit_type -> atom             : ?tokch('$1').
bit_type -> atom ':' integer : { ?tokch('$1'), ?tokch('$3') }.

bit_size_expr -> expr_max : '$1'.

list_comprehension -> '[' expr '||' lc_exprs ']' : {lc, ?line('$1'), '$2', '$4'}.
binary_comprehension -> '<<' binary '||' lc_exprs '>>' :
	{bc,?line('$1'),'$2','$4'}.
	
% in progress!
lc_exprs -> lc_expr : ['$1'].
lc_exprs -> lc_expr ',' lc_exprs : ['$1'|'$3'].

lc_expr -> expr : '$1'.
lc_expr -> expr '<-' expr : {generate, ?line('$2'), '$1', '$3'}.
% what is this?
lc_expr -> expr '<=' expr : {b_generate, ?line('$2'), '$1', '$3'}.

tuple -> '{' '}'       : {tuple, ?line('$1'), []}.
tuple -> '{' exprs '}' : {tuple, ?line('$1'), '$2'}.


% record_expr -> '#' atom '.' atom.

% TODO: MAPS!

%%%%%%%%%% records %%%%%%%%%%%%%%%%%%%%%%%%%

record_expr -> '#' atom '.' atom : {record_index,?line('$1'),element(3, '$2'),'$4'}.
record_expr -> '#' atom record_tuple : {record,?line('$1'),element(3, '$2'),'$3'}.
record_expr -> expr_max '#' atom '.' atom : {record_field,?line('$2'),'$1',element(3, '$3'),'$5'}.
record_expr -> expr_max '#' atom record_tuple : {record,?line('$2'),'$1',element(3, '$3'),'$4'}.
record_expr -> record_expr '#' atom '.' atom : {record_field,?line('$2'),'$1',element(3, '$3'),'$5'}.
record_expr -> record_expr '#' atom record_tuple : {record,?line('$2'),'$1',element(3, '$3'),'$4'}.

record_tuple -> '{' '}' : [].
record_tuple -> '{' record_fields '}' : '$2'.

record_fields -> record_field : ['$1'].
record_fields -> record_field ',' record_fields : ['$1' | '$3'].

record_field -> var '=' expr : {record_field,?line('$1'),'$1','$3'}.
record_field -> atom '=' expr : {record_field,?line('$1'),'$1','$3'}.

%%%%%%%%%%%%% functions %%%%%%%%%%%%%%%%%%%%



function_call -> expr_800 call_argument_list : 
	% stoat_cuts:transform({call,?line('$1'),'$1',element(1, '$2')}).
	% ?p("calling: ~p~n", ['$2']),
	{call, ?line('$1'),'$1', element(1, '$2')}.

fun_expr -> atom '/' integer :
	{'fun',?line('$1'),{function,element(3, '$1'),element(3, '$3')}}.
fun_expr -> 'fn' atom_or_var ':' atom_or_var '/' integer_or_var :
	{'fun',?line('$1'),{function,'$2','$4','$6'}}.
% fun_expr -> atom ':' atom '/' integer :
% 	{'fun',?line('$1'),{function,'$1','$3','$5'}}.
% fun_expr -> short_fun_clause '}' : build_fun(?line('$1'), ['$1']).
fun_expr -> '{' fun_clauses '}' : build_fun(?line('$1'), '$2').


atom_or_var -> atom : '$1'.
atom_or_var -> var : '$1'.

integer_or_var -> integer : '$1'.
integer_or_var -> var : '$1'.

fun_clauses -> fun_clause : ['$1'].
fun_clauses -> fun_clause ';' fun_clauses : ['$1' | '$3'].

fun_clause -> fun_argument_list clause_guard fun_clause_body :
	{Args,Pos} = '$1',
	% note the empty clause guard is completely ignored for now
	{Args1, Guards} = stoat_guards:compose_guards(Args),
	{clause,Pos,'fun',Args1, Guards,'$3'}.
		
fun_clause -> var argument_list clause_guard fun_clause_body :
	{clause,element(2, '$1'),element(3, '$1'),element(1, '$2'),'$3','$4'}.
	
fun_clause_body -> '|' exprs: '$2'.
	

fun_argument_list -> arg_exprs : {'$1', 0}.
fun_argument_list -> '$empty' : {[], 0}.

call_argument_list -> '(' ')' : {[],?line('$1')}.
call_argument_list -> '(' exprs ')' : {'$2',?line('$1')}.

argument_list -> '(' ')' : {[],?line('$1')}.
argument_list -> '(' arg_exprs ')' : {'$2',?line('$1')}.

% the flattening is necessary because we break up an "expression"
% when binding values during pipes. keep an eye on this.
exprs -> expr : lists:flatten(['$1']).
exprs -> expr ',' exprs : lists:flatten(['$1' | '$3']).

arg_exprs -> arg_expr : lists:flatten(['$1']).
arg_exprs -> arg_expr ',' arg_exprs : lists:flatten(['$1'|'$3']).

arg_expr -> expr arg_guards : {'$1', '$2'}.
arg_guards -> '$empty' : [].
arg_guards -> '::' atom : ['$2'].
arg_guards -> '::' '(' exprs ')' : '$3'.

guard -> exprs : ['$1'].
guard -> exprs ';' guard : ['$1'|'$3'].

% TODO : IF EXPRESSIONS
% TODO : CASE EXPRESSIONS  -- covered by case funs
% TODO : RECEIVE EXPRESSIONS
% TODO : TRY, TRY-CATCH EXPRESSIONS

% atomic -> char : '$1'.
atomic -> integer : '$1'.
atomic -> float : '$1'.
atomic -> atom : '$1'.
atomic -> strings : '$1'.


strings -> string : '$1'.
% what is this for?
strings -> string strings :
	{string,?line('$1'),?tokch('$1') ++ ?tokch('$2')}.

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

pipe -> expr_200 pipe_calls : {'$1', '$2', ?line('$1')}.
pipe_calls -> pipe_call : ['$1'].
pipe_calls -> pipe_call pipe_calls : ['$1'|'$2'].
pipe_call -> pipe_op expr_200 pipe_bindings : {'$1', '$2', '$3', ?line('$1')}.

% |{ is sugar for |> {
pipe_call -> '|{' fun_clauses '}' : {{'|>', ?line('$1')}, build_fun(?line('$1'), '$2'), [], ?line('$1')}.

pipe_op -> '|>'  : '$1'.
pipe_op -> '|+'  : '$1'.
pipe_op -> '|-'  : '$1'.
pipe_op -> '|)'  : '$1'.
pipe_op -> '|/'  : '$1'.
pipe_op -> '|m'  : '$1'.
pipe_op -> '|:'  : '$1'.
pipe_bindings -> '$empty' : [].
pipe_bindings -> pipe_binding pipe_bindings : ['$1'|'$2'].
pipe_binding -> '~' expr_700 : {'$2', ?line('$1')}.

Erlang code.

-define(tokch(Tup), element(3, Tup)).
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

%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%
-export([parse_expr/1]).
parse_expr(Tokens) ->
    case parse([{atom,0,f},{'(',0},{')',0},{'->',0}|Tokens]) of
		{ok,{function,_Lf,f,0,[{clause,_Lc,[],[],[Expr]}]}} ->
		    {ok,Expr};
		{error,_} = Err -> Err;
		{ok, Other} -> {error, notexpr, other}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_function(Cs) ->
	C1 = hd(Cs),
    Name = ?tokch(C1),
    Arity = length(element(4, C1)),
    {function, ?line(C1), Name, Arity, check_clauses(Cs, Name, Arity)}.

build_fun(Line, Cs) ->
	Clauses = Cs, %check_arg_guards(Cs),
    Name = ?tokch(hd(Clauses)),
    Arity = length(element(4, hd(Clauses))),
    CheckedCs = check_clauses(Clauses, Name, Arity),
    case Name of
        'fun' ->
            {'fun',Line,{clauses,CheckedCs}};
        Name ->
            {named_fun,Line,Name,CheckedCs}
    end.




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
	

