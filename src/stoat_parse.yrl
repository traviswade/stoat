

Nonterminals
form
attribute attr_val
expr exprs expr_100 expr_150 expr_160 expr_180 expr_190 expr_200 expr_300 expr_400
expr_500 expr_600 expr_650 expr_700 expr_800 expr_max
macro
list_comprehension lc_expr lc_exprs
binary_comprehension try_expr try_catch try_clauses try_clause
record_expr record_fields record_tuple record_field
fun_expr fun_clause fun_clauses fun_clause_body
fun_argument_list map_expr
curried_args
receive_expr
map_tuple map_fields map_field map_field_assoc map_field_exact map_key
atom_or_var integer_or_var
function function_clauses function_clause function_name
argument_list arg_exprs arg_expr arg_guards
cr_clauses cr_clause
clause_args clause_body
guard
function_call call_argument_list trailing_closure
partial_application
tuple
atomic strings list tail 
binary bin_elements bin_element bit_size_expr 
bit_expr opt_bit_size_expr opt_bit_type_list bit_type bit_type_list
add_op mult_op prefix_op list_op comp_op 
pipe pipe_op pipe_call pipe_calls pipe_bindings pipe_binding pipe_end.

Terminals
char integer float atom string sstring var
'(' ')' ',' '->' '{' '}' '[' ']' '<-' ';' '|' '<<' '>>' ':' '!'
'<=' '||' '=>' '&' '#' '.' ':=' '@'
'|>' '|+' '|-' '|)' '|/' '|<' '|:' '~' '|{' '.{' '?[' '?{' '<|'
'::'
'?'
'==' '=:=' '=/=' '<' '>' '>=' '=<' '/='
'++' '--'
'*' '/' 'div' 'rem' 'band' 'and' 'fn' 'end'
'andalso' 'orelse'
'try' 'catch' 'of' 'after' 'receive'
'+' '-' 'bor' 'bxor' 'bsl' 'bsr' 'or' 'xor' 'bnot' 'not'
'='
'when'
dot.

Rootsymbol form.

form -> attribute dot : '$1'.
form -> function dot : '$1'.

attribute -> '-' atom attr_val               : build_attribute('$2', '$3').
attr_val -> expr                     : ['$1'].
attr_val -> expr ',' exprs           : ['$1' | '$3'].
attr_val -> '(' expr ',' exprs ')'   : ['$2' | '$4'].

function -> function_clauses : build_function('$1').


function_clauses -> function_clause : ['$1'].
function_clauses -> function_clause ';' function_clauses : ['$1'|'$3'].

function_clause -> atom clause_args clause_body : 
	{Args, Guards} = stoat_guards:compose_guards('$2'),
	{clause, ?line('$1'), ?tokch('$1'), Args, stoat_guards:maybe_wrap(Guards), '$3'}.
function_clause -> atom curried_args clause_body :
	[H|T] = '$2',
	{Args, Guards} = stoat_guards:compose_guards([H]),
	{clause, ?line('$1'), ?tokch('$1'), Args, stoat_guards:maybe_wrap(Guards), curry_body(T, '$3')}.
	

	
% will fail in build_function in first position
function_clause -> clause_args  clause_body :
	{Args, Guards} = stoat_guards:compose_guards('$1'),
	{noname_clause, ?line(hd('$2')), noname, Args, stoat_guards:maybe_wrap(Guards), '$2'}.
	
	
function_clause -> atom arg_guards pipe_calls : 
	L = ?line('$1'),
	{[Arg], Guards} = stoat_guards:compose_guards([{{var, L, 'X__'}, '$2'}]),
	Expr = stoat_pipes:transform({Arg, '$3', L, Guards}),
	% flattening again. TODO: take care of this in the transform!
	{clause, ?line('$1'), ?tokch('$1'), [Arg], stoat_guards:maybe_wrap(Guards), lists:flatten(Expr)}.
	
	
clause_args -> argument_list : element(1, '$1').
curried_args ->  '[' ']' : [].
curried_args ->  '[' arg_exprs ']' : '$2'.


clause_body -> '->' exprs: '$2'.


% expr -> 'catch' expr : {'catch',?line('$1'),'$2'}.
expr -> expr_100 : '$1'.


% not allowed in fun or function bodies.
expr_100 -> expr_150 '=' expr_100 : {match,?line('$2'),'$1','$3'}.
expr_100 -> expr_150 '!' expr_100 : ?mkop2('$1', '$2', '$3').
expr_100 -> expr_150 : '$1'.

expr_150 -> expr_160 'orelse' expr_150 : ?mkop2('$1', '$2', '$3').
expr_150 -> expr_160 : '$1'.

expr_160 -> expr_200 'andalso' expr_160 : ?mkop2('$1', '$2', '$3').
expr_160 -> expr_180 : '$1'.

expr_180 -> pipe : stoat_pipes:transform('$1').
expr_180 -> expr_190 : '$1'.

expr_190 -> expr_190 '<|' expr_200 : {call, ?line('$2'),'$1', [stoat_cuts:maybe_expr2fun('$3')]}.
expr_190 -> expr_200 : '$1'.

expr_200 -> expr_300 comp_op expr_300 : ?mkop2('$1', '$2', '$3').
expr_200 -> expr_300 : '$1'.

expr_300 -> expr_400 list_op expr_300 : ?mkop2('$1', '$2', '$3').
expr_300 -> expr_400 : '$1'.

expr_400 -> expr_400 add_op expr_500 : ?mkop2('$1', '$2', '$3').
expr_400 -> expr_500 : '$1'.

expr_500 -> expr_500 mult_op expr_600 : ?mkop2('$1', '$2', '$3').
expr_500 -> expr_600 : '$1'.

expr_600 -> prefix_op expr_700 : ?mkop1('$1', '$2').
expr_600 -> map_expr : '$1'.
expr_600 -> expr_650 : '$1'.

expr_650 -> macro : '$1'.
expr_650 -> expr_700 : '$1'.

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

expr_max -> receive_expr : '$1'.
expr_max -> fun_expr : '$1'.
expr_max -> try_expr : '$1'.

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


map_expr -> '#' map_tuple          : {map, ?line('$1'),'$2'}.
map_expr -> expr_max '#' map_tuple : {map, ?line('$2'),'$1','$3'}.
map_expr -> map_expr '#' map_tuple : {map, ?line('$2'),'$1','$3'}.

map_tuple -> '{' '}' : [].
map_tuple -> '{' map_fields '}' : '$2'.

map_fields -> map_field : ['$1'].
map_fields -> map_field ',' map_fields : ['$1' | '$3'].

map_field -> map_field_assoc : '$1'.
map_field -> map_field_exact : '$1'.
map_field_assoc -> map_key '=>' expr : {map_field_assoc,?line('$1'),'$1','$3'}.
map_field_exact -> map_key ':=' expr : {map_field_exact,?line('$1'),'$1','$3'}.

map_key -> expr : '$1'.

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



function_call -> expr_800 call_argument_list '@' trailing_closure: 
	{call, ?line('$1'),'$1', element(1, '$2') ++ '$4'}.
	
function_call -> expr_800 call_argument_list trailing_closure:
	{call, ?line('$1'), '$1', '$3' ++ element(1, '$2')}.

% function_call -> expr_800 fun_expr: 
	% {call, ?line('$1'), '$1', ['$2']}.
% function_call -> expr_800 fun_expr: 
% 	{call, ?line('$1'), '$1', ['$2']}.

trailing_closure -> fun_expr : ['$1'].
trailing_closure -> '$empty' : [].
	
fun_expr -> '&' atom '/' integer :
	{'fun',?line('$1'),{function,element(3, '$2'),element(3, '$4')}}.
fun_expr -> '&' atom_or_var ':' atom_or_var '/' integer_or_var :
	{'fun',?line('$1'),{function,'$2','$4','$6'}}.
fun_expr -> '{' fun_clauses '}' : build_fun(?line('$1'), '$2').
fun_expr -> '.{' expr '}' : stoat_cuts:expr2fun('$2').
fun_expr -> '&' pipe_calls : 
	L = ?line('$1'),
	Arg = {var, L, '_'},
	stoat_cuts:expr2fun(hd(stoat_pipes:transform({Arg, '$2', L}))).



atom_or_var -> atom : '$1'.
atom_or_var -> var : '$1'.

integer_or_var -> integer : '$1'.
integer_or_var -> var : '$1'.

fun_clauses -> fun_clause : ['$1'].
fun_clauses -> fun_clause ';' fun_clauses : ['$1' | '$3'].




fun_clause -> fun_argument_list fun_clause_body :
	{Args,Pos} = '$1',
	% note the empty clause guard is completely ignored for now
	{Args1, Guards} = stoat_guards:compose_guards(Args),
	{clause,Pos,'fun',Args1, Guards,'$2'}.
		
fun_clause -> var argument_list fun_clause_body :
	{clause,element(2, '$1'),element(3, '$1'),element(1, '$2'), [],'$3'}.
	
fun_clause_body -> '|' exprs: '$2'.
	

fun_argument_list -> arg_exprs : {'$1', 0}.
fun_argument_list -> '$empty' : {[], 0}.

call_argument_list -> '(' ')' : {[],?line('$1')}.
call_argument_list -> '(' exprs ')' : {'$2',?line('$1')}.
call_argument_list -> expr_max : {['$1'], 0}.

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




try_expr -> 'try' exprs 'of' cr_clauses try_catch :
	build_try(?line('$1'),'$2','$4','$5').
try_expr -> 'try' exprs try_catch :
	build_try(?line('$1'),'$2',[],'$3').

try_catch -> 'catch' try_clauses 'end' :
	{'$2',[]}.
try_catch -> 'catch' try_clauses 'after' exprs 'end' :
	{'$2','$4'}.
try_catch -> 'after' exprs 'end' :
	{[],'$2'}.

try_clauses -> try_clause : ['$1'].
try_clauses -> try_clause ';' try_clauses : ['$1' | '$3'].

try_clause -> expr clause_body :
	L = ?line('$1'),
	{clause,L,[{tuple,L,[{atom,L,throw},'$1',{var,L,'_'}]}],[],'$2'}.
try_clause -> atom ':' expr clause_body :
	L = ?line('$1'),
	{clause,L,[{tuple,L,['$1','$3',{var,L,'_'}]}], [],'$4'}.
try_clause -> var ':' expr clause_body :
	L = ?line('$1'),
	{clause,L,[{tuple,L,['$1','$3',{var,L,'_'}]}], [],'$4'}.


atomic -> char : '$1'.
atomic -> integer : '$1'.
atomic -> float : '$1'.
atomic -> atom : '$1'.
atomic -> strings : '$1'.


strings -> string : '$1'.
strings -> sstring : stoat_strings:interpolate('$1').
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

pipe -> expr_190 pipe_calls : {'$1', '$2', ?line('$1')}.
pipe_calls -> pipe_call : ['$1'].
pipe_calls -> pipe_call pipe_calls pipe_end : ['$1'|'$2'].
pipe_call -> pipe_op expr_190 pipe_bindings : {'$1', '$2', '$3', ?line('$1')}.
% pipe_call -> pipe_op atom fun_expr pipe_bindings : 
% 	?p("t-------rying to make binding: ~p~n", ['$3']),
% 	{'$1', {call, ?line('$1'), '$2', ['$3']}, '$4', ?line('$1')}.

pipe_end -> '$empty' : ok.
pipe_end -> 'end' : ok.

% |{ is sugar for |> {
% pipe_call -> '|{' fun_clauses '}' : {{'|>', ?line('$1')}, build_fun(?line('$1'), '$2'), [], ?line('$1')}.

pipe_call -> '?[' cr_clauses pipe_bindings ']' : {'?[', '$2', '$3', ?line('$1')}.

pipe_call -> '?{' expr ',' expr '}' : { '?{', '$2', '$4', ?line('$1')}.

cr_clauses -> cr_clause : ['$1'].
cr_clauses -> cr_clause ';' cr_clauses : ['$1' | '$3'].
cr_clause -> arg_expr clause_body :
	{[Item], Guards} = stoat_guards:compose_guards(['$1']),
	{clause, ?line(Item), [Item], Guards,'$2'}.

receive_expr -> 'receive' cr_clauses 'end' :
	{'receive',?line('$1'),'$2'}.
receive_expr -> 'receive' 'after' expr clause_body 'end' :
	{'receive',?line('$1'),[],'$3','$4'}.
receive_expr -> 'receive' cr_clauses 'after' expr clause_body 'end' :
	{'receive',?line('$1'),'$2','$4','$5'}.

pipe_op -> '|>'  : '$1'.
pipe_op -> '|+'  : '$1'.
pipe_op -> '|-'  : '$1'.
pipe_op -> '|)'  : '$1'.
pipe_op -> '|/'  : '$1'.
pipe_op -> '|<'  : '$1'.
pipe_op -> '|:'  : '$1'.
pipe_bindings -> '$empty' : [].
pipe_bindings -> pipe_binding pipe_bindings : ['$1'|'$2'].
pipe_binding -> '~' expr_700 : {'$2', ?line('$1')}.

% no this probably doesn't belong in the parser.
macro -> '?' expr_700 : stoat_macros:expand_macro('$2').

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
-export([parse_expr/1, parse_exprs/1]).
parse_expr(Tokens) ->
    case parse([{atom,0,f},{'(',0},{')',0},{'->',0}|Tokens]) of
		{ok,{function,_Lf,f,0,[{clause,_Lc,[],[],[Expr]}]}} ->
		    {ok,Expr};
		{error,_} = Err -> Err;
		{ok, Other} -> {error, notexpr, other}
    end.

parse_exprs(Tokens) ->
    case parse([{atom,0,f},{'(',0},{')',0},{'->',0}|Tokens]) of
	{ok,{function,_Lf,f,0,[{clause,_Lc,[],[],Exprs}]}} ->
	    {ok,Exprs};
	{error,_} = Err -> Err
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


% build_attribute({atom,La,module}, Val) ->
%     case Val of
% 	[{atom,_Lm,Module}] ->
% 	    {attribute,La,module,Module};
% 	[{atom,_Lm,Module},ExpList] ->
% 	    {attribute,La,module,{Module,var_list(ExpList)}};
% 	_Other ->
% 	    error_bad_decl(La, module)
%     end;
build_attribute({atom,L,export}, Val) ->
    case Val of
	[ExpList] ->
	    {attribute,L,export,farity_list(ExpList)};
	_Other -> ret_err(L, {badexport, Val})
    end;
build_attribute({atom,L,def}, [K, V]) ->
	stoat_macros:register_macro(K, V),
    {attribute, registered_macro, ok};
build_attribute({atom, L, incl}, [{_Stringable, _L, F}]) ->
	stoat_macros:handle_incl(F),
	{attribute, handled_include, ok};
	
build_attribute	({atom, L, mixin}, [{_Stringable, _L, M}]) ->
	% stoat_macros:handle_incl(F),
	{attribute, mixin, M};

	
% build_attribute({atom,La,import}, Val) ->
%     case Val of
% 	[{atom,_Lm,Mod},ImpList] ->
% 	    {attribute,La,import,{Mod,farity_list(ImpList)}};
% 	_Other -> error_bad_decl(La, import)
%     end;
build_attribute({atom,L,record}, Val) ->
    case Val of
	[{atom,_L,Record},RecTuple, Opts] ->
		% TODO: this is all pretty redundant..
		stoat_macros:register_record(Record, RecTuple, Opts, {attribute, L, mrecord, {Record,record_tuple(RecTuple)}}),
	    {attribute, L, mrecord, {Record,record_tuple(RecTuple)}};
	[{atom,_L,Record},RecTuple] ->
		stoat_macros:register_record(Record, RecTuple, [], {attribute, L, mrecord, {Record,record_tuple(RecTuple)}}),
		{attribute, L, mrecord, {Record,record_tuple(RecTuple)}};
	_ -> ret_err(badrecorddecl, Val)
    end.
% build_attribute({atom,La,file}, Val) ->
%     case Val of
% 	[{string,_Ln,Name},{integer,_Ll,Line}] ->
% 	    {attribute,La,file,{Name,Line}};
% 	_Other -> error_bad_decl(La, file)
%     end;
% build_attribute({atom,La,Attr}, Val) ->
%     case Val of
% 	[Expr0] ->
% 	    Expr = attribute_farity(Expr0),
% 	    {attribute,La,Attr,term(Expr)};
% 	_Other -> throw(badattr, Attr)
%     end.
          % {cons,3,  {'fun',  3, {function,add,1}},{nil,3}}}

% farity_list({cons,_Lc,{'fun', _Lo,{function,F,A}, _},Tail}) ->
%     [{A,F}|farity_list(Tail)];
farity_list({cons,_Lc,{op,   _Lo,'/',{atom,_La,A},{integer,_Li,I}},Tail}) ->
    [{A,I}|farity_list(Tail)];
farity_list({nil,_Ln}) -> [];
farity_list(Other) ->
	?p({badfaritylist, Other}),
    ret_err(?line(Other), "bad function arity").

record_tuple({tuple,_Lt,Fields}) ->
    record_fields(Fields);
record_tuple(Other) ->
    ret_err(?line(Other), "bad record declaration").

record_fields([{atom,La,A}|Fields]) ->
    [{record_field,La,{atom,La,A}}|record_fields(Fields)];
record_fields([{match,_Lm,{atom,La,A},Expr}|Fields]) ->
    [{record_field,La,{atom,La,A},Expr}|record_fields(Fields)];
% record_fields([{typed,Expr,TypeInfo}|Fields]) ->
%     [Field] = record_fields([Expr]),
%     TypeInfo1 =
% 	case Expr of
% 	    {match, _, _, _} -> TypeInfo; %% If we have an initializer.
% 	    {atom, La, _} ->
%                 case has_undefined(TypeInfo) of
%                     false ->
%                         TypeInfo2 = maybe_add_paren(TypeInfo),
%                         lift_unions(abstract(undefined, La), TypeInfo2);
%                     true ->
%                         TypeInfo
%                 end
% 	end,
%     [{typed_record_field,Field,TypeInfo1}|record_fields(Fields)];
record_fields([Other|_Fields]) ->
    ret_err(?line(Other), "bad record field");
record_fields([]) -> [].

build_try(L,Es,Scs,{Ccs,As}) ->
    {'try',L,Es,Scs,Ccs,As}.

check_clauses(Cs, Name, Arity) -> [check_clause(C, Name, Arity) || C <- Cs].

check_clause ({clause, L, Name, As, G, B}, Name, Arity) when length(As) =:= Arity ->
		{clause, L, As, G, B};
check_clause ({noname_clause, L, _, As, G, B}, Name, Arity) when length(As) =:= Arity ->
		{clause, L, As, G, B};
check_clause ({clause,L,_N,_As,_G,_B}, _, _) ->
		ret_err(L, "head mismatch").
		
curry_body ([], Body)    -> Body;
curry_body ([H|T], Body) ->
	{Arg, Guards} = stoat_guards:compose_guards([H]),
	[{'fun', ?line(hd(Arg)),
	       {clauses, [{clause, ?line(H), Arg, 
				stoat_guards:maybe_wrap(Guards), curry_body(T, Body)}]}}].
		
ret_err(L, S) ->
    {location,Location} = get_attribute(L, location),
    return_error(Location, S).

get_attribute(L, Name) ->
    erl_scan:attributes_info(L, Name).
	
show (Item) ->
	?p("show: ~p", [Item]),
	Item.
	

