-module(prf).

-export([funs1/0, funs2/0, if1/0, if2/0, if3/0, if4/0, if5/0, if6/0, if7/0, if8/0, if9/0, if10/0, if11/0,
pipes1/0, pipes2/0]).

-export([typ1/0, typ2/0, typ3/0, typ4/0, typ5/0]).

-export([t/2, t/1]).

funs1 () -> 
	length([]).

funs2 () -> 
	fun (L) -> length(L) end ([]).
	
typ1 () -> typ1(1).
typ2 () -> typ2(1).
typ3 () -> typ3(1).
typ4 () -> typ4(1).
typ5 () -> typ5(1).

typ1 (X) when is_integer(X) -> ok;
typ1 (_) -> bad.

typ2 (X) -> case X of
	Val when is_integer(Val) -> ok;
	_ -> bad
end.

typ3 (X) -> if is_integer(X) -> ok; true -> bad end.

typ4 (X) -> case is_integer(X) of true -> ok; _ -> bad end.

typ5 (_) -> ok.
	
pipes1 () ->
	A = outf(1),
	B = outf(A),
	C = outf(B),
	D = outf(C),
	outf(D).
	
pipes2 () ->
	outf(outf(outf(outf(outf(1))))).
	
outf (A) -> length([{A}]).

if1 () -> 
	A = true, B = false,
	V1 = if A -> ok; true -> bad end,
	V2 = if B -> ok; true -> bad end,
	outf(V1),
	outf(V2).

if2 () -> 
	A = true, B = false,
	F = fun(true)-> ok; (_)-> bad end,
	outf(F(A)),
	outf(F(B)).
	
if3 () ->
	A = true, B = false,
	outf(fun(true)->ok; (_)->bad end(A)),
	outf(fun(true)->ok; (_)->bad end(B)).
	
if4 () ->
	A = true, B = false,
	outf(fun(V)-> if V->ok; true->bad end end(A)),
	outf(fun(V)-> if V->ok; true->bad end end(B)).
	
if5 () ->
	A = true, B = false,
	F = fun(V)-> if V -> ok; true -> bad end end,
	outf(F(A)), outf(F(B)).
	
if6 () ->
	A = true, B = false,
	V1 = case A of true -> ok; _ -> bad end,
	V2 = case B of true -> ok; _ -> bad end,
	outf(A), outf(B).
	
if7 () ->
	A = true, B = false,
	outf(fun(V)-> case V of true -> ok; _ -> bad end end(A)),
	outf(fun(V)-> case V of true -> ok; _ -> bad end end(B)).
	
if8 () ->
	A = true, B = false,
	F = fun(V) -> case V of true -> ok; _ -> bad end end,
	outf(F(A)), outf(F(B)).
	
if9 () ->
	A = true, B = false,
	outf(ifcase1(A)), outf(ifcase1(B)).
	
if10 () ->
	A = true, B = false,
	outf(ifcase2(A)), outf(ifcase2(B)).
if11 () ->
	A = true, B = false,
	outf(ifcase3(A)), outf(ifcase3(B)).
	
ifcase1 (true) -> ok;
ifcase1 (false) ->bad.
ifcase2 (V) -> case V of true -> ok; _ -> bad end.
ifcase3 (V) -> if V -> ok; true -> bad end.
	

t (F) -> t (F, 1000000).
t (F, N) ->
	statistics(runtime),
	statistics(wall_clock),
	times(N, F),
	{_, T1a} = statistics(runtime),
	{_, T1b} = statistics(wall_clock),
	{T1a * 1000, T1b * 1000}.
	
times (0, F) -> ok;
times (N, F) -> prf:F(), times(N-1, F).