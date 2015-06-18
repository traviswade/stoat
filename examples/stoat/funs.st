

anonymous () -> 
	lists:map({N| N + 1}, [1, 2, 3]).
	
% anonymous2 () ->
% 	lists:map({_1+1}, [1, 2, 3]).

multiclause () ->
	{{struct, L}| L; 
	L| L}.