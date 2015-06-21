

anonymous () -> 
	lists:map({N| N + 1}, [1, 2, 3]).

anoncuts () -> [
	.{_},
	.{5},
	.{_+3}].

multiclause () ->
	{{struct, L}| L; 
	L| L}.