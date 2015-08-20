

anonymous () -> 
	lists:map({N| N + 1}, [1, 2, 3]).

anoncuts () -> [
	.{_},
	.{5},
	.{_+3}].

multiclause () ->
	{{struct, L}| L; 
	L| L}.
	

trailer () ->
	m:f(1){X | X + 1}. 
	
trailer1 () ->
	m:f(1)@{X | X+1}.
	
anon_pipe () -> fn |> g |> f.

trailer_pipe () -> m:f(1) fn |> g |> f.

trailer_pipe2 () ->
	m:f(1) @fn |> g |> f.
	