
% several ways to make a fun expression:

% 1) {args|body}. Everything between the braces is the clause(s).
% Multiple clauses are separated with a ';'

anonymous () -> 
	lists:map({N| N + 1}, [1, 2, 3]).
	
% 2) For one-argument, single-clause funs : .{body},
% Underscore replacements works just as in pipes.

anoncuts () -> [
	.{_},
	.{5},
	.{_+3}].

multiclause () ->
	{{struct, L}| L; 
	L| L}.
	
% Trailing closures! Any kind of fun expression
% following a function call works as if that fun were given as the FIRST
% argument to the call.
% Alternatively, prepend an '@' and it will be treated as the LAST
% argument. No other options for now -- first is the typical position
% in Erlang so that is the default.

trailer () -> m:f(1)@{X | X + 1}. 
	
trailer1 () -> m:f(1){X | X+1}.

% 3) &{pipe expression} = single-argument, single-clause fun with the argument
% passed into the pipe.

anon_pipe () -> &|> g |> f.

% Note that the '@' precedes the '&' for trailer pipe funs in last position.

trailer_pipe () -> m:f(1) @&|> g |> f.

trailer_pipe2 () -> m:f(1) &|> g |> f.

% 4) from a local (atom/integer) or remote (atom_or_var:atom/integer) signature:

somefuns (ModuleName) ->
	[
	&local/1,
	&remote:f1/1,
	&ModuleName:f1/1].
	