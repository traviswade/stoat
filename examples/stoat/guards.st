
% we can specify guards right with the argument:
% just add '::' followed by an atom wich will resolve to a 
% BIF of the form is_{type} (some abbreviated versions, including 
% 'int' for 'integer' will be accepted), or other legal guard
% expressions using underscore substitution (see cuts.st)

f1 (X::int, Y::(int, _>X)) -> ok.

% works the same way in fun expressions

f2 () -> {|X::int| X+1}.

f3 () -> {|X::(_>4) | X+1; _ | 4}.

% and for direct-to-pipe functions, just append to the function name

f4::int |> _+1.

