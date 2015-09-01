
% Pipes: the central feature of Stoat. A hodge podge of common function 
% pipey type things.


double |> _ * 2.
add (X) -> {Y| Y + X}.

% The simplest case. |> alone works pretty much like the same
% operator in F#, Elixir, Elm, etc.
% The right hand side can be :
% - a fun or an expression that evaluates to a fun
% - an atom corresponding to a local function
% - two atoms separated by a ':' for a remote function
% - ???

basic (X) -> X |> add(2) |> _+1 |> math:log |> double.

basic1 |> add(2) |> _+1 |> math:log |> double.

maybe (F, {ok, Res})   ->  F(Res);
	(_, {error, Why})  ->  {error, Why}.
	
% Wrappers. '|+' is the "smart semicolon".
% It inserts a function into the chain that wraps all subsequent
% '|>' RH arguments. Compare the Erlang equivalent of the following:

wrappers |+ maybe |> h |> g |> f.

% Opting out of a wrapper: use '|)' instead of '|>' to skip
% the wrapper function in one step.
	
optout |+ maybe |> h |) g |> f.

% '|/' calls the RH function (probably for a side effect)
% and returns the original value.
	
tap1 |> g |/ log |> f.
	
tap2 |+ maybe |> g |/ log |> f.

% Often we run a chain of functions on a data type that is
% associated with a module and has a predictable position in
% functions in the module. In cases like this we can get rid of
% some noise by passing the {module(Position)} into the '|<'
% operator and then passing a local call minus the implied argument
% to '|:'. 
% In some common cases you can leave out the position. More options for that
% and for combining steps for efficiency on the future.
	
mod1 |< maps(-1)
	|: without([rev, id])
	|: put(x, 1)
	|: values.
	

mod2 |< mydb(1)
	|: fetch |+ maybe
	|> somesortofupdate
	|: save.
	
mod3 |< lists
	|: filter.{_>1}
	|: map.{_+10}.
	
% Case and if statements are always expressed as pipes. I don't like 
% these forms much and they will probably change.
	
case1 |> _+1
	?[
		2 -> one;
		3 -> two;
		_ -> other]
	|> atom_to_list.
	
case2 ?[
	I::(integer, _>2) -> ok; 
	_ -> error].
	


if1 ?{ok, error}.

% Local bindings! Put {~ Arg} at the end of any pipe step
% to introduce a local match / binding that you can reuse later in the pipe.
% What more could you ask?

bind1 |> _*2 ~ Double
	|> double
	|> _+Double.
	
% Backward pipe. Note that it has a higher precedence than any of the forward pipes.

back_in_fwd1 |< lists
	|: filter <| .{_>1}
	|: map <| .{_+10}.
	
% But wait! Act now and '<|' will do optional underscore replacement just like a forward pipe
	
back_in_fwd2 |< lists
	|: filter <| _>1
	|: map <| _+10.
	
with_guard::list |: reverse.



	