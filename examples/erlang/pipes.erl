-module(pipes).
-compile(export_all).

double (X__) -> X__ * 2.
add (X) -> fun (Y) -> Y + X end.

basic (X) -> double(math:log((add(2))(X) + 1)).
			
basic1 (X__) -> double(math:log((add(2))(X__) + 1)).
			
maybe (F, {ok, Res}) -> F(Res);
maybe (_, {error, Why}) -> {error, Why}.

wrappers (X__) -> maybe(f, maybe(g, maybe(h, X__))).
			
optout (X__) ->maybe(f,g(maybe(h, X__))).
		
tap1 (X__) -> f(fun (X__) -> log(X__), X__ end(g(X__))).
			
tap2 (X__) -> maybe(f, fun (X__) -> log(X__), X__ end(maybe(g, X__))).
			
mod1 (X__) ->
	maps:values(
		maps:put(x, 1, 
			maps:without([rev, id],
				X__))).
				
mod2 (X__) ->
	maybe(fun (X__) -> mydb:save(X__) end, 
		maybe(somesortofupdate, 
			mydb:fetch(X__))).

mod3 (X__) ->
	lists:map(fun(X__) -> X__+10 end, 
		lists:filter(fun(X__) -> X__>1 end, X__)).
		
case1 (X__) ->
	atom_to_list(
		case X__+1 of
			2 -> one;
			3 -> two;
			_ -> other end).
		
case2 (X__) ->
	case X__ of
		I when is_integer(I), I > 2 -> ok;
		_ -> error 
	end.

if1 (X__) -> if X__ -> ok; true -> error end.

bind1 (X__) -> Double = X__*2,
	double(Double) + Double.
	
back_in_fwd1 (X__) ->
	lists:map(fun(X__) -> X__+10 end, 
		lists:filter(fun(X__) -> X__>1 end, X__)).
		
back_in_fwd2 (X__) ->
	lists:map(fun(X__) -> X__+10 end, 
		lists:filter(fun(X__) -> X__>1 end, X__)).

with_guard (X__) when is_list(X__) ->
	lists:reverse(X__).

