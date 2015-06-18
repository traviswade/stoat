-module(pipes).
-compile(export_all).

double (X__) -> X__ * 2.
add (X) -> fun (Y) -> Y + X end.

basic (X) ->
	double(
		math:log(
			(add(2))(X) + 1)).
			
	
basic1 (X__) ->
	double(
		math:log(
			(add(2))(X__) + 1)).
			
			
maybe (F, {ok, Res}) -> F(Res);
maybe (_, {error, Why}) -> {error, Why}.

wrappers (X__) ->
	maybe(f, 
		maybe(g, 
			maybe(h, X__))).
			
optout (X__) ->
	maybe(f,
		g(maybe(h, X__))).
		
		
tap1 (X__) ->
	f(
		fun (X__) -> log(X__), X__ end(
			g(X__))).
			
tap2 (X__) ->
	maybe(f,
		fun (X__) -> log(X__), X__ end(
			maybe(g, X__))).