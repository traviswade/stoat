


double -|> _ * 2.
add (X) -> {Y| Y + X}.

basic (X) -> X |> add(2) |> _+1 |> math:log |> double.

basic1 -|> add(2) |> _+1 |> math:log |> double.

maybe (F, {ok, Res})   ->  F(Res);
	(_, {error, Why})  ->  {error, Why}.

wrappers -|+ maybe |> h |> g |> f.
	
optout -|+ maybe |> h |) g |> f.
	
tap1 -|> g |/ log |> f.
	
tap2 -|+ maybe |> g |/ log |> f.
	
mod1 -|< maps(-1)
	|: without([rev, id])
	|: put(x, 1)
	|: values.
	

mod2 -|< mydb(1)
	|: fetch |+ maybe
	|> somesortofupdate
	|: save.
	
mod3 -|< lists
	|: filter.{_>1}
	|: map.{_+10}.



	