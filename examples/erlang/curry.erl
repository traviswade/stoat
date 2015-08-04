-module(curry).

f1 (A) when is_integer(A) -> 
	fun (B) when is_integer(B) -> 
		fun (C) when is_integer(C) -> 
			A + B + C 
		end 
	end.