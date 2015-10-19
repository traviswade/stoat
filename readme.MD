
### Background and motivation

Stoat is another front end for the Erlang compiler, because that is what the world needs now. It offers a syntax that is mostly pretty similar to Erlang, adding features that:

 - Make some common functional patterns easier. Functional in the sense of creating, combining, applying and representing functions. Easier in the sense of typing with your fingers, and then later making sense of and maintaining.  
	
 - Help remove boilerplate in ways that are along the lines of what you might expect of a modern language. String interpolation. Multiline comments. Flexible module mixins. Automatic conversion between records and maps and proplists. Fewer redundant keywords and module and function names. Things like that.

There is nothing fundamentally new or profound that you can't do in Erlang with more keystrokes and lots of nesting. This module listing gives a taste of what is possible:
	
	-export([my_chain/1, my_wrapper/1]).
	
	-def(maybe(Default), {|{ok, Res}, F| F(Res); {error, _}, _| Default}).
	-def(log, io:format).

	my_chain::list
		|:  usort
		|:  dropwhile.{_ < 200}
		|:  filter.{_ rem 2 == 0} 	~ Evens
		|/  ?log(`#{length(_)} left`)
		|: 	map &math:sqrt/1
		|:  takewhile.{_ > 90}
		|>  `#{length(_)} of the #{length(Evens)} unique even numbers over 200 met the criteria`.
	
	my_wrapper |+ ?maybe([])
		|)  risky_mod:risky_fun1
		|>  risky_mod:risky_fun2
		|/  ?log(`so far so good: #{_}`)
		|>  risky_mod:risky_fun3.
		
		
Stoat is experimental, and its syntax, feature set and implementation are likely to change quite a bit. You should not use it under any circumstances.


### Documentation

See linked examples and their [Erlang equivalents](examples/erlang).

 - [Basics](examples/stoat/basic.st), nearly all in common with Erlang.

 - [Pipes](examples/stoat/pipes.st), a rich set of pipe operators that help with much of what you might want to accomplish with monads in the absence of type inference or the ability to limit side effects.

 - [Cuts and underscore substitution](examples/stoat/cuts.st) like you might find in Scala.

 - [Plain old curried functions](examples/stoat/curry.st).

 - [Function](examples/stoat/funs.st) and [guard](examples/stoat/guards.st) syntax.

 - Module [Mixins](examples/stoat/mixins.st).

 - Automatic [record conversion](examples/stoat/record_conversion.st) to and from maps and lists.

 - [String interpolation](examples/stoat/stringinterp.st).


### Technical

Stoat grew out of various parse transforms, macros and libraries. It currently uses [leex](http://erlang.org/doc/man/leex.html) and [yecc](http://erlang.org/doc/man/yecc.html) to target Erlang syntax trees. 

### Coming soon

#### Very soon

Parse transforms in stoat and Erlang.

Erlang build artifacts. Much better build support.

More flexible Record <-> map conversion

#### Medium soon

A proper preprocessor (see the current stoat_macros module).

Types. There is currently no type support. In the future the guard syntax will be expanded to integrate with typer and dialyzer.

Scripting and better console support.

emacs mode.


### Use

NOTE: Experimental. Do not use.

To use, include as a rebar dependency, and add `{plugins, [stoat]}` in your rebar.config. It will blindly recompile each `.st` file on each rebar compile.

To run a stoat shell from within Erlang: `stoat_shell:start()`. Or start your app using `erl -pa blah blah blah... -noshell -s stoat_shell`.

#### Calling Stoat from Erlang

	stoat_module:stoat_fuction(AnyErlangArgument).
	
#### Calling Erlang from Stoat

	erlang_module:erlang_function(AnyStoatArgument)

### License

Not ready for use. Do not use under any circumstances.

