
### Background and motivation

Stoat is a small language that compiles to Erlang. It is more or less a superset of Erlang, adding features that 

 - Make some common functional patterns easier. Functional in the sense of creating, combining, applying and representing functions. Easier in the sense of typing with your fingers, and then later making sense of and maintaining.  
	
 - Help remove boilerplate in ways that are along the lines of what you might expect of a modern language. String interpolation. Multiline comments. Flexible module mixins. Automatic conversion between records and maps and lists. Fewer redundant keywords and module and function names. Things like that.

Stoat diverges from Erlang only at the lexer/parser level. It generates code that is completely compatible with--and convertible to--plain Erlang that is not that different from what you might write if you had to. There is nothing fundamentally new or profound that you can't do in Erlang with more keystrokes and nesting. This module listing gives a taste of stoat:
	
	-export([my_chain/1, my_wrapper/1]).
	
	-def(maybe(Default), {{ok, Res}, F| F(Res); {error, _}, _| Default}).
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
		|) risky_mod:risky_fun1
		|> risky_mod:risky_fun2
		|/ ?log(`so far so good: #{_}`)
		|> risky_mod:risky_fun3.
		
		
Stoat is experimental, and its syntax, feature set and implementation are likely to change quite a bit. You should not use it under any circumstances.


### Documentation

See linked examples and their [Erlang equivalents](examples/erlang). In most cases they are pretty well commented.

 - [Basics](examples/stoat/basic.st), nearly all in common with Erlang.

 - [Pipes](examples/stoat/pipes.st), a rich set of pipe operators that help with much of what you might want to accomplish with monads in the absence of type inference or the ability to limit side effects.

 - [Cuts and underscore substitution](examples/stoat/cuts.st) like you might find in Scala.

 - [Plain old curried functions](examples/stoat/curry.st).

 - [Function](examples/stoat/funs.st) and [guard](examples/stoat/guards.st) syntax.

 - Module [Mixins](examples/stoat/mixins.st).

 - Automatic [record conversion](examples/stoat/record_conversion.st) to and from maps and lists.

 - [String interpolation](examples/stoat/stringinterp.st).


### Technical

Stoat grew out of various parse transforms, macros and libraries. It uses [leex](http://erlang.org/doc/man/leex.html) and [yecc](http://erlang.org/doc/man/yecc.html) to target Erlang syntax trees. Although this method provides very good compatibility with Erlang and has made some nice features pretty much free (robust shell, pretty-printed Erlang as an intermediate) and generally made stoat easy to get up and running, it imposes some important limitations and is likely to change in the future.

### Coming soon

#### Very soon

Parse transforms in stoat and Erlang.

Erlang build artifacts. Smarter build support. The rebar plugin should be ashamed of itself.

More flexible Record <-> map conversion

#### Medium soon

A proper preprocessor (see the current stoat_macros module).

Types. There is currently no type support. In the future the guard syntax will be expanded to integrate with typer and dialyzer.

Scripting and better console support.

emacs mode.

#### Not so soon

javascript target

### Use

NOTE: Experimental. Do not use.

To use, include as a rebar dependency, and add `{plugins, [stoat]}` in your rebar.config. It will blindly recompile each `.st` file on each rebar compile.

To run a stoat shell from within Erlang: `stoat_shell:start()`. Or start your app using something like `erl -pa blah blah blah... -noshell -s stoat_shell`.

#### Calling Stoat from Erlang

	stoat_module:stoat_fuction(AnyErlangArgument).
	
#### Calling Erlang from Stoat

	erlang_module:erlang_function(AnyStoatArgument)

### License

Not ready for use. Do not use under any circumstances.

