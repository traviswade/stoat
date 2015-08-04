
### Background and motivation

Stoat is an overgrown set of ill-advised Erlang parse transforms. It adds a variety of features to the language, complicating its syntax unnecessarily and promoting questionable practices.

Many of the features involve patterns for creating, combining, applying and representing functions that are built in or easily expressed in other functional(ish) languages but clunky in Erlang.

Currently Stoat uses [leex](http://erlang.org/doc/man/leex.html) and [yecc](http://erlang.org/doc/man/yecc.html) to target erl syntax trees, which can then be compiled to bytecode or native code. Although this method provides very good compatibility with Erlang and has made early drafts very straightforward to develop, it imposes some important limitations and is likely to change in the future.

### Major features

See linked examples and their [Erlang equivalents](examples/erlang).

[Pipes](examples/stoat/pipes.st)

[Cuts and underscore substitution](examples/stoat/cuts.st)

[Plain old curried functions](examples/stoat/curry.st)

[Function](examples/stoat/funs.st) and [guard](examples/stoat/guards.st) syntax

[Mixins]()

[String interpolation](examples/stoat/stringinterp.st)

### Coming soon

Complete language features.

Proper preprocessor.

REPL.

### Use

Not ready for use.

### License

Do not use. Not ready.

