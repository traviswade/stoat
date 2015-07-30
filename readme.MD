
Stoat is an Erlang-like language for the Erlang VM.

### Background and motivation

Stoat is an overgrown set of ill-advised Erlang parse transforms. It adds a variety of features to the language, complicating its syntax unnecessarily and

Most of the features involve patterns for creating, combining, applying and representing functions that are built in or easily expressed in other functional(-ish) languages but clunky in Erlang; there are some more general additions as well.

Currently it uses leex and yecc to target Erlang syntax trees, which can then be compiled to bytecode or native code. Although this method provides very good compatibility with Erlang and has made early drafts very straightforward to develop, it imposes some important limitations and is likely to change in the future.

### Major features

[Pipes]()

[Cuts and underscore substitution]()

[Function and guard syntax]()

[Mixins]()

[String interpolation]*

### Major shortcomings

### Use

### License