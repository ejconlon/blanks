# blanks

[![CircleCI](https://circleci.com/gh/ejconlon/blanks/tree/master.svg?style=svg)](https://circleci.com/gh/ejconlon/blanks/tree/master)

Fill-in-the-blanks - A library factoring out substitution from ASTs.

It's a pain to track de Bruijn indices yourself to implement capture-avoiding subsititution,
so this library provides some wrappers that help. One of the best libraries for this is
[bound](https://hackage.haskell.org/package/bound), which uses a clever representation to make
these operations safe and fast. The tradeoff is that you have to define a `Monad` instance
for your expression functor, which in practice can be tricky. (It's even trickier to derive
`Eq` and `Show`!)

This library takes the simpler, slower, and rather "succ-y" free-monad-ish approach,
but with a twist. It expects you to rewrite all name-binding constructors in your expression
as annotations on a single "binder" constructor. This allows you to use the provided `Scope`
type (or a variant) as a wrapper around your expression functor, which is only required to
implement `Functor`. This representation is less safe (since you can inspect and manipulate
bound variables), but if you stick to the provided combinators, things will work out fine.

You'll get most of what you want by just importing this module unqualified.
See `Scope` for the basic wrapper and `LocScope` for a wrapper with annotations you can use
for source locations and the like. See the test suite for examples.
