So You Think You're a Functional Language
=========================================

> Pronunciation: *Sɪt yə fuːl* (Sit Ya Fool)

Various implementations of bar-induction.  Basically a stress-test of
higher-order functional programming in each language.

The bar-induction implementation in each example is the function `raw`, along
with the helpers it needs, this is about half the code.  The second half is an
optimizer and pretty printer for the extracted tree.  Take the word 'pretty'
with a grain of salt.

The algorithm is typable in PCF.  So in theory you can write the code in just
about any language.  E.g., η-expanding everything, it maps onto good ol' Pascal
just fine.  But to have run-times within the age of the universe, you need more
sharing: use some laziness and pull things outside of λ-abstractions
appropriately.

Note that the run time is very sensitive to the order of evaluation.
Specifically, on the `lift` functions which order do we put the two sub-searches
on, and also, searching for the pivot, which way round are merged `arbitrary`
and `different` parameters.  I have kept this identical between the different
versions.

* **S.hs** The gold-standard Haskell version.  Uses Applicative.  Monad is also
  defined but not used.  Assumes that `Word` is 64-bits.  Compiling with `ghc
  -main-is S -O2 S` on my desktop, the run-time is about 0.4 seconds.

* **R.hs** Slightly shorter, but less informative, Haskell version, without
  using monadic programming.  Runtime is about 0.32s on my desktop.  Just to
  raise a finger at the longer implementations, we slip in a little `deriving
  Functor` into the pretty printer, and use it.

* **Opt.py** Python.  Everything is unboxed as far as practicable in the name of
  performance.  Also the `lambda x=x:` trick used is for creating closures.
  Run-time is about 10 seconds on my desktop.

* **sytyaful.c** C.  About half the runtime of the Haskell version, and 10 times
  as much code.  All the lazyness, and all the bindings, pedantically spelled
  out item by item, and then glued together with custom allocaters.  You have to
  be a masochist to either write or read this code.  Living proof to the fact
  that just because you can do it, doesn't mean it's a good idea.

* **tree.rs** Rust.  Transliteration of S.hs, but things specialised by type.
  Everything is boxed using `Rc`.  Only a factor of two slower than the Haskell
  version.

  Needs the nightly compilers for `Lazy`.  `Lazy` doesn't appear to work well
  for use in a function signature or data-structure, forcing us to jump through
  hoops to use it.

  A useful amount of unboxing.  But more should be possible&mdash;you can't seem
  to do much in the way of lifetime gymnastics inside data-structures.  You
  can't do explicit lifetime intersections, and universal lifetime quantifiers
  inside datatypes appear to not instantiate properly.

* **search.rs** Rust again, transliteration of R.hs and Opt.py.  Reaches 7
  levels of nested parentheses.  Compared with 2 for the Haskell version.

  Made some progress with unboxing this one.  Quite a bit faster than tree.rs,
  runtime is 0.9 seconds on my desktop, about half the speed of R.hs.

* **otree.ml** OCaml.  Transliteration of S.hs, again specialising everything.
  Top marks for making the `lazy` monad explicit.  I've always found it deeply
  ironic that Haskell goes to such great length to hide computational monads,
  while promoting monads in general.

* **osearch.ml** Ocaml again.  The downside of the language being strict by
  default, is that you don't get the same degree of strictness analysis and
  unboxing with `lazy` as GHC gives you on Haskell code.  So to eek out
  performance to match the Rust version, we expand some simple code into
  repetitive line-noise.

* **msearch.sml**, **mtree.sml** A couple of Standard ML implementations.  50%
  more lines than the Haskell version, and compiling with mlton, the run-time is
  only a little slower.  So comes second on both metrics.

* **search.js** Javascript (node.js).  Takes about 3.5 seconds on my desktop,
  running with node.js.  Which is pretty decent for a dynamically typed language
  with no separate compilation phase.

* **search.lua** Lua.  A lovely little scripting language that can do real
  functional programming.  It's got nice clean semantics with the glaring
  exception that variables are global by default, unless you declare them
  to be local.  Similar run-time to the Python version, without any of
  the performance hacks I did for the latter.  (Update: running with luajit
  speeds it up by a factor of about 3.)

* **C++** I did one once.  It was not pretty.  Too embarrassing to make public.
  Friends don't let friends do C++.  Attempting to unbox anything falls afoul of
  the fact that what-passes-for-a-λ in C++ has copy semantics not reference
  semantics, killing the sharing required for sane run-times.
