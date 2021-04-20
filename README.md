So You Think You're a Functional Language
=========================================

Various implementations of bar-induction.  Basically a stress-test of
higher-order functional programming in each language.

The bar-induction implementation in each example is the function `raw`, along
with the helpers it needs, this is about half the code.  The second half is an
optimizer and pretty printer for the extracted tree.  Take the word 'pretty'
with a grain of salt.

The algorithm is typable in PCF.  So in theory you can write the code in just
about any language.  E.g., η-expanding everything, it maps onto good 'ole Pascal
just fine.  But to have run-times within the age of the universe, you need more
sharing: use some laziness and pull things outside of λ-abstractions
appropriately.

* **S.hs** The gold-standard Haskell version.  Uses Applicative.  Monad is also
  defined but not used.  Assumes that `Word` is 64-bits.  Compiling with `ghc
  -main-is S -O2 S` on my desktop, the run-time is about 0.55 seconds.

* **R.hs** Slightly shorter, but less informative, Haskell version, without
  using monadic programming.  Runtime is about 0.42s on my desktop.

* **Opt.py** Python.  Everything is unboxed as far as practicable in the name of
  performance.  Also the `lambda x=x:` trick used is for creating closures.
  Run-time is about 20 seconds on my desktop.

* **tree.rs** Rust.  Transliteration of S.hs, but things specialised by type.
  Everything is boxed using `Rc`.  Only a factor of four slower than the Haskell
  versions.

  Needs the nightly compilers for `Lazy`.  `Lazy` sucks for use in a function
  signature or data-structure.  That makes it useless for most purposes, as far
  as I can see.  But we're doing functional programming, so we just wrap the
  object in a λ-abstraction.

  Attempting to un-box anything much failed, on my inability to do lifetime
  gymnastics inside data-structures.  You can't do explicit lifetime
  intersections, and universal lifetime quantifiers inside datatypes appear
  to not instantiate properly.

* **search.rs** Rust again, transliteration of R.hs and Opt.py.  Reaches 7
  levels of nested parentheses.  Compared with 2 for the Haskell version.  A bit
  faster than tree.rs, but not much.

* **C++** I did one once.  It was not pretty.  Too embarrassing to make public.
  Friends don't let friends do C++.  Attempting to unbox anything falls afoul of
  the fact that what-passes-for-a-λ in C++ has copy semantics not reference
  semantics, killing the sharing required for sane run-times.
