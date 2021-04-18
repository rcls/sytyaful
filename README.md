So You Think You're a Functional Language
=========================================

Various implementations of bar-induction.

Basically a stress-test of functional programming.  The algorithm is typable in
Gödel's T.

* **S.hs** The gold-standard Haskell version.  Uses Applicative.  Monad is also
  defined but not used.  Assumes that Word is 64-bits.

* **R.hs** Slightly shorter but less informative Haskell version, without using
  monadic programming.  Uses big-ints (`Numeric.Natural`), so it's slightly
  slower.

* **Opt.py** Python.  Everything is hand-inlined and unboxed as far as
  practicable in the name of performance, and the `lambda x=x:` trick used
  for creating closures.

* **tree.rs** Rust.  Transliteration of S.hs, but things specialised by type.
  Everything is boxed using `Rc`.  Only a factor of two slower than the
  Haskell versions.

  Attempting to un-box anything much failed, on my inability to do lifetime
  gymnastics inside data-structures.  You can't do explicit lifetime
  intersections, and universal lifetime quantifiers inside datatypes appear
  to not instantiate properly.

* **search.rs** Rust again, transliteration of R.hs.

* **C++** I did one once.  It was not pretty.  To embarrassing to make public.
  Attempting to unbox anything falls afoul of the fact that
  what-passes-for-a-lambda in C++ has copy semantics not reference semantics
  and the sharing required for sane run-times disappears.
