parray
======

`parray` is a reimplementation of/playground for understanding Data Parallel Haskell.

Features
--------

* Pure Haskell. I don't touch the GHC source code (that thing is scary), so this could theoretically be portable.
* Efficient representation of arrays. Unlike the current DPH implementation in GHC, which uses the same array representation described in "Work Efficient Higher-Order Vectorisation," I do better. In the paper's representation, an array of arrays of arrays of functions will be represented as (in pseudo-Haskell) `Vector (Clo (PDatas e) (e -> a -> b) (Int -> PData e -> PData a -> PData b))`. In my representation, it will be `Clo (PData (PData (PData e))) (e -> a -> b) (Int -> PData e -> PData a -> PData b)`. Essentially, the current DPH scales well to nested arrays, but not to doubly nested arrays.

Todo
----

* Split it into multiple modules. Since I was doing this originally only to understand what was going on, I wrote it in one huge and ugly file.
* Finish the core parts. Concatenation isn't properly working for nested arrays or primitive arrays.
* Write a bunch of core combinators (`mapP`, `foldP`, etc.)
* Finish the other parts (like sum types)
