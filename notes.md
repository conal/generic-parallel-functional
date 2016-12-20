# Notes for a paper about generic parallel functional programming

## Outline

*   Introduction
*   Generic programming in Haskell
*   Some useful data types:
    *   Right-lists
    *   Left-lists
    *   Top-down trees
    *   Bottom-up trees
    *   Shape-indexed variations
    *   Top-down and bottom-up bushes
*   Parallel scan
*   FFT
*   Related work
*   Reflections/conclusions

## Abstract

Parallel programming, whether imperative or functional, has long focused on arrays as the central data type.
Meanwhile, typed functional programming has explored a variety of data types, including lists and various forms of trees.
*Generic* functional programming decomposes these data types into a small set of fundamental building blocks: sum, product, composition, and their associated identities.
Definitions over these few fundamental type constructions then automatically assemble into algorithms for an infinite set of data types---some familiar and some new.
This paper presents generic functional formulations for two important and well-known classes of parallel algorithms: parallel scan (generalized prefix sum) and Fast Fourier Transform (FFT).
Notably, arrays play no role in these formulations.
Consequent benefits include a simpler and more compositional style, much use of common algebraic patterns (such as `Functor`, `Applicative`, `Foldable`, and `Traversable`), and freedom from possibility of run-time indexing errors.
The functional generic style also clearly reveals deep commonality among what otherwise appears to be quite different algorithms.
Instantiating the generic formulations to "top-down" and "bottom-up" trees, two well-known algorithms for each of parallel scan and FFT naturally emerge, as well as two possibly new algorithms.

## Introduction

## Generic programming in Haskell

["Functor algebra"]

## Some useful data types

### Right-lists and left-lists

Let's start with a very familiar data type of lists:

``` haskell
data List a = Nil | Cons a (List a)
```

This data type is sometimes more specifically called "cons lists" (for historical reasons going back to early Lisp implementations).
One might also call them "right lists", since they grow rightward.

``` haskell
data ListR a = NilR | a :< ListR a
```

Alternatively, there are "snoc lists" or "left lists", which grow leftward:

``` haskell
data ListL a = NilL | ListL a >: a
```

In terms of our functor algebra, we are using sum, unit, identity, and product:

``` haskell
type ListR =~ U1 :+: Par1 :*: ListR
type ListL =~ U1 :+: ListR :*: Par1
```

Spelling out the isomorphism explicitly,

``` haskell
instance Generic1 ListR where
  type Rep1 ListR = U1 :+: Par1 :*: ListR
  from NilR = L1 U1
  from (a :< as) = R1 (Par1 a :*: as)
  to (L1 U1) = NilR
  to (R1 (Par1 a :*: as)) = a :< as
```

``` haskell
instance Generic1 ListL where
  type Rep1 ListL = U1 :+: ListL :*: Par1
  from NilL = L1 U1
  from (a :< as) = R1 (as :*: Par1 a)
  to (L1 U1) = NilL
  to (R1 (as :*: Par1 a)) = as >: a
```

Not only are `ListR` and `ListL` isomorphic to their underlying representation functors, but also to each other, as follows:

``` haskell
rToL :: ListR a -> ListL a
rToL NilR = NilL
rToL (a :< as) = rToL as >: a

lToR :: ListL a -> ListR a
lToR NilL = NilR
lToR (as >: a) = a :< lToR as
```

Since these list types are easily isomorphic, why would we want to distinguish between them?
One reason is that they may capture different intents.
For instance, a zipper for right lists comprises a left-list for the (reversed) elements leading up to a position, a current element of focus, and a right-list for the not-yet-visited elements:

``` haskell
data ZipperListR a = ZipperListR (ListL a) a (ListR a)
```

or

``` haskell
type ZipperListR = ListL :*: Par1 :*: ListR
```

[To do: review Conor McBride's papers, and cite them here.]

Another reason is that have usefully different instances for standard type classes, leading---as we will see---to different operational characteristics, especially with regard to parallelism.

### Top-down trees

After lists, trees are perhaps the most commonly used data structure in functional programming.
Moreover, in contrast with lists, the symmetry possible with trees naturally leads to parallel-friendly algorithms.
Also unlike lists, there are quite a few varieties of trees.

Start with a simple binary leaf tree, i.e., one in which data occurs only in leaves:

``` haskell
data Tree a = Leaf a | Branch (Tree a) (Tree a)
```

As one variation, we could instead place data in the branch nodes:

``` haskell
data Tree a = Leaf | Branch (Tree a) a (Tree a)
```

Another variation is ternary rather than binary leaf trees:

``` haskell
data Tree a = Leaf a | Branch (Tree a) (Tree a) (Tree a)
```

Already, this style of definition is starting to show some strain.
The repetition seen in the data type definition will also appear in instance definitions.
For instance, for ternary leaf trees:

``` haskell
instance Functor Tree where
  fmap h (Leaf a) = Leaf (h a)
  fmap h (Branch t1 t2 t3) = Branch (fmap h t1) (fmap h t2) (fmap h t3)

instance Foldable Tree where
  foldMap h (Leaf a) = h a
  foldMap h (Branch t1 t2 t3) = foldMap h t1 <> foldMap h t2 <> foldMap h t3

instance Traversable Tree where
  traverse h (Leaf a) = Leaf <$> h a
  traverse h (Branch t1 t2 t3) =
    Branch <$> traverse h t1 <*> traverse h t2 <*> traverse h t3
```

Not only do we have repetition *within* each instance definition (the three occurrences of `fmap h` above), we also have repetition *among* instances for $n$-ary trees for different $n$.
Fortunately, we can simplify and unify with a shift in formulation.
Think of a branch node as having not $n$ subtrees, but rather a single uniform $n$-tuple of subtrees.
Assume for now that we have a functor of finite lists statically indexed by length as well as element type:

``` haskell
type Vec :: Nat -> * -> *  -- abstract for now
```

[Explain notation.]

Then we can define a single type of $n$-ary leaf trees:

``` haskell
data Tree n a = Leaf a | Branch (Vec n (Tree a))
```

The instance definitions are simpler than even for binary trees given above:

``` haskell
instance Functor (Tree n) where
  fmap h (Leaf a) = Leaf (h a)
  fmap h (Branch ts) = Branch ((fmap.fmap) f ts)

instance Foldable (Tree n) where
  foldMap h (Leaf a) = h a
  foldMap h (Branch ts) = (foldMap.foldMap) h ts

instance Traversable (Tree n) where
  traverse h (Leaf a) = Leaf <$> h a
  traverse f (Branch ts) = Branch <$> (traverse.traverse) f ts
```

[Note `(<$>)` is infix `fmap`.]

Notice that these instance definitions rely on very little about the `Vec n` functor.
Specifically, for each of `Functor`, `Foldable`, and `Traversable`, the instance for `Tree n` needs only the corresponding instance for `Vec n`.
For this reason, we can easily generalize `Vec n` as follows:

``` haskell
data Tree f a = Leaf a | Branch (f (Tree a))
```

The instance definitions for "$f$-ary" trees are exactly as with $n$-ary, except for making the requirements on $f$ implicit:

``` haskell
instance Functor f => Functor (Tree f) where ...
instance Foldable f => Foldable (Tree f) where ...
instance Traversable f => Traversable (Tree f) where ...
```

[Type-check all of these definitions with a test module.]

This generalization covers "list-ary" (rose) trees and even "tree-ary" trees.

Just as there are both left- and right-growing lists, trees come in two flavors as well.
The forms above are all "top-down", in the sense that successive unwrappings of branch nodes reveal subtrees moving from the top downward.
(No unwrapping for the top level, one unwrapping for the collection of next-to-top subtrees, another for the collection of next level down, etc.)
There are also "bottom-up" trees, in which successive branch node unwrappings reveal the information in subtrees from the bottom moving upward.
In short:

*   A top-down leaf tree is either a leaf or an $f$-structure of trees.
*   A bottom-up leaf tree is either a leaf or a tree of $f$-structures.

For reasons explained below, we'll call top-down trees "`TreeR`" and bottom-up trees "`TreeL`".
In Haskell,

``` haskell
data TreeR f a = LeafR a | BranchR (f (TreeR a))

data TreeL f a = LeafL a | BranchL (TreeL (f a))
```

Bottom-up trees (`TreeL`) are a canonical example of "nested" or "non-regular" data types, requiring polymorphic recursion [citations].

### Shape-indexed variations

[As GADTs and as type families.]

Some algorithms work only on collections of a limited size.
For instance, the most common parallel scan and FFT algorithms are limited to arrays of size $2^n$, while the more general (not just binary) Cooley-Tukey FFT algorithms require composite size, i.e., $m \cdot n$ for integers $m, n \ge 2$.
In array-based algorithms, these restrictions can be realized in one of two ways:

*   Check array sizes dynamically, incurring a performance penalty; or
*   Document the restriction, assume the best, and blame the library user if the assumption is violated.

A third---much less commonly used--option is to statically verify the size restriction at the call site, perhaps by using a dependently typed language and providing proofs as part of the call.

A lightweight compromise is to simulate some of the power dependent types via type-level encodings of sizes, as with our the of `Nat` for indexing the `Vec` type above.
There are many possible definitions for `Nat`.
For this paper, assume that `Nat` is a kind-promoted version [cite] of the following data type of Peano numbers:

``` haskell
data Nat = Zero | Succ Nat
```

Thanks to promotion, `Nat` is not only a new data type with value-level constructors `Zero` and `Succ`, but also a new *kind* with *type-level* constructors `Zero` and `Succ`.

#### GADT formulation

Now we can define the length-indexed `Vec` type above, which is the canonical example of dependent types in either full dependently typed languages or as simulated with generalized algebraic data types (GADTs).
As with lists, there are right- and left-growing versions:
The former (borrowing constructor names from right- and left-lists):

``` haskell
data VecR :: Nat -> * -> * where
  NilR :: VecR Zero a
  (:<) :: a -> VecR n a -> VecR (Succ n) a

data VecL :: Nat -> * -> * where
  NilL :: VecL Zero a
  (:<) :: VecL n a -> a -> VecL (Succ n) a
```

For leaf trees, we have a choice between perfect and imperfect trees.
A "perfect" leaf tree is one in which all leaves are at the same depth.
Both forms can be "statically shaped", but we'll just perfect trees in this paper.
For a perfect tree, we need only a single type-level number signifying the depth of all leaves:

``` haskell
data RPow :: (* -> *) -> Nat -> * -> * where
  L :: a -> RPow h Z a
  B :: h (RPow h n a) -> RPow h (S n) a

data LPow :: (* -> *) -> Nat -> * -> * where
  L :: a -> LPow h Z a
  B :: LPow h n (h a) -> LPow h (S n) a
```


#### Type family formulation

### Top-down and bottom-up bushes

## Parallel scan

## FFT

## Related work

## Reflections/conclusions

*   How would this work look with full dependent types?

* * * * * * * * * * * * * * * * * * * *

## Misc

*   Alternative to arrays:
    *   More compositional.
    *   Exploit operations from existing abstractions (type classes).
    *   Safe from numeric indexing errors.
*   Infinite family of correct algorithms indexed by data type.
    It's easier to select a data type than implement a correct parallel algorithm.
*   "Parallel-friendly" might be more suitable than "parallel".
*   Reveals commonality of some algorithms that appear quite different.
    Theme: postpone optimization.
