% -*- latex -*-

\documentclass[acmlarge,authorversion]{acmart} % ,anonymous

%% \usepackage[colorlinks,urlcolor=black,citecolor=black,linkcolor=black]{hyperref} % ,draft=true

%include polycode.fmt
%include forall.fmt
%include greek.fmt
%include formatting.fmt

\input{macros}

\bibliographystyle{plainnat}
%% % (author date) form
%% \usepackage[]{natbib}
%% \bibpunct();A{},
%% \let\cite=\citep
\citestyle{acmauthoryear}

% \title{Two generic functional parallel algorithms}
% \title{Generic functional parallel algorithms}
% \subtitle{Scan and FFT}
\title{Generic functional parallel algorithms: Scan and FFT}

%% \authorinfo{Conal Elliott}{Target}{conal@@conal.net}
\author{Conal Elliott}
\email{conal@@conal.net}
\affiliation{%
  \institution{Target}
}

%% Temporary
\setcopyright{rightsretained}
\settopmatter{printacmref=false, printccs=true, printfolios=true}

\begin{document}

\maketitle

\section{Abstract}

Parallel programming, whether imperative or functional, has long focused on arrays as the central data type.
Meanwhile, typed functional programming has explored a variety of data types, including lists and various forms of trees.
\emph{Generic} functional programming decomposes these data types into a small set of fundamental building blocks: sum, product, composition, and their associated identities.
Definitions over these few fundamental type constructions then automatically assemble into algorithms for an infinite set of data types---some familiar and some new.
This paper presents generic functional formulations for two important and well-known classes of parallel algorithms: parallel scan (generalized prefix sum) and Fast Fourier Transform (FFT).
Notably, arrays play no role in these formulations.
Consequent benefits include a simpler and more compositional style, much use of common algebraic patterns---such as |Functor|, |Applicative|, |Foldable|, and |Traversable|\out{ \cite{McBride:2008}}---and freedom from possibility of run-time indexing errors.
The functional generic style also clearly reveals deep commonality among what otherwise appears to be quite different algorithms.
Instantiating the generic formulations to ``top-down'' and ``bottom-up'' trees as well as ``bushes'', two well-known algorithms for each of parallel scan and FFT naturally emerge, as well as two possibly new algorithms.

\section{Introduction}

%% \section{Datatype-generic programming in Haskell}

There is a long, rich history of datatype-generic programming in Haskell.
See, e.g., \cite{Gibbons06DatatypeGeneric,DatatypeGenericComparison2012}.
The basic idea of most such designs is to relate a broad range of types to a small set of basic ones via isomorphism (or more accurately, embedding-projection pairs), particularly binary sums and products and their corresponding identities (``void'' and ``unit'').
These type primitives serve to connect algorithms with data types in the following sense:
\begin{itemize}
\item Each data type of interest is encoded into and decoded from these type primitives.
\item Each (generic) algorithm is defined over these same primitives.
\end{itemize}
In this way, algorithms and data types are defined independently and then automatically work together.

One version of this general scheme is found in |GHC.Generics|, in which the type primitives are really \emph{functor} building blocks \cite{HaskellWikiGhcGenerics}.
For this paper, we'll use six: sum, product, composition, and their three corresponding identities, as in \figref{ghc-generics}.

There are additional definitions that capture recursion and meta-data such as field names and operator fixity, but the collection in \figref{ghc-generics} suffices for this paper.
To make the encoding of data types easy, |GHC.Generics| comes with a generic deriving mechanism (enabled by the |DeriveGeneric| flag), so that for regular (not generalized) algebraic data types, one can simply write ``|data ... deriving Generic|'' for types of kind |*| \cite{Magalhaes:2010}.
For type constructors of kind |* -> *|, as in this paper, one derives |Generic1| instead.
Instances for non-regular algebraic data types can be defined explicitly, which amounts to giving an encoding functor |Rep f| and encoding and decoding operations |to1| and |from1|, as in \figref{Generic1}.
\figpairW{0.52}{0.40}{ghc-generics}{Functor building blocks}{
\begin{code}
data     (f  :+:  g)  a = L1 (f a) | R1 (g a)  SPC  -- sum
data     (f  :*:  g)  a = f a :*: g a               -- product
newtype  (g  :.:  f)  a = Comp1 (g (f a))           -- composition

data     V1           a                             -- void
newtype  U1           a = U1                        -- unit
newtype  Par1         a = Par1 a                    -- singleton
\end{code}
}{Generic1}{Functor encoding and decoding}{
\vspace{1.9ex}
\begin{code}
-- Representable types of kind |* -> *|.
class Generic1 f where
  type Rep1 f :: * -> *
  from1  :: f a -> Rep1 f a
  to1    :: Rep1 f a -> f a
\end{code}
\vspace{1.9ex}
}
To define a generic algorithm, one provides class instances for these primitives and writes a default definition for each method in terms of |from1| and |to1|.

The effectiveness of generic programming relies on having at our disposal a variety of data types, each corresponding to a unique composition of the small set of generic type building blocks.
In contrast, parallel algorithms are usually designed and implemented in terms of the \emph{single} data type of arrays.
The various array algorithms involve idiosyncratic patterns of traversal and construction of this single data type.
For instance, a parallel array reduction with an associative operator involves recursive or iterative generation of numeric indices for extracting elements, taking care that each element is visited exactly once and combined left-to-right.
Frequently, adjacent element pairs are combined, resulting in an array of half size, and the process repeats until there is only one value remaining.
Alternatively, the array is split, with each half processed independently and results combined later.
(Often, such operations are performed in place, littering the original array with partial reduction results.)
The essential idea of such an algorithm is the natural fold for perfect, binary leaf trees, but this essence is obscured by an \emph{encoding} of trees as arrays.
The correctness of the algorithm depends on careful translation of the natural tree algorithm.
Mistakes typically hide in the tedious details of index arithmetic, which must be perfectly consistent with the particular encoding chosen.
Those mistakes will not be caught by a type-checker (unless programmed with dependent types and full correctness proofs), instead manifesting at run-time in the form of incorrect results and/or index out-of-bound errors.
Note also that this array reduction algorithm only works for arrays whose size is a power of two.
This restriction is a dynamic condition rather than part of the type signature.
If we use the essential data type (a perfect, binary leaf tree) directly rather than via an encoding, it is easy to capture this restriction in the type system and check it statically.
The Haskell-based formulations below use GADTs (generalized algebraic data types) and type families.

When we use natural, recursively defined data types \emph{explicitly}, we can use standard programming patterns such as folds and traversals \out{\cite{McBride:2008}} directly.
In a language like Haskell, those patterns follow known laws and are well supported by the programming ecosystem.
The use of array encodings makes those patterns \emph{implicit}, as a sort of informal guide only, distancing programs from the elegant and well-understood laws and abstractions that motivate the programs, justify their correctness, and point to algorithmic variations that solve related problems or make different implementation trade-offs.

Even the \emph{determinacy} of an array-based parallel algorithm can be difficult to ensure or validate.
When the result is an array rather than a single value, as in scans and FFTs, values are written to indexed locations.
In the presence of parallelism, determinacy depends on those write indices being distinct, which again is a subtle, encoding-specific property, unlikely to be verified automatically.

Given these severe drawbacks, why are arrays so widely used in designing, implementing, and explaining parallel algorithms?
One benefit is a relatively straightforward mapping from algorithm to efficient implementation primitives.
As we will see below, however, we can instead write algorithms in elegant, modular style using a variety of data types and the standard algebraic abstractions on those data types--such as |Functor|, |Applicative|, |Foldable|, and |Traversable| \cite{McBride:2008}---\emph{and} generate very efficient implementations.
Better yet, we can define such algorithms generically.

\todo{contributions}

\section{Some useful data types}

\subsection{Right-lists and left-lists}

Let's start with a very familiar data type of lists:
\begin{code}
data List a = Nil | Cons a (List a)
\end{code}
This data type is sometimes more specifically called ``cons lists''\out{ (for historical reasons going back to early Lisp implementations)}.
One might also call them ``right lists'', since they grow rightward:
\begin{code}
data RList a = RNil | a :< RList a
\end{code}
Alternatively, there are ``snoc lists'' or ``left lists'', which grow leftward:
\begin{code}
data LList a = LNil | LList a >: a
\end{code}
In terms of our functor algebra, we are using sum, unit, identity, and product:
\begin{code}
type RList =~ U1 :+: Par1 :*: RList
type LList =~ U1 :+: RList :*: Par1
\end{code}
Spelling out the isomorphisms explicitly,
\begin{code}
instance Generic1 RList where
  type Rep1 RList = U1 :+: Par1 :*: RList
  from RNil       = L1 U1
  from (a :< as)  = R1 (Par1 a :*: as)
  to (L1 U1)               = RNil
  to (R1 (Par1 a :*: as))  = a :< as

instance Generic1 LList where
  type Rep1 LList = U1 :+: LList :*: Par1
  from LNil       = L1 U1
  from (a :< as)  = R1 (as :*: Par1 a)
  to (L1 U1)               = LNil
  to (R1 (as :*: Par1 a))  = as >: a
\end{code}

|RList| and |LList| are isomorphic not only to their underlying representation functors, but also to each other, as follows:\notefoot{Probably drop these definitions.}
\begin{code}
rToL :: RList a -> LList a
rToL RNil       = LNil
rToL (a :< as)  = rToL as >: a

lToR :: LList a -> RList a
lToR LNil       = RNil
lToR (as >: a)  = a :< lToR as
\end{code}
Since these list types are easily isomorphic, why would we want to distinguish between them?
One reason is that they may capture different intentions.
For instance, a zipper for right lists comprises a left-list for the (reversed) elements leading up to a position and a right-list for the not-yet-visited elements \cite{HuetZipper1997,McBride01derivative}:
\begin{code}
data ListZipper a = ListZipper (LList a) (RList a)
\end{code}
or
\begin{code}
type ListZipper = LList :*: RList
\end{code}
This same type serves as a zipper for left-lists as well.

Another reason for distinguishing left- from right-lists is that they have usefully different instances for standard type classes, leading---as we will see---to different operational characteristics, especially with regard to parallelism.

\subsection{Top-down trees}

After lists, trees are perhaps the most commonly used data structure in functional programming.
Moreover, in contrast with lists, the symmetry possible with trees naturally leads to parallel-friendly algorithms.
Also unlike lists, there are quite a few varieties of trees.

Start with a simple binary leaf tree, i.e., one in which data occurs only in leaves:
\begin{code}
data Tree a = Leaf a | Branch (Tree a) (Tree a)
\end{code}
As one variation, we could instead place data in the branch nodes:
\begin{code}
data Tree a = Leaf | Branch (Tree a) a (Tree a)
\end{code}
Another variation is ternary rather than binary leaf trees:
\begin{code}
data Tree a = Leaf a | Branch (Tree a) (Tree a) (Tree a)
\end{code}

%format t1
%format t2
%format t3

Already, this style of definition is starting to show some strain.
The repetition present in the data type definition will be mirrored in instance definitions.
For instance, for ternary leaf trees,
\begin{code}
instance Functor Tree where
  fmap h (Leaf a)           = Leaf (h a)
  fmap h (Branch t1 t2 t3)  = Branch (fmap h t1) (fmap h t2) (fmap h t3)

instance Foldable Tree where
  foldMap h (Leaf a)           = h a
  foldMap h (Branch t1 t2 t3)  = foldMap h t1 <> foldMap h t2 <> foldMap h t3

instance Traversable Tree where
  traverse h (Leaf a)           = fmap Leaf (h a)
  traverse h (Branch t1 t2 t3)  = liftA3 Branch (traverse h t1) (traverse h t2) (traverse h t3)
\end{code}

Not only do we have repetition \emph{within} each instance definition (the three occurrences each of |fmap h|, |foldMap h|, and |traverse h| above), we also have repetition \emph{among} instances for $n$-ary trees for different $n$.
Fortunately, we can simplify and unify with a shift in formulation.
Think of a branch node not as having $n$ subtrees, but rather a single uniform $n$-tuple of subtrees.
Assume for now that we have a functor of finite lists statically indexed by length as well as element type:
\begin{code}
type Vec :: Nat -> * -> * SPC -- abstract for now
instance Functor      (Vec n) where ...
instance Foldable     (Vec n) where ...
instance Traversable  (Vec n) where ...
\end{code}
Define a single type of $n$-ary leaf trees, polymorphic over $n$:
\begin{code}
data Tree n a = Leaf a | Branch (Vec n (Tree a))
\end{code}
The more general vector-based instance definitions are simpler than even the binary-only versions given above:
\begin{code}
instance Functor (Tree n) where
  fmap h (Leaf a)     = Leaf (h a)
  fmap h (Branch ts)  = Branch ((fmap.fmap) f ts)

instance Foldable (Tree n) where
  foldMap h (Leaf a)     = h a
  foldMap h (Branch ts)  = (foldMap.foldMap) h ts

instance Traversable (Tree n) where
  traverse h (Leaf a)     = fmap Leaf (h a)
  traverse f (Branch ts)  = fmap Branch ((traverse.traverse) f ts)
\end{code}

Notice that these instance definitions rely on very little about the |Vec n| functor.
Specifically, for each of |Functor|, |Foldable|, and |Traversable|, the instance for |Tree n| needs only the corresponding instance for |Vec n|.
For this reason, we can easily generalize from |Vec n| as follows:
\begin{code}
data Tree f a = Leaf a | Branch (f (Tree a))
\end{code}
The instance definitions for ``|f|-ary'' trees are exactly as with $n$-ary, except for making the requirements on |f| explicit:
\begin{code}
instance Functor      f => Functor      (Tree f) where ...
instance Foldable     f => Foldable     (Tree f) where ...
instance Traversable  f => Traversable  (Tree f) where ...
\end{code}
This generalization covers ``list-ary'' (rose) trees and even ``tree-ary'' trees.
With this functor-parametrized tree type, we can reconstruct $n$-ary trees as |Tree (Vec n)|.

Just as there are both left- and right-growing lists, |f|-ary trees come in two flavors as well.
The forms above are all ``top-down'', in the sense that successive unwrappings of branch nodes reveal subtrees moving from the top downward.
(No unwrapping for the top level, one unwrapping for the collection of next-to-top subtrees, another for the collection of next level down, etc.)
There are also ``bottom-up'' trees, in which successive branch node unwrappings reveal the information in subtrees from the bottom moving upward.
In short:
\begin{itemize}
\item
  A top-down leaf tree is either a leaf or an |f|-structure of trees.
\item
  A bottom-up leaf tree is either a leaf or a tree of |f|-structures.
\end{itemize}
In Haskell,
\begin{code}
data TTree f a = TLeaf a | TBranch (f (TTree a))

data BTree f a = BLeaf a | BBranch (BTree (f a))
\end{code}
Bottom-up trees (|BTree|) are a canonical example of ``nested'' or ``non-regular'' data types, requiring polymorphic recursion \cite{Bird1998}.

\subsection{Statically shaped variations}\seclabel{statically-shaped-types}

Some algorithms work only on collections of restricted size.
For instance, the most common parallel scan and FFT algorithms are limited to arrays of size $2^n$, while the more general (not just binary) Cooley-Tukey FFT algorithms require composite size, i.e., $m \cdot n$ for integers $m, n \ge 2$.
In array-based algorithms, these restrictions can be realized in one of two ways:
\begin{itemize}
\item
  Check array sizes dynamically, incurring a performance penalty; or
\item
  Document the restriction, assume the best, and blame the library user when the assumption is violated.
\end{itemize}
A third---much less commonly used---option is to statically verify the size restriction at the call site, perhaps by using a dependently typed language and providing proofs as part of the call.

A lightweight compromise is to simulate some of the power of dependent types via type-level encodings of sizes, as with our use of |Nat| for indexing the |Vec| type above.
There are many possible definitions for |Nat|.
For this paper, assume that |Nat| is a kind-promoted version of the following data type of Peano numbers (constructed via zero and successor):
\begin{code}
data Nat = Z | S Nat
\end{code}
Thanks to promotion, |Nat| is not only a new data type with value-level constructors |Z| and |S|, but also a new \emph{kind} with \emph{type-level} constructors |Z| and |S| \cite{yorgey2012giving}.

\subsubsection{GADT formulation}

Now we can define the length-indexed |Vec| type mentioned above, which is the canonical example of dependent types in either full dependently typed languages or as simulated with generalized algebraic data types (GADTs).
As with lists, there are right- and left-growing versions: The former (borrowing constructor names from right- and left-lists):

Recall that the generic representations of |RList| and |LList| were built out of sum, unit, identity, and product.
With static shaping, the sum disappears from the representation, moving from dynamic to static choice:
\begin{code}
instance Generic1 (RVec Z) where
  type Rep1 (RVec Z) = U1
  from RNil = U1
  to U1 = RNil
instance Generic1 (RVec n) => Generic1 (RVec (S n)) where
  type Rep1 (RVec (S n)) = Par1 :*: RVec n
  from (a :< as) = Par1 a :*: as
  to (Par1 a :*: as) = a :< as

instance Generic1 (LVec Z) where
  type Rep1 (LVec Z) = U1
  from RNil = U1
  to U1 = RNil
instance Generic1 (LVec n) => Generic1 (LVec (S n)) where
  type Rep1 (LVec (S n)) = LVec n :*: Par1
  from (a :< as) = Par1 a :*: as
  to (Par1 a :*: as) = a :< as
\end{code}

For leaf trees, we have a choice between imperfect and perfect trees.
A ``perfect'' leaf tree is one in which all leaves are at the same depth.
Both imperfect and perfect can be ``statically shaped'', but we'll use just perfect trees in this paper, for which we need only a single type-level number signifying the depth of all leaves.
For succinctness, rename |Leaf| and |Branch| to ``|L|'' and ``|B|''.
For reasons soon to be explained, also rename the types |TTree| and |BTree| to ``|RPow|'' and ``|LPow|'':
\begin{code}
data RPow :: (* -> *) -> Nat -> * -> * SPC where
  L :: a -> RPow f Z a
  B :: f (RPow f n a) -> RPow f (S n) a

data LPow :: (* -> *) -> Nat -> * -> * SPC where
  L :: a -> LPow f Z a
  B :: LPow f n (f a) -> LPow f (S n) a
\end{code}

As with vectors, statically shaped |f|-ary trees are generically represented like their dynamically shaped counterparts but with dynamic choice (sum) replaced by static choice:
\begin{code}
instance Generic1 (RPow f Z) where
  type Rep1 (RPow f Z) = Par1
  from1 (L a) = Par1 a
  to1 (Par1 a) = L a

instance  Generic1 (RPow f n) => Generic1 (RPow f (S n)) where
  type Rep1 (RPow f (S n)) = f :.: RPow f n
  from1 (B ts) = Comp1 ts
  to1 (Comp1 ts)  = B ts

instance Generic1 (LPow f Z) where
  type Rep1 (LPow f Z) = Par1
  from1 (L a) = Par1 a
  to1 (Par1 a) = L a

instance  Generic1 (LPow f n) => Generic1 (LPow f (S n)) where
  type Rep1 (LPow f (S n)) = LPow f n :.: f
  from1 (B ts) = Comp1 ts
  to1 (Comp1 ts) = B ts
\end{code}

We can then give these statically shaped data types |Functor|, |Foldable|, and |Traversable| instances exactly matching the dynamically shaped versions given above.
In addition, they have |Applicative| and |Monad| instances, left as an exercise for the reader.
Since all of these types are memo tries \cite{Hinze00memofunctions}, their class instances instance follow homomorphically from the corresponding instances for functions \cite{long-type-class-morphisms}.

\subsubsection{Type family formulation}

Note that |RVec n| and |LVec n| are essentially $n$-ary functor \emph{products} of |Par1|.
Similarly, |RPow f n| and |LPow f n| are $n$-ary functor \emph{compositions} of |f|.
Functor product and functor composition are both associative but only up to isomorphism.
This limit to associativity is exactly why both exist and are useful.
While |RVec| and |RPow| are right associations, |LVec| and |LPow| are left associations.

Instead of the GADT-based definitions given above for |RVec|, |LVec|, |RPow|, and |LPow|, we can make the repeated product and repeated composition more apparent by using closed type families \cite{ClosedTypeFamilies:2014}, with instances defined inductively over type-level natural numbers.
Vectors:
\begin{code}
type family RVec n where
  RVec Z      = U1
  RVec (S n)  = Par1 :*: RVec n

type family LVec n where
  LVec Z      = U1
  LVec (S n)  = LVec n :*: Par1
\end{code}
Trees:
\begin{code}
type family RPow h n where
  RPow h Z      = Par1
  RPow h (S n)  = h :.: RPow h n

type family LPow h n where
  LPow h Z      = Par1
  LPow h (S n)  = LPow h n :.: h
\end{code}

Note the similarity between the |RVec| and |RPow| type family instances and the following definitions of multiplication and exponentiation on Peano numbers (with RHS parentheses for emphasis):
\begin{code}
0      * a = 0
(1+n)  * a = a + (n * a)

h ^ 0      = 1
h ^ (1+n)  = h * (h ^ n)
\end{code}
Likewise, the type family instances for |LVec| and |LPow| are analogous to the following equivalent definitions of Peano multiplication and exponentiation:
\begin{code}
0      * a = 0
(n+1)  * a = (n * a) + n

h ^ 0      = 1
h ^ (n+1)  = (h ^ n) * h
\end{code}

Because these type-family-based definitions are expressed in terms of existing generic building blocks, we directly inherit many existing class instances rather than having to define them.
A downside is that we \emph{cannot} provide them, which will pose a challenge (though easily surmounted) with FFT on vectors, as well as custom instances for displaying structures.

Although |RPow| and |LPow| work with any functor argument, we will use uniform pairs in the examples below.
The uniform |Pair| functor can be defined in a variety of ways, including |Par1 :*: Par1|, |RVec N2|, |LVec N2|, or its own algebraic data type:
\begin{code}
data Pair a = a :# a deriving (Functor,Foldable,Traversable)
\end{code}
For convenience, define top-down and bottom-up \emph{binary} trees:

> type RBin = RPow Pair
> type LBin = LPow Pair

\subsection{Bushes}\seclabel{bushes}

In contrast to vectors, the tree types above are perfectly balanced, as is helpful in obtaining naturally parallel algorithms.
From another perspective, however, they are quite imbalanced.
The functor composition operator is used fully left-associated for |LPow| and fully right-associated for |RPow| (hence the names).
It's easy to define a composition-balanced type as well:
\begin{code}
type family Bush n where
  Bush Z      = Pair
  Bush (S n)  = Bush n :.: Bush n
\end{code}
While each |RBin n| and |LBin n| holds $2^n$ elements, each statically shaped |Bush n| holds $2^{2^n}$ elements.
Moreover, there's nothing special about |Pair| or \emph{binary} composition here.
Either could be replaced or generalized.

Our ``bush'' type is adapted from an example of nested data types that has a less regular shape \cite{Bird1998}:
\begin{code}
data Bush a = NilB | ConsB a (Bush (Bush a))
\end{code}

Bushes are to trees as trees are to vectors, in the following sense.
Functor product is associative up to isomorphism.
Where |RVec| and |LVec| choose fully right- or left-associated products, |RPow f| and |LPow f| form perfectly and recursively balanced products.
Likewise, functor composition is associative up to isomorphism.
Where |RPow f| and |LPow f| are fully right- and left-associated compositions, |Bush f| forms balanced compositions.
Many other variations are possible, but the |Bush| definition above will suffice for this paper.

\section{Parallel scan}

Given a sequence $a_0,\ldots,a_{n-1}$, the ``prefix sum'' is a sequence $b_0,\ldots,b_n$ such that $b_k = \sum_{0 \le i < k}{a_i}$.
More generally, for any associative operation $\oplus$, the ``prefix scan'' is defined by $b_k = \bigoplus_{0 \le i < k}{a_i}$, with $b_0$ being the identity for $\oplus$.
(One can define a similar operation if we assume semigroup---lacking identity element---rather than monoid, but the development is more straightforward with identity.)

Parallel scan has broad applications, including the following, taken from a longer list in \cite{BlellochTR90}:
\begin{itemize}
\item Adding multi-precision numbers
\item Polynomial evaluation
\item Solving recurrences
\item Sorting
\item Solving tridiagonal linear systems
\item Lexical analysis
\item Regular expression search
\item Labeling components in two dimensional images
\end{itemize}
Scans may be ``prefix'' (from the left, as above) or or ``suffix'' (from the right).
We will just develop prefix scan, but generic suffix scan works out in the same way.

Note that $a_k$ does \emph{not} influence $b_k$.
Often scans are classified as ``exclusive'', as above, or ``inclusive'', where $a_k$ does contribute to $b_k$.
Note also that there is one more output element than input, which is atypical in the literature on parallel prefix algorithms, perhaps because scans are often performed in place.
As we will see below, the unconventional choice of exclusive+total above makes for an elegant generic decomposition.

The standard list prefix scans in Haskell, |scanl| and |scanr|, also yield one more output element than input, which is possible for lists.
For other data types, such as trees and especially perfect ones, there may not be a place to stash the extra value.
For a generic scan applying to many different data types, we can simply form a product, so that scanning maps |f a| to |f a :* a|.
The extra summary value is the fold over the whole input structure.
Thus, we have the following class for left-scannable functors:
\begin{code}
class Functor f => LScan f where lscan :: Monoid a => f a -> f a :* a
\end{code}
The |Functor| superclass is just for convenience and can be dropped in favor of more verbose signatures elsewhere.

When |f| is in |Traversable|, there is simple and general specification using operations from the standard Haskell libraries:
\begin{code}
lscan == swap . mapAccumL (\ acc a -> (acc <> a,acc)) mempty
\end{code}
where |(<>)| and |mempty| are the combining operation and its identity from |Monoid|, and
\begin{code}
mapAccumL :: Traversable t => (b -> a -> b :* c) -> b -> t a -> b :* t c
\end{code}
Although all of the example types in this paper are indeed in |Traversable|, using this |lscan| specification as an implementation would result in an entirely sequential implementation, since data dependencies are \emph{linearly} threaded through the whole computation.

Rather than defining |LScan| instances for all of our data types, the idea of generic programming is to define instances only for the small set of fundamental functor combinators and then automatically compose instances for other types via the generic encodings (derived automatically when possible).
To do so, provide a default signature and definition for functors with such encodings:
\begin{code}
class Functor f => LScan f where
  lscan :: Monoid a => f a -> f a :* a
  default lscan :: (Generic1 f, LScan (Rep1 f), Monoid a) => f a -> f a :* a
  lscan = first to1 . lscan . from1
\end{code}

Once we define |LScan| instances for our six fundamental combinators, one can simply write ``|instance LScan F|'' for any functor |F| having a |Generic1| instance (derived automatically or defined manually).
For our statically shaped vector, tree, and bush functors, we have a choice: use the GADT definitions with their manually defined |Generic1| instances (exploiting the |lscan| default), or use the type family versions without the need for the encoding (|from1|) and decoding (|to1|) steps.

\subsection{Easy instances}

Four of the six needed generic |LScan| instances are easily handled:
\begin{code}
instance LScan V1    where lscan = \ SPC case

instance LScan U1    where lscan U1 = (U1, mempty)

instance LScan Par1  where lscan (Par1 a) = (Par1 mempty, a)

instance (LScan f, LScan g) => LScan (f :+: g) where
  lscan (L1  fa  ) = first L1  (lscan fa  )
  lscan (R1  ga  ) = first R1  (lscan ga  )
\end{code}
Comments:
\begin{itemize}
\item Since there are no values of type |V1 a|, a complete case expression needs no clauses.
(The definition relies on the |LambdaCase| and |EmptyCase| language extensions.)
\item An empty structure can only generate another empty structure with a summary value of |mempty|.
\item For a singleton value |Par1 a|, the combination of values before the first and only one is |mempty|, and the summary is the value |a|.
\item For a sum, scan whichever structure is present, and re-tag.
(The higher-order function |first| applies a function to the first element of a pair, carrying the second element along unchanged.)
\end{itemize}

Just as the six functor combinators guide the composition of parallel algorithms, they also determine the performance characteristics of those parallel algorithms in a compositional manner.
Following \cite{Blelloch96programmingparallel}, consider two aspect of performance:
\begin{itemize}
\item \emph{work}, the total number of primitive operations performed, and
\item \emph{depth}, the longest dependency chain, and hence a measure of ideal parallel. computation time 
\end{itemize}
For parallel scan, work and depth of |U1|, |V1|, and |Par1| are all zero.
For sums,
\begin{code}
W  (f :+: g)  = W  f `max` W  g
D  (f :+: g)  = D  f `max` D  g
\end{code}

\noindent
With the four easy instances out of the way, we have only two left to define: product and composition.

\subsection{Product}

Suppose we have linear scans, as in \figref{two-scans}.
We will see later how these individual scans arise from particular functors |f| and |g| (of sizes five and eleven respectively), but for now take them as given.
To understand |lscan| on functor products, consider how to combine the scans of |f| and |g| into scan for |f :*: g|.

Because we are left-scanning, every prefix of |f| is also a prefix of |f :*: g|, so the |lscan| results for |f| are also correct results for |f :*: g|.
The prefixes of |g| are not prefixes of |f :*: g|, however, since each |g|-prefix misses all of |f|.
The prefix \emph{sums}, therefore, are lacking the summary (fold) of all of |f|, which corresponds to the last output of the |lscan| result for |f|.
All we need to do, therefore, is adjust each |g| result by the final |f| result, as shown in \figref{lsums-lv5xlv11-highlight}.
\figpair{two-scans}{|lscan @(RVec N5)| and |lscan @(RVec N11)|}{
\vspace{1.8ex}
\incpicW{0.6}{lsums-lv5}
\vspace{2.4ex}
\incpic{lsums-lv11}
\vspace{1ex}
}{lsums-lv5xlv11-highlight}{|lscan @(RVec N5 :*: RVec N11)| \stats{26}{11}}{\incpic{lsums-lv5xlv11-highlight}}
The general product instance:
\begin{code}
instance (LScan f, LScan g) => LScan (f :*: g) where
  lscan (fa :*: ga) = (fa' :*: fmap (fx NOP <> ) ga', fx <> gx)
   where
     (fa'  , fx)  = lscan fa
     (ga'  , gx)  = lscan ga
\end{code}
The work for |f :*: g| is the combined work for each, plus the cost of adjusting the result for |g|.
The depth is the maximum depth for |f| and |g|, plus one more step to adjust the final |g| result.
\begin{code}
W  (f :*: g) = W f + W g + ssize g + 1
D  (f :*: g) = (D f `max` D g) + 1
\end{code}

We now have enough functionality for scanning vectors using either the GADT or type family definitions from \secref{statically-shaped-types}.
\figref{lsums-rv8-no-hash-no-opt} shows |lscan| for |RVec N8| (\emph{right} vector of length 8).
The zero-additions can be easily optimized away, resulting in \figref{lsums-rv8}.
In this picture (and many more like it below), the data types are shown in flattened form in the input and output (labeled |In| and |Out|), and the work and depth are shown in the caption (as \emph{W} and \emph{D}).
As promised, there is always one more output than input, and the last output is the fold that summarizes the entire structure being scanned.

\figp{
\circdef{lsums-rv8-no-hash-no-opt}{|lscan @(RVec N8)|, unoptimized}{36}{8}}{
\circdef{lsums-rv8}{|lscan @(RVec N8)|, optimized}{28}{7}
}

The combination of left scan and right vector is particularly unfortunate, as it involves quadratic work and linear depth.
The source of quadratic work is the product instance's \emph{right} adjustment combined with the right-associated shape of |RVec|.
Each single element (left) is used to adjust the entire suffix (right), requiring linear work at each step, summing to quadratic.
We can verify the complexity by using the definition of |RVec| and the complexities for the generic building blocks involved.
\begin{code}
W (RVec 0) = W U1 = 0
W (RVec (S n)) = W (Par1 :*: RVec n) = W Par1 + W (RVec n) + ssize (RVec n) + 1 = W (RVec n) + O (n)

D (RVec 0) = D U1 = 0
D (RVec (S n)) = D (Par1 :*: RVec n) = (D Par1 `max` D (RVec n)) + 1 = D (RVec n) + O(1)
\end{code}
Thus
\begin{code}
W  (RVec n)  = O (pow n 2)
D  (RVec n)  = O (n)
\end{code}
In contrast, with left-associated vectors, each prefix summary (left) is used to update a single element (right), leading to linear work, as shown in \figref{lsums-lv8-no-hash-no-opt} and (optimized) \figref{lsums-lv8}.
\figp{
\circdef{lsums-lv8-no-hash-no-opt}{|lscan @(LVec N8)|, unoptimized}{16}{8}}{
\circdef{lsums-lv8}{|lscan @(LVec N8)|, optimized}{7}{7}
}
\begin{code}
W (LVec 0) = W U1 = 0
W (LVec (S n)) = W (LVec n :*: Par1) = W (LVec n) + W Par1 + ssize Par1 + 1 = W (LVec n) + 2

D  (RVec 0) = W U1 = 0
D  (RVec (S n)) = D (Par1 :*: RVec n) = (D Par1 `max` D (RVec n)) + 1 = D (RVec n) + O(1)
\end{code}
Thus
\begin{code}
W  (LVec n) = O (n)
D  (LVec n) = O (n)
\end{code}
Performing a suffix/right scan on a left vector also leads to quadratic work, reduced to linear by switching to right vectors.

Although work is greatly reduced (from quadratic to linear), depth remains at linear, because unbalanced data types lead to unbalanced parallelism.
Both |RVec| and |LVec| are ``parallel'' in a sense, but we only get to perform small computations in parallel with large one (especially apparent in the unoptimized \figreftwo{lsums-rv8-no-hash-no-opt}{lsums-lv8-no-hash-no-opt}), so that the result is essentially sequential.

To get a more parallelism, we could replace a type like |LVec N16| with a isomorphic product such as |LVec N5 :*: LVec N11|, resulting in \figref{lsums-lv5xlv11-highlight}, reducing depth from 15 to 11.
More generally, scan on |LVec m :*: LVec n| has depth |((m-1) `max` (n-1)) + 1 = max m n|.
For an ideal partition adding up to |p|, we'll want |m = n = p/2|.
For instance, replace |LVec N16| with the isomorphic product |LVec N8 :*: LVec N8|, resulting in \figref{lsums-lv8xlv8} with depth eight.
Can we decrease the depth any further?
Not as a single product, but we can as more than one product, as shown in \figref{lsums-lv5-5-6-l} with depth six.
Again, the more balance, the better.
\figp{
\circdef{lsums-lv8xlv8}{|lscan @(LVec N8 :*: LVec N8)|}{22}{8}}{
\circdef{lsums-lv5-5-6-l}{|lscan @((LVec N5 :*: LVec N5) :*: LVec N6)|}{24}{6}}

\subsection{Composition}

We now come to the last of our six functor combinators, namely composition, i.e., structures of structures.
Suppose we have a triple of quadruples: |LVec N3 :.: LVec N4|.
We know how to scan each quadruple, as in \figref{triple-scan}.
How can we combine the results of each scan into a scan for |LVec N3 :.: LVec N4|?
We already know the answer, since this composite type is essentially |(LVec N4 :*: LVec N4) :*: LVec N4|, the scan for which is determined by the |Par1| and product instances and is shown in \figref{lsums-lv3olv4-highlight}.

\figpairW{0.34}{0.58}{triple-scan}{triple |lscan @(LVec N4)|}{
\incpic{lsums-lv4}

\incpic{lsums-lv4}

\incpic{lsums-lv4}
}{lsums-lv3olv4-highlight}{|lscan @(LVec N3 :.: LVec N4)| \stats{18}{5}}{\incpic{lsums-lv3olv4-highlight}}

Let's reflect on this example as we did with binary products above.
The prefixes of the first quadruple are all prefixes of the composite structure, so their prefix sums are prefix sums of the composite and so are used as they are.
For every following quadruple, the prefix sums are lacking the sum of all elements from the earlier quadruples and so must be adjusted accordingly, as emphasized in \figref{lsums-lv3olv4-highlight}.

Now we get to the surprising heart of generic parallel scan!
Observe that the sums of elements from all earlier quadruples are computed entirely from the final summary results from each quadruple.
We end up needing the sum of every \emph{prefix} of the triple of summaries, and so we are computing not just three prefix scans over |LVec N4| but also \emph{one additional scan} over |LVec N3|.
Moreover, the apparent inconsistency of adjusting all quadruples \emph{except} for the first one is an illusion brought on by premature optimization.
We can instead adjust \emph{every} quadruple by the corresponding result of this final scan of summaries, the first summary being zero.
These zero-additions can then be optimized away later.
\out{See \circuitrefdef{lsums-lv5olv7-highlight}{Scan for |LVec N5 :.: LVec N7|}{59}{10} for a larger example showing this same pattern.}

The general case is captured in an |LScan| instance for functor composition:
\begin{code}
instance (LScan g, LScan f, Zip g) =>  LScan (g :.: f) where
  lscan (Comp1 gfa) = (Comp1 (zipWith adjustl tots' gfa'), tot)
   where
     (gfa', tots)  = unzip (fmap lscan gfa)
     (tots',tot)   = lscan tots
     adjustl t     = fmap (t NOP <>)
\end{code}
The work for scanning |g :.: f| includes work for each |f|, work for the |g| of summaries, and updates to all results (before optimizing away the zero adjust, which doesn't change order).
The depth is the depth of |f| (since each |f| is handled in parallel with the others), followed by the depth of a single |g| scan.\footnote{This simple depth analysis is pessimistic in that it does not account for the fact that some |g| work can begin before all |f| work is complete.}
\begin{code}
W  (g :.: f) = ssize g *. W f + W g + ssize g *. ssize f
D  (g :.: f) = D f + D g
\end{code}

\subsection{More examples}

We now know how to scan the full vocabulary of generic functor combinators, and we've seen the consequences for several data types.
Let's now examine how well generic scan works for some other example structures.
We have already seen |Pair :.: LVec N8| as |LVec N8 :*: LVec N8| in \figref{lsums-lv8xlv8}.
The reverse composition leads to quite a different computation shape, as \figref{lsums-lv8-p} shows.
Yet another factoring appears in \figref{lsums-lv4olv4}.
\figp{
\circdef{lsums-lv8-p}{|lscan @(LVec N8 :.: Pair)|}{22}{8}}{
\circdef{lsums-lv4olv4}{|lscan @(LVec N4 :.: LVec N4)|}{24}{6}}

Next let's try functor exponentiation in its left- and right-associated forms.
We just saw the equivalent of |RPow (LVec N4) N2| (and |LPow (LVec N4) N2|) as \figref{lsums-lv4olv4}.
\figreftwo{lsums-rb4}{lsums-lb4} show |RBin N4| and |LBin N4| (top-down and bottom-up perfect binary leaf trees of depth four).\notefoot{Maybe drop the ``|RBin|'' and ``|LBin|'' shorthands.}
\figp{
\circdef{lsums-rb4}{|lscan @(RBin N4)|}{32}{4}}{
\circdef{lsums-lb4}{|lscan @(LBin N4)|}{26}{6}}
Complexities for |RPow h|:
\begin{code}
W (RPow h 0) = W Par1 = 0
W (RPow h (S n)) = W (h :.: RPow h n) = ssize h *. W (RPow h n) + W h + pow (ssize h) (S n)

D (RPow h 0) = D Par1 = 0
D (RPow h (S n)) = D (h :.: RPow h n) = D h + D (RPow h n)
\end{code}
For any fixed |h|, |W h + pow (ssize h) (S n) = O (n)|, so the Master Theorem gives a solution \cite[Chapter 4]{Cormen:2009} for |W|.
Since |D h = O (1)| (again, for fixed |h|), |D| has a simple solution.
\begin{code}
W  (RPow h n) = O (ssize (RPow h n) *. log (ssize (RPow h n)))
D  (RPow h n) = O (n) = O (log (ssize (RPow h n)))
\end{code}
Complexity for |LPow h| work out somewhat differently:
\begin{code}
W (LPow h 0) = W Par1 = 0
W (LPow h (S n)) = W (LPow h n :.: h) = ssize (LPow h n) *. W h + W (LPow h n) + pow (ssize h) (S n)

D (LPow h 0) = D Par1 = 0
D (LPow h (S n)) = D (LPow h n :.: h) = D (LPow h n) + D h
\end{code}
With a fixed |h|, |ssize (LPow h n) *. W h + pow (ssize h) (S n) = O (ssize (LPow h n))|, so the Master Theorem gives a solution \emph{linear} in |ssize (LPow h n)|, while the depth is again logarithmic:
\begin{code}
W  (LPow h n) = O (ssize (LPow h n))
D  (LPow h n) = O (n) = O (log (ssize (LPow h n)))
\end{code}
For this reason, parallel scan on bottom-up trees can do much less work than on top-down trees.
They also have fan-out bounded by |ssize h|, as contrasted with the linear fan-out for top-down trees---an important consideration for hardware implementations.
On the other hand, the depth for bottom-up trees is about twice the depth for top-down trees.

Specializing these |RPow h| and |RPow h| scan algorithms to |h = Pair| yields two well-known algorithms: |lscan| on |RBin n| is from \citep{Sklansky1960}, while |lscan| on |LBin n| is from \citep{LadnerFischer1980}.

Finally, consider the |Bush| type from \secref{bushes}.
Figures~\ref{fig:lsums-bush0} through~\ref{fig:lsums-bush3} show |lscan| for bushes of depth zero through three.
Depth complexity:\out{\notefoot{I'm giving an exact analysis, not an asymptotic one. My conclusion doesn't match measurements for $n=2,3$. Find and fix my mistake.}}
\begin{code}
D (Bush 0) = D Pair = 1
D (Bush (S n)) = D (Bush n :.: Bush n) = D (Bush n) + D (Bush n) = 2 *. D (Bush n)
\end{code}
Hence
\begin{code}
D (Bush n) = pow 2 n = pow 2 (log2 (log2 (ssize (Bush n)))) = log2 (ssize (Bush n))
\end{code}
Work complexity is trickier:
\begin{code}
W (Bush 0) = W Pair = 1
W (Bush (S n))  = W (Bush n :.: Bush n) = ssize (Bush n) *. W (Bush n) + W (Bush n) + ssize (Bush (S n))
                = pow 2 (pow 2 n) *. W (Bush n) + W (Bush n) + pow 2 (pow 2 (n+1))
                = (pow 2 (pow 2 n) + 1) *. W (Bush n) + pow 2 (pow 2 (n+1))
\end{code}
A closed form solution is left for later work.
\figp{
\circdef{lsums-bush0}{|lscan @(Bush N0)|}{1}{1}}{
\circdef{lsums-bush1}{|lscan @(Bush N1)|}{4}{2}}%
\figp{
\circdef{lsums-bush2}{|lscan @(Bush N2)|}{29}{5}}{
\circdef{lsums-bush3}{|lscan @(Bush N3)|}{718}{10}}%
\figreftwo{lscan-stats-16}{lscan-stats-256} offer an empirical comparison, including some optimizations not taken into account in the complexity analysis above.
Note that top-down trees have the least depth, bottom-up trees have the least work.
Bushes appear to provide a compromise, with less work than top-down trees and less depth than bottom-up trees.

\begin{figure}
\begin{minipage}{0.43\linewidth}
 \centering
  \lscanStats{
    \lscanStat{|RBin N4|}{32}{4}
    \lscanStat{|LBin N4|}{26}{6}
    \lscanStat{|Bush N2|}{29}{5}
  }
  \vspace*{-3ex}
  \captionof{figure}{|lscan| for 16 values}
  \label{fig:lscan-stats-16}
\end{minipage}
\begin{minipage}{0.49\linewidth}
 \centering
  \lscanStats{
    \lscanStat{|RBin N8|}{1024}{8}
    \lscanStat{|LBin N8|}{502}{14}
    \lscanStat{|Bush N3|}{718}{10}
  }
  \vspace*{-3ex}
  \captionof{figure}{|lscan| for 256 values}
  \label{fig:lscan-stats-256}
\end{minipage}
\end{figure}

\subsection{Some convenient packaging}

For generality, |lscan| works on arbitrary monoids.
For convenience, let's define some specializations.
One way to do so is to provide functions that map between non-monoids and monoids.
Start with a class similar to |Generic| for providing alternative representations:
\begin{code}
class Newtype n where
  type O n :: *
  pack    :: O n -> n
  unpack  :: n -> O n
\end{code}
This class is from \cite{newtype-generics}, which also defines many instances for commonly used types.
Given this vocabulary, we can scan structures over a non-monoid by packing values into a chosen monoid, scanning, and then unpacking:\footnote{The |(***)| operation applies two given functions to the respective components of a pair, and the ``|@@|'' notation is visible type application \cite{eisenberg2016visible}.}
\begin{code}
lscanNew  ::  forall n o f. (Newtype n, o ~ O n, LScan f, Monoid n) =>  f o -> f o :* o
lscanNew = (fmap unpack *** unpack) . lscan . fmap (pack @n)
\end{code}

\begin{code}
lsums, lproducts :: (LScan f, Num a) => f a -> f a :* a
lalls :: LScan f => f Bool -> f Bool :* Bool
lsums      = lscanNew @(Sum a)
lproducts  = lscanNew @(Product a)
lalls      = lscanNew @All
...
\end{code}

\subsectiondef{Applications}

As a first simple example application of parallel scan, let's construct powers of a given number to fill a structure |f|.
A simple implementation builds a structure with identical values using |pure| (from |Applicative|) and then calculates all prefix products:

> powers :: (LScan f, Applicative f, Num a) => a -> f a :* a
> powers = lproducts  . pure

\figref{powers-rb4-no-hash} shows one instance of |powers|.
A quick examination shows that there is a lot of redundant computation due to the special context of scanning over identical values.
For instance, for an input $x$, we compute $x^2$ eight times and $x^4$ four times.
Fortunately, automatic common subexpression elimination (CSE) removes these redundancies easily, resulting in \figref{powers-rb4}.
\figp{
\circdef{powers-rb4-no-hash}{|powers @(RBin N4)| --- without CSE}{32}{4}}{
\circdef{powers-rb4}{|powers @(RBin N4)| --- with CSE}{15}{4}}

Building on this example, let's define polynomial evaluation, mapping a structure of coefficients $a_0, \ldots, a_{n-1}$ and a parameter $x$ to $\sum_{0 \le i < n} a_i x^i$\out{$a_0 + a_1 x + \cdots + a_{n-1} x^{n-1}$}.
A very simple formulation is to construct all of the powers of $x$ and then form a dot product with the coefficients:

\begin{code}
evalPoly  ::  (LScan f, Foldable f, Applicative f, Num a) =>  f a -> a -> a
evalPoly coeffs x = coeffs <.> fst (powers x)

(<.>) :: (Foldable f, Applicative f, Num a) => f a -> f a -> a
u <.> v = sum (liftA2 (*) u v)
\end{code}
See \figreftwo{evalPoly-rb4}{evalPoly-lb4}.
\figp{
\circdef{evalPoly-rb4}{|evalPoly @(RBin N4)|}{29+15}{9}}{
\circdef{evalPoly-lb4}{|evalPoly @(LBin N4)|}{29+15}{11}}

\note{If I have room, do scan-based addition.}

\subsection{Relation to known parallel scan algorithms}

\section{FFT}

%format C = "\mathbb{C}"

\subsection{Background}

The Fast Fourier Transform (FFT) algorithm computes the Discrete Fourier Transform (DFT), reducing work from $O (n^2)$ to $O (n \log n)$.
First discovered by Gauss \cite{GaussFFTHistory}, the algorithm was rediscovered by \citet{danielson1942some}, and later by \citet{CooleyTukey}, whose work popularized the algorithm.

Given a sequence of complex numbers, $x_0, \ldots, x_{N-1}$, the DFT is defined as
$$ X_k =  \sum\limits_{n=0}^{N-1} x_n e^{\frac{-i2\pi kn}{N}} \text{, \quad for\ } 0 \le k < N $$
Naively implemented, this DFT definition leads to quadratic work.
The main trick to FFT is to factor $N$ and then optimize the DFT definition, removing some exponentials that turn out to be equal to one.
For $N = N_1 N_2$,
$$ X_k =
      \sum_{n_1=0}^{N_1-1} 
        \left[ e^{-\frac{2\pi i}{N} n_1 k_2} \right]
          \left( \sum_{n_2=0}^{N_2-1} x_{N_1 n_2 + n_1}  
                  e^{-\frac{2\pi i}{N_2} n_2 k_2 } \right)
        e^{-\frac{2\pi i}{N_1} n_1 k_1}
$$
In this form, we can see two smaller sets of DFTs: $N_1$ of size $N_2$ each, and $N_2$ of size $N_1$ each.
If we use the same method for solving these $N_1 + N_2$ smaller DFTs, we get a recursive FFT algorithm.
See \figrefdef{factored-dft}{Factored DFT \cite{JohnsonCooleyTukeyPic}}{\pic{cooley-tukey-general}}.

Rather than implementing FFT via sequences/arrays as usual, let's take a step back and consider a more structured approach.

\subsection{Factor types, not numbers!}

The summation formula above exhibits a trait typical of array-based algorithms, namely index arithmetic, which is tedious to write and to read.
This arithmetic has a purpose, however, which is interpret an array as an array of arrays.
In a higher-level formulation, we might replace arrays and index arithmetic by an \emph{explicit} nesting of structures.
We have already seen the fundamental building block of structure nesting, namely functor composition.
Instead of factoring numbers that represent type sizes, factor the types themselves.

As with scan, define a class of FFT-able structures and a generic default.
One new wrinkle is that the result shape differs from the original shape, so we'll use an associated functor ``|FFO|'':

\begin{code}
class FFT f where
  type FFO f :: * -> *
  fft :: f C -> FFO f C
  default fft  ::  ( Generic1 f, Generic1 (FFO f), FFT (Rep1 f) , FFO (Rep1 f) ~ Rep1 (FFO f) ) =>  f C -> FFO f C
  fft xs = to1 . fft xs . from1
\end{code}

Again, instances for |U1| and |Par1| are easy to define (exercise).
We will \emph{not} be able to define an instance for |f :*: g|.
Instead, for small functors, such as short vectors, we can simply use the DFT definition.
The uniform pair case simplifies particularly nicely:

> instance FFT Pair where
>   type FFO Pair = Pair
>   fft (a :# b) = (a + b) :# (a - b)

The final case is |g :.: f|, which is the heart of FFT.
\figref{factored-dft} tells us almost all we need to know.

> instance NOP ... => FFT (g :.: f) where
>   type FFO (g :.: f) = FFO f :.: FFO g
>   fft = Comp1 . ffts' . transpose . twiddle . ffts' . unComp1

where |ffts'| performs several non-contiguous FFTs in parallel:

> ffts' :: ... => g (f C) -> FFO g (f C)
> ffts' = transpose . fmap fft . transpose

Finally, the ``twiddle factors'' are all powers of a primitive $N^{\text{th}}$ root of unity:

> twiddle :: ... => g (f C) -> g (f C)
> twiddle = (liftA2.liftA2) (*) omegas
>
> omegas :: ... => g (f (Complex a))
> omegas = fmap powers (powers (exp (- i * 2 * pi / fromIntegral (size @(g :.: f)))))

The |size| method calculates the size of a structure.
Unsurprisingly, the size of a composition is the product of the sizes.\notefoot{Define much earlier for use in complexity analyses, and drop this paragraph.}

%format WO = W"_"omegas
%format DO = D"_"omegas
%format WT = W"_"twiddle
%format DT = D"_"twiddle
%format WFs = W"_"ffts'
%format DFs = D"_"ffts'

Complexity of |fft| depends on complexity of |twiddle| and |omegas|.
Since |powers| (defined in \secref{Applications}) is a prefix scan, we can compute |omegas| efficiently in parallel, with one |powers| for |g| and then one more for each element of the resulting |g C|, the latter collection being constructed in parallel.
Thanks to scanning on constant structures, |powers| requires only linear work even on top-down trees.
Depth of |powers| is logarithmic.
\begin{code}
WO  (g (f C))  = O (ssize g + ssize g *. ssize f) = O (ssize (g :.: f))
DO  (g (f C))  = log2 (ssize g) + log2 (ssize f)
               = log2 (ssize g *. ssize f)
               = log2 (ssize (g :.: f)
\end{code}
After constructing |omegas|, |twiddle| multiplies two |g :.: f| structures element-wise, with linear work and constant depth.\notefoot{Probably replace |WT (g (f C))| with |WT f g| and likewise for |DT|.}
\begin{code}
WT  (g (f C))  = WO (g (f C)) + O (ssize (g :.: f))
               = O (ssize (g :.: f)) + O (ssize (g :.: f))
               = O (ssize (g :.: f))
DT  (g (f C))  = DO (g (f C)) + O (1)
               = log2 (ssize (g :.: f)) + O (1)
\end{code}
Returning to |fft @(g :.: f)|, the first |ffts'|, on |g :.: f|, does |ssize f| many |fft| on |g| (thanks to |transpose|), in parallel (via |fmap|).
The second |ffts'|, on |f :.: g|, does |ssize g| many |fft| on |f|, also in parallel.
Altogether,
\begin{code}
W (g :.: f)  = ssize g *. W f + WT (g (f C)) + ssize f *. W g
             = ssize g *. W f + O (ssize (g :.: f)) + ssize f *. W g

D (g :.: f)  = DFs (g (f C)) + DT (g (f C)) + DFs (f (g C))
             = D g + log2 (ssize (g :.: f)) + O (1) + D f
\end{code}
Note the symmetry of these results, so that |W (g :.: f) = W (f :.: g)| and |D (g :.: f) = D (f :.: g)|.
For this reason, top-down and bottom-up trees will have the same work and depth complexities.

The definition of |fft| for |g :.: f| can be simplified (without changing complexity):
\begin{code}
    Comp1 . ffts' . transpose . twiddle . ffts' . unComp1
==  {- definition of |ffts'| (and associativity of |(.)|) -}
    Comp1 . transpose . fmap fft . transpose .  transpose .  twiddle .  transpose . fmap fft . transpose . unComp1
==  {- |transpose . transpose == id| -}
    Comp1 . transpose . fmap fft . twiddle . transpose . fmap fft . transpose . unComp1
==  {- |transpose . fmap h == traverse h| -}
    Comp1 . traverse fft . twiddle . traverse fft . transpose . unComp1
\end{code}

\subsection{Comparing data types}

The top-down and bottom-up tree algorithms correspond to two popular binary FFT variations known as ``decimation in time'' and ``decimation in frequency'' (``DIT'' and ``DIF''), respectively.
In the array formulation, these variations arise from choosing $N_1$ small or $N_2$ small, respectively (most commonly 2 or 4).
Consider top-down trees first, starting with work:\notefoot{To do: Instance and complexity for |Par1| earlier.}
\begin{code}
W (RPow h 0) = W Par1 = 0
W (RPow h (S n))  = W (h :.: RPow h n)
                  = ssize h *. W (RPow h n) + O (ssize (h :.: RPow h n)) + ssize (RPow h n) *. W h
                  = ssize h *. W (RPow h n) + O (pow (ssize h) (S n)) + ssize (RPow h n) *. W h
                  = ssize h *. W (RPow h n) + O (ssize (RPow h n))
\end{code}
By the Master Theorem,
\begin{code}
W (RPow h n) = O (ssize (RPow h n) *. log (ssize (RPow h n)))
\end{code}
Next, depth:
\begin{code}
D (RPow h 0) = D Par1 = 0
D (RPow h (S n))  = D (h :.: RPow h n)
                  = D h + D (RPow h n) + log2 (ssize (h :.: RPow h n)) + O (1)
                  = D h + D (RPow h n) + log2 (pow (ssize h) (S n)) + O (1)
                  = D (RPow h n) + O (n)
\end{code}
Thus,\notefoot{I think FFT can have logarithmic depth. Hm.}
\begin{code}
D (RPow h n) = O (pow n 2) = O (logSq (ssize (RPow h n)))
\end{code}
As mentioned above, |W (g :.: f) = W (f :.: g)| and |D (g :.: f) = D (f :.: g)|, so top-down and bottom-up trees have the same work and depth complexities.

Next, consider bushes.
Work:
\begin{code}
W (Bush 0) = W Pair = 2
W (Bush (S n))  = W (Bush n :.: Bush n)
                = ssize (Bush n) *. W (Bush n) + O (ssize (Bush n :.: Bush n)) + ssize (Bush n) *. W (Bush n)
                = 2 *. ssize (Bush n) *. W (Bush n) + O (ssize (Bush (S n)))
\end{code}
A closed form solution is left for later work.

\todo{Consistent structure for these proofs throughout the paper.}

\figreftwo{fft-rb4}{fft-lb4} show |fft| for top-down and bottom-up binary trees of depth four, and \figreftwo{fft-bush2}{fft-bush3} for bushes of depth two and three.
\notefoot{Probably drop |Bush N3|.}
Each complex number appears as its real and imaginary components.
\figp{
\circdef{fft-rb4}{|fft @(RBin N4)|}{197}{8}}{
\circdef{fft-lb4}{|fft @(LBin N4)|}{197}{8}}
% \figo{\circdef{fft-bush2}{|fft @(Bush N2)|}{186}{6}}
\figp{
\circdef{fft-bush2}{|fft @(Bush N2)|}{186}{6}}{
\circdef{fft-bush3}{|fft @(Bush N3)|}{7310}{14}}

\figreftwo{fft-stats-16}{fft-stats-256} gives an empirical comparison.
\begin{figure}
\begin{minipage}{0.43\linewidth}
 \centering
  \fftStats{
    \fftStat{|RBin N4|}{74}{40}{74}{197}{8}
    \fftStat{|LBin N4|}{74}{40}{74}{197}{8}
    \fftStat{|Bush N2|}{72}{32}{72}{186}{6}
  }
  \vspace*{-3ex}
  \captionof{figure}{FFT for 16 complex values}
  \label{fig:fft-stats-16}
\end{minipage}
\begin{minipage}{0.49\linewidth}
 \centering
  \fftStats{
    \fftStat{|RBin N8|}{2690}{2582}{2690}{8241}{20}
    \fftStat{|LBin N8|}{2690}{2582}{2690}{8241}{20}
    \fftStat{|Bush N3|}{2528}{1922}{2528}{7310}{14}
  }
  \vspace*{-3ex}
  \captionof{figure}{FFT for 256 complex values}
  \label{fig:fft-stats-256}
\end{minipage}
\end{figure}
The total counts include literals, many of which are non-zero only due to numerical inexactness.
Pleasantly, the |Bush| instance of generic FFT appears to improve over the classic DIT and DIF algorithms in both work and depth.

\section{Related work}

\section{Reflections/conclusions}

\begin{itemize}
\item
  How would this work look with full dependent types?
\end{itemize}

%% \begin{center}\rule{0.5\linewidth}{\linethickness}\end{center}

\bibliography{bib}

\end{document}

