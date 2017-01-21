% -*- latex -*-

\documentclass[preprint]{sigplanconf}

% I don't like links while drafting, since they result in a bookmarks pane
% in Acrobat Reader.  Turn off "draft" later.
\usepackage[colorlinks,urlcolor=black,citecolor=black,linkcolor=black]{hyperref} % ,draft=true

% \pagenumbering{arabic}

% author-date form
\usepackage[]{natbib}
\bibpunct();A{},
\let\cite=\citep

%include polycode.fmt
%include forall.fmt
%include greek.fmt
%include mine.fmt

\input{macros}

\setlength\mathindent{1ex}

\title{Two generic functional parallel algorithms}

\authorinfo{Conal Elliott}{Target}{conal@@conal.net}

\bibliographystyle{plainnat}

\begin{document}

\maketitle

\section{Abstract}

Parallel programming, whether imperative or functional, has long focused on arrays as the central data type.
Meanwhile, typed functional programming has explored a variety of data types, including lists and various forms of trees.
\emph{Generic} functional programming decomposes these data types into a small set of fundamental building blocks: sum, product, composition, and their associated identities.
Definitions over these few fundamental type constructions then automatically assemble into algorithms for an infinite set of data types---some familiar and some new.
This paper presents generic functional formulations for two important and well-known classes of parallel algorithms: parallel scan (generalized prefix sum) and Fast Fourier Transform (FFT).
Notably, arrays play no role in these formulations.
Consequent benefits include a simpler and more compositional style, much use of common algebraic patterns (such as |Functor|, |Applicative|, |Foldable|, and |Traversable|), and freedom from possibility of run-time indexing errors.
The functional generic style also clearly reveals deep commonality among what otherwise appears to be quite different algorithms.
Instantiating the generic formulations to ``top-down'' and ``bottom-up'' trees, two well-known algorithms for each of parallel scan and FFT naturally emerge, as well as two possibly new algorithms.

\section{Introduction}

\section{Datatype-generic programming in Haskell}

There is a long, rich history of datatype-generic programming in Haskell~\note{refs}.
The basic idea of most such designs is to relate a broad range of types to a small set of basic ones via isomorphism (or more accurately, embedding-projection pairs), particularly binary sums and products and their corresponding identities (``void'' and ``unit'').
These type primitives serve to connect algorithms with data types in the following sense:
\begin{itemize}
\item Each data type of interest is somehow encoded into and decoded from these type primitives, and each algorithm is defined on the primitives.
\item Each (generic) algorithm is defined over these same primitives.
\end{itemize}
In this way, algorithms and data types are defined independently and then automatically work together.

One version of this general scheme is found in |GHC.Generics|, in which the type primitives are really \emph{functor} building blocks.
\note{Also, cite the wiki page and the Haddock docs.}
For this paper, we'll use six: sum, product, composition, and their three corresponding identities, as in \figrefdef{ghc-generics}{Functor building blocks}{
\begin{code}
data     (f  :+:  g)  a = L1 (f a) | R1 (g a)  SPC  -- lifted |Either|
data     (f  :*:  g)  a = f a :*: g a               -- lifted |(,)|
newtype  (g  :.:  f)  a = Comp1 (g (f a))           -- composition

data     V1           a                             -- lifted |Void|
newtype  U1           a = U1                        -- lifted |()|
newtype  Par1         a = Par1 a                    -- singleton
\end{code}
}
There are additional definitions that capture recursion and meta-data such as field names and operator fixity, but the collection in \figref{ghc-generics} suffices for this paper.
To make the encoding of data types easy, |GHC.Generics| comes with a generic deriving mechanism (enabled by the |DeriveGeneric| flag), so that for regular (not generalized) algebraic data types, one can simply write ``|data ... deriving Generic|'' for types of kind |*| \cite{Magalhaes:2010}.
For type constructors of kind |* -> *|, as in this paper, one derives |Generic1| instead.
Instances for non-regular algebraic data types can be defined explicitly, which amounts to giving an encoding functor |Rep f| and encoding and decoding operations |to1| and |from1|, as in \figrefdef{Generic1}{Functor encoding and decoding}{
\begin{code}
-- Representable types of kind |* -> *|.
-- This class is derivable in GHC with the |DeriveGeneric| flag on.
class Generic1 f where
  --  Generic representation type
  type Rep1 f :: * -> *
  --  Convert from the datatype to its representation
  from1  :: f a -> (Rep1 f) a
  --  Convert from the representation to the datatype
  to1    :: (Rep1 f) a -> f a
\end{code}
}.

To define a generic algorithm, one gives class instances for these primitives and gives a general definition in terms of |from1| and |to1|.

\section{Some useful data types}

\subsection{Right-lists and left-lists}

Let's start with a very familiar data type of lists:
\begin{code}
data List a = Nil | Cons a (List a)
\end{code}
This data type is sometimes more specifically called ``cons lists'' (for historical reasons going back to early Lisp implementations).
One might also call them ``right lists'', since they grow rightward.
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
  from RNil = L1 U1
  from (a :< as) = R1 (Par1 a :*: as)
  to (L1 U1) = RNil
  to (R1 (Par1 a :*: as)) = a :< as

instance Generic1 LList where
  type Rep1 LList = U1 :+: LList :*: Par1
  from LNil = L1 U1
  from (a :< as) = R1 (as :*: Par1 a)
  to (L1 U1) = LNil
  to (R1 (as :*: Par1 a)) = as >: a
\end{code}

|RList| and |LList| are isomorphic not only to their underlying representation functors, but also to each other, as follows:
\begin{code}
rToL :: RList a -> LList a
rToL RNil = LNil
rToL (a :< as) = rToL as >: a

lToR :: LList a -> RList a
lToR LNil = RNil
lToR (as >: a) = a :< lToR as
\end{code}
Since these list types are easily isomorphic, why would we want to distinguish between them?
One reason is that they may capture different intentions.
For instance, a zipper for right lists comprises a left-list for the (reversed) elements leading up to a position, a current element of focus, and a right-list for the not-yet-visited elements:
\begin{code}
data ZipperRList a = ZipperRList (LList a) a (RList a)
\end{code}
or
\begin{code}
type ZipperRList = LList :*: Par1 :*: RList
\end{code}
\note{To do: review Conor McBride's papers, and cite them here.}

Another reason is that have usefully different instances for standard type classes, leading---as we will see---to different operational characteristics, especially with regard to parallelism.

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
For instance, for ternary leaf trees:
\begin{code}
instance Functor Tree where
  fmap h (Leaf a) = Leaf (h a)
  fmap h (Branch t1 t2 t3) =
    Branch (fmap h t1) (fmap h t2) (fmap h t3)

instance Foldable Tree where
  foldMap h (Leaf a) = h a
  foldMap h (Branch t1 t2 t3) =
    foldMap h t1 <> foldMap h t2 <> foldMap h t3

instance Traversable Tree where
  traverse h (Leaf a) = Leaf <#> h a
  traverse h (Branch t1 t2 t3) =
    Branch <#> traverse h t1 <*> traverse h t2 <*> traverse h t3
\end{code}
Note that |<#>| is infix |fmap|.

Not only do we have repetition \emph{within} each instance definition (the three occurrences of |fmap h| above), we also have repetition \emph{among} instances for $n$-ary trees for different $n$.
Fortunately, we can simplify and unify with a shift in formulation.
Think of a branch node as having not $n$ subtrees, but rather a single uniform $n$-tuple of subtrees.
Assume for now that we have a functor of finite lists statically indexed by length as well as element type:
\begin{code}
type Vec :: Nat -> * -> * SPC -- abstract for now
instance Functor      (Vec n) where ...
instance Foldable     (Vec n) where ...
instance Traversable  (Vec n) where ...
\end{code}
\out{\note{Explain notation.}}
Define a single type of $n$-ary leaf trees, polymorphic over $n$:
\begin{code}
data Tree n a = Leaf a | Branch (Vec n (Tree a))
\end{code}

The more general vector-based instance definitions are simpler than even the binary-only versions given above:
\begin{code}
instance Functor (Tree n) where
  fmap h (Leaf a) = Leaf (h a)
  fmap h (Branch ts) = Branch ((fmap.fmap) f ts)

instance Foldable (Tree n) where
  foldMap h (Leaf a) = h a
  foldMap h (Branch ts) = (foldMap.foldMap) h ts

instance Traversable (Tree n) where
  traverse h (Leaf a) = Leaf <#> h a
  traverse f (Branch ts) =
    Branch <#> (traverse.traverse) f ts
\end{code}

Notice that these instance definitions rely on very little about the |Vec n| functor.
Specifically, for each of |Functor|, |Foldable|, and |Traversable|, the instance for |Tree n| needs only the corresponding instance for |Vec n|.
For this reason, we can easily generalize from |Vec n| as follows:
\begin{code}
data Tree f a = Leaf a | Branch (f (Tree a))
\end{code}
The instance definitions for ``|f|-ary'' trees are exactly as with $n$-ary, except for making the requirements on |f| implicit:
\begin{code}
instance Functor      f => Functor      (Tree f) where ...
instance Foldable     f => Foldable     (Tree f) where ...
instance Traversable  f => Traversable  (Tree f) where ...
\end{code}
This generalization covers ``list-ary'' (rose) trees and even ``tree-ary'' trees.
With this functor-parametrized tree type, we can reconstruct $n$-ary trees as |Tree (Vec n)|.

Just as there are both left- and right-growing lists, trees come in two flavors as well.
The forms above are all ``top-down'', in the sense that successive unwrappings of branch nodes reveal subtrees moving from the top downward.
(No unwrapping for the top level, one unwrapping for the collection of next-to-top subtrees, another for the collection of next level down, etc.) There are also ``bottom-up'' trees, in which successive branch node unwrappings reveal the information in subtrees from the bottom moving upward.
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
Bottom-up trees (|LTree|) are a canonical example of ``nested'' or ``non-regular'' data types, requiring polymorphic recursion \cite{Bird1998}.

\subsection{Statically shaped variations}\seclabel{statically-shaped-types}

Some algorithms work only on collections of restricted size.
For instance, the most common parallel scan and FFT algorithms are limited to arrays of size $2^n$, while the more general (not just binary) Cooley-Tukey FFT algorithms require composite size, i.e., $m \cdot n$ for integers $m, n \ge 2$.
In array-based algorithms, these restrictions can be realized in one of two ways:

\begin{itemize}
\item
  Check array sizes dynamically, incurring a performance penalty; or
\item
  Document the restriction, assume the best, and blame the library user if the assumption is violated.
\end{itemize}

A third---much less commonly used--option is to statically verify the size restriction at the call site, perhaps by using a dependently typed language and providing proofs as part of the call.

A lightweight compromise is to simulate some of the power dependent types via type-level encodings of sizes, as with our use of |Nat| for indexing the |Vec| type above.
There are many possible definitions for |Nat|.
For this paper, assume that |Nat| is a kind-promoted version \note{cite} of the following data type of Peano numbers (constructed via zero and successor):
\begin{code}
data Nat = Z | S Nat
\end{code}
Thanks to promotion, |Nat| is not only a new data type with value-level constructors |Z| and |S|, but also a new \emph{kind} with \emph{type-level} constructors |Z| and |S|.

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
  type Rep1 (RVec Z) = Par1 :*: RVec n
  from (a :< as) = Par1 a :*: as
  to (Par1 a :*: as) = a :< as

instance Generic1 (LVec Z) where
  type Rep1 (LVec Z) = U1
  from RNil = U1
  to U1 = RNil
instance Generic1 (LVec n) => Generic1 (LVec (S n)) where
  type Rep1 (LVec Z) = LVec n :*: Par1
  from (a :< as) = Par1 a :*: as
  to (Par1 a :*: as) = a :< as
\end{code}

For leaf trees, we have a choice between perfect and imperfect trees.
A ``perfect'' leaf tree is one in which all leaves are at the same depth.
Both forms can be ``statically shaped'', but we'll use just perfect trees in this paper, for which we need only a single type-level number signifying the depth of all leaves.
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

instance  Generic1 (RPow f n) =>
          Generic1 (RPow f (S n)) where
  type Rep1 (RPow f (S n)) = f :.: RPow f n
  from1 (B ts) = Comp1 ts
  to1 (Comp1 ts)  = B ts

instance Generic1 (LPow f Z) where
  type Rep1 (LPow f Z) = Par1
  from1 (L a) = Par1 a
  to1 (Par1 a) = L a

instance  Generic1 (LPow f n) =>
          Generic1 (LPow f (S n)) where
  type Rep1 (LPow f (S n)) = LPow f n :.: f
  from1 (B ts) = Comp1 ts
  to1 (Comp1 ts) = B ts
\end{code}

We can then give these statically shaped data types |Functor|, |Foldable|, and |Traversable| instances exactly matching the dynamically shaped versions given above.
In addition, they have |Applicative| and |Monad| instances, left as an exercise for the reader.
\note{Maybe provide and mention homomorphisms with the function instances as well as the trie (representable functor) connection.}

\subsubsection{Type family formulation}

Note that |RVec n| and |LVec n| are each essentially an $n$-ary functor \emph{product} of |Par1|.
Similarly, |RPow f n| and |LPow f n| are each an $n$-ary functor \emph{composition} of |f|.
Functor product and functor composition are both associative but only up to isomorphism.
This limit to associativity is exactly why both exist and are useful.
While |RVec| and |RPow| are right associations, |LVec| and |LPow| are left associations.

Instead of the GADT-based definitions given above for |RVec|, |LVec|, |RPow|, and |LPow|, we can make the repeated product and repeated composition more apparent by using type families \note{cite}, with instances defined inductively over type-level natural numbers.
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
\note{Something about tries and logarithms, or ``Naperian functors''.}

Because these type-family-based definitions are expressed in terms of existing generic building blocks, we directly inherit many existing class instances rather than having to define them.
A downside is that we \emph{cannot} provide them, which will pose a challenge (though easily surmounted) with FFT on vectors, as well as custom instances for displaying structures.

\subsection{Bushes}\seclabel{bushes}

In contrast to vectors, our tree types are perfectly balanced, as is helpful in obtaining naturally parallel algorithms.
From another perspective, however, they are quite imbalanced.
The functor composition operator is used fully left-associated for |LPow| and fully right-associated for |RPow| (hence the names).
It's easy to define a composition-balanced type as well:
\begin{code}
type family Bush n where
  Bush Z      = Pair
  Bush (S n)  = Bush n :.: Bush n
\end{code}
The uniform |Pair| functor can be defined in a variety of ways, including as |Par1 :*: Par1|.
Whereas each |RPow Pair n| and |LPow Pair n| holds $2^n$ elements, each statically shaped |Bush n| holds $2^{2^n}$ elements.
Moreover, there's nothing special about |Pair| or \emph{binary} composition here.
Either could be replaced or generalized.

Our ``bush'' type is adapted from an example of nested data types that has a more less regular shape \cite{Bird1998}:
\begin{code}
data Bush a = NilB | ConsB a (Bush (Bush a))
\end{code}

Bushes are to trees as trees are to vectors, in the following sense.
Functor product is associative up to isomorphism.
Where |RVec| and |LVec| choose fully right or left associated products, |RPow f| and |LPow f| form perfectly and recursively balanced products.
Likewise, functor composition is associative up to isomorphism.
Where |RPow f| and |LPow f| are fully right- and left-associated compositions, |Bush f| forms balanced compositions.
Many other variations are possible, but the |Bush| definition above will suffice for this paper.

\note{The previous paragraph is somewhat redundant with the preceding paragraphs. Tighten.}

\section{Parallel scan}

Given $a_0,\ldots,a_{n-1}$, the ``prefix sum'' is a sequence $b_0,\ldots,b_n$ such that
\[b_k = \sum\limits_{0 \le i < k}{a_i}\]
More generally, given any associative operation $\oplus$, the ``prefix scan'' is defined by
\[b_k = \bigoplus\limits_{0 \le i < k}{a_i}\]
with $b_0$ being the identity for $\oplus$.
(One can define a similar operation if we assume semigroup---lacking identity element---rather than monoid, but the development is more straightforward with identity.)

Parallel scan has surprisingly broad applications, including the following, taken from a longer list in \cite{BlellochTR90}:
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
As we will see below, the choice of exclusive+total above makes for a more elegant generic decomposition.

The standard list prefix scans in Haskell, |scanl| and |scanr|, also yield one more output element than input, which is possible for lists.
For other data types, such as trees and especially perfect ones, there may not be a place to stash the extra value.
For a generic scan applying to many different data types, we can simply form a product, so that scanning maps |f a| to |f a :* a|.\footnote{Please forgive the overloading of ``|:*|'' for infix type-level and functor-level products throughout the paper.}
The extra summary value is the fold over the whole input structure.
Thus, we have the following class for left-scannable functors:
\begin{code}
class Functor f => LScan f where
  lscan :: Monoid a => f a -> f a :* a
\end{code}
The |Functor| superclass is just for convenience and can be dropped in favor of more verbose signatures elsewhere.

When |f| is in |Traversable|, there is simple and general specification using operations from the standard Haskell libraries:
\begin{code}
lscan == swap . mapAccumL (\ acc a -> (acc <> a,acc)) mempty
\end{code}
where |mapAccumL| has type
\begin{code}
Traversable t => (b -> a -> b :* c) -> b -> t a -> b :* t c
\end{code}

Although all of the example types in this paper are indeed in |Traversable|, using this |lscan| specification as an implementation results in an entirely sequential implementation, since data dependencies are \emph{linearly} threaded through the whole computation.

Rather than defining |LScan| instances for all of our data types, the idea of generic programming is to define instances only for the small set of fundamental functor combinators and then automatically compose instances for other types via the generic encodings (derived automatically when possible).
To do so, provide a default signature and definition for functors with such encodings \note{mention and cite |DefaultSignatures|}:
\begin{code}
class Functor f => LScan f where
  lscan :: Monoid a => f a -> f a :* a
  default lscan  ::  (Generic1 f, LScan (Rep1 f), Monoid a)
                 =>  f a -> f a :* a
  lscan = first to1 . lscan . from1
\end{code}

As an example of a sequential (non-parallel) scan, see \circuitrefdef{lsums-lv8}{Linear scan example}{8}{7}.
In this picture (and many more like it below), the data types are shown in flattened form in the input and output (labeled |In| and |Out|), and the work and depth are shown in the caption (as \emph{W} and \emph{D}).
``Work'' is the total number of primitive operations performed, while ``depth'' is the longest dependency chain, and hence a measure of ideal parallel computation time \needcite{}.
As promised, there is always one more output than input, and the last output is the fold that summarizes the entire structure being scanned.

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

With these easy instances out of the way, we have only two left to define: product and composition.

\subsection{Product}

Suppose we have linear scans of size, as in \figrefdef{two-scans}{Two scans}{
\vspace{-2ex}
\wfig{2.0in}{lsums-lv5}
\vspace{-5ex}
\wfig{3.3in}{lsums-lv11}
}.
We will see later how these individual scans arise from particular functors |f| and |g| (of sizes five and eleven respectively), but for now take them as given.
To understand |lscan| on functor products, consider how to combine the scans of |f| and |g| into scan for |f :*: g|.

Because we are left-scanning, every prefix of |f| is also a prefix of |f :*: g|, so the |lscan| results for |f| are also correct results for |f :*: g|.
The prefixes of |g| are not prefixes of |f :*: g|, however, since each |g|-prefix misses all of |f|.
The prefix \emph{sums}, therefore, are lacking the sum of all of |f|, which corresponds to the last output of the |lscan| result for |f|.
All we need to do, therefore, is adjust \emph{each} |g| result by the final |f| result, as shown in \circuitrefdef{lsums-lv5xlv11-highlight}{Product scan}{26}{11}.
The general product instance is in \figrefdef{product-scan}{Product scan definition}{
\begin{code}
instance (LScan f, LScan g) => LScan (f :*: g) where
  lscan (fa :*: ga) = (fa' :*: ((fx <> NOP) <#> ga'), fx <> gx)
   where
     (fa'  , fx)  = lscan fa
     (ga'  , gx)  = lscan ga
\end{code}
}.

We now have enough functionality for scanning vectors using either the GADT or type family definitions from \secref{statically-shaped-types}.
\circuitrefdef{lsums-rv8-no-hash-no-opt}{scan for |RVec N8|, unoptimized}{45}{8} shows |lscan| for |RVec N8| (\emph{right} vector of length 8).
There are some zero additions that can be easily optimized away, resulting in \circuitrefdef{lsums-rv8}{scan for |RVec N8|, optimized}{29}{7}.

The combination of left scan and right vector is particularly unfortunate, as it involves quadratic work and linear depth.
The source of quadratic work is the product instance's \emph{right} adjustment combined with the right-associated shape of |RVec|.
Each single element (left) is used to adjust the entire suffix (right), requiring linear work at each step, summing to quadratic.

In contrast, with left-associated vectors, each prefix summary (left) is used to update a single element (right), leading to linear work, as shown in \circuitrefdef{lsums-lv8-no-hash-no-opt}{scan for |LVec N8|, unoptimized}{25}{8} and \figref{lsums-lv8} (optimized).

Performing a suffix/right scan on a left vector also leads to quadratic work, reduced to linear by switching to right vectors.

Although work is greatly reduced (from quadratic to linear), depth remains at linear.
The reason is that unbalanced data types lead to unbalanced parallelism.
Both |RVec| and |LVec| are ``parallel'' in a degenerate sense, but we only get to perform small computations in parallel with large one (more apparent in \figreftwo{lsums-rv8-no-hash-no-opt}{lsums-lv8-no-hash-no-opt}), so that the result is essentially sequential.

To get a more parallelism, we could replace a type like |LVec N16| with a isomorphic product such as |LVec N5 :*: LVec N11|, resulting in \figref{lsums-lv5xlv11-highlight}, reducing depth from 15 to 11.
More generally, scan on |LVec m :*: LVec n| has depth |max (m-1) (n-1) + 1 = max m n|.
For an ideal partition adding up to |p|, we'll want |m = n = p/2|.
For instance, replace |LVec N16| with the isomorphic product |LVec N8 :*: LVec N8|, resulting in \circuitrefdef{lsums-lv8xlv8}{|lscan| on |LVec N8 :*: LVec N8|}{23}{8} with depth 8.

Can we do better?
Not as a single product, but we can as more than one product, as shown in
\circuitdef{lsums-lv5-5-6-l}{|(LVec N5 :*: LVec N5) :*: LVec N6|}{25}{6}
\circuitdef{lsums-lv5-5-6-r}{|LVec N5 :*: (LVec N5 :*: LVec N6)|}{31}{7}
\figreftwo{lsums-lv5-5-6-l}{lsums-lv5-5-6-r}.
Again, the more balance, the better.

\subsection{Composition}

We now come to the last of our six functor combinators, namely composition, i.e., a structure of structures.
Suppose we have a triple of quadruples: |LVec N3 :.: LVec N4|.
We know how to scan each quadruple, as in \figrefdef{triple-scan}{triple scan}{
\vspace{-3ex}
\wfig{2.5in}{lsums-lv4}
\vspace{-3ex}
\wfig{2.5in}{lsums-lv4}
\vspace{-3ex}
\wfig{2.5in}{lsums-lv4}
}.
How can we combine the results of each scan into a scan for |LVec N3 :.: LVec N4|?
We already know the answer, since this composite type is essentially |(LVec N4 :*: LVec N4) :*: LVec N4|, the scan for which is determined by the |Par1| and product instances and is shown in \circuitrefdef{lsums-lv3olv4-highlight}{Scan for |LVec N3 :.: LVec N4|}{18}{5}.

Let's reflect on this example as we did with binary products above.
The prefixes of the first quadruple are all prefixes of the composite structure, so their prefix sums are prefix sums of the composite and so are used as they are.
For every following quadruple, the prefix sums are lacking the sum of all elements from the earlier quadruples and so must be adjusted accordingly, as emphasized in \figref{lsums-lv3olv4-highlight}.

Now we get to the surprising heart of generic parallel scan!
Observe that the sums of elements from earlier quadruples are computed entirely from the final summary results from each quadruple.
We end up needing the sum of every \emph{prefix} of the triple of summaries, and so we are computing not just three prefix scans over |LVec N4| but also \emph{one additional scan} over |LVec N3|.
Moreover, the apparent inconsistency of adjusting all quadruples \emph{except} for the first one is an illusion brought on by premature optimization.
We can instead adjust \emph{every} quadruple by the corresponding result of this final scan of summaries, the first summary being zero.
These zero-additions can then be optimized away later.
See \circuitrefdef{lsums-lv5olv7-highlight}{Scan for |LVec N5 :.: LVec N7|}{59}{10} for a larger example showing this same pattern.

The general case is captured in an |LScan| instance for functor composition, in \figrefdef{composition-scan}{Composition scan definition}{
\begin{code}
instance (LScan g, LScan f, Zip g) =>  LScan (g :.: f) where
  lscan (Comp1 gfa) = (Comp1 (zipWith adjustl tots' gfa'), tot)
   where
     (gfa', tots)  = unzip (lscan <#> gfa)
     (tots',tot)   = lscan tots
     adjustl t     = fmap (t <>)
\end{code}
}.

\subsection{More examples}

We now know how to scan the full vocabulary of generic functor combinators, and we've seen the consequences for several data types.
Let's now examine how well generic scan works for some other example structures.
We have already seen |Pair :.: LVec N8| as |LVec N8 :*: LVec N8| in \figref{lsums-lv8xlv8}.
The reverse composition leads to quite a different computation shape, as \circuitrefdef{lsums-lv8-p}{|LVec N8 :.: Pair|}{23}{8} shows.
Yet another factoring appears in \circuitrefdef{lsums-lv4olv4}{|LVec N4 :.: LVec N4|}{25}{6}.

Next let's try functor exponentiation in its left- and right-associated form.
We just saw the equivalent of |RPow (LVec N4) N2| (and |LPow (LVec N4) N2|) as \figref{lsums-lv4olv4}.
\circuitdef{lsums-rb4}{|RPow (LVec N4) N2|}{33}{4}
\circuitdef{lsums-lb4}{|LPow (LVec N4) N2|}{27}{6}
\figreftwo{lsums-rb4}{lsums-lb4} show |RPow Pair N4| and |LPow Pair N4| (top-down and bottom-up perfect binary leaf trees of depth four).

Finally, consider the |Bush| type from \secref{bushes}.
Figures~\ref{fig:lsums-bush0} through~\ref{fig:lsums-bush3} show |lscan| for bushes of depth zero through three.
\circuitdef{lsums-bush0}{|Bush N0|}{2}{1}
\circuitdef{lsums-bush1}{|Bush N1|}{5}{2}
\circuitdef{lsums-bush2}{|Bush N2|}{30}{5}
\circuitdef{lsums-bush3}{|Bush N3|}{719}{10}

\note{Efficiency remarks about |RPow|, |LPow|, and |Bush|.}

\subsection{Complexity analysis}

\note{Either in this section or sprinkled throughout the functor combinators and examples above.}

\subsection{Some convenient packaging}

For generality, |lscan| works on arbitrary monoids.
For convenience, let's define some specializations.
One way to do so is by providing functions that map non-monoidal values to and from monoids.
Start with a class similar to |Generic| for providing alternative representations:
\begin{code}
class Newtype n where
  type O n :: *
  pack    :: O n -> n
  unpack  :: n -> O n
\end{code}
This class is from \cite{newtype-generics}, which also defines many instances for commonly used types.
Given this vocabulary, we can scan structures of non-monoidal values by packing values into a chosen monoid, scanning, and then unpacking:
\begin{code}
lscanN  ::  forall n o f. (Newtype n, o ~ O n, LScan f, Monoid n)
        =>  f o -> f o :* o
lscanN = (fmap unpack *** unpack) . lscan . fmap (pack @n)
\end{code}

\begin{code}
lsums, lproducts :: (LScan f, Num a) => f a -> f a :* a
lalls :: LScan f => f Bool -> f Bool :* Bool
lsums      = lscanN @(Sum a)
lproducts  = lscanN @(Product a)
lalls      = lscanN @All
...
\end{code}
The |(***)| operation applies two given functions to the respective components of a pair.

\subsectiondef{Applications}

As a first simple example application of parallel scan, let's construct powers of a given number to fill a structure |f|.
A simple implementation builds a structure with identical values using |pure| (from |Applicative|) and then calculates all prefix products:

> powers :: (LScan f, Applicative f, Num a) => a -> f a :* a
> powers = lproducts  . pure

\circuitrefdef{powers-rb4-no-hash}{|powers @(RBin N4)|}{32}{4} shows one instance of |powers|.
A quick examination shows that there is a lot of redundant computation due to the special context of scanning over identical values.
For instance, for an input $x$, we compute $x^2$ eight times and $x^4$ four times.
Fortunately, automatic common subexpression elimination (CSE) removes these redundancies easily, resulting in \circuitrefdef{powers-rb4}{|powers @(RBin N4)| --- with CSE}{15}{4}.

Building on this example, let's define polynomial evaluation, mapping a structure of coefficients $a_0, \ldots, a_{n-1}$ and a parameter $x$ to $\sum_{0 \le i < n} a_i \cdot x^i$\out{$a_0 + a_1 x + \cdots + a_{n-1} x^{n-1}$}.
A very simple formulation is to construct all of the powers of $x$ and then form a dot product with the coefficients:

\begin{code}
evalPoly  ::  (LScan f, Foldable f, Applicative f, Num a)
          =>  f a -> a -> a
evalPoly coeffs x = coeffs <.> fst (powers x)

(<.>) :: (Foldable f, Applicative f, Num a) => f a -> f a -> a
u <.> v = sum (liftA2 (*) u v)
\end{code}
See \circuitrefdef{evalPoly-rb4}{|evalPoly @(RBin N4)|}{29+15}{9}.



\subsection{Relation to known parallel scan algorithms}

\section{FFT}

\subsection{Background}

The Fast Fourier Transform (FFT) algorithm computes the Discrete Fourier Transform (DFT), reducing work from $O(n^2)$ to $O(n \log n)$.
First discovered by Gauss \cite{GaussFFTHistory}, the algorithm was rediscovered by \citet{danielson1942some}, and later by \citet{CooleyTukey}, who popularized the algorithm.

Given a sequence of complex numbers, $x_0, \ldots, x_{N-1}$, the DFT is defined as
$$ X_k =  \sum\limits_{n=0}^{N-1} x_n e^{\frac{-i2\pi kn}{N}} \text{, \quad for\ } 0 \le k < N $$
Naively implemented, this DFT definition leads to quadratic work.
The main trick to FFT is to factor $N$ into $N_1 N_2$ and then optimize the DFT definition, removing some exponentials that turn out to be equal to one.
The simplified result:
$$
      \sum_{n_1=0}^{N_1-1} 
        \left[ e^{-\frac{2\pi i}{N} n_1 k_2} \right]
          \left( \sum_{n_2=0}^{N_2-1} x_{N_1 n_2 + n_1}  
                  e^{-\frac{2\pi i}{N_2} n_2 k_2 } \right)
        e^{-\frac{2\pi i}{N_1} n_1 k_1}
$$
In this form, we have two smaller sets of DFTs: $N_1$ of size $N_2$ each, and $N_2$ of size $N_1$ each.
See \figrefdef{factored-dft}{Factored DFT}{\pic{cooley-tukey-general}}, from \cite{JohnsonCooleyTukeyPic}.

Rather than implementing FFT via sequences/arrays as usual, let's take a step and consider a more structured approach.

\subsection{Factor types, not numbers!}

The summation formula above exhibits a trait typical of array-based algorithms, namely index arithmetic, which is tedious to write and to read.
This arithmetic has a purpose, however, which is interpret an array as an array of arrays.
In a higher-level formulation, we might replace arrays and index arithmetic by an \emph{explicit} nesting of structures.
We have already seen the fundamental building block of structure nesting, namely functor composition.
Instead of factoring numbers, factor types.

As with scan, define a class of FFT-able structures and a generic default.
One new wrinkle is that the result shape differs from the original shape, so we'll use an associated functor ``|FFO|'':

\begin{code}
class FFT f where
  type FFO f :: * -> *
  fft :: f C -> FFO f C
  default fft  ::  ( Generic1 f, Generic1 (FFO f), FFT (Rep1 f)
                   , FFO (Rep1 f) ~ Rep1 (FFO f) )
               =>  f C -> FFO f C
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

where |ffts'| performs several non-contiguous FFTs:

> ffts' :: ... => g (f C) -> FFO g (f C)
> ffts' = transpose . fmap fft . transpose

Finally, the ``twiddle factors'' are all powers of a primitive $N^{\text{th}}$ root of unity:

> twiddle :: ... => g (f C) -> g (f C)
> twiddle = (liftA2.liftA2) (*) (omegas (size @(g :.: f)))
>
> omegas :: ... => Int -> g (f (Complex a))
> omegas n =
>  powers <$> powers (exp (- i * 2 * pi / fromIntegral n))

The |size| method calculates the size of a structure.
The ``|@|'' notation here is visible type application \cite{eisenberg2016visible}.
Unsurprisingly, the size of a composition is the product of the sizes.

Since |powers| is a scan (as defined in \secref{Applications}), we can compute |omegas| efficiently in parallel.

\subsection{Comparing data types}

\figreftwo{fft-rb4}{fft-lb4} show |fft| for top-down and bottom-up binary trees of depth four, and \figref{fft-bush2} for a bush of depth two.
Each complex number appears as its real and imaginary components.
\circuitdef{fft-rb4}{|RPow Pair N4|}{197}{8}
\circuitdef{fft-lb4}{|LPow Pair N4|}{197}{8}
\circuitdef{fft-bush2}{|Bush N2|}{186}{6}

The top-down and bottom-up tree algorithms correspond to two popular FFT variations known as ``decimation in time'' and ``decimation in frequency'' (``DIT'' and ``DIF''), respectively.
In the array formulation, these variations arise from choosing $N_1=2$ or $N_2=2$.
\figreftwo{fft-stats-16}{fft-stats-256} offer a more detailed comparison.
\figdef{fft-stats-16}{FFT for 16 complex values}{
\fftStats{
  \stat{|RPow Pair N4|}{74}{40}{74}{197}{8}
  \stat{|LPow Pair N4|}{74}{40}{74}{197}{8}
  \stat{|Bush      N2|}{72}{32}{72}{186}{6}
}}
\figdef{fft-stats-256}{FFT for 256 complex values}{
\fftStats{
  \stat{|RPow Pair N8|}{2690}{2582}{2690}{8241}{20}
  \stat{|LPow Pair N8|}{2690}{2582}{2690}{8241}{20}
  \stat{|Bush      N3|}{2528}{1922}{2528}{7310}{14}
}}
(The total operation counts include constants.)
Unlike scan, top-down and bottom-up trees lead to exactly the same work and depth.
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
