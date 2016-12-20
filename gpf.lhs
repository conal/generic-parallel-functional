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

\input{macros}

%include polycode.fmt
%include forall.fmt
%include greek.fmt
%include mine.fmt

\setlength\mathindent{1ex}

\title{Two generic functional parallel algorithms}

\authorinfo{Conal Elliott}{Target}{conal@@conal.net}

\bibliographystyle{plainnat}

\begin{document}

\maketitle

\section{Abstract}

Parallel programming, whether imperative or functional, has long focused on arrays as the central data type. Meanwhile, typed functional programming has explored a variety of data types, including lists and various forms of trees. \emph{Generic} functional programming decomposes these data types into a small set of fundamental building blocks: sum, product, composition, and their associated identities. Definitions over these few fundamental type constructions then automatically assemble into algorithms for an infinite set of data types---some familiar and some new. This paper presents generic functional formulations for two important and well-known classes of parallel algorithms: parallel scan (generalized prefix sum) and Fast Fourier Transform (FFT). Notably, arrays play no role in these formulations. Consequent benefits include a simpler and more compositional style, much use of common algebraic patterns (such as |Functor|, |Applicative|, |Foldable|, and |Traversable|), and freedom from possibility of run-time indexing errors. The functional generic style also clearly reveals deep commonality among what otherwise appears to be quite different algorithms. Instantiating the generic formulations to ``top-down'' and ``bottom-up'' trees, two well-known algorithms for each of parallel scan and FFT naturally emerge, as well as two possibly new algorithms.

\section{Introduction}

\section{Generic programming in Haskell}

\note{``Functor algebra''}

\section{Some useful data types}

\subsection{Right-lists and left-lists}

Let's start with a very familiar data type of lists:
\begin{code}
data List a = Nil | Cons a (List a)
\end{code}

This data type is sometimes more specifically called ``cons lists'' (for historical reasons going back to early Lisp implementations). One might also call them ``right lists'', since they grow rightward.
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

Spelling out the isomorphism explicitly,
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

Not only are |RList| and |LList| isomorphic to their underlying representation functors, but also to each other, as follows:
\begin{code}
rToL :: RList a -> LList a
rToL RNil = LNil
rToL (a :< as) = rToL as >: a

lToR :: LList a -> RList a
lToR LNil = RNil
lToR (as >: a) = a :< lToR as
\end{code}

Since these list types are easily isomorphic, why would we want to distinguish between them? One reason is that they may capture different intents. For instance, a zipper for right lists comprises a left-list for the (reversed) elements leading up to a position, a current element of focus, and a right-list for the not-yet-visited elements:
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

After lists, trees are perhaps the most commonly used data structure in functional programming. Moreover, in contrast with lists, the symmetry possible with trees naturally leads to parallel-friendly algorithms. Also unlike lists, there are quite a few varieties of trees.

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

Already, this style of definition is starting to show some strain. The repetition seen in the data type definition will also appear in instance definitions. For instance, for ternary leaf trees:
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
  traverse h (Leaf a) = Leaf <$> h a
  traverse h (Branch t1 t2 t3) =
    Branch <$> traverse h t1 <*> traverse h t2 <*> traverse h t3
\end{code}

Not only do we have repetition \emph{within} each instance definition (the three occurrences of |fmap h| above), we also have repetition \emph{among} instances for $n$-ary trees for different $n$. Fortunately, we can simplify and unify with a shift in formulation. Think of a branch node as having not $n$ subtrees, but rather a single uniform $n$-tuple of subtrees. Assume for now that we have a functor of finite lists statically indexed by length as well as element type:
\begin{code}
type Vec :: Nat -> * -> *  -- abstract for now
\end{code}

\note{Explain notation.}

Then we can define a single type of $n$-ary leaf trees:
\begin{code}
data Tree n a = Leaf a | Branch (Vec n (Tree a))
\end{code}

The instance definitions are simpler than even for binary trees given above:
\begin{code}
instance Functor (Tree n) where
  fmap h (Leaf a) = Leaf (h a)
  fmap h (Branch ts) = Branch ((fmap.fmap) f ts)

instance Foldable (Tree n) where
  foldMap h (Leaf a) = h a
  foldMap h (Branch ts) = (foldMap.foldMap) h ts

instance Traversable (Tree n) where
  traverse h (Leaf a) = Leaf <$> h a
  traverse f (Branch ts) =
    Branch <$> (traverse.traverse) f ts
\end{code}

\note{Note that |<$>| is infix |fmap|.}

Notice that these instance definitions rely on very little about the |Vec n| functor. Specifically, for each of |Functor|, |Foldable|, and |Traversable|, the instance for |Tree n| needs only the corresponding instance for |Vec n|. For this reason, we can easily generalize |Vec n| as follows:
\begin{code}
data Tree f a = Leaf a | Branch (f (Tree a))
\end{code}

The instance definitions for ``|f|-ary'' trees are exactly as with $n$-ary, except for making the requirements on |f| implicit:
\begin{code}
instance Functor      f => Functor      (Tree f) where ...
instance Foldable     f => Foldable     (Tree f) where ...
instance Traversable  f => Traversable  (Tree f) where ...
\end{code}

\note{Type-check all of these definitions with a test module.}

This generalization covers ``list-ary'' (rose) trees and even ``tree-ary'' trees.

With this functor-parametrized tree type, we can reconstruct $n$-ary trees as |Tree (Vec n)|.

Just as there are both left- and right-growing lists, trees come in two flavors as well. The forms above are all ``top-down'', in the sense that successive unwrappings of branch nodes reveal subtrees moving from the top downward. (No unwrapping for the top level, one unwrapping for the collection of next-to-top subtrees, another for the collection of next level down, etc.) There are also ``bottom-up'' trees, in which successive branch node unwrappings reveal the information in subtrees from the bottom moving upward. In short:

\begin{itemize}
\item
  A top-down leaf tree is either a leaf or an |f|-structure of trees.
\item
  A bottom-up leaf tree is either a leaf or a tree of |f|-structures.
\end{itemize}

In Haskell,
\begin{code}
data TTree f a = TLeaf a | TBranch (f (TDTree a))

data BTree f a = BLeaf a | BBranch (BTree (f a))
\end{code}

Bottom-up trees (|LTree|) are a canonical example of ``nested'' or ``non-regular'' data types, requiring polymorphic recursion \note{citations}.

\subsection{Statically shaped variations}

\note{As GADTs and as type families.}

Some algorithms work only on collections of a limited size. For instance, the most common parallel scan and FFT algorithms are limited to arrays of size $2^n$, while the more general (not just binary) Cooley-Tukey FFT algorithms require composite size, i.e., $m \cdot n$ for integers $m, n \ge 2$. In array-based algorithms, these restrictions can be realized in one of two ways:

\begin{itemize}
\item
  Check array sizes dynamically, incurring a performance penalty; or
\item
  Document the restriction, assume the best, and blame the library user if the assumption is violated.
\end{itemize}

A third---much less commonly used--option is to statically verify the size restriction at the call site, perhaps by using a dependently typed language and providing proofs as part of the call.

A lightweight compromise is to simulate some of the power dependent types via type-level encodings of sizes, as with our the of |Nat| for indexing the |Vec| type above. There are many possible definitions for |Nat|. For this paper, assume that |Nat| is a kind-promoted version \note{cite} of the following data type of Peano numbers (zero and successor):
\begin{code}
data Nat = Z | S Nat
\end{code}

Thanks to promotion, |Nat| is not only a new data type with value-level constructors |Z| and |S|, but also a new \emph{kind} with \emph{type-level} constructors |Z| and |S|.

\subsubsection{GADT formulation}

Now we can define the length-indexed |Vec| type mentioned above, which is the canonical example of dependent types in either full dependently typed languages or as simulated with generalized algebraic data types (GADTs). As with lists, there are right- and left-growing versions: The former (borrowing constructor names from right- and left-lists):

Recall that the generic representations of |RList| and |LList| were built out of sum, unit, identity, and product. With static shaping, the sum disappears from the representation, moving from dynamic to static choice:
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

For leaf trees, we have a choice between perfect and imperfect trees. A ``perfect'' leaf tree is one in which all leaves are at the same depth. Both forms can be ``statically shaped'', but we'll just perfect trees in this paper. For a perfect tree, we need only a single type-level number signifying the depth of all leaves. For succinctness, rename |Leaf| and |Branch| to ``|L|'' and ``|B|''. For reasons soon to be explained, also rename the types |TTree| and |BTree| to ``|RPow|'' and ``|LPow|'':
\begin{code}
data RPow :: (* -> *) -> Nat -> * -> * where
  L :: a -> RPow f Z a
  B :: f (RPow f n a) -> RPow f (S n) a

data LPow :: (* -> *) -> Nat -> * -> * where
  L :: a -> LPow f Z a
  B :: LPow f n (f a) -> LPow f (S n) a
\end{code}

As with vectors, statically shaped |f|-ary trees, are generically represented like their dynamically shaped counterparts but with dynamic choice (sum) replaced by static choice:
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

We can then give these statically shaped data types |Functor|, |Foldable|, and |Traversable| instances exactly matching the dynamically shaped versions given above. In addition, they have |Applicative| and |Monad| instances, left as an exercise for the reader. \note{Maybe provide and mention homomorphisms with the function instances as well as the trie (representable functor) connection.}

\subsubsection{Type family formulation}

Note that |RVec n| and |LVec n| are each essentially an $n$-ary functor \emph{product} of |Par1|. Similarly, |RPow f n| and |LPow f n| are each an $n$-ary functor \emph{composition} of |f|. Functor product and functor composition are both associative but only up to isomorphism. This limit to associativity is exactly why both exist and are useful. While |RVec| and |RPow| are right associations, |LVec| and |LPow| are left associations.

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

Note the similarity between the |RVec| and |RPow| type family instances and the following definitions of multiplication and exponentiation on Peano numbers (with RHS parenthesizes for emphasis):
\begin{code}
m * 0      = 0
m * (1+n)  = m + (m * n)

m ^ 0      = 1
m ^ (1+n)  = m * (m ^ n)
\end{code}

Likewise, the type family instances for |LVec| and |LPow| are analogous to the following equivalent definitions of Peano multiplication and exponentiation:
\begin{code}
m * 0      = 0
m * (n+1)  = (m * n) + n

m ^ 0      = 1
m ^ (n+1)  = (m ^ n) * m
\end{code}

\note{Something about tries and logarithms, or ``Naperian functors''.}

Because these type-family-based definitions are expressed in terms of existing generic building blocks, we directly inherit many existing class instances rather than having to define them. A downside is that we \emph{cannot} provide them, which will pose a challenge (though easily surmounted) with FFT on vectors, as well as custom instances for displaying structures.

\subsection{Top-down and bottom-up bushes}

In contrast to vectors, our tree types are perfectly balanced, as is helpful in obtaining naturally parallel algorithms. From another perspective, however, they are quite imbalanced. The functor composition operator is used fully left-associated for |LPow| and fully right-associated for |RPow| (hence the names). It's easy to define a composition-balanced type as well:
\begin{code}
type family Bush n where
  Bush Z      = Pair
  Bush (S n)  = Bush n :.: Bush n
\end{code}

There's nothing special about |Pair| or \emph{binary} composition here. We could easily generalize to |RPow (Bush n) m| or |LPow (Bush n) m|.

Whereas each |RPow Pair n| and |LPow Pair n| holds $2^n$ elements, each statically shaped |Bush n| hold $2^{2^n}$ elements.

Our ``bush'' type is adapted from an example of nested data types \note{cite ``Nested Datatypes''} that has a less regular shape:
\begin{code}
data Bush a = NilB | ConsB a (Bush (Bush a))
\end{code}

Bushes are to trees as trees are to vectors, in the following sense. Functor product is associative up to isomorphism. Where |RVec| and |LVec| choose fully right or left associated products, |RPow f| and |LPow f| form perfectly and recursively balanced products. Likewise, functor composition is associative up to isomorphism. Shifting perspective, where |RPow f| and |LPow f| are fully right- and left-associated compositions, |Bush f| forms balanced compositions. Many other variations are possible, but the |Bush| definition above will suffice for this paper.

\section{Parallel scan}

Given $a_0,\ldots,a_{n-1}$, the ``prefix sum'' is a sequence $b_0,\ldots,b_n$ such that

\[b_k = \sum\limits_{0 \le i < k}{a_i}\]

More generally, given any associative operation $\oplus$, the ``prefix scan'' is defined by

\[b_k = \bigoplus\limits_{0 \le i < k}{a_i}\]

with $b_0$ being the identity for $\oplus$. (One can define a similar operation if we assume semigroup---lacking identity element---rather than monoid, but the development is more straightforward with identity.)

Parallel scan has surprising broad applications, including the following, taken from a longer list in \href{http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.128.6230}{\emph{Prefix
Sums and Their Applications}}:

\begin{itemize}
\item
  Adding multi-precision numbers
\item
  Polynomial evaluation
\item
  Solving recurrences
\item
  Sorting
\item
  Solving tridiagonal linear systems
\item
  Lexical analysis
\item
  Regular expression search
\item
  Labeling components in two dimensional images
\end{itemize}

Scans may be ``prefix'' (from the left, as above) or or ``suffix'' (from the right). We will just develop prefix scan, but generic suffix scan works out in the same way.

Note that $a_k$ does \emph{not} influence $b_k$ and as such. Often scans are classified as ``exclusive'' as above or ``inclusive'', where $a_k$ does contribute to $b_k$. Note also that there is one more output than input, which is atypical in parallel computing, perhaps because they're often performed in place. As we will see below, the choice exclusive+total above makes for a more elegant generic decomposition.

The standard list prefix scans in Haskell, |scanl| and |scanr| also yield one more output than input, which is possible for lists. For other data types, such as trees and especially perfect ones, there may not be a place to stash the extra value. For a generic scan applying to many different data types, we can simply form a product, so that scanning maps |f a| to |f a :* a|. \note{Note use of infix product throughout the paper.} The extra summary value is the fold over the whole input structure. Thus, we have the following class for left-scannable functors:
\begin{code}
class Functor f => LScan f where
  lscan :: Monoid a => f a -> f a :* a
\end{code}

\note{Maybe rename to ``|LScannable|''.}

\note{Maybe remove the |Functor| superclass, which I think just serves convenience.}

When |f| is in |Traversable|, there is simple and general specification using operations from the standard Haskell libraries:
\begin{code}
lscan == swap . mapAccumL ( acc a -> (acc <> a,acc)) mempty
\end{code}
where |mapAccumL| has type
\begin{code}
Traversable t => (b -> a -> b :* c) -> b -> t a -> b :* t c
\end{code}

Although all of the example types in this paper are indeed in |Traversable|, using this |lscan| specification as an implementation results in an entirely sequential implementation, since data dependencies are \emph{linearly} threaded through the whole computation.

Rather than defining |LScan| instances for all of our data types, the idea of generic programming is to define instances only for the small set of fundamental functor combinators and then automatically compose instances for other types via the generic encodings (derived automatically when possible). To do so, provide a default signature and definition for functors with such encodings:
\begin{code}
class Functor f => LScan f where
  lscan :: Monoid a => f a -> f a :* a
  default lscan  ::  (Generic1 f, LScan (Rep1 f))
                 =>  Monoid a => f a -> f a :* a
  lscan = first to1 . lscan . from1
\end{code}

Once we define |LScan| instances for our six fundamental combinators and given |Generic1| instances (defined automatically or manually) for a functor |Foo|, one can simply write |instance LScan Foo|. For our statically shaped vector, tree, and bush functors, we can use the GADT definitions with their manually defined |Generic1| instances, exploiting the |lscan| default, or we can use the type family versions without the need for the encoding (|from1|) and decoding (|to1|) steps.

\subsection{Easy instances}

\section{FFT}

\section{Related work}

\section{Reflections/conclusions}

\begin{itemize}
\item
  How would this work look with full dependent types?
\end{itemize}

\begin{center}\rule{0.5\linewidth}{\linethickness}\end{center}

\bibliography{bib}

\end{document}
