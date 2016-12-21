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

%format :< = " \triangleleft "
%format >: = " \triangleright "

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

Bottom-up trees (|LTree|) are a canonical example of ``nested'' or ``non-regular'' data types, requiring polymorphic recursion \cite{Bird1998}.

\subsection{Statically shaped variations}\seclabel{statically-shaped-types}

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

Our ``bush'' type is adapted from an example of nested data types that has a less regular shape:
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

As an example of a sequential (non-parallel) scan, see \circuitrefdef{lsums-lv8}{Linear scan example}{8}{7}.
In this picture (and many more like it below), the data types are shown in flattened form in the input and output (labeled |In| and |Out|), and the work and depth are shown in the caption (as \emph{W} and \emph{D}).
As promised, there is always one more output than input, and the last output is the fold that summarizes the entire structure being scanned.

Once we define |LScan| instances for our six fundamental combinators and given |Generic1| instances (defined automatically or manually) for a functor |Foo|, one can simply write |instance LScan Foo|. For our statically shaped vector, tree, and bush functors, we can use the GADT definitions with their manually defined |Generic1| instances, exploiting the |lscan| default, or we can use the type family versions without the need for the encoding (|from1|) and decoding (|to1|) steps.

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
\note{|LambdaCase| and |EmptyCase| extensions.}
\item An empty structure can only generate another empty structure with a summary value of |mempty|.
\item For a singleton value |Par1 a|, the combination of values before the first and only one is |mempty|, and the summary is the single values |a|.
\item For a sum, scan whichever structure is present and re-tag.
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

Because we are left-scanning every prefix of |f| is also a prefix of |f :*: g|, so the |lscan| results for |f| are also correct results for |f :*: g|.
The prefixes of |g|, are not prefixes of |f :*: g|, however, since each is missing all of |f|.
The prefix \emph{sums}, therefore, are lacking the sum of all of |f|, which corresponds to the last output of the |lscan| result for |f|.
All we need to do, therefore, is adjust \emph{each} |g| result by the final |f| result, as shown in \circuitrefdef{lsums-lv5xlv11-highlight}{Product scan}{26}{11}.

The general product instance is in \figrefdef{product-scan}{Product scan}{
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
There are also some zero additions that can be easily optimized away, resulting in \circuitrefdef{lsums-rv8}{scan for |RVec N8|, optimized}{29}{7}.

The combination of left scan and right vector is particularly unfortunate, as it involves quadratic work and linear depth.
\note{Define ``work'' and ``depth'' earlier.}
The source of quadratic work is the product instance's \emph{right} adjustment combined with the right-associated shape of |RVec|.
Each single element (left) is used to adjust the entire suffix (right), requiring linear work at each step, adding up to quadratic.

In contrast, with left-associated vectors, each prefix summary (left) is used to update a single element (right), leading to linear work, as shown in \circuitrefdef{lsums-lv8-no-hash-no-opt}{scan for |LVec N8|, unoptimized}{25}{8} and \figref{lsums-lv8} (optimized).

Performing a suffix/right scan on a \emph{left} vector also leads to quadratic work, reduced by linear by switching to right vectors.

Although work is greatly reduced (from quadratic to linear), depth remains at linear.
The reason is that unbalanced data types lead to unbalanced parallelism.
Both |RVec| and |LVec| are ``parallel'' in a degenerate sense, but we only get to perform small computations in parallel with large one (more apparent in \figreftwo{lsums-rv8-no-hash-no-opt}{lsums-lv8-no-hash-no-opt}), so that the result is essentially sequential.

To get a more parallelism, we could replace a type like |LVec N16| with a isomorphic product such as |LVec N5 :*: LVec N11|, resulting in \figref{lsums-lv5xlv11-highlight}, reducing depth from 15 to 11.
More generally, scan on |LVec m :*: LVec n| has depth |max (m-1) (n-1) + 1 = max m n|.
For an ideal partition adding up to |p|, we'll want |m = n = p/2|.
For instance, replace |LVec N16| with the isomorphic product |LVec N8 :*: LVec N8|, resulting in \circuitrefdef{lsums-lv8xlv8}{|lscan| on |LVec N8 :*: LVec N8|}{23}{8}.

Can we do better?
Not as a single product, but we can as more than one product, as shown in
\circuitrefdef{lsums-lv5-5-6-l}{|(LVec N5 :*: LVec N5) :*: LVec N6|}{25}{6}
\circuitrefdef{lsums-lv5-5-6-r}{|LVec N5 :*: (LVec N5 :*: LVec N6)|}{31}{7}
\figreftwo{lsums-lv5-5-6-l}{lsums-lv5-5-6-r}.

Again, the more balance, the better.

\subsection{Composition}

We now come to the last of our six functor combinators, namely composition, i.e., a structure of structures.
Suppose we have a triple of quadruples, i.e., |LVec N3 :.: LVec N4|.
We know how to scan each of the quadruples, as in \figrefdef{triple-scan}{triple scan}{
\vspace{-3ex}
\wfig{2.5in}{lsums-lv4}
\vspace{-3ex}
\wfig{2.5in}{lsums-lv4}
\vspace{-3ex}
\wfig{2.5in}{lsums-lv4}
}.
How can we combine the results of each scan into the scan |LVec N3 :.: LVec N4|?
We already know the answer, since this composite type is essentially |(LVec N4 :*: LVec N4) :*: LVec N4|, the scan for which is determined by the |Par1| and product instances and is shown in \circuitrefdef{lsums-lv3olv4-highlight}{Scan for |LVec N3 :.: LVec N4|}{18}{5}.

Let's reflect on this example as we did with binary products above.
The prefixes of the first quadruple are all prefixes of the composite structure, so their prefix sums are prefix sums of the composite and so are used as they are.
For every following quadruple, the prefix sums are lacking the sum of all elements from the earlier quadruples and so must be adjusted accordingly, as emphasized in the figure.

Now we get to the surprising heart of generic parallel scan!
Observe that the sums of elements from earlier quadruples are computed entirely from the final summary results from each quadruple.
We end up needing the sum of every \emph{prefix} of the triple of summaries, and so we are computing not just three prefix scans over |LVec N4| but also \emph{one additional scan} over |LVec N3|.
Moreover, the apparent inconsistency of adjusting all quadruples \emph{except} for the first one is an illusion brought on by premature optimization.
We can instead adjust \emph{every} quadruple by the corresponding result of this final scan of summaries, the first summary being zero.
These zero-additions can then be optimized away later.
See \circuitrefdef{lsums-lv5olv7-highlight}{Scan for |LVec N5 :.: LVec N7|}{59}{10} for a larger example showing this same pattern.

The general case is captured in an |LScan| instance for functor composition, in \figrefdef{composition-scan}{Composition scan}{
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
Let's now see how well generic scan works for some other example structures.
We have already seen |Pair :.: LVec N8| as |LVec N8 :*: LVec N8| in \figref{lsums-lv8xlv8}.
The reverse composition leads to quite a different computation shape, as \circuitrefdef{lsums-lv8-p}{|LVec N8 :.: Pair|}{23}{8} shows.
Yet another factoring appears in \circuitrefdef{lsums-lv4olv4}{|LVec N4 :.: LVec N4|}{25}{6}.



\subsection{Complexity analysis}

\note{Either in this section or sprinkled throughout the functor combinators and examples above.}

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
