# Revisions

I've addressed suggestions as described below, with the change description described in square brackets.

## Mandatory

* Please ensure that you are using the correct document style, including
  author-year citations with correct usage as described in the call for
  papers.  These requirements are necessary for inclusion in PACM PL.

[I replaced all of noun-phrase citations, fixed the abstract formatting, and switched to the latest document style (unmodified).]

* There is a considerable body of previous work in the FP community on parallel scan and FFT, in both non-generic and generic contexts, and related work on functional hardware description languages. The final version the paper should cite and compare with appropriate related work in these domains.

[Added section 5, "Related work". (Somehow, I'd forgotten to write this section.)]

## Suggested but not mandatory

### Review 48A

I've addressed several suggestions as follows, with the change description described in square brackets.

p1, "GHC.Generics" -> "the library GHC.Generics" (at first mention).

[done]

p1, It would be better to refer to Figure 2 at the end of the
previous sentence, when Generic_1 is first mentioned.

[done]

p3, When RList and LList are first 'defined' informally using
the isomorphism notation, the fact that this is not valid
Haskell notation needs to be explicitly mentioned.

["These two types are isomorphic to types assembled from the functor building blocks of Figure 1"]

p3, It would be beneficial to present the instances of Generic_1
for RList and LList side-by-side rather than above-and-below.
This makes it easier to compare them, and also saves space.  The
same comment applies a number of other places in the paper too,
and the author should carefully review all the definitions and
see which could naturally be placed side-by-side.

[Done in several places.]

p4, "Start with" -> "Let us start with".

[Done]

p4, Why is considering branch-labelled trees relevant at this
point, beyond the fact that they are another example of trees?
If they are important for the subsequent development explain
why, otherwise this example could be removed.

[Removed.]

p5, Similarly, why is considering bottom-up trees relevant?

[Added "As we'll see below, they give rise to important versions of parallel scan and FFT."]

p7, "also rename" -> "we also rename".

["let's also rename ..."]

p9, Section 4 seems to assume that the reader already knowns
what parallel scan is.  This needs to be explained here!

[In what's now section 3, second paragraph, immediately after the list of scan applications, "An efficient, \emph{parallel} scan algorithm thus enables each of these applications to be performed in parallel." The first paragraph introduces "prefix sum" and its generalization to "scan".]

p10, "a place to stash" -> "a natural place to store".

[done]

p10, "so, provide" -> "so, we provide".

["To do so, we can simply provide ..."]

p11, Can smaller examples be used in Figures 3 and 4 -- is using
numbers are large as 11 and 5 really necessary here?  The same
comment applies to many of the examples in the paper.

[Perhaps, but I refer to Figure 4 later to illustrate more complex compositions that I think are easier to see with size 16 than size 8.]

p11, "consider two aspect" -> "we consider two aspects".

["consider two aspects of"]

p17, Figure 20 is incomprehensible in both the print and PDF
versions of the paper.  Can a smaller example be used?

[removed.]

p18, Explain what 'powers' means with a simple example, e.g.
for lists we have powers 2 = [2^0, 2^1, 2^2, 2^3, ...]

["... construct powers of a given number $x$ to fill a structure |f|, so that successive elements are $x^0, x^1, x^2$ etc."]

p20, "scan, define" -> "scan, we define".

["scan, we can define"]

p21, In the second para, "the" is missing in a number of places.

[done]

p23, Figure 31 is just a blur of black lines!

[removed.]

### Review 48B

p. 1: From the first sentence, this paper commits to being about Haskell.  So, it would probably be helpful to mention Haskell in the abstract, if not the title.  (Otherwise, non-Haskeller readers will be lured in by the title and abstract and then left in the cold when they start to read the paper.)

[Broadened from "in Haskell" to "in functional languages", and replaced a Haskell-specific citation with a non-Haskell-specific one.]

p. 2: "Even the determinacy of an array-based parallel algorithm can be difficult to ensure or verify."  The paper should explain in what way the generic encoding makes it any easier to ensure or verify determinacy.

[Added "imperative" before "array-based".]

p. 5: "The more general vector-based instance definitions are simpler than even the binary-only versions given above" -- the instance definitions above are ternary-only.

["than even for the binary-only version |Tree| type given above"]

p. 6: "Thanks to promotion..." -- assuming you enable the `DataKinds` GHC extension, right?

["Thanks to promotion (via the |DataKinds| language extension), ..."]

p. 7: "the sum disappears from the representation" -- but clarify that this is at the expense of having to define separate instances for `(RVec Z)` and `(RVec (S n))`.

[added ", and each |Generic1| instance split into two"]

p. 8: "This limit to associativity is exactly why both exist and are useful." -- I didn't understand this; please clarify.

["Functor product and functor composition are both associative only up to isomorphism. While |RVec| and |RPow| are right associations, |LVec| and |LPow| are left associations. As we will see below, different associations, though isomorphic, lead to different algorithms."]

p. 8: "A downside is that we *cannot* provide them..." -- explain why.

["we *cannot* provide them (since instances already exist)"]

p. 9: "Our 'bush' type is adapted from..." -- I didn't understand how this paper's bush type is "adapted" from the Bird and Meertens version; it looks like something entirely different.

[Changed to "is inspired by". My Bush type is a depth-indexed version of the result of moving the values from nodes to leaves in the Bird and Meertens version. I guess it could be called a "perfect leaf bush".]

p. 9: "`RPow f` and `LPow f` form perfectly and recursively balanced products" -- I didn't understand this; product isn't used in the definitions of `RPow` and `LPow`.

["Where |RVec| and |LVec| choose fully right- or left-associated products, |RBin| and |LBin| form perfectly and recursively balanced products (being repeated compositions of |Pair|).
Likewise, functor composition is associative up to isomorphism.
Where |RBin| and |LBin| are fully right- and left-associated compositions, |Bush n| forms balanced compositions."]

p. 9: "our choice of exclusive+total" -- what does "total" mean here?

["the unconventional choice of exclusive+total above" --> "the additional output"]

p. 11: "The higher-order function `first` applies a function to the first element of a pair, carrying the second element along unchanged" -- I read `first` as "extract first element" (the more typical usage) and was briefly confused.  Is there a better name you can use?

[It's a standard operation from the `Arrow` class. I added reference to the paper "Generalising monads to arrows".]

p. 14: "not just three prefix scans over `LVec 4` but also *one additional scan* over `LVec 3`" -- in figure 12, could you somehow highlight the operations that are part of this one additional scan so that they are obvious?  (I think I can see which ones they are, but I'm not completely certain.)  Yes, the figures are automatically generated, but it's worth doing this step of manual post-processing on one figure if it makes the point more clear.

[I added "(highlighted in Figure 12)", though I take it from your comment that the highlighting didn't make the point.]

p. 22: Why don't you show the depth complexity for bushes?

*[Oops! Added, though without closed form solution.]*

The paper should use the proper formatting for an abstract, instead of having the abstract be the first section of the paper body.

[Fixed.]

Throughout the paper, avoid using citations as nouns.

[Fixed.]

p. 5: It looks like there are a few occurrences of `f` in the instance definitions for `(Tree n)` that are supposed to be `h`.

[Fixed.]

p. 8: In the code here (and elsewhere in the paper too), `family` ought to appear in bold since it's a keyword.

[Fixed.]

p. 8: In the second definition of Peano multiplication (corresponding to `LVec`), it looks like there's a typo: it should be `(n + 1) * a = (n * a) + a`, not `(n + 1) * a = (n * a) + n`, right?  (Also, is there any reason that the second definitions of Peano multiplication and exponentiation need to use `n + 1` instead of `1 + n` as the ones above do?)

[Fixed. (Yes; I meant the `n+1` vs `1+n` to suggest `LVec` vs `RVec`.)]

p. 10: "there is simple and general specification" -> "there is a..."

[Fixed.]

p. 11: "two aspect of performance"

[Fixed.]

p. 13: "to get a more parallelism"

[Fixed.]

p. 19: "Figures 25 and 26 shows"

[Fixed.]

p. 19: "which is interpret"

[Fixed.]

p. 22: "Figures 32 and 33 gives"

[Fixed.]

p. 24: "the Sklansky's parallel scan" -> "Sklansky's parallel scan"

[Fixed.]

p. 24: "decimation in frequency" -- missing a closing quote.

[Fixed.]

p. 24: "Which other known scan and FFT algorithms..." -- This sentence should end in a question mark.

[Fixed.]

### Review #48C

* p1, Abstract: the abstract should be in a \begin{abstract}, rather
  than as the first section.

[Fixed.]

* p2, Fig 1: calling Singleton `Par` is a bit mysterious at first.

[I agree. I chose to keep the names from `GHC.Generics`.]

* p5, "f-ary" trees: you ought to mention that this is the free monad
  for the functor `f`.

[Added "(also known as the ``free monad'' for the functor |f|)".]
