Dear author,

The 22nd ACM SIGPLAN International Conference on Functional Programming
(ICFP 2017) program committee is delighted to inform you that your paper
#48 has been conditionally accepted to appear in the conference.

       Title: Generic functional parallel algorithms: Scan and FFT
     Authors: Conal Elliott (Target)
  Paper site: https://icfp17.hotcrp.com/paper/48?cap=048aEwX0Qp2Apag

Your paper was one of 44 accepted out of 126 submissions. Congratulations!

Reviews and comments on your paper are appended to this email. These
materials will also be available from the submissions site.

Your paper has been conditionally accepted with the expectation that you
will revise it to address the specific points that are listed in the
"MANDATORY REVISIONS" comment that is attached here and also available on
the paper site.  Further information about the process for submitting the
final version of your paper will be provided shortly.

Please contact the program chair, Mark Jones <mpj@pdx.edu>, if you have any
questions or concerns.

- ICFP 2017 Submissions

===========================================================================
                           ICFP 2017 Review #48A
                    Updated 29 Apr 2017 11:24:38am EDT
---------------------------------------------------------------------------
      Paper #48: Generic functional parallel algorithms: Scan and FFT
---------------------------------------------------------------------------

                      Overall merit: B. OK paper, but I will not champion
                                        it
                         Confidence: X. I am an expert in this area

                         ===== Paper summary =====

This paper shows how generic programming techniques can be used
to develop generic versions of two well-known parallel algorithms:
parallel scan and fast fourier transform.  Abstracting from the
usual concrete formulation in terms of arrays reveals a number
of interesting connections that might otherwise be missed.

                      ===== Comments to authors =====

General comments:

The standard of writing in the paper is excellent, and the
topic is interesting and relevant for ICFP.  The level of
technical sophistication in the paper is high, involving
lots of detailed, abstract code that makes it fairly difficult
going even for an expert reader.  On balance, I enjoyed the
paper and think that others would too.  However, I have
three concerns with the paper as it currently stands:

* First of all, and most significantly, it fails to place
  its contributions properly in context.  In particular,
  there is a large body of previous work in the FP community
  on parallel scan and FFT, in both non-generic and generic
  contexts, but almost none of this is referenced or compared
  with.  There is also many decades of relevant work on
  functional hardware description languages, none of which
  is mentioned.  A proper comparison with related work is
  required befor the paper is suitable for publication.

* It is rather code heavy, and gives the impression that the
  text was written around the code.  Many of the definitions
  are straightforward based on the types involved, and indeed,
  I would expect that many could be derived automatically.  It
  would be beneficial to consider omitting some more of the
  details (with the full code being provided as supplimentary
  material) to improve the readability of the paper.

* Similarly, it is rather figure heavy (31 in total).  The
  figures were automatically generated from the Haskell code,
  so it was perhaps too easy to include lots of them.  Are
  they all strictly necessary?  It would be benefical to be
  more economical with the use of figures, or to reduce the
  size of the examples used, if this is possible.

If these concerns were properly addressed (along with the
more specific comments detailed below), the paper would
make a good contribution for the ICFP conference.

Specific comments:

p1, The abstract is too long, and should be compressed.  Much of
the initial text on generic programming can be removed.

p1, "GHC.Generics" -> "the library GHC.Generics" (at first mention).
[done]

p1, Some additional remarks are required for the manner in which
recursion is treated.  The statement that "There are additional
definitions that capture recursion ...., but the collection in
Figure 1 suffices for this paper" is not sufficient.

p1, It would be better to refer to Figure 2 at the end of the
previous sentence, when Generic_1 is first mentioned.
[done]

p2, The first paragraph on this page is too long, and should be
compressed or split up.  There are many places in this paragraph
where it would be appropriate to add some references.

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

p4, Section 3.2 on top-down trees could be presented much more
concisely, as this is standard generic progamming material.

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

p9, Six pages of preliminaries in section 3 is rather a lot
before getting to the meat of the paper in section 4.  It would
be beneficial to compress this section to at most four pages.

p9, Introducing prefix sum and prefix scan in their general
version obscures the basic ideas, which are very simple.  It
would be preferable to give examples, such as

   prefixsum [a,b,c,d] = [0, a, a+b, a+b+c, a+b+c+d]

from which it is then easy to define the general case.

p9, Section 4 seems to assume that the reader already knowns
what parallel scan is.  This needs to be explained here!
[clarified.]

p10, "a place to stash" -> "a natural place to store".
[done]

p10, "so, provide" -> "so, we provide".
["To do so, we can simply provide ..."]

p11, Can smaller examples be used in Figures 3 and 4 -- is using
numbers are large as 11 and 5 really necessary here?  The same
comment applies to many of the examples in the paper.

p11, "consider two aspect" -> "we consider two aspects".
["aspects"]

p15, Is section 4.4 really necessary for the published version
of the paper?  Sections 4.2 and 4.3 are already very technical,
so it would be good to omit 4.4 if it is not so important.

p17, Figure 20 is incomprehensible in both the print and PDF
versions of the paper.  Can a smaller example be used?
[removed.]

p18, Explain what 'powers' means with a simple example, e.g.
for lists we have powers 2 = [2^0, 2^1, 2^2, 2^3, ...]
[done]

p18, Clarify where 'automatic CSE' performed - by GHC?

p19, The idea of factoring types rather than numbers is very nice!
[Thanks.]

p20, "scan, define" -> "scan, we define".
["scan, we can define"]

p21, In the second para, "the" is missing in a number of places.
[done]

p23, Figure 31 is just a blur of black lines!
[removed.]

===========================================================================
                           ICFP 2017 Review #48B
---------------------------------------------------------------------------
      Paper #48: Generic functional parallel algorithms: Scan and FFT
---------------------------------------------------------------------------

                      Overall merit: A. Good paper, I will champion it
                         Confidence: Y. I am knowledgeable in this area,
                                        but not an expert

                         ===== Paper summary =====

This paper advocates for parallel programming with data types built from a collection of composable generic building blocks, rather than arrays.  If we express algorithms such as parallel reduction in terms of data structures such as trees rather than in terms of arrays, it is easier to encode essential restrictions and invariants in the type system; in this paper, "trees" gets generalized to any data structure that can be built out of sums, products, and compositions of functors, and the algorithms that operate on these generic data structures can also be defined compositionally out of instances defined for each building block.  Furthermore, the performance characteristics of the algorithms (according to a Blelloch-style work-depth model) can also be determined compositionally using the same building blocks.  The paper uses this approach to define, and characterize the performance of, two families of parallel algorithms, prefix scan and FFT, demonstrates how the resulting ge
 neric algorithms subsume various traditional array-based formulations, as well as suggesting new implementation strategies.

                      ===== Comments to authors =====

I enjoyed reading this paper and learned a lot from it.  The paper has a nice visual language for illustrating the parallel work and depth of the algorithms (and the automatically generated figures are cool); it was educational to see the difference between, for instance, `Pair . LVec 8` versus `LVec 8 . Pair`.

In principle, the approach in this paper makes it easier to explore the space of correct-by-construction parallel algorithms for a given problem.  It's hard to say how useful this approach is for evaluating parallel performance in practice, since things like cache effects, constant factors, and the presence or absence of instruction-level parallelism all matter a lot.  Even so, though, the approach in the paper is of independent mathematical and pedagogical interest.  And it would be interesting to find out if, for instance, an implementation of FFT using the `Bush` formulation in the paper actually does provide a performance improvement over the traditional top-down and bottom-up FFT approaches.  I look forward to follow-up work on this.

I support acceptance of the paper, but have a number of presentational suggestions that the authors may wish to take into account for a revision:

  - p. 1: From the first sentence, this paper commits to being about Haskell.  So, it would probably be helpful to mention Haskell in the abstract, if not the title.  (Otherwise, non-Haskeller readers will be lured in by the title and abstract and then left in the cold when they start to read the paper.)
[Broadened to functional languages.]
  - p. 2: "Even the determinacy of an array-based parallel algorithm can be difficult to ensure or verify."  The paper should explain in what way the generic encoding makes it any easier to ensure or verify determinacy.
[Added "imperative" before "array-based".]
  - p. 5: "The more general vector-based instance definitions are simpler than even the binary-only versions given above" -- the instance definitions above are ternary-only.
["than even for the binary-only version |Tree| type given above"]
  - p. 5: A picture might help to visualize bottom-up trees as opposed to top-down ones.
  - p. 6: "Thanks to promotion..." -- assuming you enable the `DataKinds` GHC extension, right?
[clarified]
  - p. 7: "the sum disappears from the representation" -- but clarify that this is at the expense of having to define separate instances for `(RVec Z)` and `(RVec (S n))`.
[added ", and each |Generic1| instance split into two"]
  - p. 7: "A 'perfect' leaf tree is..." -- maybe this definition should go at the first point where the paper uses the term.
  - p. 8: "This limit to associativity is exactly why both exist and are useful." -- I didn't understand this; please clarify.
["Functor product and functor composition are both associative only up to isomorphism. While |RVec| and |RPow| are right associations, |LVec| and |LPow| are left associations. As we will see below, different associations, though isomorphic, lead to different algorithms."]
  - p. 8: "A downside is that we *cannot* provide them..." -- explain why.
["we *cannot* provide them (since instances already exist)"]
  - p. 9: "Our 'bush' type is adapted from..." -- I didn't understand how this paper's bush type is "adapted" from the Bird and Meertens version; it looks like something entirely different.
[Changed to "is inspired by". My Bush type is a depth-indexed version of the result of moving the values from nodes to leaves in the Bird and Meertens version. I guess it could be called a "perfect leaf bush".]
  - p. 9: "`RPow f` and `LPow f` form perfectly and recursively balanced products" -- I didn't understand this; product isn't used in the definitions of `RPow` and `LPow`.
["Where |RVec| and |LVec| choose fully right- or left-associated products, |RBin| and |LBin| form perfectly and recursively balanced products (being repeated compositions of |Pair|).
Likewise, functor composition is associative up to isomorphism.
Where |RBin| and |LBin| are fully right- and left-associated compositions, |Bush n| forms balanced compositions."]
  - p. 9: "our choice of exclusive+total" -- what does "total" mean here?
["the unconventional choice of exclusive+total above" --> "the additional output"]
  - p. 11: "The higher-order function `first` applies a function to the first element of a pair, carrying the second element along unchanged" -- I read `first` as "extract first element" (the more typical usage) and was briefly confused.  Is there a better name you can use?
[It's a standard operation from the `Arrow` class. I added reference to the paper "Generalising monads to arrows".]
  - p. 14: "not just three prefix scans over `LVec 4` but also *one additional scan* over `LVec 3`" -- in figure 12, could you somehow highlight the operations that are part of this one additional scan so that they are obvious?  (I think I can see which ones they are, but I'm not completely certain.)  Yes, the figures are automatically generated, but it's worth doing this step of manual post-processing on one figure if it makes the point more clear.
[I added "(highlighted in Figure 12)", though I take it from your comment that the highlighting didn't make the point.]
  - p. 22: Why don't you show the depth complexity for bushes?
*[Oops! Added, though without closed form solution.]*

Typos/grammar/formatting:

  - The paper should use the proper formatting for an abstract, instead of having the abstract be the first section of the paper body.
[Fixed.]
  - Throughout the paper, avoid using citations as nouns.
[Fixed.]
  - It's pretty much impossible to read the tiny "In" and "Out" labels in figures 3-16.
  - p. 5: It looks like there are a few occurrences of `f` in the instance definitions for `(Tree n)` that are supposed to be `h`.
[Fixed.]
  - p. 8: In the code here (and elsewhere in the paper too), `family` ought to appear in bold since it's a keyword.
[Fixed.]
  - p. 8: In the second definition of Peano multiplication (corresponding to `LVec`), it looks like there's a typo: it should be `(n + 1) * a = (n * a) + a`, not `(n + 1) * a = (n * a) + n`, right?  (Also, is there any reason that the second definitions of Peano multiplication and exponentiation need to use `n + 1` instead of `1 + n` as the ones above do?)
[Fixed. (Yes; I meant the `n+1` vs `1+n` to suggest `LVec` vs `RVec`.)]
  - p. 10: "there is simple and general specification" -> "there is a..."
[Fixed.]
  - p. 11: "two aspect of performance"
[Fixed.]
  - p. 13: "to get a more parallelism"
[Fixed.]
  - p. 19: "Figures 25 and 26 shows"
[Fixed.]
  - p. 19: "which is interpret"
[Fixed.]
  - p. 22: "Figures 32 and 33 gives"
[Fixed.]
  - p. 24: "the Sklansky's parallel scan" -> "Sklansky's parallel scan"
[Fixed.]
  - p. 24: "decimation in frequency" -- missing a closing quote.
[Fixed.]
  - p. 24: "Which other known scan and FFT algorithms..." -- This sentence should end in a question mark.
[Fixed.]

===========================================================================
                           ICFP 2017 Review #48C
---------------------------------------------------------------------------
      Paper #48: Generic functional parallel algorithms: Scan and FFT
---------------------------------------------------------------------------

                      Overall merit: A. Good paper, I will champion it
                         Confidence: X. I am an expert in this area

                         ===== Paper summary =====

This paper shows how generic data structures can be combined to give
algorithms for generalised prefix sum and for FFT.

Section 3 devotes itself to introducing the generic machinery needed
for what is to come. This is an overview of many of the techniques
that are well-known in the literature.

Sections 4 and 5 are the main novel contribution of the paper, where
prefix sum and FFT are discussed.

On the whole the paper is excellently written, and concepts are
carefully introduced before they are used.

I recommend acceptance.

                      ===== Comments to authors =====

Minor comments
--------------

* p1, Abstract: the abstract should be in a \begin{abstract}, rather
  than as the first section.
[Fixed.]

* p2, Fig 1: calling Singleton `Par` is a bit mysterious at first.
[I agree. I chose to keep the names from `GHC.Generics`.]

* p5, "f-ary" trees: you ought to mention that this is the free monad
  for the functor `f`.
[Added "(also known as the ``free monad'' for the functor |f|)".]

===========================================================================
                           ICFP 2017 Review #48D
---------------------------------------------------------------------------
      Paper #48: Generic functional parallel algorithms: Scan and FFT
---------------------------------------------------------------------------

                      Overall merit: A. Good paper, I will champion it
                         Confidence: X. I am an expert in this area

                      ===== Comments to authors =====

Some comments that might be helpful to the authors:

(1) The implementation of scan for composition is nice, but its description could/should be liberated from the Haskellisms and generic programming framework that you build.  In the introduction, you could get the general idea across by showing the special case of  scan for Sequence (Sequence a), assuming a scan for Sequence a (where Sequence is some collection type with a map that the reader can think of as parallel-friendly lists).

(2) The relationship to the Slansky/Ladner algorithms (sometimes these are called "divide and conquer" (for the one with two recursive calls) vs "contraction" (for the one that adds pairwise first)) is a nice observation -- I like the idea that these really come from different data layouts.

(3) While the contribution of identifying what the abstract code and work/depth for these scans are is significant, I think that in practice these operations are not efficiently implemented just by giving this spec-level code.  Have you thought about implementation at all?  If not, it would be good to be clearer about this upfront.


===========================================================================
                                  Comment
      Paper #48: Generic functional parallel algorithms: Scan and FFT
---------------------------------------------------------------------------
# CONDITIONALLY ACCEPTED PAPER - MANDATORY REVISIONS

## General Information

Your paper has been "Conditionally Accepted" with the expectation that
you will revise your paper in line with the specific changes listed
below.  Of course, you are also encouraged to make other changes to your
paper that have been suggested by the reviews and comments for your
paper on the icfp17.hotcrp.com site.  You are also expected to ensure
that the revised version of your paper meets the formatting requirements
that were set out in the call for papers, including both the document
style and the specific form and use of citations specified there.  These
requirements are necessary for inclusion in PACM PL.

As indicated in the call for papers, the deadline for completing and
submitting the revised version of your paper is June 5.  A short, final
round of reviewing will be used to determine whether the mandatory
revisions have been adequately addressed, and this will determine the
final Accept or Reject status of the paper.  The intent and expectation
is that the mandatory revisions can be addressed within the five week
period between notification on May 1 and submission on June 5, and that
conditionally accepted papers will, in general, be accepted in the
second phase.

Your revised submission should clearly document how the mandatory
revisions have been addressed.  To that end, it must be accompanied by a
cover letter mapping each mandatory revision request to specific parts
of the paper.  The cover letter should facilitate a quick second review,
allowing for confirmation of final acceptance within two weeks.
Conversely, the absence of a cover letter will be grounds for the
paper's rejection.

Further information about the process for submitting the final version
of your paper will be provided shortly.  Please contact the program
chair, Mark Jones <mpj@pdx.edu>, if you have any questions about this
process.

## List of Mandatory Revisions for Paper #48

* Please ensure that you are using the correct document style, including
  author-year citations with correct usage as described in the call for
  papers.  These requirements are necessary for inclusion in PACM PL.
[Done.]

* There is a considerable body of previous work in the FP community on parallel scan and FFT, in both non-generic and generic contexts, and related work on functional hardware description languages. The final version the paper should cite and compare with appropriate related work in these domains.
