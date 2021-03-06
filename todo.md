# To do

# Don't

*   Scan-based addition.
*   Maybe rename `LScan` to "`LScannable`".
*   Perhaps [submit as a Pearl](http://icfp17.sigplan.org/track/icfp-2017-papers#Call-for-Papers) instead.
*   Use "Figure" instead of "Fig." in figure captions.

# Done


*   Figure 10's caption is too wide for the new format, so I shrank the font.
    Better option?
*   Relate to Gabi Keller's work if I hear back from her.
*   Comments in side-by-side code.
*   FFT depth for `Bush n`.
*   Related work
    *   "There is a considerable body of previous work in the FP community on parallel scan and FFT, in both non-generic and generic contexts, and related work on functional hardware description languages. The final version the paper should cite and compare with appropriate related work in these domains."
    *   Parallel scan: waiting to hear back from Gabi Keller.
    *   FFT: good, I think. Maybe:
        *   [*Matrix algebra and applicative programming*](https://www.cs.indiana.edu/cgi-bin/techreports/TRNNN.cgi?trnum=TR222)
    *   Functional hardware description languages.
*   How I might shorten the conference submission:
    *   Side-by-side code.
        Would save a lot of vertical space.
    *   Tighten prose
    *   Drop:
        *   Explicit `RList`/`LList` isomorphism.
        *   Zipper discussion.
*   Tweaks for acmart:
    *   Abstract *before* `maketitle` (p 14). Really?
    *   "Thee captions for figures must be entered after the figure bodies, and for the tables before the table bodies." (p 15)
*   Check for accidental paragraphs after `\\end{code}`.
*   Related work:
    *   FFT in muFP, Ruby, and Lava.
    *   See "The study of butterflies" by Geraint Jones and Mary Sheeran, 1990.
*   Sections:
    *   Introduction
    *   Contributions
    *   Conclusions
    *   Relation to known parallel scan algorithms. [done]
    *   Complexity analysis. [done as much as I know how]
*   Fill in the constraint ellipses in FFT definitions.
*   Type-check all definitions with a test module, especially in the early sections on trees.
*   Title:
    *   "Generic functional parallel algorithms: scan and FFT" (current)
    *   "Two generic functional parallel algorithms"
    *   "Generic parallel scan and FFT"
    *   "Generic parallel functional programming: two examples"
*   Check for accidental paragraphs after `\\end{code}`.
    Same for the "Compiling to categories" paper.
*   Misc:
    *   Infinite family of correct algorithms indexed by data type.
        It's easier to select a data type than implement a correct parallel algorithm.
    *   "Parallel-friendly" might be more suitable than "parallel".
    *   Reveals commonality of some algorithms that appear quite different.
        Theme: postpone optimization.
*   Point to "Compiling to categories" as an explanation of the figures in the paper.
    Mention that the compiler generates not only figures but synthesizable Verilog.
*   Consistency of figure captions
*   Cite "work" and "depth"
*   Update from old sigplanconf.cls
*   Stats for scan (as for FFT).
    Don't, since the stats are in the captions.
*   "`RBin`"
*   Something about tries and logarithms, or Naperian functors.
*   Not: mention and cite `DefaultSignatures`.
*   Maybe provide and mention homomorphisms with the function instances as well as the trie (representable functor) connection.
*   Define `Pair` before or at its first use.
*   Remove code comment indentation.
