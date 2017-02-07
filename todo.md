# To do

*   Sections:
    *   Introduction
    *   Contributions
    *   Conclusions
    *   Related work
    *   Relation to known parallel scan algorithms.
    *   Complexity analysis
*   Consistency of figure captions
*   Fill in the constraint ellipses in FFT definitions.
*   Type-check all definitions with a test module, especially in the early sections on trees.
*   Title:
    *   "Generic functional parallel algorithms: scan and FFT" (current)
    *   "Two generic functional parallel algorithms"
    *   "Generic parallel scan and FFT"
    *   "Generic parallel functional programming: two examples"
*   Check for accidental paragraphs after `\\end{code}`.
*   Misc:
    *   Infinite family of correct algorithms indexed by data type.
        It's easier to select a data type than implement a correct parallel algorithm.
    *   "Parallel-friendly" might be more suitable than "parallel".
    *   Reveals commonality of some algorithms that appear quite different.
        Theme: postpone optimization.
*   Cite "work" and "depth"
*   Maybe rename `LScan` to "`LScannable`".
*   Tweaks for acmart:
    *   Abstract *before* `maketitle` (p 14). Really?
    *   "Thee captions for figures must be entered after the figure bodies, and for the tables before the table bodies." (p 15)

# Done

*   Update from old sigplanconf.cls
*   Stats for scan (as for FFT).
    Don't, since the stats are in the captions.
*   "`RBin`"
*   Something about tries and logarithms, or Naperian functors.
*   Not: mention and cite `DefaultSignatures`.
*   Maybe provide and mention homomorphisms with the function instances as well as the trie (representable functor) connection.
*   Define `Pair` before or at its first use.
*   Remove code comment indentation.
