# To do

*   Sections:
    *   Introduction
    *   Conclusions
    *   Related work
    *   Relation to known parallel scan algorithms.
    *   Complexity analysis
*   Stats for scan (as for FFT)
*   Type-check all definitions with a test module, especially in the early sections on trees.
*   Title:
    *   "Two generic functional parallel algorithms" (current)
    *   "Generic parallel scan and FFT"
    *   "Generic parallel functional programming: two examples"
*   Update from old sigplanconf.cls
*   Check for accidental paragraphs after `\\end{code}`.
*   Misc:
    *   Infinite family of correct algorithms indexed by data type.
        It's easier to select a data type than implement a correct parallel algorithm.
    *   "Parallel-friendly" might be more suitable than "parallel".
    *   Reveals commonality of some algorithms that appear quite different.
        Theme: postpone optimization.

# Done

*   Something about tries and logarithms, or Naperian functors.
*   Not: mention and cite `DefaultSignatures`.
*   Maybe provide and mention homomorphisms with the function instances as well as the trie (representable functor) connection.
*   Define `Pair` before or at its first use.
*   Define "work" and "depth" at their first use.
*   Remove code comment indentation.
*   Maybe rename `LScan` to "`LScannable`".
    No. Too wide.
