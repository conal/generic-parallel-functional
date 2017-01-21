# To do

*   Type-check all definitions with a test module, especially in the early sections on trees.
*   Title:
    *   "Two generic functional parallel algorithms" (current)
    *   "Generic parallel scan and FFT"
*   Update from old sigplanconf.cls
*   Check for accidental paragraphs after `\\end{code}`.
*   Misc:
    *   Infinite family of correct algorithms indexed by data type.
        It's easier to select a data type than implement a correct parallel algorithm.
    *   "Parallel-friendly" might be more suitable than "parallel".
    *   Reveals commonality of some algorithms that appear quite different.
        Theme: postpone optimization.

# Done

*   Define `Pair` before or at its first use.
*   Define "work" and "depth" at their first use.
*   Remove code comment indentation.
*   Maybe rename `LScan` to ``|LScannable|''.
    No. Too wide.
