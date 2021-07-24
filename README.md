# pretty-egg

This is an experiment in implementing a pretty printer using egg. In the current implementation, egg provides memoization and extraction, which would be annoying to implement otherwise.

This experiment is inspired by two related pretty printing papers:
- "Polynomial-Time Optimal Pretty-Printing Combinators with Choice"
- "A Pretty But Not Greedy Printer (Functional Pearl)"

They roughly implement the same interface, which was first described in "Optimal Pretty-Printing Combinators" (which I have not read!). The first paper uses "bottom-up tree rewriting and dynamic programming" to flesh out their theory (but don't use it in their implementation). The second paper describes a slightly simpler interface, has better exposition overall, but does not talk about tree automata.

Questions:
- Is the runtime performance acceptable?
- What is the runtime complexity?
- Are there egg rewrites that could speed up the implementation?
- Is it better to implement the layout dimension summarization as an extractor (like it is now) or as an analysis?
- Can Knuth-Plass be written in a similar style?
