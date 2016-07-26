# sokoban

**Note:** This version of Sokoban in Haskell is a rewrite of [Johannes
Ahlmann's](https://github.com/codinguncut/Sokoban). The algorithms are basically the
same, but different design decisions have been made in respect to data types and
some minor algorithms in helper functions. Furthermore, the overall architecture
has been moved over to [`stack`](https://haskellstack.org).  Therefore, one can
just run `stack build` and be up and running (speaking of running: `stack exec
sokoban-exe` runs the executable).

Also, the image files from the original repo have been renamed in the
restructuring process.

The original idea came from [Ruby Quiz #5 –
Sokoban](https://rubyquiz.com/quiz5.html). And the recorded live
coding session of the original code can be found at [Haskell
Uncut](https://youtube.com/user/entirelysubjective).
