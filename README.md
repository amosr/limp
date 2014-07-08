limp
====

Eventually, this will become a pure Haskell library for Linear Integer/Mixed Programming.

So far, however, I only have a program representation.
I think the program representation has some minor advantages over existing ones: we separate Z-valued variables (Int) from R-valued variables (Double), which should make retrieving values a little nicer.
The Linear type for linear functions is also indexed over whether it is purely Z, or mixed.
While linear programming generally only supports less-than-or-equal comparisons, an less-than on Z can be changed to <=+1.
I'm not yet sure whether the added complication on Linear is worth this minor syntactic sugar, though.



To do
===
Implement a Simplex-style algorithm for Canon. This probably won't be as fast as a real matrix-based simplex, but would be easy to implement.
After both are implemented, we can check them against each other on random inputs.

And lots more.
