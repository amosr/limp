limp
====

This package provides two representations for linear programs: "Numeric.Limp.Program", which is what I expect end-users to use, and
"Numeric.Limp.Canon", which is simpler, but would be less nice for writing linear programs.
You can convert programs from the Program representation to the Canon representation using "Numeric.Limp.Canon.Convert", and then pretty-print the program using "Numeric.Limp.Canon.Pretty".

There is a very simple branch-and-bound solver in "Numeric.Limp.Solve.Branch.Simple", and a simplex solver for relaxed (real only) programs in "Numeric.Limp.Solve.Simplex.Maps".
See the limp-cbc package for a simple external solver.

