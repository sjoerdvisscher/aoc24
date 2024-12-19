Bindings to the Coin/CBC integer linear programming solver
========

This package provides bindings to the CBC library.
These bindings work for simple programs, but might not be suitable for production use: see issues #7 and #8.

In some cases, you may be better off pretty-printing the program with Numeric.Limp.Canon.Pretty in the limp package, writing the resulting program to file, and passing it to the external program.
I hope to update this library to solve via the external program soon.

