[AP-Assignment0 - Look at Those Curves](https://github.com/tudorgk/AP-Assignment0)
=======================================

The objective of this assignment is to gain hands-on programming experience with Haskell.

The goal is to implement a library, Curves, for working with piecewise linear curves in the plane (based on an idea by Hansen and Rischel, who in turn got the idea from Maarten M. Fokkinga).

SYNOPSIS
--------

Example of usage:

```haskell
toFile  (hilbert $ hilbert $ hilbert $ hilbert $ curve (point (0,0)) []) "hilbert.svg"
```

DESCRIPTION OF IMPLEMENTATION
-----------------------------

