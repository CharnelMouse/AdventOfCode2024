Solutions for problems in Advent of Code 2024: https://adventofcode.com/2024

Goals:

If using R, everything in base R, i.e. no loading of packages; Possible exception: any day that requires handling numbers too large for base R, e.g. gmp for modular arithmetic with integers too large for base R's %/% and %%. No regex.
If using Factor, only included libraries, but there are a lot of them.

To load the Factor code, navigate to the project folder, and type the following:

```factor
cwd "/Factor" append add-vocab-root
cwd set-current-directory
USE: AOC2024
```
