ReverseDiffSource.jl
====================

_Reverse automated differentiation from an expression or a function_


|Julia release (0.3)  | Julia nightly (0.4)|
|---------------|:-----------:|
|[![ReverseDiffSource](http://pkg.julialang.org/badges/ReverseDiffSource_release.svg)](http://pkg.julialang.org/?pkg=ReverseDiffSource&ver=release)           |  [![ReverseDiffSource](http://pkg.julialang.org/badges/ReverseDiffSource_nightly.svg)](http://pkg.julialang.org/?pkg=ReverseDiffSource&ver=nightly) |

Latest (Julia nightly & release) [![Build Status](https://travis-ci.org/JuliaDiff/ReverseDiffSource.jl.svg?branch=master)](https://travis-ci.org/JuliaDiff/ReverseDiffSource.jl)  

[![Coverage Status](https://coveralls.io/repos/JuliaDiff/ReverseDiffSource.jl/badge.png?branch=master)](https://coveralls.io/r/JuliaDiff/ReverseDiffSource.jl?branch=master)




This package provides a function `rdiff()` that generates valid Julia code for the calculation of derivatives up to any order for a user supplied expression or generic function.

Installation : `Pkg.add("ReverseDiffSource")`

Package documentation and examples can be found [here](http://reversediffsourcejl.readthedocs.org/en/master/index.html).

