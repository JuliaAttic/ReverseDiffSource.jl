ReverseDiffSource.jl
====================

_Reverse automated differentiation from an expression or a function_

|Julia 0.3 | Julia 0.4 | Julia 0.5 | master (on nightly + release) | Coverage |
|:--------:|:---------:|:---------:|:-----------------------------:|:-----------:|
|[![ReverseDiffSource](http://pkg.julialang.org/badges/ReverseDiffSource_0.3.svg)](http://pkg.julialang.org/?pkg=ReverseDiffSource&ver=0.3) | [![ReverseDiffSource](http://pkg.julialang.org/badges/ReverseDiffSource_0.4.svg)](http://pkg.julialang.org/?pkg=ReverseDiffSource&ver=0.4) | [![ReverseDiffSource](http://pkg.julialang.org/badges/ReverseDiffSource_0.5.svg)](http://pkg.julialang.org/?pkg=ReverseDiffSource&ver=0.5) | [![Build Status](https://travis-ci.org/JuliaDiff/ReverseDiffSource.jl.svg?branch=master)](https://travis-ci.org/JuliaDiff/ReverseDiffSource.jl) | [![Coverage Status](https://coveralls.io/repos/JuliaDiff/ReverseDiffSource.jl/badge.png?branch=master)](https://coveralls.io/r/JuliaDiff/ReverseDiffSource.jl?branch=master) |

This package provides a function `rdiff()` that generates valid Julia code for the calculation of derivatives up to any order for a user supplied expression or generic function. Install with `Pkg.add("ReverseDiffSource")`. Package documentation and examples can be found [here](http://reversediffsourcejl.readthedocs.org/en/master/index.html).

This version of automated differentiation operates at the source level (provided either in an expression or a generic function) to output Julia code calculating the derivatives (in a expression or a function respectively). Compared to other automated differentiation methods it does not rely on method overloading or new types and should, in principle, produce fast code.

Usage examples:
- derivative of x³
```
    julia> rdiff( :(x^3) , x=2.)  # 'x=2.' indicates the type of x to rdiff
    :(begin
        (x^3,3 * x^2.0)  # expression calculates a tuple of (value, derivate)
        end)
```

- first 10 derivatives of `sin(x)`  (notice the simplifications)
```
    julia> rdiff( :(sin(x)) , order=10, x=2.)  # derivatives up to order 10
    :(begin
            _tmp1 = sin(x)
            _tmp2 = cos(x)
            _tmp3 = -_tmp1
            _tmp4 = -_tmp2
            _tmp5 = -_tmp3
            (_tmp1,_tmp2,_tmp3,_tmp4,_tmp5,_tmp2,_tmp3,_tmp4,_tmp5,_tmp2,_tmp3)
        end)
```

- works on functions too
```
	julia> rosenbrock(x) = (1 - x[1])^2 + 100(x[2] - x[1]^2)^2   # function to be derived
	julia> rosen2 = rdiff(rosenbrock, (ones(2),), order=2)       # orders up to 2
		(anonymous function)
```

- gradient calculation of a 3 hidden layer neural network for backpropagation
```
    # w1-w3 are the hidden layer weight matrices, x1 the input vector
    function ann(w1, w2, w3, x1)
        x2 = w1 * x1
        x2 = log(1. + exp(x2))   # soft RELU unit
        x3 = w2 * x2
        x3 = log(1. + exp(x3))   # soft RELU unit
        x4 = w3 * x3
        1. / (1. + exp(-x4[1]))  # sigmoid output
    end

    w1, w2, w3 = randn(10,10), randn(10,10), randn(1,10)
    x1 = randn(10)
    dann = rdiff(ann, (w1, w2, w3, x1))
    dann(w1, w2, w3, x1) # network output + gradient on w1, w2, w3 and x1
```

