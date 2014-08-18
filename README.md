<<<<<<< HEAD
ReverseDiffSource.jl
====================

_Expression based Reverse automated differentiation_


!! Package under development, expect some API changes !!


### Main function exposed by the package : reversediff

`reversediff( ex::Expression, outsym::Symbol, skipgradient=false, evalmod=Main; init...)`

- `ex` is the expression containing the code to derive
- `outsym` is the symbol of the variable (should be a real), within `ex`, that contains the result fo code from which derivation is needed
- `skipgradient` (optional), if set to `true` the code is parsed but no derivation logic is produced
- `evalmod` (optional) indicates in which environment the code should be evaluated, useful if called from another module. Do note that external references (variables that are not parameters but are present in `ex`) are looked for in the Main module, not in `evalmod`.
- `init` are keyword arguments that both 1) indicate which variables appearing in `ex` are the variables to derive `outsym` against, and 2) specify initial values for these variables. Initial values allow the evaluation of the code (needed to fetch the correct derivation rule for each function). Allowed types for parameters are Float64, Vector{Float64} and Matrix{Float64}.

Output = a 3-tuple : 
- an expression that contains code that can be evaluated once and does not depend on the value of variables (useful if you want to create a function that minimizes allocations and improves performance).
- a second expression containing the actual calculations of both `outsym` and its gradient for all specified variables in `init`
- the symbol of the variable in the second expression containing the result of `outsym`. This is necessary because the current reverse AD process sometimes changes the variable name. All the gradient parts are allways the symbol in the `init` keyword args prefixed with `d...`


#### Examples 

```julia
ex = :( res = x + y )

ex1, ex2, outsym = reversediff(ex, :res, x=1.0, y=2.0)

ex2
# quote 
#     res = +(x,y)
#     dy = 0.0
#     dres = 1.0
#     dx = 0.0
#     dx += dres
#     dy += dres
# end
```

As you can see, that's pretty unoptimized code, but it works !

Another one, with a vector parameter
```julia
ex = :( res = x[1]^2 + (x[2]-2x[3])^4 )

ex1, ex2, outsym = AutoDiff.reversediff(ex, :res, x=zeros(3))   # x is vector
ex2
# quote 
#     tmp_1 = x[1]
#     tmp_2 = ^(tmp_1,2)
#     tmp_3 = x[2]
#     tmp_4 = x[3]
#     tmp_5 = *(2,tmp_4)
#     tmp_6 = -(tmp_3,tmp_5)
#     tmp_7 = ^(tmp_6,4)
#     res = +(tmp_2,tmp_7)
#     dtmp_5 = 0.0
#     dtmp_4 = 0.0
#     dtmp_3 = 0.0
#     dtmp_2 = 0.0
#     dres = 1.0
#     dtmp_1 = 0.0
#     dtmp_7 = 0.0
#     fill!(dx,0.0)
#     dtmp_6 = 0.0
#     dtmp_2 += dres
#     dtmp_7 += dres
#     dtmp_6 += *(4,^(tmp_6,-(4,1)),dtmp_7)
#     dtmp_3 += dtmp_6
#     dtmp_5 -= dtmp_6
#     dtmp_4 += *(2,dtmp_5)
#     dx[3] = dtmp_4
#     dx[2] = dtmp_3
#     dtmp_1 += *(2,^(tmp_1,-(2,1)),dtmp_2)
#     dx[1] = dtmp_1
# end
```

### What if your function is unknown to ReverseDiffSource ?
You can define your own derivation rules by calling the `@deriv_rule` macro :

`@deriv_rule(sig::Expr, var::Symbol, rule::Expr)`

- `sig` the function signature, allows you to specify rules that depend on the parameter types
- `var` the symbol indicating which of the variables in the signature is the one you're giving the rule of derivation
- `rule` the code, with the symbol `ds` reserved for indicating how to use the gradient accumulator of the function result to calculate the gradient accumulator of `var`


### Examples

```julia
foo(x) = sin(x) * exp(-x*x) # your function

# tell reversediff() how to handle foo when deriving against x
@deriv_rule  foo(x)     x        dx += ( cos(x)*exp(-x*x) - 2x * sin(x) * exp(-x*x) ) * ds
```

You are now able to use `foo()` in your code :
```julia

ex = quote
	y = foo(x)
	z = sin(y)
	res = log(z)
end

ex1, ex2, outsym = reversediff(ex, :res, x=1.0)
ex2
# quote 
#     y = foo(x)
#     z = sin(y)
#     res = log(z)
#     dz = 0.0
#     dy = 0.0
#     dres = 1.0
#     dx = 0.0
#     dz += /(dres,z)
#     dy += *(cos(y),dz)
#     dx += *(-(*(cos(x),exp(*(-(x),x))),*(*(2,x),sin(x),exp(*(-(x),x)))),dy)
# end
```

You can also build your function using eval : 
```julia
@eval function bar(x)
		$ex2
		(res, dx)
	end

	
bar(1.0)
#(-1.1886262943182573,-1.314252857209243)

# check that d(bar)/dx is correct
[ bar(1.0)[2]  (bar(1.001)[1]-bar(1.)[1]) / 0.001 ]
```



## Target goals 

- I am trying a different underlying representation of the model, going from the current AST manipulation to a custom graph type. This should make the code easier to debug and improve. This is where most of my efforts are directed rigth now.
- I want to add for loops, comprehension would be next. This has become necessary as the differentiation rules definitions are now parsed to graphs too and they make use of loops.
- Higher order derivatives are also a goal, at least up to 2. 
=======
ReverseDiffSource.jl
====================

[![Build Status](https://travis-ci.org/fredo-dedup/ReverseDiffSource.jl.png)](https://travis-ci.org/fredo-dedup/ReverseDiffSource.jl)

Reverse automated differentiation from source


This package provides a function `rdiff()` that generates valid Julia code for the calculation of derivatives up to any order for a user supplied expression or generic function.

Package documentation and examples can be found [here](http://reversediffsourcejl.readthedocs.org/en/master/index.html).
>>>>>>> devl
