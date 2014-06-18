Calling ``rdiff()`` with a function
***********************************

The differentiation function ``rdiff()`` can also be called with a generic function instead of an Expression::

	rdiff( func::Function, init::Tuple; order::Int)
 
Arguments
^^^^^^^^^

:func: is a Julia generic function.

:init: is a tuple containing initial values for each parameter of ``func``.

:order: (keyword arg, default = 1) is an integer indicating the derivation order (1 for 1st order, etc.). Order 0 is allowed and will produce a function that is a processed version of ``ex`` with some variables names rewritten and possibly some optimizations.

Output
^^^^^^

A function, evaluated in the same module that ``func`` is from and returning a tuple containing the expression value and the derivative at first, second , etc.. order.


Usage
^^^^^

``rdiff`` takes a function defined with the same subset of Julia statements ( assigments, getindex, setindex!, for loops, function calls ) as the Expression variant of ``rdiff()`` and transforms it into another function whose call will return the derivatives at all orders between 0 and the order specified. 

The generated function will attempt to remove all uneeded calculations (e.g.  x + 0) and factorize repeated function calls as much as possible.

All the variables appearing in the init argument are considered as the expression's arguments and a derivative is calculated for it (and cross derivatives if order is >= 2). 

For orders >= 2 *only a single variable, of type Real or Vector, is allowed*. For orders 0 and 1 variables can be of type Real, Vector or Matrix and can be in an unlimited number::

	julia> rosenbrock(x) = (1 - x[1])^2 + 100(x[2] - x[1]^2)^2   # function to be derived
	julia> rosen2 = rdiff(rosenbrock, (ones(2),), order=2)       # orders up to 2
		(anonymous function)
	julia> rosen2([1,2])
		(100,[-400.0,200.0],
		2x2 Array{Float64,2}:
		  402.0  -400.0
		 -400.0   200.0)

``rdiff`` runs several simplification heuristics on the generated code to remove neutral statements and factorize repeated calculations. For instance calculating the derivatives of ``sin(x)`` for large orders will reduce to the calculations of ``sin(x)`` and ``cos(x)``::

	julia> rdiff( :(sin(x)) , order=10, x=2.)  # derivatives up to order 10
	:(begin 
	        _tmp1 = sin(x)
	        _tmp2 = cos(x)
	        _tmp3 = -_tmp1
	        _tmp4 = -_tmp2
	        _tmp5 = -_tmp3
	        (_tmp1,_tmp2,_tmp3,_tmp4,_tmp5,_tmp2,_tmp3,_tmp4,_tmp5,_tmp2,_tmp3)
	    end)

When a second derivative expression is needed, only a single derivation variable is allowed. If you are dealing with a function of several (scalar) variables you will have you aggregate them into a vector::

	julia> rosenbrock(x) = (1 - x[1])^2 + 100(x[2] - x[1]^2)^2   # function to be derived
	julia> rosen2 = rdiff(rosenbrock, (ones(2),), order=2)       # orders up to 2
		(anonymous function)
	julia> rosen2([1,2])
		(100,[-400.0,200.0],
		2x2 Array{Float64,2}:
		  402.0  -400.0
		 -400.0   200.0)

``rosen2(x)`` returns a tuple containing respectively the value of the expression at ``x``, the gradient (a 2-vector) and the hessian (a 2x2 matrix)

Limitations
^^^^^^^^^^^

* When determining the influence of a variable on a several ``setindex!`` on the same variable, for example ``a[2:3] = x ; a[3:4] = 2x``, the algorithm will overstate the influence of ``x`` if there is an overlap on the indices, ``a[3]`` in this case. The current algorithm is left with this limitation due to the complexity of tracking each different element of arrays and also because this should hardly occur in normal code.

* The canonical implementation of ``for`` loops derivation in reverse accumulation requires the caching of the complete state of each iteration which makes the generated code complex and memory intensive. The current algorithm uses a simpler approach that limits the kind of loops that can be correctly derived : in short, loops should not have any kind of recursivity in them (the calculations of each iteration should not depend on the calculations of previous iterations)::

	# will work
	for i in 1:n
		a = f(x[i])
		b = a + g(y[i])
		c[i] = b
	end

	# will not work
	for i in 1:n
		c[i] = f( c[i-1] )
	end

The single exception (that I can think of) of recursive iterations that should work are simple accumulations::

		# will work
		for i in 1:n
			a += b[i]    # new a value depends on previous a
		end	

* ``for`` loops are limited to a single index. If you have a ``for i,j in 1:10, 1:10`` in your expression you will have to translate it to nested loops as a workaround

* Each variable in the expression should be type-stable (not change from a scalar to a vector for example) from one evaluation to the next.

* Only a limited set of Julia semantics are supported at this stage. Some frequently used statements such as comprehensions, ``if else``, ``while`` loops cannot be used in the expression.

* Mutating functions cannot be used (with the exception of ``setindex!``) at this stage.

* ....


