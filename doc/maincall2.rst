Calling ``rdiff()`` with a function
***********************************

.. warning:: 
	Besides being less tested, this version of ``rdiff()`` will not work if there are ``for`` loops in the function definition.

Calling syntax::

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

``rdiff`` takes a function defined with the same subset of Julia statements ( assigments, getindex, setindex!, for loops, function calls ) as the Expression variant of ``rdiff()`` and transforms it into another function whose call will return the derivatives at all orders between 0 and the order specified:: 

	julia> rosenbrock(x) = (1 - x[1])^2 + 100(x[2] - x[1]^2)^2   # function to be derived
	julia> rosen2 = rdiff(rosenbrock, (ones(2),), order=2)       # orders up to 2
		(anonymous function)
	julia> rosen2([1,2])
		(100,[-400.0,200.0],
		2x2 Array{Float64,2}:
		  402.0  -400.0
		 -400.0   200.0)

The generated function will attempt to remove all uneeded calculations (e.g.  x + 0) and factorize repeated function calls as much as possible.

All the variables appearing in the init argument are considered as the expression's arguments and a derivative is calculated for it (and cross derivatives if order is >= 2). 

For orders >= 2 *only a single variable, of type Real or Vector, is allowed*. For orders 0 and 1 variables can be of type Real, Vector or Matrix and can be in an unlimited number. If you are dealing with a function of several (scalar) variables you will have you aggregate them into a vector (as in the example above).


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

	# will (probably) not work
	for i in 1:n
		c[i] = f( c[i-1] )
	end
	
* However simple accumulations are an instance of recursive calculations that should work::

		# will work
		for i in 1:n
			a += b[i]    # new a value depends on previous a
		end	

* ``for`` loops are limited to a single index. If you have a ``for i,j in 1:10, 1:10`` in your expression you will have to translate it to nested loops as a workaround

* All variables should be type-stable (not change from a scalar to a vector for example).

* Only a limited set of Julia semantics are supported at this stage. Some frequently used statements such as comprehensions, ``if else``, ``while`` loops cannot be used in the expression.

* Mutating functions cannot be used (with the exception of ``setindex!``).

* ....


