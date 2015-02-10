Calling ``rdiff()`` with a function
***********************************

.. warning:: 
	Currently ``rdiff()`` will not work with ``for`` loops in the function definition. This is caused by ``uncompressed_ast`` call not giving access to the original function definition but to an interpreted version that is more challenging to parse.

Calling syntax::

	rdiff( func::Function, init::Tuple; order::Int)
 
Arguments
^^^^^^^^^

:func: is a Julia generic function.

:init: is a tuple containing initial values for each parameter of ``func``.

:order: (keyword arg, default = 1) is an integer indicating the derivation order (1 for 1st order, etc.). Order 0 is allowed and will produce a function that is a processed version of ``ex`` with some variables names rewritten and possibly some optimizations.

:evalmod: (default=Main) module where the expression is meant to be evaluated. External variables and functions should be evaluable in this module.

:debug: (default=false) if true ``rdiff`` dumps the graph of the generating expression, instead of the expression.

:allorders: (default=true) tells rdiff whether to generate the code for all orders up to ``order`` (true) or only the last order.

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

* No ``for`` loops allowed for this ``rdiff`` version.

* All variables should be type-stable (not change from a scalar to a vector for example).

* Only a limited set of Julia semantics are supported at this stage. Some frequently used statements such as comprehensions, ``if else``, ``while`` loops cannot be used in the expression.

* Mutating functions cannot be used (with the exception of ``setindex!`` and ``setfield!``).


