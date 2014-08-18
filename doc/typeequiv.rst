Defining new types : ``@typeequiv()``
*************************************


When your expression uses composite types you need to tell ReverseDiffSource how to handle them. More specifically you need to tell the equivalent vector size so that the reverse accumulation algorithm knows how to process functions using these types as arguments. This the the role of the macro call ``@typeequiv``::

		@typeequiv typ::Expr siz::Integer

Arguments
^^^^^^^^^

:typ: is the name of the type

:siz: is the size of the equivalent vector.


Example
^^^^^^^

Suppose you have type ``Bar`` defined as::

	type Bar
	    x
	    y
	end

And an associated function ``norm(z::Bar)``::

	norm(z::Bar) = z.x*z.x + z.y*z.y

And finally an expression making use of ``Bar`` and ``norm()``::

	ex = :( z = Bar(2^a, sin(a)) ; norm(z) )

Several things need to be done to correctly derive that expression : first declare the equivalent vector of ``Bar``, declare how the constructor of ``Bar`` derives for each of its 2 arguments, and finally how ``norm`` derives for its single argument::

	@typeequiv   Bar           2          # Derivative of Bar can be represented as a vector of 2
	
	@deriv_rule  Bar(x,y)      x  ds[1]   # Derivative accumulator of x is increased by ds[1]
	@deriv_rule  Bar(x,y)      y  ds[2]   # Derivative accumulator of y is increased by ds[2]
	
	@deriv_rule  norm(z::Bar)  z  [ 2*z.x , 2*z.y ] .* ds  # Note : produces a 2-vector since z is a Bar

We are now ready to derive::

	julia> res = rdiff(ex, a=0.)
	julia> @eval df(a) = $res

	julia> df(1)
		(1.708073418273571,(4.909297426825682,))




 

