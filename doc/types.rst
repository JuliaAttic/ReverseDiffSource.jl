Working with composite types
****************************


When encountering a composite type, ReverseDiffSource builds a ``Vector{Any}`` to hold its derivative accumulator. Its structure is derived from the fields of the composite type: Float for a Real number, an array of Floats for Arrays, or another ``Vector{Any}`` if the field is a type. No special declaration has to be made beforehand to ReverseDiffSource.

However you do need to declare how each function using the composite type changes its derivative accumulator. 

Suppose you have type ``Bar`` defined as::

	type Bar
	    x
	    y
	end

And an associated function ``norm(z::Bar)``::

	norm(z::Bar) = z.x*z.x + z.y*z.y

And finally an expression to derive making use of ``Bar`` and ``norm()``::

	ex = :( z = Bar(2^a, sin(a)) ; norm(z) )

You need to declare how both the constructor ``Bar`` and the function ``norm`` behave regarding the derivative accumulator (which will be a 2 element vector of type ``Any`` for the two fields ``x`` and``y``)::

	@deriv_rule  Bar(x,y)      x  ds[1]   # Derivative accumulator of x is increased by ds[1]
	@deriv_rule  Bar(x,y)      y  ds[2]   # Derivative accumulator of y is increased by ds[2]
	
	@deriv_rule  norm(z::Bar)  z  Any[ 2*z.x*ds , 2*z.y*ds ]  # Note : produces a 2-vector since z is a Bar

We are now ready to derive::

	julia> res = rdiff(ex, a=0.)
	julia> @eval df(a) = $res

	julia> df(1)
		(4.708073418273571,6.454474871305244)




 

