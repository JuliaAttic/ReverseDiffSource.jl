Defining new functions : ``@deriv_rule()``
******************************************


ReverseDiffSource comes with the derivations instructions for a limited set of functions such as ``*``, ``+``, ``/``, ``transpose``, ``exp``, ``log``, ....  You can 'teach' the package derivation methods for new functions with the macro call ``@deriv_rule``::

		@deriv_rule ex::Expr var::Symbol rule::Expr

Arguments
^^^^^^^^^

:ex: is the function signature, with each argument specified

:var: is the symbol of the argument you derive for.

:rule: is an expression to calculate the value to be added to the derivative accumulator for variable ``var``.


Usage
^^^^^

``rule`` should contain an expression that can be parsed by ReverseDiffSource (syntax limitations mentionned in previous chapter apply here). All symbols in it should either be one of the arguments in the function signature or the special symbol ``ds`` that represents the derivative accumulator of the function.

Julia's multiple dispatch rules apply to the definition : you can define different rules for a given function depending on the type of its arguments::

	@deriv_rule *(x::Real         , y::Real )            y     x * ds
	@deriv_rule *(x::Real         , y::AbstractArray)    y     x .* ds
	@deriv_rule *(x::AbstractArray, y::Real )            y     sum(x .* ds)
	@deriv_rule *(x::AbstractArray, y::AbstractArray)    y     x' * ds

Example
^^^^^^^

Suppose you defined a function ``foo(x)``::

	foo(x) = log(1+sin(x))

This function is in turn used in the expression you want to derive::

	ex = :( 2 ^ foo(x) )

Define the derivation of ``foo`` by its argument::

	@deriv_rule foo(x)   x   cos(x) / ( 1 + sin(x)) * ds

You can now derive ``ex``::

	julia> rdiff( :( 2 ^ foo(x) ) , x=1) 
		:(begin 
		        _tmp1 = 2^foo(x)
		        (_tmp1,((cos(x) / (1.0 + sin(x))) * (0.6931471805599453_tmp1),))
		    end)


