ReverseDiffSource internals
***************************

All the core of the functions in the package ( differentiation, removal of neutral statements, factorization of identical calls) rely on 2 structures:
	
	1. The ExNode composite type that represents either:
		- a single operation (a function call)
		- an external reference (a variable that can be a parameter for derivation or a reference to a variable outside the scope of the expression)
		- a constant
	ExNodes have parents which are typically the arguments of the function. Collectively they make a DAG but with several additions : 
		- the order of arguments (parent nodes) is significant ( ``a ^ b`` is not the same as ``b ^ a``)
		- there needs to be additionnal ordering information as statements not related sometimes need to execute in a specific order, this information is in the ``precedence`` field.
	2. The ExGraph composite type that stores
		- ExNodes in a vector (in the order of execution), 
		- information on how to map ExNodes to variable names (used and set), 
		- and optionnaly information on how to map nodes to 'outer' nodes. This last mapping is necessary when the ExGraph is embedded in another parent graph ( the inner scope of for loops is represented as a subgraph). 


Arguments
^^^^^^^^^

:func: is a Julia generic function.



