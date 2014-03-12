module ReverseDiffSource

	global	parent_mod  # holds the module where the expression is evaluated
	parent_mod = Main   # default

	include("reversediff.jl")

	setevalmodule(m::Module) = ( parent_mod = m)
  
	export 
		reversediff, 
		setevalmodule,
		@deriv_rule, deriv_rule, declareType

end # module Autodiff
