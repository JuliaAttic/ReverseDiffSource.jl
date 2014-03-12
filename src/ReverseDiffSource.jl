module ReverseDiffSource

	# global	parent_mod::Module  # holds the module where the expression is evaluated
	# parent_mod = Main   # default

	include("reversediff.jl")

	# function setevalmodule(m::Module)
	# 	println(" before $m")
	# 	parent_mod = m
	# 	println(" after $m $(parent_mod)")
 #  	end

	export 
		reversediff, 
		@deriv_rule, deriv_rule, declareType

end # module Autodiff
