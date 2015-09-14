module ReverseDiffSource
using Compat

	include("reversediff.jl")

	export 
		reversediff, 
		@deriv_rule, deriv_rule, declareType

end # module ReverseDiffSource
