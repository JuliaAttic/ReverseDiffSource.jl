module ReverseDiffSource

	include("reversediff.jl")

	export 
		reversediff, 
		@deriv_rule, deriv_rule, declareType

end # module Autodiff
