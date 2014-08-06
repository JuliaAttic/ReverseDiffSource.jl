module Test

using Distributions
using ReverseDiffSource



# two parameter distributions
import Distributions: Normal, Uniform

for d in [:Normal, :Uniform ]
	@eval begin
		function ($d)(p1::Array, p2::Array)
			ds = Array($d, size(p1))
			for i in 1:length(ds)
				ds[i] = ($d)(p1[i], p2[i])
			end
			ds
		end

		function ($d)(p1::Array, p2::Real)
			ds = Array($d, size(p1))
			for i in 1:length(ds)
				ds[i] = ($d)(p1[i], p2)
			end
			ds
		end

		function ($d)(p1::Real, p2::Array)
			ds = Array($d, size(p2))
			for i in 1:length(ds)
				ds[i] = ($d)(p1, p2[i])
			end
			ds
		end
	end 
end

############# logpdf vectorization on the distribution argument   ################
import Distributions: logpdf

function logpdf{T<:Distribution}(ds::Array{T}, x::AbstractArray)
	res = Array(Float64, size(ds))
	size(ds) == size(x) || error("x and distributions sizes do not match")
	for i in 1:length(x)
		res[i] = logpdf(ds[i], x[i])
	end
	res
end


#####################################################################################
#####################################################################################

##########################################################################################
#
#    MCMC specific derivation rules
#
##########################################################################################

####### creates multiple rules at once for logpdf(Distrib, x)
# FIXME : using undocumented dprefix function of ReverseDiffSource (should be replaced)
macro dlogpdfd(dist::Symbol, rule)
	sig = :( logpdf($(Expr(:(::), :d, dist)), x::Real) )
	ReverseDiffSource.deriv_rule( sig, :d, rule ) 

	sig = :( logpdf($(Expr(:(::), :d, dist)), x::AbstractArray) )
	rule2 = ReverseDiffSource.substSymbols(rule, {:x => :(x[i]), :ds => :(ds[i])})
	ReverseDiffSource.deriv_rule( sig, :d, :(for i in 1:length(x) ; $rule2 ; end))

	sig = :( logpdf($(Expr(:(::), :d, Expr(:curly, :Array, dist))), x::AbstractArray) )
	rule2 = ReverseDiffSource.substSymbols(rule, {:dd1 => :(dd1[i]), :dd2 => :(dd2[i]), :dd3 => :(dd3[i]), 
		:x => :(x[i]), :ds => :(ds[i]), :d => :(d[i]) })
	ReverseDiffSource.deriv_rule(sig, :d, :(for i in 1:length(x) ; $rule2 ; end))
end

macro dlogpdfx(dist::Symbol, rule)
	sig = :( logpdf($(Expr(:(::), :d, dist)), x::Real) )
	ReverseDiffSource.deriv_rule( sig, :x, rule ) 

	sig = :( logpdf($(Expr(:(::), :d, dist)), x::AbstractArray) )
	rule2 = ReverseDiffSource.substSymbols(rule, {:dx => :(dx[i]), :x => :(x[i]), :ds => :(ds[i])})
	ReverseDiffSource.deriv_rule( sig, :x, :(for i in 1:length(x) ; $rule2 ; end))

	sig = :( logpdf($(Expr(:(::), :d, Expr(:curly, :Array, dist))), x::AbstractArray) )
	rule3 = ReverseDiffSource.substSymbols(rule2, {:d => :(d[i])})
	ReverseDiffSource.deriv_rule( sig, :x, :(for i in 1:length(x) ; $rule3 ; end))
end


####### derivation for Distribution types constructors
# ReverseDiffSource.declareType(Distribution, :Distribution)

for d in [:Bernoulli, :TDist, :Exponential, :Poisson]  
	# ReverseDiffSource.declareType(eval(d), d)

	ReverseDiffSource.deriv_rule(:( ($d)(p::Real) ),          :p, :( ds[1] ))
	ReverseDiffSource.deriv_rule(:( ($d)(p::AbstractArray) ), :p, :( copy(ds1[1]) ))
end

for d in [ :Normal, :Uniform, :Weibull, :Gamma, :Cauchy, :LogNormal, :Binomial, :Beta, :Laplace]
	# ReverseDiffSource.declareType(eval(d), d)

	ReverseDiffSource.deriv_rule(:( ($d)(p1::Real, p2::Real) ),                   :p1, :( ds[1] ) )
	ReverseDiffSource.deriv_rule(:( ($d)(p1::Real, p2::Real) ),                   :p2, :( ds[2] ) )
	ReverseDiffSource.deriv_rule(:( ($d)(p1::AbstractArray, p2::AbstractArray) ), :p1, :( copy(ds[1]) ) )
	ReverseDiffSource.deriv_rule(:( ($d)(p1::AbstractArray, p2::AbstractArray) ), :p2, :( copy(ds[2]) ) )
end

#######   Normal distribution
@dlogpdfx Normal (d.μ - x) / (d.σ * d.σ) * ds
@dlogpdfd Normal (x - d.μ) / (d.σ*d.σ) * ds  ((x - d.μ)*(x - d.μ) / (d.σ*d.σ) - 1.) / d.σ * ds )

## Uniform distribution
@dlogpdfx Uniform dx += 0.
@dlogpdfd Uniform ( dd1 += (d.a <= x <= d.b) / (d.b - d.a) * ds ;
					dd2 += (d.a <= x <= d.b) / (d.a - d.b) * ds )



# this makes the model function easier to generate compared to a Float64
#   - embeds the error throwing when log-likelihood reaches -Inf
#   - calculates the sum when logpdf() returns an Array
type OutOfSupportError <: Exception ; end

immutable LLAcc
	val::Float64
	function LLAcc(x::Real)
		isfinite(x) || throw(OutOfSupportError())
		new(x)
	end
end
+(ll::LLAcc, x::Real)           = LLAcc(ll.val + x)
+(ll::LLAcc, x::Array{Float64}) = LLAcc(ll.val + sum(x))

# ReverseDiffSource.declareType(MCMC.LLAcc, :LLAcc) # declares new type to Autodiff

####### derivation rules  ############
# (note : only additions are possible with LLAcc type )
ReverseDiffSource.@deriv_rule getfield(x::LLAcc, f      )      x     dx1 = ds

ReverseDiffSource.@deriv_rule +(x::LLAcc, y      )             x     dx1 += ds1
ReverseDiffSource.@deriv_rule +(x::LLAcc, y::Real)             y     dy += ds1
ReverseDiffSource.@deriv_rule +(x::LLAcc, y::AbstractArray)    y     for i in 1:length(y) ; dy[i] += ds1 ; end










# naming conventions
const ACC_SYM = :__acc       # name of accumulator variable
const PARAM_SYM = :__beta    # name of parameter vector


#######################################################################
#   generates the log-likelihood function
#######################################################################
# - 'init' contains the dictionary of model params and their initial value
# - If 'debug' is set to true, the function returns only the function expression
#  that would have been created

function generateModelFunction(model::Expr; gradient=false, debug=false, init...)

	model.head != :block && (model = Expr(:block, model))  # enclose in block if needed
	length(model.args)==0 && error("model should have at least 1 statement")

	vsize, pmap, vinit = modelVars(;init...) # model param info

	model = translate(model) # rewrite ~ statements
	rv = symbol("$(ACC_SYM)v")  # final result in this variable
	model = Expr(:block, [ :($ACC_SYM = LLAcc(0.)), # add log-lik accumulator initialization
		                   model.args, 
		                   # :( $ACC_SYM = $(Expr(:., ACC_SYM, Expr(:quote, :val)) ) )]... )
		                   :( $rv = $(Expr(:., ACC_SYM, Expr(:quote, :val)) ) )]... )

	## build function expression
	if gradient  # case with gradient
		head, body, outsym = ReverseDiffSource.reversediff(model, 
			                                               rv, false, MCMC; 
			                                               init...)

		body = [ vec2var(;init...),  # assigments beta vector -> model parameter vars
		         body.args,
		         :(($outsym, $(var2vec(;init...))))]

		# enclose in a try block
		body = Expr(:try, Expr(:block, body...),
				          :e, 
				          quote 
				          	if isa(e, OutOfSupportError)
				          		return(-Inf, zero($PARAM_SYM))
				          	else
				          		rethrow(e)
				          	end
				          end)

	else  # case without gradient
		head, body, outsym = ReverseDiffSource.reversediff(model, 
			                                               rv, true, MCMC; 
			                                               init...)

		body = [ vec2var(;init...),  # assigments beta vector -> model parameter vars
		         body.args,
		         outsym ]

		# enclose in a try block
		body = Expr(:try, Expr(:block, body...),
				          :e, 
				          quote 
				          	if isa(e, OutOfSupportError)
				          		return(-Inf)
				          	else
				          		rethrow(e)
				          	end
				          end)

	end

	# build and evaluate the let block containing the function and var declarations
	fn = gensym("ll")
	body = Expr(:function, Expr(:call, fn, :($PARAM_SYM::Vector{Float64})),	Expr(:block, body) )
	body = Expr(:let, Expr(:block, :(global $fn), head.args..., body))

	# println("#############\n$body\n############")

	debug ? body : (eval(body) ; (eval(fn), vsize, pmap, vinit) )
end


#### translates ~ into regular syntax
function translate(ex::Expr)
	if ex.head == :block 
		return Expr(:block, translate(ex.args)...)

	#  handles ~ for julia 0.2
	elseif :(a ~ b).head == :call &&  # test if < julia 0.3 (cf. #4882)
		ex.head == :call && ex.args[1] == :~

		length(ex.args) == 3 || error("Syntax error in ($ex)")

		ex2 = ex.args[3]
		if isa(ex2, Expr) && length(ex2.args)==2 && ex2.args[1] == :+   #  ~+  (right censoring) statement
			return :( $ACC_SYM += logccdf( $(ex2.args[2]), $(ex.args[2]) ) )

		elseif isa(ex2, Expr) && length(ex2.args)==2 && ex2.args[1] == :-  #  ~-  (left censoring) statement			
			return :( $ACC_SYM += logcdf( $(ex2.args[2]), $(ex.args[2]) ) )

		elseif isa(ex2, Expr) || isa(ex2, Symbol)   # ~ statement
			return :( $ACC_SYM += logpdf( $(ex2), $(ex.args[2]) ) )

		else
			error("Syntax error in ($ex)")

		end

	#  handles ~ for julia 0.3+
	elseif :(a ~ b).head == :macrocall &&  # test if >= julia 0.3 (cf. #4882)
		ex.head == :macrocall && ex.args[1] == symbol("@~")

		length(ex.args) == 3 || error("Syntax error in ($ex)")

		ex2 = ex.args[3]
		if isa(ex2, Expr) && length(ex2.args)==2 && ex2.args[1] == :+   #  ~+  (right censoring) statement
			return :( $ACC_SYM += logccdf( $(ex2.args[2]), $(ex.args[2]) ) )

		elseif isa(ex2, Expr) && length(ex2.args)==2 && ex2.args[1] == :-  #  ~-  (left censoring) statement			
			return :( $ACC_SYM += logcdf( $(ex2.args[2]), $(ex.args[2]) ) )

		elseif isa(ex2, Expr) || isa(ex2, Symbol)   # ~ statement
			return :( $ACC_SYM += logpdf( $(ex2), $(ex.args[2]) ) )

		else
			error("Syntax error in ($ex)")

		end

	else
		return ex
	end
end
translate(ex::Vector) = map(translate, ex)
translate(ex::Any) = ex

### creates mapping statements from Vector{Float64} to model parameter variables
function vec2var(;init...)
	ex = Expr[]
	pos = 1
	for (v,i) in init
		sz = size(i)
		if length(sz) == 0  # scalar
			push!(ex, :($v = $PARAM_SYM[ $pos ]) )
			pos += 1
		elseif length(sz) == 1  # vector
			r = pos:(pos+sz[1]-1)
			push!(ex, :($v = $PARAM_SYM[ $(Expr(:(:), pos, pos+sz[1]-1)) ]) )
			pos += sz[1]
		else # matrix case  (needs a reshape)
			r = pos:(pos+prod(sz)-1)
			push!(ex, :($v = reshape($PARAM_SYM[ $(Expr(:(:), pos, pos+prod(sz)-1)) ], $(sz[1]), $(sz[2]))) )
			pos += prod(sz)
		end
	end
	ex
end

### creates mapping statements from model parameter variables to Vector{Float64}
# FIXME : using undocumented dprefix function of ReverseDiffSource (should be replaced)
function var2vec(;init...)
	ex = {}
	for (v,i) in init
		sz = size(i)
		if in(length(sz), [0,1]) # scalar or vector
			push!(ex, ReverseDiffSource.dprefix(v))
		else # matrix case  (needs a reshape)
			push!(ex, :( vec($(ReverseDiffSource.dprefix(v))) ) )
		end
	end
	Expr(:vcat, ex...)
end

### returns parameter info : total size, vector <-> model parameter map, inital values vector
function modelVars(;init...)
	# init = [(:x, 3.)]
    pars = Dict{Symbol, NTuple{2}}()
    pos = 1
    vi = Float64[]

    for (par, def) in init  # par, def = init[1]
    	isa(def, Real) || isa(def, AbstractVector) || isa(def, AbstractMatrix) ||
    		error("unsupported type for parameter $(par)")

        pars[par] = (pos, size(def))
        pos += length(def)
        vi = [vi, float64([def...])]
    end
    (pos-1, pars, vi)
end



end
