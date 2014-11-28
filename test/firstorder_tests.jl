#################################################################
#
#    1st order derivation testing
#
#################################################################

# reload("ReverseDiffSource")
# m = ReverseDiffSource

#####  Error thresholds  #####
DIFF_DELTA = 1e-9
ERROR_THRESHOLD = 2e-2

good_enough(x,y) = isfinite(x) ? (abs(x-y) / max(ERROR_THRESHOLD, abs(x))) < ERROR_THRESHOLD : isequal(x,y) 
good_enough(t::Tuple) = good_enough(t[1], t[2])

#####  single gradient check  #####
#  compares numerical gradient to automated gradient
function compare( ex::Expr, x0::Union(Float64, Vector{Float64}, Matrix{Float64}) )
	# print("testing $ex with size(x) = $(size(x0))")
	nx = length(x0)  

	ex2 = m.rdiff( ex, x=x0 )
	dfunc(x0) = eval( :(let x = $x0 ; $ex2 ; end) )

	l0, (grad0,) = dfunc(x0)  
	if ndims(x0) == 0  # scalar
		grad1 = ( dfunc( x0 + DIFF_DELTA)[1] - l0 ) / DIFF_DELTA
	else # vector and matrices
		grad1 = zeros(size(grad0))
		for i in 1:nx  # i=1
			x1 = copy(x0)
			x1[i] += DIFF_DELTA
			grad1[i] = ( dfunc(x1)[1][1] - l0 ) / DIFF_DELTA
		end
	end

	if !all(good_enough, zip([grad0], [grad1]))
		rg0 = map(x -> round(x,5), grad0)
		rg1 = map(x -> round(x,5), grad1)
		println("\nGradient false for $ex at x=$x0, expected $rg1, got $rg0")
		# println( ex2 )
		error()
	else
		println(" ok")
	end
end

macro compare(ex::Expr, x0)
	if isa(x0, Union(Float64, Vector{Float64}, Matrix{Float64}))
		compare(ex, x0)
	elseif isa(x0, Union(Symbol, Expr))
		compare(ex, eval(x0))
	end
end


## variables of different dimension for testing
v0ref = 2.
v1ref = [2., 3, 0.1, 0, -5]
v2ref = [-1. 3 0 ; 0 5 -2]


###### sum()
@compare sum(x)  v0ref   # fait une erreur  m.rdiff( :(a = sum(x);a), x= 1.0 )
@compare sum(x)  v1ref
@compare sum(x)  v2ref

###### abs()
@compare abs(x)        v0ref
@compare sum(abs(x))   v1ref .+ 0.1(sign(v1ref).==0.)
@compare sum(abs(x))   v2ref .+ 0.1(sign(v2ref).==0.)

###### sin()
@compare sin(x)      v0ref
@compare sum(sin(x)) v1ref
@compare sum(sin(x)) v2ref

###### cos()
@compare cos(x)      v0ref
@compare sum(cos(x)) v1ref
@compare sum(cos(x)) v2ref

###### exp()
@compare exp(x)      v0ref
@compare sum(exp(x)) v1ref
@compare sum(exp(x)) v2ref

###### log()
@compare log(x)      v0ref
@compare sum(log(x)) abs( v1ref .+ 0.1(sign(v1ref).==0.) )
@compare sum(log(x)) abs( v2ref .+ 0.1(sign(v2ref).==0.) )

###### sqrt()
@compare sqrt(x)      v0ref
@compare sum(sqrt(x)) abs( v1ref .+ 0.1(sign(v1ref).==0.))

###### - (unary)
@compare -x      v0ref
@compare sum(-x) v1ref
@compare sum(-x) v2ref

###### ' transpose
@compare x'                v0ref
@compare sum(x')           v1ref
@compare sum(x')           v2ref
@compare transpose(x)      v0ref
@compare sum(transpose(x)) v1ref
@compare sum(transpose(x)) v2ref

###### +
@compare x + 1.    v0ref
@compare 1. + x    v0ref

@compare sum(x + v1ref) v1ref
@compare sum(v1ref + x) v1ref
@compare sum(x + v2ref) v2ref
@compare sum(v2ref + x) v2ref

###### -
@compare x - 1.    v0ref
@compare 1. - x    v0ref

@compare sum(x - v1ref)    v1ref
@compare sum(v1ref - x)    v1ref
@compare sum(x - 0.5v2ref) v2ref
@compare sum(3v2ref - x)   v2ref

###### .+
@compare x  .+ 1.   v0ref
@compare 1. .+ x    v0ref

@compare sum(x  .+ 2.)   v1ref
@compare sum(3. .+ x)    v1ref
@compare sum(x .+ v1ref) v1ref
@compare sum(v1ref .+ x) v1ref

@compare sum(x  .+ 2.)   v2ref
@compare sum(3. .+ x)    v2ref
@compare sum(x .+ v2ref) v2ref
@compare sum(v2ref .+ x) v2ref

###### .-
@compare x  .- 1.   v0ref
@compare 1. .- x    v0ref

@compare sum(x  .- 2.)   v1ref
@compare sum(3. .- x)    v1ref
@compare sum(x .- v1ref) v1ref
@compare sum(v1ref .- x) v1ref

@compare sum(x  .- 2.)   v2ref
@compare sum(3. .- x)    v2ref
@compare sum(x .- v2ref) v2ref
@compare sum(v2ref .- x) v2ref

###### .*
@compare x  .* 1.   v0ref
@compare 1. .* x    v0ref

@compare sum(x  .* 2.)   v1ref
@compare sum(3. .* x)    v1ref
@compare sum(x .* v1ref) v1ref
@compare sum(v1ref .* x) v1ref

@compare sum(x  .* 2.)   v2ref
@compare sum(3. .* x)    v2ref
@compare sum(x .* v2ref) v2ref
@compare sum(v2ref .* x) v2ref

###### ./
@compare x  ./ 1.   v0ref
@compare 1. ./ x    v0ref

@compare sum(x      ./ 2.)             v1ref
@compare sum(3.     ./  x)             v1ref .+ 0.1(sign(v1ref) .== 0.)
@compare sum(x      ./ (v1ref .+ 0.1)) v1ref
@compare sum(2v1ref ./  x)             v1ref .+ 0.1(sign(v1ref) .== 0.)

@compare sum(x      ./ 2.)             v2ref
@compare sum(3.     ./  x)             v2ref .+ 0.1(sign(v2ref) .== 0.)
@compare sum(x      ./ (v2ref .+ 0.1)) v2ref
@compare sum(2v2ref ./  x)             v2ref .+ 0.1(sign(v2ref) .== 0.)

###### /
@compare   x / 3.   v0ref
@compare -1. / x    v0ref

@compare sum(x /   2.)     v1ref
@compare sum(x / -0.5)     v2ref

###### max
@compare max( x, 1.)   v0ref
@compare max(1.,  x)   v0ref

@compare sum(max(            x,                   2.1))  v1ref
@compare sum(max(          -3.,                     x))  v1ref
@compare sum(max(            x, 1. .- abs(v1ref)./2.1))  v1ref
@compare sum(max(0.5abs(v1ref),                x.+0.1))  v1ref

@compare sum(max(            x      , -1.1                )) v2ref
@compare sum(max(          0.1      , x                   )) v2ref
@compare sum(max(            x      , -abs(v2ref)./2.+0.1 )) v2ref
@compare sum(max(0.5abs(v2ref).+0.1 , x                   )) v2ref

###### min
@compare min( x, 1.)   v0ref
@compare min(1.,  x)   v0ref

@compare sum(min(            x,              2.))  v1ref
@compare sum(min(          -3.,               x))  v1ref
@compare sum(min(            x, -abs(v1ref)./2.))  v1ref
@compare sum(min(0.5abs(v1ref),               x))  v1ref

@compare sum(min(            x,             -1.))  v2ref
@compare sum(min(           0.,               x))  v2ref
@compare sum(min(            x, -abs(v2ref)./2.))  v2ref
@compare sum(min(0.5abs(v2ref),               x))  v2ref

###### maximum
@compare maximum(x)   v0ref
@compare maximum(x)   v1ref
@compare maximum(x)   v2ref

###### minimum
@compare minimum(x)   v0ref
@compare minimum(x)   v1ref
@compare minimum(x)   v2ref

###### dot
@compare dot(  x, 3.)   v0ref
@compare dot(-1.,  x)   v0ref

@compare dot(            x, -abs(v1ref)./2.)  v1ref
@compare dot(0.5abs(v1ref),               x)  v1ref

###### ^
@compare x ^ 3.   v0ref
@compare 3. ^ x   v0ref

###### .^
@compare x .^ 3.    v0ref
@compare 3 .^ x     v0ref
@compare x .^ -3.   v0ref

@compare sum( x .^  2)  v1ref
@compare sum( x .^ -1)  v1ref .+ 0.1(sign(v1ref) .== 0.)
@compare sum( 2 .^  x)  v1ref

@compare sum( x .^  2)  v2ref
@compare sum( x .^ -1)  v2ref .+ 0.1(sign(v2ref) .== 0.)
@compare sum( 2 .^  x)  v2ref

###### *
@compare  x * 3.    v0ref
@compare -2 * x     v0ref

@compare  sum(ones(5)'   * x)            v1ref
@compare  sum(x'         * v1ref)        v1ref
@compare  sum(ones(5)    * x')           v1ref
@compare  sum(x          * (v1ref./2.)') v1ref

@compare  sum(2ones(5,2) * x)            v2ref
@compare  sum(zeros(1,2) * x)            v2ref
@compare  sum(x          * ones(3,2))    v2ref
@compare  sum(x          * -1ones(3,1))  v2ref


