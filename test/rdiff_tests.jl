#####  Error thresholds  #####
DIFF_DELTA = 1e-9
ERROR_THRESHOLD = 2e-2

good_enough(x,y) = isfinite(x) ? (abs(x-y) / max(ERROR_THRESHOLD, abs(x))) < ERROR_THRESHOLD : isequal(x,y) 
good_enough(t::Tuple) = good_enough(t[1], t[2])

#####  single gradient check  #####
#  compares numerical gradient to automated gradient
function compare( ex::Expr, x0::Union(Float64, Vector{Float64}, Matrix{Float64}) )
	# ex = :(x+v0ref) ; x0= 1.0
	# ex = :(x*tz) ; x0 = [-3., 2, 0]
	# ex = :(sum( sin(x) )) ; x0 = v1ref

	println("testing $ex with size(x) = $(size(x0))")
	nx = length(x0)  

	# ex1, ex2, outsym = ReverseDiffSource.reversediff( :(res = sum($ex)), :res, x=x0	)
	# ex2 = ReverseDiffSource.reversediff( :(res=sum($ex)), :res, x=x0 )
	ex2 = m.rdiff( ex, x=x0 )
	# m.rdiff( :(x+1.0), x=x0 )

	@eval dfunc(x) = $ex2
	# dfunc(x0)

	# fsym = gensym()
	# @eval let 
	# 	global $fsym
	# 	($fsym)(x) = ($ex2 ; (res, dx))
	# end
	# myf = eval(fsym)

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

	# println(grad1)
	if !all(good_enough, zip([grad0], [grad1]))
		rg0 = map(x -> round(x,5), grad0)
		rg1 = map(x -> round(x,5), grad1)
		println("Gradient false for $ex at x=$x0, expected $rg0, got $rg1")
		# println( ex2 )
		error()
	end
end

macro compare(ex::Expr, x0)
	if isa(x0, Union(Float64, Vector{Float64}, Matrix{Float64}))
		compare(ex, x0)
	elseif isa(x0, Union(Symbol, Expr))
		compare(ex, eval(x0))
	end
end


using Base.Test

reload("ReverseDiffSource")
m = ReverseDiffSource



## variables of different dimension for testing
v0ref = 2.
v1ref = [2., 3, 0.1, 0, -5]
v2ref = [-1. 3 0 ; 0 5 -2]

## regular functions
compare(:(x+v0ref), 1.0)

@compare sin(x) 1.0
@compare sum( sin(x) ) v1ref

###### sum()
@compare sum(x)  v0ref
@compare sum(x)  v1ref
@compare sum(x)  v2ref

###### abs()
@compare abs(x)        v0ref
@compare sum(abs(x))   v1ref
@compare sum(abs(x))   v2ref

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
@compare sum(log(x)) v1ref
@compare sum(log(x)) v2ref

###### sqrt()
@compare sqrt(x)      v0ref
@compare sum(sqrt(x)) v1ref
@compare sum(sqrt(x)) v2ref

###### - (unary)
@compare -x      v0ref
@compare sum(-x) v1ref
@compare sum(-x) v2ref

###### ' transpose
@compare x'      v0ref
@compare sum(x') v1ref
@compare sum(x') v2ref
@compare transpose(x) v0ref
@compare transpose(x) v1ref
@compare transpose(x) v2ref

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

@compare sum(x - v1ref) v1ref
@compare sum(v1ref - x) v1ref

@compare sum(x - 0.5v2ref) v2ref
@compare sum(3v2ref - x) v2ref

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

@compare sum(x      ./ 2.)                                v1ref
@compare sum(3.     ./ (x     .+ 0.1(sgn(x) .== 0.))      v1ref
@compare sum(x      ./ (v1ref .+ 0.1(sgn(v1ref) .== 0.))  v1ref
@compare sum(2v1ref ./ (x     .+ 0.1(sgn(x) .== 0.))      v1ref

@compare sum(x      ./ 2.)                                v2ref
@compare sum(3.     ./ (x     .+ 0.1(sgn(x) .== 0.))      v2ref
@compare sum(x      ./ (v2ref .+ 0.1(sgn(v2ref) .== 0.))  v2ref
@compare sum(2v2ref ./ (x     .+ 0.1(sgn(x) .== 0.))      v2ref


###### max
@compare max( x, 1.)   v0ref
@compare max(1.,  x)   v0ref

@compare sum(max(            x,              2.))  v1ref
@compare sum(max(          -3.,               x))  v1ref
@compare sum(max(            x, -abs(v1ref)./2.))  v1ref
@compare sum(max(0.5abs(v1ref),               x))  v1ref

@compare sum(max(            x,             -1.))  v2ref
@compare sum(max(           0.,               x))  v2ref
@compare sum(max(            x, -abs(v2ref)./2.))  v2ref
@compare sum(max(0.5abs(v2ref),               x))  v2ref

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

###### dot
@compare dot(  x, 3.)   v0ref
@compare dot(-1.,  x)   v0ref

@compare dot(            x, -abs(v1ref)./2.)  v1ref
@compare dot(0.5abs(v1ref),               x)  v1ref

@compare dot(            x, -abs(v2ref)./2.)  v2ref
@compare dot(0.5abs(v2ref),               x)  v2ref



@test_combin    x.^y      x->x<=0 ? 0.2 : x  size(x)==size(y) || ndims(x)==0 || ndims(y)==0


@test_combin    max(x,y)  x->x+0.001  size(x)==size(y) || ndims(x)==0 || ndims(y)==0 
# (x slightly shifted to avoid numerical derivation fail )


@test_combin    x^y       ndims(x)==ndims(y)==0
@test_combin    x/y       y->y==0 ? 0.1 : y ndims(x)==0 || ndims(y)==0
@test_combin    x*y       ndims(x)==0 || ndims(y)==0 || size(x,2)==size(y,1)



tz = transpose(v1ref)
@compare x*tz            [-3., 2, 0]
@compare tz*x            v1ref
@compare v2ref*x         [-3., 2, 0]
@compare v2ref[:,1:2]*x  [-3. 2 0 ; 1 1 -2] # FIXME : fails


##  ref  testing
@compare x[2]               v1ref
@compare x[2:3]             v1ref
@compare x[2:end]           v1ref

@compare x[2:end]           v2ref
@compare x[2]               v2ref
@compare x[2:4]             v2ref
@compare x[:,2]             v2ref
@compare x[1,:]             v2ref
@compare x[2:end,:]         v2ref
@compare x[:,2:end]         v2ref
@compare x[2]+x[1]           v2ref
@compare log(x[2]^2+x[1]^2)  v2ref
