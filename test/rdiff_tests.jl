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

	println("testing $ex with size(x) = $(size(x0))")
	nx = length(x0)  

	# ex1, ex2, outsym = ReverseDiffSource.reversediff( :(res = sum($ex)), :res, x=x0	)
	# ex2 = ReverseDiffSource.reversediff( :(res=sum($ex)), :res, x=x0 )
	ex2 = m.rdiff( :(sum($ex)), x=x0 )
	# m.rdiff( :(x+1.0), x=x0 )

	@eval dfunc(x) = $ex2
	# dfunc(x0)

	# fsym = gensym()
	# @eval let 
	# 	global $fsym
	# 	($fsym)(x) = ($ex2 ; (res, dx))
	# end
	# myf = eval(fsym)

	l0, grad0 = dfunc(x0)  
	grad0 = grad0[1]
	if ndims(x0) == 0  # scalar
		grad1 = ( dfunc( x0 + DIFF_DELTA)[1] - l0 ) / DIFF_DELTA
	else # vector and matrices
		grad1 = zeros(size(grad0))
		for i in 1:nx  # i=1
			x1 = copy(x0)
			x1[i] += DIFF_DELTA
			grad1[i] = ( dfunc(x1)[1] - l0 ) / DIFF_DELTA
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


using Base.Test

reload("ReverseDiffSource")
m = ReverseDiffSource



## variables of different dimension for testing
v0ref = 2.
v1ref = [2., 3, 0.1, 0, -5]
v2ref = [-1. 3 0 ; 0 5 -2]

## regular functions
compare(:(x+v0ref), 1.0)

compare(:(sin(x)), v0ref)

@test_combin    x+y       size(x)==size(y) || ndims(x)==0 || ndims(y)==0
@test_combin    x+y+z     size(x)==size(y)==size(z) || 
							(ndims(x)==0 && size(y)==size(z)) || 
							(ndims(y)==0 && size(x)==size(z)) ||
							(ndims(z)==0 && size(x)==size(z))
@test_combin    sum(x)
@test_combin    x-y       size(x)==size(y) || ndims(x)==0 || ndims(y)==0
@test_combin    x.*y      size(x)==size(y) || ndims(x)==0 || ndims(y)==0
@test_combin    x./y  	  y->y==0 ? 0.1 : y  size(x)==size(y) || ndims(x)==0 || ndims(y)==0

@test_combin    x.^y      x->x<=0 ? 0.2 : x  size(x)==size(y) || ndims(x)==0 || ndims(y)==0
@test_combin    sin(x)
@test_combin    abs(x)    x->x==0 ? 0.001 : x 
@test_combin    cos(x)
@test_combin    exp(x)
@test_combin    log(x)    x->x<=0 ? 0.1 : x

@test_combin    transpose(x)
@test_combin    x' 

@test_combin    max(x,y)  x->x+0.001  size(x)==size(y) || ndims(x)==0 || ndims(y)==0 
# (x slightly shifted to avoid numerical derivation fail )

@test_combin    min(x,y)  size(x)==size(y) || ndims(x)==0 || ndims(y)==0

@test_combin    x^y       ndims(x)==ndims(y)==0

@test_combin    x/y       y->y==0 ? 0.1 : y ndims(x)==0 || ndims(y)==0

@test_combin    x*y       ndims(x)==0 || ndims(y)==0 || size(x,2)==size(y,1)
tz = transpose(v1ref)
compare(:(x*tz), [-3., 2, 0]) 
compare(:(tz*x), v1ref)  
compare(:(v2ref*x), [-3., 2, 0])
compare(:(v2ref[:,1:2]*x), [-3. 2 0 ; 1 1 -2]) # FIXME : fails

@test_combin    dot(x,y)  ndims(x)==1 && ndims(y)==1 && size(x)==size(y)

##  ref  testing
compare(:(x[2]),              v1ref)
compare(:(x[2:3]),            v1ref)
compare(:(x[2:end]),          v1ref)

compare(:(x[2:end]),          v2ref)
compare(:(x[2]),              v2ref)
compare(:(x[2:4]),            v2ref)
compare(:(x[:,2]),            v2ref)
compare(:(x[1,:]),            v2ref)
compare(:(x[2:end,:]),        v2ref)
compare(:(x[:,2:end]),        v2ref)

compare(:(x[2]+x[1]),          v2ref)
compare(:(log(x[2]^2+x[1]^2)), v2ref)
