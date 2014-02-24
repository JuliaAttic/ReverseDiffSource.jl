#########################################################################
#
#    testing script for gradients calculated by reversediff()
#
#########################################################################


include("../src/ReverseDiffSource.jl")

include("../test/helper_functions.jl")

## variables of different dimension for testing
v0ref = 2.
v1ref = [2., 3, 0.1, 0, -5]
v2ref = [-1. 3 0 ; 0 5 -2]

## regular functions
compare(:(x+v0ref), 1.0)
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
