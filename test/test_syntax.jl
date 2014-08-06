##################################################
#  Syntax testing  (uses examples in the doc)
##################################################

using Base.Test

reload("ReverseDiffSource")
m = ReverseDiffSource

#########  rdiff ###########

m.rdiff( :(x^3) , x=2.)             # first order
m.rdiff( :(x^3) , order = 3, x=2.)  # orders up to 3

m.rdiff( :(sin(x)) , order=10, x=2.)  # derivatives up to order 10

res = m.rdiff( :(sin(x)) , order=10, x=2.)
@eval foo(x) = $res
foo(2.)

ex = :( (1 - x[1])^2 + 100(x[2] - x[1]^2)^2 )  # the rosenbrock function
res = m.rdiff(ex, x=zeros(2), order=2)
m.@eval foo(x) = $res
foo([0.5, 2.])

#########  rdiff (function) ###########

rosenbrock(x) = (1 - x[1])^2 + 100(x[2] - x[1]^2)^2   # function to be derived
rosen2 = m.rdiff(rosenbrock, (ones(2),), order=2)       # orders up to 2
rosen2([1,2])
rosen2([0.5,2])

test(x) = exp(x)
m.rdiff(test, (1.,), order=5)
isgeneric(exp)

m.rdiff( :(x^3) , x=2.)             # first order
m.rdiff( :(x^3) , order = 3, x=2.)  # orders up to 3

m.rdiff( :(sin(x)) , order=10, x=2.)  # derivatives up to order 10

res = m.rdiff( :(sin(x)) , order=10, x=2.)
@eval foo(x) = $res
foo(2.)

ex = :( (1 - x[1])^2 + 100(x[2] - x[1]^2)^2 )  # the rosenbrock function
res = m.rdiff(ex, x=zeros(2), order=2)
m.@eval foo(x) = $res
foo([0.5, 2.])


#########  @deriv_rule  ###########

m.@deriv_rule *(x::Real         , y::Real )            y     x * ds
m.@deriv_rule *(x::Real         , y::AbstractArray)    y     x .* ds
m.@deriv_rule *(x::AbstractArray, y::Real )            y     sum(x .* ds)
m.@deriv_rule *(x::AbstractArray, y::AbstractArray)    y     x' * ds


foo(x) = log(1+sin(x))

m.@deriv_rule foo(x)   x   cos(x) / ( 1 + sin(x)) * ds

res = m.rdiff( :( 2 ^ foo(x) ) , x=1)
@eval myf(x) = $res

(myf(1.0)[1] - myf(0.999)[1]) * 1000


######### @typeequiv ############

type Bar
    x
    y
end
	
norm(z::Bar) = z.x*z.x + z.y*z.y

norm(Bar(1,1))

ex = :( z = Bar(a*a, sin(a)) ; norm(z) )
a = 1
@eval $ex

m.@typeequiv   Bar           2
m.@deriv_rule  Bar(x,y)      x  ds[1]
m.@deriv_rule  Bar(x,y)      y  ds[2]
m.@deriv_rule  norm(z::Bar)  z  [ 2*z.x , 2*z.y ] .* ds

res = m.rdiff(ex, a=0.)
@eval tt(a) = $res

tt(1)


###### internals - tograph example  ######


