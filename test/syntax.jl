##################################################
#  Syntax testing  (uses examples in the doc)
##################################################

#########  rdiff ###########

m.rdiff( :(x^3) , x=Float64)             # first order
m.rdiff( :(x^3) , order = 3, x=Float64)  # orders up to 3

m.rdiff( :(sin(x)) , order=10, x=Float64)  # derivatives up to order 10

res = m.rdiff( :(sin(x)) , order=10, x=Float64)
@eval foo1(x) = $res
foo1(2.)

ex = :( (1 - x[1])^2 + 100(x[2] - x[1]^2)^2 )  # the rosenbrock function
res = m.rdiff(ex, x=Vector{Float64}, order=2)
m.@eval foo2(x) = $res
foo2([0.5, 2.])

#########  rdiff (function) ###########

rosenbrock(x) = (1 - x[1])^2 + 100(x[2] - x[1]^2)^2   # function to be derived
rosen2 = m.rdiff(rosenbrock, (Vector{Float64},), order=2)       # orders up to 2
rosen2([1,2])
rosen2([0.5,2])

test(x) = exp(x)
m.rdiff(test, (Float64,), order=5)

m.rdiff( :(x^3) , x=Float64)             # first order
m.rdiff( :(x^3) , order = 3, x=Float64)  # orders up to 3

m.rdiff( :(sin(x)) , order=10, x=Float64)  # derivatives up to order 10

res = m.rdiff( :(sin(x)) , order=10, x=Float64)
@eval foo3(x) = $res
foo3(2.)

ex = :( (1 - x[1])^2 + 100(x[2] - x[1]^2)^2 )  # the rosenbrock function
res = m.rdiff(ex, x=Vector{Float64}, order=2)
m.@eval foo4(x) = $res
foo4([0.5, 2.])


#########  @deriv_rule  ###########

m.@deriv_rule *(x::Real         , y::Real )            y     x * ds
m.@deriv_rule *(x::Real         , y::AbstractArray)    y     x .* ds
m.@deriv_rule *(x::AbstractArray, y::Real )            y     sum(x .* ds)
m.@deriv_rule *(x::AbstractArray, y::AbstractArray)    y     x' * ds


foo5(x) = log(1+sin(x))

m.@deriv_rule foo5(x)   x   cos(x) / ( 1 + sin(x)) * ds

res = m.rdiff( :( 2 ^ foo5(x) ) , x=Int)
@eval myf(x) = $res

(myf(1.0)[1] - myf(0.999)[1]) * 1000


######### @typeequiv ############

module Sandbox2
    type Bar
        x::Float64
        y::Float64
    end

    norm(z::Bar) = z.x*z.x + z.y*z.y
end

ex = quote
    z = Sandbox2.Bar(2^a, sin(a))
    Sandbox2.norm(z)
end

m.@deriv_rule  Sandbox2.Bar(x,y)      x  ds[1]   # Derivative accumulator of x is increased by ds[1]
m.@deriv_rule  Sandbox2.Bar(x,y)      y  ds[2]   # Derivative accumulator of y is increased by ds[2]

m.@deriv_rule  Sandbox2.norm(z::Sandbox2.Bar)  z  Any[ 2*z.x*ds , 2*z.y*ds ]  # Note : produces a 2-vector since z is a Bar

res = m.rdiff(ex, a=Float64)
@eval df(a) = $res

df(1)

###### internals - tograph example  ######

m.plot( m.tograph( :(a = sin(x+1) ; 2exp(a)) ))


###### higher order   ######

ex = :( (1 - x[1])^2 + 100(x[2] - x[1]^2)^2 )  # the rosenbrock function

res = m.rdiff(ex, x=Vector{Float64}, order=2)
res = m.rdiff(ex, x=Vector{Float64}, order=3)
