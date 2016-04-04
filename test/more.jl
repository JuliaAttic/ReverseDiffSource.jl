##################################################
#  More tests to improve coverage
##################################################

# using Base.Test
# reload("ReverseDiffSource")
# m = ReverseDiffSource

#########  rdiff ###########

m.@deriv_rule .+(x::Real   , y::AbstractArray)    x     (a=0.; for x in ds ; a+=x ; end; a)

ex = m.rdiff( :( sum( x .+ [1., 2.]) ), x=1.)
ex = m.rdiff( :( sum( x .+ [1., 2.]) ), x=1., allorders=false)

######### plot  ############

ex = quote
	a = 0.
	for i in 1:10
		a += i
	end
	a
end

m.plot( m.tograph(ex) )

########## show() for nodes and graphs  ##########

g = m.tograph(ex)
println(g.nodes[4])
println(g)


########## 'ignore' argument  ##########

ex = :(p^3+y)

@test m.rdiff( ex , p=2., y=3., ignore=:y) ==
        striplinenumbers(:(begin ; (p ^ 3 + y,3 * p ^ 2) ; end))
@test m.rdiff( ex , p=2., y=3., ignore=:p) ==
        striplinenumbers(:(begin ; (p ^ 3 + y,1.0) ; end))
@test m.rdiff( ex , p=2., y=3., ignore=[:p;]) ==
        striplinenumbers(:(begin ; (p ^ 3 + y,1.0) ; end))

@test m.rdiff( ex , p=2., y=3., ignore=:p, order=3) ==
        striplinenumbers(:(begin ; (p ^ 3 + y,1.0, 0.0, 0.0) ; end))

@test_throws ErrorException m.rdiff( ex , p=2., y=3., order=3)
@test_throws ErrorException m.rdiff( ex , p=2., y=3., ignore=(:y,:p), order=3)


ex = :(dot(p,p) ^ sum(y))

m.rdiff( ex , p=[1.,2.], y=[3.,1.], ignore=[:p;])
m.rdiff( ex , p=[1.,2.], y=[3.,1.], ignore=[:y;])
m.rdiff( ex , p=[1.,2.], y=2, ignore=[:y;])
m.rdiff( ex , p=[1.,2.], y=2, ignore=[:p;])

m.rdiff( ex , p=[1.,2.], y=2, ignore=[:y;], order=2)
m.rdiff( ex , p=[1.,2.], y=2, ignore=[:p;], order=2)

function ploglikelihood(p::Vector{Float64}, v::Vector)
  Xp = v[2]*p
  dot(Xp, v[3])-sum(log(1+exp(Xp)))
end


args = (ones(3), Any[1.,2.,[3.,2.,-2]])

dplog = m.rdiff( ploglikelihood, args, ignore=[:v;])
dplog(ones(3), Any[1.,2.,[3.,2.,-2]])

@test ploglikelihood(args...) == dplog(args...)[1]
dplog(args...)[2]


####### error conditions  #############

@test_throws UndefVarError m.rdiff( :( x * abcd ), x=1.)    # undefined external
@test_throws ErrorException m.rdiff( :( log(x) ), x=-1.)    # unevaluable function
@test_throws ErrorException m.rdiff( :( [1] > [2] ), x=-1.) # unevaluable comparison

@test_throws ErrorException m.tograph(:(log(a) = 1,2))   # incorrect LHS

###### allorders rdiff flags  ################

m.rdiff( :( log(x) ), x=1., allorders=false)

###### BitArray type  ############

m.rdiff( :( sum(x .* falses(2)) ), x=1.  )

####### Issue #25 (splinary not trigered) #############
# (happens when function are prefixed with a module)

module B; end

x = 2.
ex = quote
  B.max(1., x, 3.)
end

m = ReverseDiffSource
m.rdiff(ex, x=2.)

function foo(x,y,z)
    return x + y + z
end
f = m.rdiff(foo,(1,1,1))


###### Issue 32  (allorders removing some legitimate variables)  ######

ex = m.rdiff( :(y*x^3+y^5) , x=2., y=1., order=1)
@test length(eval(:(x=2.;y=1.;$ex))) == 3

ex = m.rdiff( :(y*x^3+y^5) , x=2., y=1., order=1, allorders=false)
@test length(eval(:(x=2.;y=1.;$ex))) == 2
