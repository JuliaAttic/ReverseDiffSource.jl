##################################################
#  More tests to improve coverage
##################################################

# using Base.Test
# reload("ReverseDiffSource")
# m = ReverseDiffSource

#########  rdiff ###########

m.@deriv_rule .+(x::Real   , y::AbstractArray)    x     (a=0.; for x in ds ; a+=x ; end; a)

ex = m.rdiff( :( sum( x .+ [1., 2.]) ), x=Float64)
ex = m.rdiff( :( sum( x .+ [1., 2.]) ), x=Float64, allorders=false)

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

@test m.rdiff( ex , p=Float64, y=Float64, ignore=:y) ==
        striplinenumbers(:(begin ; (p ^ 3 + y,3 * p ^ 2) ; end))
@test m.rdiff( ex , p=Float64, y=Float64, ignore=:p) ==
        striplinenumbers(:(begin ; (p ^ 3 + y,1.0) ; end))
@test m.rdiff( ex , p=Float64, y=Float64, ignore=[:p;]) ==
        striplinenumbers(:(begin ; (p ^ 3 + y,1.0) ; end))

@test m.rdiff( ex , p=Float64, y=Float64, ignore=:p, order=3) ==
        striplinenumbers(:(begin ; (p ^ 3 + y,1.0, 0.0, 0.0) ; end))

@test_throws ErrorException m.rdiff( ex , p=Float64, y=Float64, order=3)
@test_throws ErrorException m.rdiff( ex , p=Float64, y=Float64, ignore=(:y,:p), order=3)


ex = :(dot(p,p) ^ sum(y))

m.rdiff( ex , p=Vector{Float64}, y=Vector{Float64}, ignore=[:p;])
m.rdiff( ex , p=Vector{Float64}, y=Vector{Float64}, ignore=[:y;])
m.rdiff( ex , p=Vector{Float64}, y=Vector{Float64}, ignore=[:y;])
m.rdiff( ex , p=Vector{Float64}, y=Vector{Float64}, ignore=[:p;])

m.rdiff( ex , p=Vector{Float64}, y=Vector{Float64}, ignore=[:y;], order=2)
m.rdiff( ex , p=Vector{Float64}, y=Vector{Float64}, ignore=[:p;], order=2)


####### error conditions  #############

@test_throws UndefVarError m.rdiff( :( x * abcd ), x=1.)    # undefined external
@test_throws ErrorException m.rdiff( :( log(x) ), x=-1.)    # unevaluable function
@test_throws ErrorException m.rdiff( :( [1] > [2] ), x=-1.) # unevaluable comparison

@test_throws ErrorException m.tograph(:(log(a) = 1,2))   # incorrect LHS

###### allorders rdiff flags  ################

m.rdiff( :( log(x) ), x=Float64, allorders=false)

###### BitArray type  ############

m.rdiff( :( sum(x .* falses(2)) ), x=Float64  )

####### Issue #25 (splinary not trigered) #############
# (happens when function are prefixed with a module)

module B; end

x = 2.
ex = quote
  B.max(1., x, 3.)
end

m.rdiff(ex, x=Float64)

function foo(x,y,z)
    return x + y + z
end
f = m.rdiff(foo,(Float64,Float64,Float64))


###### Issue #32  (allorders removing some legitimate variables)  ######

ex = m.rdiff( :(y*x^3+y^5) , x=Float64, y=Float64, order=1)
@test length(eval(:(x=2.;y=1.;$ex))) == 3

ex = m.rdiff( :(y*x^3+y^5) , x=Float64, y=Float64, order=1, allorders=false)
@test length(eval(:(x=2.;y=1.;$ex))) == 2

###### Issue #48  (order 0 regression)  ######

@test m.rdiff(:(dot(z, z)), z=Vector{Float64}, order=0) ==
		striplinenumbers(:(begin ; dot(z,z) ; end))
