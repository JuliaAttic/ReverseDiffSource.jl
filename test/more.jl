##################################################
#  More tests to improve coverage 
##################################################

# using Base.Test
# reload("ReverseDiffSource")
# m = ReverseDiffSource


#########  rdiff ###########

m.@deriv_rule .+(x::Real   , y::AbstractArray)    x     (a=0.; for x in ds ; a+=x ; end; a)


ex = m.rdiff( :( sum( x .+ [1., 2.]) ), x=1.)

######### plot  ############

ex = quote 
	a = 0.
	for i in 1:10
		a += i
	end
	a
end

m.plot( m.tograph(ex))


########## show() for nodes and graphs  ##########

g = m.tograph(ex)
println(g.nodes[4])
println(g)


####### error conditions  #############

@test_throws UndefVarError m.rdiff( :( x * abcd ), x=1.)    # undefined external
@test_throws ErrorException m.rdiff( :( log(x) ), x=-1.)    # unevaluable function
@test_throws ErrorException m.rdiff( :( [1] > [2] ), x=-1.) # unevaluable comparison

@test_throws ErrorException m.addgraph!(:( y + 2), g, Dict(:z => g.nodes[4]))   # y not mapped

@test_throws ErrorException m.tograph(:(log(a) = 1,2))   # incorrect LHS

###### allorders rdiff flags  ################

m.rdiff( :( log(x) ), x=1., allorders=false)

###### BitArray type  ############

m.rdiff( :( sum(x .* falses(2)) ), x=1.  )



