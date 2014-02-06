#########################################################################
#    testing script for gradients calculated by reversediff()
#########################################################################

# using AutoDiff

module AutoDiff
    include("src/reverse/proto.jl")
    include("src/reverse/graph_funcs.jl")
    include("src/reverse/reversegraph.jl")
    include("src/reverse/graph_code.jl")
    include("src/reverse/proto_rules.jl")

    function reversediff(ex, paramsym::Vector{Symbol}, outsym=nothing)
        # ex = :( 2^x ) ; paramsym = [:x] ; outsym = nothing
        # ex = :( a[2] ) ; paramsym = [:a] ; outsym = nothing
        # ex = :( x + y ); paramsym = [:x, :y] ; outsym = nothing
        # ex = :( x[1] + x[2] ) ; paramsym = [:x] ; outsym = nothing
        g, d, exitnode = Proto.tograph(ex)
        (outsym != nothing) && !haskey(d, outsym) && error("can't find output var $outsym")
        (outsym == nothing) && (exitnode == nothing) && error("can't identify expression's output")
        
        (exitnode, outsym) = outsym == nothing ? (exitnode, :res) : ( d[outsym], outsym) 

        g.exitnodes = { outsym => exitnode }

        Proto.splitnary!(g)
        Proto.dedup!(g)
        Proto.simplify!(g)
        Proto.prune!(g)
        Proto.evalsort!(g)
        Proto.calc!(g)

        dg, dnodes = Proto.reversegraph(g, g.exitnodes[outsym], paramsym)
        g.nodes = [g.nodes, dg]
        for i in 1:length(paramsym)
            g.exitnodes[Proto.dprefix(paramsym[i])] = dnodes[i]
        end
        # println(g.nodes)

        # g.exitnodes[:dy] == g.nodes[4]
        # g.exitnodes[:dx] == g.nodes[4]
        # g.exitnodes[:res] == g.nodes[3]

        # (g2, length(g2))
        Proto.splitnary!(g)
        Proto.dedup!(g)
        Proto.evalconstants!(g)
        Proto.simplify!(g)
        Proto.prune!(g)
        #  (g2, length(g2))
        Proto.evalsort!(g)

        println(g.nodes)
        Proto.tocode(g)
    end

end

include("test/reverse/helper_functions.jl")

## variables of different dimension for testing
v0ref = 2.
v1ref = [2., 3, 0.1, 0, -5]
v2ref = [-1. 3 0 ; 0 5 -2]

## regular functions
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
deriv1(:(x*tz), [-3., 2, 0]) 
deriv1(:(tz*x), v1ref)  
deriv1(:(v2ref*x), [-3., 2, 0])
deriv1(:(v2ref[:,1:2]*x), [-3. 2 0 ; 1 1 -2]) 

@test_combin    dot(x,y)  ndims(x)==1 && ndims(y)==1 && size(x)==size(y)


##  ref  testing
deriv1(:(x[2]),              v1ref)
deriv1(:(x[2:3]),            v1ref)
deriv1(:(x[2:end]),          v1ref)

deriv1(:(x[2:end]),          v2ref)
deriv1(:(x[2]),              v2ref)
deriv1(:(x[2:4]),            v2ref)
deriv1(:(x[:,2]),            v2ref)
deriv1(:(x[1,:]),            v2ref)
deriv1(:(x[2:end,:]),        v2ref)
deriv1(:(x[:,2:end]),        v2ref)

deriv1(:(x[2]+x[1]),          v2ref)
deriv1(:(log(x[2]^2+x[1]^2)), v2ref)

# fail case when individual elements of an array are set several times
# model = :(x::real(3); y=x; y[2] = x[1] ; y ~ TestDiff())

