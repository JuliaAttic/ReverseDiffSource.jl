#########################################################################
#
#   Differentiation function
#
#########################################################################

reversediff(ex; init...) = reversediff(ex, nothing; init...)
function reversediff(ex, outsym::Union(Symbol, Nothing); init...)

    println("=== $init")

    length(init)>=1 || 
        error("There should be at least one parameter specified, none found")

    paramsym = Symbol[ e[1] for e in init]
    paramvalues = [ e[2] for e in init]

    g, d, ext, exitnode = tograph(ex)
    (outsym != nothing) && 
        !haskey(d, outsym) && 
            error("can't find output var $outsym")
    (outsym == nothing) && 
        (exitnode == nothing) && 
            error("can't identify unambiguously expression's output")
    
    (exitnode, outsym) = outsym == nothing ? (exitnode, :res) : ( d[outsym], outsym) 
    g.exitnodes = { outsym => exitnode }

    splitnary!(g)
    evalconstants!(g)
    simplify!(g)
    prune!(g)

    evalsort!(g)
    # println(Dict(paramsym, paramvalues))
    calc!(g, params=Dict(paramsym, paramvalues))

    dg, dnodes = reversegraph(g, g.exitnodes[outsym], paramsym)
    g.nodes = [g.nodes, dg]
    for i in 1:length(paramsym)
        g.exitnodes[dprefix(paramsym[i])] = dnodes[i]
    end

    splitnary!(g)
    evalconstants!(g)
    simplify!(g)
    prune!(g)

    resetvar()
    tocode(g)
end
