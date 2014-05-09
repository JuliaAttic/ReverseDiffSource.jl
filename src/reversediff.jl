#########################################################################
#
#   Differentiation function
#
#########################################################################

reversediff(ex; init...) = reversediff(ex, nothing; init...)

function reversediff(ex, outsym=nothing; init...)

    println("=== $init")

    length(init)>=1 || 
        error("There should be at least one parameter specified, none found")

    paramsym = Symbol[ e[1] for e in init]
    paramvalues = [ e[2] for e in init]

    g = tograph(ex)
    !haskey(g.set_inodes.vk, outsym) && error("can't find output var $outsym")
    exitnode = g.set_inodes.vk[outsym]
    if outsym==nothing
        g.set_inodes[ exitnode] = :out
    end

    splitnary!(g)
    prune!(g, [exitnode])
    simplify!(g)

    calc!(g, params=Dict(paramsym, paramvalues))

    dg = reversegraph(g, exitnode, paramsym)
    g.nodes = [g.nodes, dg.nodes]
    g.set_inodes = BiDict(merge(g.set_inodes.kv, dg.set_inodes.kv))

    splitnary!(g)
    prune!(g)
    simplify!(g)

    resetvar()

    tocode(g)
end
