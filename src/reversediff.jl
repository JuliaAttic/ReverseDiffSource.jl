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

    println("=== tograph")
    g = tograph(ex)
    !haskey(g.set_inodes.vk, outsym) && error("can't find output var $outsym")
    exitnode = g.set_inodes.vk[outsym]
    if outsym==nothing
        g.set_inodes[ exitnode] = :out
    end

    splitnary!(g)
    println("=== prune")
    prune!(g, [exitnode])
    println("=== simplify")
    simplify!(g)

    println("=== calc")
    calc!(g, params=Dict(paramsym, paramvalues))

    println("=== reversegraph")
    dg = reversegraph(g, exitnode, paramsym)
    append!(g.nodes, dg.nodes)
    g.set_inodes = BiDict(merge(g.set_inodes.kv, dg.set_inodes.kv))

    splitnary!(g)
    println("=== prune2")
    prune!(g)
    println("=== simplify2")
    simplify!(g)

    resetvar()

    println("=== tocode")
    tocode(g)
end




#### experimental  #####

    function ndiff(ex, order::Int, paramsym::Symbol, outsym=nothing)
        # ex = :( 2^x ) ; paramsym = [:x] ; outsym = nothing
        g, d, exitnode = Proto.tograph(ex)
        (outsym != nothing) && !haskey(d, outsym) && error("can't find output var $outsym")
        (outsym == nothing) && (exitnode == nothing) && error("can't identify expression's output")
        
        (exitnode, outsym) = outsym == nothing ? (exitnode, :res) : ( d[outsym], outsym) 

        ndict = { outsym => exitnode, :exitnode => exitnode}
        Proto.splitnary!(g)
        Proto.dedup!(g, ndict)
        Proto.simplify!(g, ndict)
        Proto.prune!(g, ndict)
        Proto.evalsort!(g)

        for i in 1:order
            Proto.calc!(g)
            dg, dnodes = Proto.reversegraph(g, ndict[:exitnode], [paramsym])
            g = [g, dg]
            ndict[ Proto.dprefix("$i$paramsym") ] = dnodes[1]
            ndict[ :exitnode ] = dnodes[1]

            Proto.splitnary!(g)
            Proto.dedup!(g, ndict)
            Proto.evalconstants!(g)
            Proto.simplify!(g, ndict)
            Proto.prune!(g, ndict)
            Proto.evalsort!(g)

            exitnode = dnodes[1]
        end
        #  (g2, length(g2))
        delete!(ndict, :exitnode)
        Proto.tocode(g, ndict)
    end



