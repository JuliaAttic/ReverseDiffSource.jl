#########################################################################
#
#   Graph to expression conversion
#
#########################################################################

function tocode(g::ExGraph)

    #### creates expression for names qualified by a module
    mexpr(ns) = length(ns) == 1 ? ns[1] : Expr(:., mexpr(ns[1:end-1]), QuoteNode(ns[end]) )


    valueof(n::ExNode, child::ExNode) = n.val
    valueof(n::NFor,   child::ExNode) = valueof(n.val[child], n)

    translate(n::NConst) = n.main
    translate(n::NComp)  = Expr(:comparison,
                                valueof(n.parents[1],n),
                                n.main,
                                valueof(n.parents[2],n) )

    translate(n::NRef)   = Expr(:ref, Any[ valueof(x,n) for x in n.parents ]...)
    translate(n::NDot)   = Expr(:(.), valueof(n.parents[1],n), n.main)

    function translate(n::NIn)
        isa(n.parents[1], NFor) && return n.parents[1].val[n]
        return n.parents[1].val
    end

    function translate(n::NCall)
        # special translation cases
        op = n.parents[1].main
        if op == vcat
            return Expr(      :vect, Any[ valueof(x,n) for x in n.parents[2:end] ]...)
        elseif op == colon
            return Expr(       :(:), Any[ valueof(x,n) for x in n.parents[2:end] ]...)
        elseif op == transpose
            return Expr(symbol("'"),                 valueof(n.parents[2], n) )
        elseif op == tuple
            return Expr(     :tuple, Any[ valueof(x,n) for x in n.parents[2:end] ]...)
        end

        # default translation
        thing_module(op::DataType) = tuple(fullname(op.name.module)..., op.name.name)

        function thing_module(op::Function)
          fname = isbuiltin(op) ? builtin_name(op) : Base.function_name(op)
          tuple(fullname(Base.function_module(op, @compat Tuple{Vararg{Any}}))...,
                fname)
        end
        # symbol(string(op)) )

        mt = try
                thing_module(op)
             catch e
                error("[tocode] cannot find module of $op ($(typeof(op)))")
             end

        # try to strip module names for brevity
        try
            mt2 = (:Base, mt[end])
            eval(:( $(mexpr(mt)) == $(mexpr(mt2)) )) &&  (mt = mt2)
            mt2 = (mt[end],)
            eval(:( $(mexpr(mt)) == $(mexpr(mt2)) )) &&  (mt = mt2)
        end

        Expr(:call, mexpr( mt ), Any[ valueof(x,n) for x in n.parents[2:end] ]...)
    end

    function translate(n::NExt)
        hasnode(g.exti, n)  || return n.main
        sym = g.exti[n]  # should be equal to n.main but just to be sure..
        hassym(g.exto, sym) || return n.main
        return getnode(g.exto, sym).val  # return node val in parent graph
    end

    function translate(n::NSRef)
        np = n.parents
        (length(np) == 2) && return :( $(valueof(np[1],n)) = $(valueof(np[2],n)) )

        :( $(Expr(:ref, valueof(np[1],n), Any[ valueof(x,n) for x in np[3:end]]...)) = $(valueof(np[2],n)) )
    end

    function translate(n::NSDot)
        np = n.parents
        :( $(Expr(:., valueof(np[1],n), n.main)) = $(valueof(np[2],n)) )
    end

    translate(n::NFor) = Expr(:for,
                              Expr(:(=), n.main[1], valueof(n.parents[1],n)),
                              tocode(n.main[2]))

    ### do a precount of nodes references for speedup
    nref1 = Dict(zip(g.nodes, zeros(Int, length(g.nodes))))
    for n in g.nodes
        for p in n.parents
            nref1[p] += 1
        end
    end

    ### do a precount of nodes references by NFor or setfield/index or getfield/index
    nref2 = Dict(zip(g.nodes, falses(length(g.nodes))))
    for n in g.nodes
        if isa(n, NFor)
            for p in n.parents[2:end]  # ignore index range
                nref2[p] = true
            end
        end
    end

    ### do a precount of nodes references setfield/index or getfield/index
    nref3 = Dict(zip(g.nodes, falses(length(g.nodes))))
    for n in g.nodes
        if isa(n, Union{NSRef, NSDot, NRef, NDot})
            nref3[n.parents[1]] = true
        end
    end

    evalsort!(g)
    out = Any[]
    for n in g.nodes
        n.val = translate(n)       # translate to Expr
        stat, lhs = ispivot(n, g, nref1, nref2, nref3)

        if stat && isa(n, Union{NSRef, NSDot})
            push!(out, n.val)
            n.val = n.parents[1].val

        elseif stat && isa(n, NFor)
            push!(out, n.val)

            g2 = n.main[2]
            valdict = Dict()
            for (k, sym) in g2.seto
              valdict[k] = getnode(g2.seti, sym).val
            end
            n.val = valdict

        elseif stat && (lhs != n.val)
            if lhs == nothing && n == g.nodes[end] # last statement without assignment
                push!(out, :( $(n.val) ))

            else
                ( lhs in [ nosym, nothing] ) && ( lhs = newvar() )
                push!(out, :( $lhs = $(n.val) ))
            end

            n.val = lhs

        end

    end

    return Expr(:block, out...)
end

#####################################################################
#  variable name assigned to this node
#####################################################################
const nosym = 0x7c883061f2344364  # code for no symbol associated

function getname(n::ExNode, g::ExGraph)
    if hasnode(g.seti, n)
        sym = g.seti[n]
        hassym(g.exto, sym) || return sym
        return getnode(g.exto, sym).val
    elseif hasnode(g.exti, n)
        sym = g.exti[n]
        hassym(g.exto, sym) || return sym
        return getnode(g.exto, sym).val
    else
        return nosym
    end

end


#####################################################################
#  tells if an assignment should be created for this node
#####################################################################

# always print nodes that change a variable's state
ispivot(n::Union{NSRef, NSDot, NFor},
        g::ExGraph, nref1, nref2, nref3) = (true, nothing)

# print only if names are linked
function ispivot(n::Union{NExt, NRef, NDot}, g::ExGraph,
                 nref1, nref2, nref3)
    sym = getname(n, g)
    sym != nosym && return (true, sym)

    # it is in the precedence of another node and is (by another path) a parent of an exitnode
    ps = filter(x -> (n in x.precedence), g.nodes)
    if length(ps) > 0
        sv = collect(nodes(g.seti))
        isancestor(n, sv, g, ps) && return (true, nosym)
    end

    # otherwise do not create assignment
    return (false, nothing)
end

# print constants that are named or modified by a for loop
function ispivot(n::NConst, g::ExGraph,
                 nref1, nref2, nref3)
    sym = getname(n, g)
    sym != nosym && return (true, sym)

    # it is used in a setfield/index or getfield/index
    nref3[n] && return (true, nosym)

    for x in filter(x -> isa(x, NFor) && n in x.parents[2:end], g.nodes)
        fg = x.main[2]
        isym = fg.exto[n]
        hassym(fg.exti, isym) && hassym(fg.exti, isym) && return (true, nosym)
    end

    (false, nothing)
end

# print only if used in For loop
function ispivot(n::NIn, g::ExGraph, nref1, nref2, nref3)
    sym = getname(n, g)
    sym != nosym && return (true, sym)

    # it is used in a for loop (except index range)
    nref2[n] && return (true, nosym)

    # it is in the precedence of another node and is (by another path) a parent of an exitnode
    ps = filter(x -> (n in x.precedence), g.nodes)
    if length(ps) > 0
        sv = collect(nodes(g.seti))
        isancestor(n, sv, g, ps) && return (true, nosym)
    end

    (false, nothing)
end


function ispivot(n::Union{NCall, NComp}, g::ExGraph, nref1, nref2, nref3)
    sym = getname(n, g)

    # it has a name assigned
    sym != nosym && return (true, sym)

    # it is used in a for loop (except index range)
    # any(x -> isa(x, NFor) && n in x.parents[2:end], g.nodes) &&
    #   return (true, nosym)
    nref2[n] && return (true, nosym)

    # it is used in a setfield/index or getfield/index
    # any(x -> isa(x, Union{NSRef, NSDot, NRef, NDot}) && n == x.parents[1], g.nodes) &&
    #   return (true, nosym)
    nref3[n] && return (true, nosym)

    # it is used more than once
    # (sum(x -> sum(n .== x.parents), g.nodes) > 1) && return (true, nosym)
    (nref1[n] > 1) && return (true, nosym)

    # it is in the precedence of another node and is (by another path) a parent of an exitnode
    ps = filter(x -> (n in x.precedence), g.nodes)
    if length(ps) > 0
        sv = collect(nodes(g.seti))
        isancestor(n, sv, g, ps) && return (true, nosym)
    end

    # otherwise do not create assignment
    return (false, nothing)
end
