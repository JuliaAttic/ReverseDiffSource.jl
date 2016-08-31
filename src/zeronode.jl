#########################################################################
#
#   zeronode() : Builds the graph generating the diff accumulator starting node
#
#########################################################################
#
#   - Generated graph contains an External node :tv that should be mapped to the source node
#
#########################################################################

function zeronode(n)
    v = n.val

    if isa(v, Union{Real, Symbol, DataType, TypeConstructor, Function, Module})
        return tograph( :(0.) )

    elseif isa(v, Range)
        return tograph( :( zeros(2) ) )

    elseif isa(v, Array) && (eltype(v) <: Real)  # is it an array of Reals ?
        return tograph( :( zeros(size(tv)) ) )

    elseif isa(v, BitArray)  # is it an array of bits ?
        return tograph( :( zeros(size(tv)) ) )

    elseif isa(v, Tuple) && all( map( x -> typeof(x) <: Real, v ) ) # is it a Tuple of Reals ?
        return tograph( :( zeros(length(tv)) ) )

    elseif (isa(v, Array) && isleaftype(eltype(v))) ||  # array of concrete type ?
           (isa(v, Array) && (eltype(v) == Any)) && (length(v) > 20) # large Array{Any}
           # IMPORTANT : if longer than 20, we will presume that elements
           #  of Array{Any} arrays are identical in structure

        # build element constructor
        n2 = NConst(:abcd, [], [], v[1], false)
        ge = zeronode(n2)

        # build loop sub-graph
        fg = ExGraph()
        ni  = addnode!(fg, NExt(:i))  ; fg.exti[ni] = :i    # loop index
        nv  = addnode!(fg, NExt(:v))  ; fg.exti[nv] = :v    # array to fill
        nt  = addnode!(fg, NExt(:tv)) ; fg.exti[nt] = :tv   # ref node
        nt2 = addnode!(fg, NRef(:getidx, [nt, ni]))         # ref node[i]

        nr = addgraph!(ge, fg, @compat Dict( :tv => nt2))
        ns = addnode!(fg, NSRef(:setidx, [nv, nr, ni]) )
        fg.seti[ns] = :v

        # build final graph
        g  = tograph( :( Array(Any, size(tv)) ) )
        nv = getnode(g.seti, nothing) ; fg.exto[nv] = :v
        nt = addnode!(g, NExt(:tv))   ; fg.exto[nt] = :tv ; g.exti[nt] = :tv
        nr = addgraph!( :( 1:length(nv) ), g, @compat Dict( :nv => nv) )
        nf = addnode!(g, NFor( Any[:i, fg], [ nr, nv, nt ]) )
        ns = addnode!(g, NIn( :v, [ nf ]) ) ; fg.seto[ns] = :v
        g.seti[ns] = nothing

        return g

    elseif (isa(v, Array) && (eltype(v) == Any)) ||  # small Array{Any} (presumably coming from a type)
            isa(v, Tuple) # or tuple
        g  = tograph( :( Array(Any, $(length(v)) ) ) )
        nv = addnode!(g, NExt(:tv)) ; g.exti[nv] = :tv
        # TODO : optimize to an array{Float64} instead of array{Any} if all fields are Reals

        for i in 1:length(v)
            ni      = addnode!(g, NConst(i))
            nf      = addnode!(g, NRef(:getidx, [ getnode(g.exti, :tv), ni ], [], v[i], false) )

            ng      = zeronode( nf )
            nn      = addgraph!(ng, g, @compat Dict( :tv => nf ))
            ns      = addnode!(g, NSRef(:setidx, [getnode(g.seti, nothing), nn, ni]))
            g.seti[ns] = nothing
        end

        return g

    elseif isleaftype(typeof(v)) # composite type
        g  = tograph( :( Array(Any, $(length(fieldnames(v))) ) ) )
        nv = addnode!(g, NExt(:tv)) ; g.exti[nv] = :tv
        # TODO : optimize to an array{Float64} instead of array{Any} if all fields are Reals

        for (i, n2) in enumerate(fieldnames(typeof(v)))  # i, n2 = 1, :val
            # create node for holding field value
            nf      = addnode!(g, NDot(QuoteNode(n2), [ getnode(g.exti, :tv) ], [], getfield(v, n2), false) )

            ng      = zeronode( nf )
            nn      = addgraph!(ng, g, @compat Dict( :tv => nf ))
            ni      = addnode!(g, NConst(i))
            ns      = addnode!(g, NSRef(:setidx, [getnode(g.seti, nothing), nn, ni]))
            g.seti[ns] = nothing
        end

        return g

    else
        error("[zeronode] Unable to build diff accumulator for node $(repr(n)[1:min(40, end)])")

    end
end
