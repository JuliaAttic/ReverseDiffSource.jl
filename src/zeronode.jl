#########################################################################
#
#   zeronode() : Builds the graph generating the diff accumulator starting node
#
#########################################################################
#
#   - Generated graph contains an External node :tv that should be mapped to the source node
#
#########################################################################

# wrapper for nodes, with error message showing nodes
function zeronode(n::ExNode)
    try
        return zeronode(n.val)
    catch e
        error("[zeronode] $e for node $(repr(n)[1:min(40, end)])")
    end
end

## 0. where derivation makes no sense
zeronode(t::Type{Symbol}) = tograph( :(0.) )
zeronode(t::Type{DataType}) = tograph( :(0.) )
zeronode(t::Type{TypeConstructor}) = tograph( :(0.) )
zeronode(t::Type{Type}) = tograph( :(0.) )
zeronode(t::Type{Function}) = tograph( :(0.) )
zeronode(t::Type{Module}) = tograph( :(0.) )

# 0. for scalars
zeronode{T<:Real}(t::Type{T})    = tograph( :(0.) )

# [0.,0.] for ranges and Complex
zeronode{T<:Complex}(t::Type{T}) = tograph( :( zeros(2) ) )
zeronode{T<:Range}(t::Type{T}) = tograph( :( zeros(2) ) )

# Float64 arrays for ..
zeronode{T<:BitArray}(t::Type{T}) = tograph( :( zeros(size(tv)) ) )
zeronode{T<:Real}(t::Type{Array{T}}) = tograph( :( zeros(size(tv)) ) )


function zeronode(v)

    # Arrays or equivalent
    if method_exists(getindex, (v, Int64)) && # can be indexed
       method_exists(eltype, (v,)) &&
       method_exists(size, (v,))              # has a shape

        if eltype(v) <: Real
            return tograph( :( zeros(size(tv)) ) )  # array of zeros

        elseif isleaftype(eltype(v))
            # build element constructor
            n2 = NConst(:abcd, [], [], eltype(v), false)
            ge = zeronode(n2)

            # build loop sub-graph
            fg = ExGraph()
            ni  = addnode!(fg, NExt(:i))  ; fg.exti[ni] = :i    # loop index
            nv  = addnode!(fg, NExt(:v))  ; fg.exti[nv] = :v    # array to fill
            nt  = addnode!(fg, NExt(:tv)) ; fg.exti[nt] = :tv   # ref node
            nt2 = addnode!(fg, NRef(:getidx, [nt, ni]))         # ref node[i]

            nr = addgraph!(ge, fg, Dict( :tv => nt2))
            ns = addnode!(fg, NSRef(:setidx, [nv, nr, ni]) )
            fg.seti[ns] = :v

            # build final graph
            g  = tograph( :( Array(Any, size(tv)) ) )
            nv = getnode(g.seti, nothing) ; fg.exto[nv] = :v
            nt = addnode!(g, NExt(:tv))   ; fg.exto[nt] = :tv ; g.exti[nt] = :tv
            nr = addgraph!( :( 1:length(nv) ), g, Dict( :nv => nv) )
            nf = addnode!(g, NFor( Any[:i, fg], [ nr, nv, nt ]) )
            ns = addnode!(g, NIn( :v, [ nf ]) ) ; fg.seto[ns] = :v
            g.seti[ns] = nothing

            return g
        else
          error("Can't build a diff accumulator for node (avoid Array{Any} types)")
        end

    # Tuples or equivalent
    # TODO  :could tuples give tuple accumulators ?
    elseif method_exists(getindex, (v, Int64)) && # can be indexed
           method_exists(eltype, (v,)) &&
           method_exists(length, (v,))            # has a length (typically a Tuple)

        if eltype(v) <: Real
            return tograph( :( zeros(length(tv)) ) )  # array of zeros

        elseif isleaftype(eltype(v))
            # build element constructor
            n2 = NConst(:abcd, [], [], eltype(v), false)
            ge = zeronode(n2)

            # build loop sub-graph
            fg = ExGraph()
            ni  = addnode!(fg, NExt(:i))  ; fg.exti[ni] = :i    # loop index
            nv  = addnode!(fg, NExt(:v))  ; fg.exti[nv] = :v    # array to fill
            nt  = addnode!(fg, NExt(:tv)) ; fg.exti[nt] = :tv   # ref node
            nt2 = addnode!(fg, NRef(:getidx, [nt, ni]))         # ref node[i]

            nr = addgraph!(ge, fg, Dict( :tv => nt2))
            ns = addnode!(fg, NSRef(:setidx, [nv, nr, ni]) )
            fg.seti[ns] = :v

            # build final graph
            g  = tograph( :( Array(Any, size(tv)) ) )
            nv = getnode(g.seti, nothing) ; fg.exto[nv] = :v
            nt = addnode!(g, NExt(:tv))   ; fg.exto[nt] = :tv ; g.exti[nt] = :tv
            nr = addgraph!( :( 1:length(nv) ), g, Dict( :nv => nv) )
            nf = addnode!(g, NFor( Any[:i, fg], [ nr, nv, nt ]) )
            ns = addnode!(g, NIn( :v, [ nf ]) ) ; fg.seto[ns] = :v
            g.seti[ns] = nothing

            return g
        else
            g  = tograph( :( Array(Any, $(length(v.parameters)) ) ) )
            nv = addnode!(g, NExt(:tv)) ; g.exti[nv] = :tv
            # TODO : optimize to an array{Float64} instead of array{Any} if all fields are Reals

            for i in 1:length(v.parameters)
                ni      = addnode!(g, NConst(i))
                nf      = addnode!(g, NRef(:getidx, [ getnode(g.exti, :tv), ni ], [], v.parameters[i], false) )

                ng      = zeronode( nf )
                nn      = addgraph!(ng, g, Dict( :tv => nf ))
                ns      = addnode!(g, NSRef(:setidx, [getnode(g.seti, nothing), nn, ni]))
                g.seti[ns] = nothing
            end

            return g
        end

    # Composite types or equivalent
    elseif Base.isstructtype(v) && # type with fields
           method_exists(fieldnames, (v,))

        g  = tograph( :( Array(Any, $(length(fieldnames(v))) ) ) )
        nv = addnode!(g, NExt(:tv)) ; g.exti[nv] = :tv

        for (i, n2) in enumerate(fieldnames(v))  # i, n2 = 1, :val
            # create node for holding field value
            nf      = addnode!(g, NDot(QuoteNode(n2), [ getnode(g.exti, :tv) ], [], fieldtype(v, n2), false) )

            ng      = zeronode( nf )
            nn      = addgraph!(ng, g, Dict( :tv => nf ))
            ni      = addnode!(g, NConst(i))
            ns      = addnode!(g, NSRef(:setidx, [getnode(g.seti, nothing), nn, ni]))
            g.seti[ns] = nothing
        end

        return g

    else
        error("Unable to build diff accumulator")

    end
end
