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
    # n = m.NConst(:abcd, [], [], LogNormal(4,5), false)
    # n = m.NConst(:abcd, [], [], Any[1., [1., [1., 3.]]] , false)
    v = n.val

    if isa(v, Real) || isa(v, Symbol) || isa(v, DataType) || isa(v, TypeConstructor)
        return tograph( :(0.) )

    elseif isa(v, Range)
        return tograph( :( zeros(2) ) )

    elseif isa(v, Array) && (eltype(v) <: Real)  # is it an array of Reals ?
        return tograph( :( zeros(size(tv)) ) )

    elseif isa(v, BitArray)  # is it an array of bits ?
        return tograph( :( zeros(size(tv)) ) )

    elseif isa(v, Tuple) && all( map( x -> typeof(x) <: Real, v ) ) # is it a Tuple of Reals ?
        return tograph( :( zeros(length(tv)) ) )

    elseif isa(v, Array) && isleaftype(eltype(v)) # array of concrete type ?
        # n = NConst(:abcd, [], [], [Beta(4,5), Beta(2,3)], false)
        # v = n.val

        # build element constructor
        n2 = NConst(:abcd, [], [], v[1], false)
        ge = zeronode(n2)
        # tocode(ge)

        # build loop sub-graph
        fg = ExGraph()
        ni = addnode!(fg, NExt(:i)) ; fg.exti[ni] = :i
        nv = addnode!(fg, NExt(:v)) ; fg.exti[nv] = :v
        nr = addgraph!(ge, fg, Dict( :tv => nv))
        ns = addnode!(fg, NSRef(:setidx, [nv, nr, ni]) )
        fg.seti[ns] = :v
        # fg
        # tocode(fg)

        # build final graph
        g  = tograph( :( cell( $(length(v)) ) ) )
        nv = getnode(g.seti, nothing) ; fg.exto[nv] = :v
        nt = addnode!(g, NExt(:tv)) ; g.exti[nv] = :tv
        nr = addgraph!( :( 1:length(tv) ), g, Dict( :tv => nt) )
        nf = addnode!(g, NFor( Any[:i, fg], [ nr, nv ]) )
        ns = addnode!(g, NIn( :v, [ nf ]) ) ; fg.seto[ns] = :v
        g.seti[ns] = nothing

        return g
    
    elseif isa(v, Array) && (eltype(v) == Any) # general Array
        error("[zeronode] to be implemented !")

    elseif isa(v, Tuple)  # general Tuple
        error("[zeronode] to be implemented !")

    elseif isleaftype(typeof(v)) # composite type
        g  = tograph( :( cell( $(length(names(v))) ) ) )
        nv = addnode!(g, NExt(:tv))
        g.exti[nv] = :tv
        # TODO : optimize to an array{Float64} instead of array{Any} if all fields are Reals

        for (i, n2) in enumerate(names(typeof(v)))  # i, n2 = 1, :nrmd
            # create node for holding field value
            nf      = addnode!(g, NDot(QuoteNode(n2), [ getnode(g.exti, :tv) ], [], getfield(v, n2), false) ) 

            ng      = zeronode( nf )
            nn      = addgraph!(ng, g, Dict( :tv => nf ))
            ni      = addnode!(g, NConst(i))
            ns      = addnode!(g, NSRef(:setidx, [getnode(g.seti, nothing), nn, ni]))
            g.seti[ns] = nothing
        end

        return prune!(g)

    else
        error("[zeronode] Unable to build diff accumulator for node $(repr(n)[1:min(40, end)])")

    end
end

    # m.tocode( zeronode(  m.NConst(:abcd, [], [], 12      , false) ))
    # m.tocode( zeronode(  m.NConst(:abcd, [], [], 12.2    , false) ))
    # m.tocode( zeronode(  m.NConst(:abcd, [], [], 12.2-4im, false) ))

    # m.tocode( zeronode(  m.NConst(:abcd, [], [], 5:6        , false) ))
    # m.tocode( zeronode(  m.NConst(:abcd, [], [], 5:0.2:10   , false) ))

    # m.tocode( zeronode(  m.NConst(:abcd , [] , [] , Bernoulli(0.5)               , false) ))
    # m.tocode( zeronode(  m.NConst(:abcd , [] , [] , Bernoulli(0.5)               , false) ))
    # m.tocode( zeronode(  m.NConst(:abcd , [] , [] , TDist(0.5)                   , false) ))
    # m.tocode( zeronode(  m.NConst(:abcd , [] , [] , Exponential(0.5)             , false) ))
    # m.tocode( zeronode(  m.NConst(:abcd , [] , [] , Poisson(0.5)                 , false) ))

    # m.tocode( zeronode(  m.NConst(:abcd , [] , [] , Beta(2           , 3)        , false) ) )
    # m.tocode( zeronode(  m.NConst(:abcd , [] , [] , Normal(0.5       , 0.3)      , false) ) )
    # m.tocode( zeronode(  m.NConst(:abcd , [] , [] , Uniform(0.5      , 0.87)     , false) ) )
    # m.tocode( zeronode(  m.NConst(:abcd , [] , [] , Weibull(0.5      , 0.3)      , false) ) )
    # m.tocode( zeronode(  m.NConst(:abcd , [] , [] , Gamma(0.5        , 0.3)      , false) ) )
    # m.tocode( zeronode(  m.NConst(:abcd , [] , [] , Cauchy(0.5       , 0.3)      , false) ) )
    # m.tocode( zeronode(  m.NConst(:abcd , [] , [] , LogNormal(0.5    , 0.3)      , false) ) )
    # m.tocode( zeronode(  m.NConst(:abcd , [] , [] , Binomial(5       , 0.3)      , false) ) )
    # m.tocode( zeronode(  m.NConst(:abcd , [] , [] , Beta(0.5         , 0.3)      , false) ) )
    # m.tocode( zeronode(  m.NConst(:abcd , [] , [] , Laplace(0.5      , 0.3)      , false) ) )


    # type Abcd
    #     a::Float64
    #     b
    #     c::Vector{Float64}
    # end
    # m.tocode( zeronode(  m.NConst(:abcd, [], [], Abcd(2., 3., [1,2 ])        , false) ))

    # type Abcd2
    #     a::Float64
    #     b
    # end
    # m.tocode( zeronode(  m.NConst(:abcd, [], [], Abcd2(2., 3.)        , false) ))


    # m.tocode( zeronode(  m.NConst(:abcd, [], [], [1 , 2 ]        , false) ))
    # m.tocode( zeronode(  m.NConst(:abcd, [], [], [1., 3.] , false) ))
    # m.tocode( zeronode(  m.NConst(:abcd, [], [], [Beta(4,5), Beta(2,3)], false) ))
    # m.tocode( zeronode(  m.NConst(:abcd, [], [], Any[1., Any[1., [1., 3.]]] , false) ))  # fails
