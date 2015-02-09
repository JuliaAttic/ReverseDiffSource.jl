######################  setup   ################################
    Pkg.status()

    cd(joinpath(Pkg.dir("ReverseDiffSource"), "test"))
    include("runtests.jl")
    include("firstorder_tests.jl")
    include("index_tests.jl")

############## adding end and : for ref  #########################
    reload("ReverseDiffSource") ; m = ReverseDiffSource

    tex = quote
        a = zeros(5,5)
        a[1:4,2] = x
        a[3,1:5] = x
        sum(a)
    end

    dex = m.rdiff(tex, x=1.)
    @eval let x=3.; $dex ; end

    g = m.tograph(:( x[:] ))
    m.tocode(g)
    g = m.tograph(:( x[1:4] ))
    m.tocode(g)

    g = m.tograph(:( x[1:end] ))
    m.tocode(g)

    g = m.tograph(:( x[1:end-1] ))
    m.tocode(g)
    g = m.tograph(:( x[1:end, end-3:end-1,:] ))
    m.tocode(g)

############## external symbols resolution  #########################
    reload("ReverseDiffSource") ; m = ReverseDiffSource

    m.tograph( :( sin(x) ))
    g = m.tograph( :( Base.sin(x) ))
    m.simplify!( g )

    g = m.tograph( :( Base.sin(4.) ))
    m.simplify!( g )

    ###################### modules ########################################
        module Abcd
            module Abcd2
                type Argf ; end
                function probe()
                    println(current_module())
                    eval( :( a = 1 ))
                    current_module().eval( :( a = 2 ) )
                end
                function probe2()
                    println(repr(Argf))
                end
            end
        end

        Abcd.Abcd2.probe()


        Abcd.Abcd2.probe2()
        a

        t = Abcd.Abcd2.Argf
        tn = t.name
        tn.module
        fullname(tn.module)

        t = Abcd.Abcd2
        names(t)
        typeof(t)

        tn = t.name
        tn.module
        fullname(tn.module)


        t = Abcd.Abcd2.probe2
    t.module

    reload("ReverseDiffSource") ; m = ReverseDiffSource

    g = m.tograph( :(  max(1.,  x) ) )
    m.simplify!(g)
    m.calc!(g, params=Dict( :x => [1,2,3,4]))
    g

    m.zeronode(g.nodes[1])
    m.zeronode(g.nodes[2])

    dg = m.reversegraph(g, m.getnode(g.seti, nothing), [:x] )
    append!(g.nodes, dg.nodes)
    nn = m.getnode(dg.seti, m.dprefix(:x))   # nn in g.nodes
    ns = m.newvar("_dv")
    g.seti[nn] = ns
    m.tocode(g)
    push!(voi, ns)

    g |> m.splitnary! |> m.prune! |> m.simplify!

    m.rdiff( :( sum(x)), x=ones(5) )

    collect(keys(m.drules))


        append!(g.nodes, dg.nodes)
        nn = addnode!( g, NCall(tuple, [ getnode(dg.seti, dprefix(p)) for p in paramsym] ) )
        ns = newvar("_dv")
        g.seti[nn] = ns
        push!(voi, ns)

        g |> splitnary! |> prune! |> simplify!

    ex
    g = m.tograph(ex)

    g = m.tograph( :( Vector{Float64} ) )


    m.calc!(g, params=Dict( :x => ones(4) ) )

    gs = map( m.zeronode, g.nodes)
    gs

    m.zeronode( g.nodes[15] )


    m.rdiff( :( x[2]) , x=zeros(3))

    f = sin
    typeof(f)
    f2 = methods(f).defs
    typeof(f2)

    Base.function_module(sin) 
    methods(sin)
    Base.function_module(sin, ) 
    methods(max)
    Base.function_module(max)
    Base.function_module(max, (Real, Real))
    Base.function_module(max, (FloatingPoint, FloatingPoint))

    fullname( Base.function_module(Bar) )
    fullname( Base.function_module(sin) )

    Base.MPFR.max(2., 3.)


############### undef error  ###########################
    reload("ReverseDiffSource") ; m = ReverseDiffSource

    module Sandbox
        type Abcd
            a::Float64
            b::Vector{Float64}
        end
        foo(t::Abcd) = t.a + t.b[2]
    end
    m.@deriv_rule Sandbox.Abcd(a,b) a ds[1]
    m.@deriv_rule Sandbox.Abcd(a,b) b ds[2]
    m.@deriv_rule Sandbox.foo(t)    t Any[ ds, [0., ds]]

    tex = quote
        z = Sandbox.Abcd(1., [x, x])
        Sandbox.foo(z)
    end
    x = 3.
    eval(tex)

    dtex = m.rdiff(tex, x=1.)
    eval(dtex)
    dtex = m.rdiff(tex, x=1., order=2)
    eval(dtex)
    dtex = m.rdiff(tex, x=1., order=3)
    eval(dtex)

    tex = quote
        z = [ Sandbox.Abcd(1., [x, x]), Sandbox.Abcd(x*x, [1, x]) ]
        Sandbox.foo(z[1]) + Sandbox.foo(z[2])
    end
    dtex = m.rdiff(tex, x=1.)
    eval(dtex)
    dtex = m.rdiff(tex, x=1., order=2)
    eval(dtex)
    dtex = m.rdiff(tex, x=1., order=3)
    eval(dtex)


    g = m.tograph(tex)
    g.seti = m.NSMap([m.getnode(g.seti, nothing)], [ nothing ])    

    g |> m.splitnary! |> m.prune! |> m.simplify!
    m.calc!(g, params=Dict(:x => 1.))
    voi = Any[ nothing ]

        i = 1
            dg = m.reversegraph(g, m.getnode(g.seti, voi[i]), [:x])
            append!(g.nodes, dg.nodes)
            nn = collect(m.nodes(dg.seti))[1]  # only a single node produced
            ns = m.newvar("_dv")
            g.seti[nn] = ns
            push!(voi, ns)
            m.tocode(g)

            m.splitnary!(g)
            m.tocode(g)
            m.prune!(g)
            m.tocode(g)
            m.simplify!(g)
            m.tocode(g)

            m.calc!(g, params=Dict(:x => 1.))

            m.zeronode(g.nodes[15])


        i = 2
            dg = m.reversegraph(g, m.getnode(g.seti, voi[i]), [:x])
            append!(g.nodes, dg.nodes)
            nn = collect(m.nodes(dg.seti))[1]  # only a single node produced
            ns = m.newvar("_dv")
            g.seti[nn] = ns
            push!(voi, ns)
            m.tocode(g)

            m.splitnary!(g)
            m.tocode(g)
            m.prune!(g)
            m.tocode(g)
            m.simplify!(g)
            m.tocode(g)

            m.calc!(g, params=Dict(:x => 1.))



    dg = m.reversegraph(g, m.getnode(g.seti, nothing), [:x])

    m.drules[(Sandbox.Abcd,1)]
        append!(g.nodes, dg.nodes)


##########################

    tex = quote  # D:\frtestar\.julia\v0.4\ReverseDiffSource\test\indexing.jl, line 128:
        a = zeros(4) # line 129:
        b = zeros(2) # line 131:
        b += x # line 132:
        a[1:2] = b # line 133:
    end
    g = m.tograph(tex)

################  new bug   ##################
    reload("ReverseDiffSource") ; m = ReverseDiffSource
    ex = quote
        a=zeros(3)
        b=zeros(3)
        b[2]=x
        a[1]=x
        sum(a)+sum(b)
    end
    m.rdiff(ex, x=2)

    g = m.tograph(ex)
    outsym = nothing
    g.seti = m.NSMap([m.getnode(g.seti, outsym)], [ outsym ])    

    g |> m.splitnary! |> m.prune! |> m.simplify!
    g |> m.splitnary! |> m.prune!

    g |> m.prune! |> m.simplify!
    m.tocode(g)

    g |> m.prune! |> m.evalsort! |> m.simplify!
    m.tocode(g)

    g |> m.splitnary!
    m.tocode(g)
    g |> m.prune!
    m.tocode(g)
    g |> m.simplify!
    m.tocode(g)

    m.calc!(g, params=Dict(:x => 2))
    m.tocode(g)

    ov = m.getnode(g.seti, outsym).val 
    isa(ov, Real) || error("output var should be a Real, $(typeof(ov)) found")

    voi = Any[ outsym ]

    dg = m.reversegraph(g, m.getnode(g.seti, outsym), [:x])
    append!(g.nodes, dg.nodes)
    m.tocode(g)
    g

    for p in [:x]
        nn = m.getnode(dg.seti, m.dprefix(p))  # find the exit node of deriv of p
        ns = m.newvar("_dv")
        g.seti[nn] = ns
        push!(voi, ns)
    end

    m.tocode(g)
    g |> m.splitnary!
    m.tocode(g)
    g |> m.prune!
    m.tocode(g)
    g |> m.simplify!
    m.tocode(g)

    voin = map( s -> m.getnode(g.seti, s), voi )
    nf = m.addnode!(g, m.NConst(tuple))
    exitnode = m.addnode!(g, m.NCall(:call, [nf, voin...]))
    g.seti = m.NSMap( [exitnode], [nothing])  # make this the only exitnode of interest

    m.tocode(g)
    g |> m.splitnary!
    m.tocode(g)
    g |> m.prune!
    m.tocode(g)
    g |> m.simplify!
    m.tocode(g)



################ evalsort optim  #######################
    reload("ReverseDiffSource") ; m = ReverseDiffSource

    tex = quote  # D:\frtestar\.julia\v0.4\ReverseDiffSource\test\indexing.jl, line 128:
        a = zeros(4) # line 129:
        b = zeros(2) # line 131:
        b += x # line 132:
        a[1:2] = b # line 133:
    end
    g = m.tograph(tex)

    m.markalloc!(g)
    m.evalsort!(g)

    ns = filter(n -> !n.alloc, g.nodes)
    fl = Tuple[]
    for i
    nl = collect( zip(ns, map(n -> (n.main, vcat(n.parents, n.precedence)), ns)) )

    help(sort)
    comp(a,b) = (hash(a[1].main) < hash(b[1].main)) #&& (a[2] < b[2])
    sort!(nl, lt=comp)
    sort!(ns, lt= (a,b) -> b[1] in a[2] & !(a[1] in b[2]))
    hash( nl[1][1])


    ns = collect( zip(g.nodes, map(n -> vcat(n.parents, n.precedence), g.nodes)) )
    help(sort)
    sort!(ns, lt= (a,b) -> length(a[2]) < length(b[2]))
    sort!(ns, lt= (a,b) -> b[1] in a[2] & !(a[1] in b[2]))
    g.nodes = [ n[1] for n in ns ]
    g


    function evalsort!(g)
      ns = m.ExNode[]
      while length(ns) < length(g.nodes)
        canary = length(ns)
        nl = setdiff(g.nodes, ns)
        for n in nl
          any(x -> x in nl, n.parents) && continue
          any(x -> x in nl, n.precedence) && continue
          push!(ns,n)
        end
        (canary == length(ns)) && error("[evalsort!] cycle in graph")
      end
      g.nodes = ns

      g
    end
    function evalsort2!(g)
      nr = Set(g.nodes)
      ns = m.ExNode[]

      while length(nr) > 0
        canary = length(ns)
        for n in nr
          any(x -> x in nr, n.parents) && continue
          any(x -> x in nr, n.precedence) && continue
          push!(ns,n) ; delete!(nr,n)
        end
        (canary == length(ns)) && error("[evalsort!] cycle in graph")
      end
      g.nodes = ns

      g
    end

    evalsort!(g)    
    evalsort2!(g)    

    @time for i in 1:10000; evalsort!(g); end  # 1.10s
    @time for i in 1:10000; evalsort2!(g); end  # 0.14s


#########################################################
    x0 = [1., 1.]
    x = x0
    eval(tex)

    dtex = m.rdiff(tex, x=x)
    x = rand(2,2)
    eval(dtex)

    v2ref


    dtex = m.rdiff(tex, x=1., order=2)

    g = m.tograph(tex)
    g.seti = m.NSMap([m.getnode(g.seti, nothing)], [ nothing ])    

    g |> m.splitnary!
    g |> m.prune!
    g |> m.simplify!

    m.tocode(g)

quote 
    _tmp14 = zeros(4)
    _tmp15 = zeros(2)
    _tmp15 = _tmp15 + x
    _tmp14[1:2] = _tmp15
    _tmp15[1] = _tmp15[1] + x[1]
    _tmp14[3:4] = _tmp15
    _tmp14[_idx2]
end

_idx2

    n = g.nodes[16]

    setindex!(n.parents[1].val, n.parents[2].val, [n2.val for n2 in n.parents[3:end]]...)
    n.parents[1].val


    m.calc!(g, params=Dict(:x => x0))
    g


    voi = Any[ nothing ]

        i = 1
            dg = m.reversegraph(g, m.getnode(g.seti, voi[i]), [:x])
            append!(g.nodes, dg.nodes)
            nn = collect(m.nodes(dg.seti))[1]  # only a single node produced
            ns = m.newvar("_dv")
            g.seti[nn] = ns
            push!(voi, ns)
            m.tocode(g)

            m.splitnary!(g)
            m.tocode(g)
            m.prune!(g)
            m.tocode(g)
            m.simplify!(g)
            m.tocode(g)

            m.calc!(g, params=Dict(:x => 1.))

            m.zeronode(g.nodes[15])


###################### rules rewrite   ######################################
    reload("ReverseDiffSource") ; m = ReverseDiffSource
    @deriv_rule reverse(x)   x     0.

    m.rdiff( :(sin(x)) , order=3, x=2.)

###################### rules rewrite   ######################################
    using Distributions
    reload("ReverseDiffSource") ; m = ReverseDiffSource

    function zeronode(n)  
        # n = m.NConst(:abcd, [], [], LogNormal(4,5), false)
        # n = m.NConst(:abcd, [], [], Any[1., [1., [1., 3.]]] , false)
        # n = m.NConst(:abcd, [], [], (4., 5.), false)
        # n = m.NConst(:abcd, [], [], Any[1., [1., [1., 3.]]] , false)
        v = Bool[true,true,false,true,true,false]


        v = n.val

        if isa(v, Real) || isa(v, Symbol)
            return m.tograph( :(0.) )

        elseif isa(v, Range)
            return m.tograph( :( zeros(2) ) )

        elseif isa(v, Array) && (eltype(v) <: Real)  # is it an array of Reals ?
            g          = m.ExGraph()
            v1         = m.addnode!(g, m.NExt(:tv)) ; g.exti[v1] = :tv

            exitnode = m.addnode!(g, m.NCall(zeros, [v1]))
            g.seti[exitnode] = nothing
            return g

        elseif isa(v, Tuple) && all( map( x -> typeof(x) <: Real, v ) ) # is it a Tuple of Reals ?
            g  = m.ExGraph()
            v1 = m.addnode!(g, m.NExt(:tv)) ; g.exti[v1] = :tv

            exitnode = m.addnode!(g, m.NCall(zeros, [v1]))
            g.seti[exitnode] = nothing
            return g

        elseif isa(v, Array) && isleaftype(eltype(v)) # array of a single concrete type ?
            # n = m.NConst(:abcd, [], [], [Beta(4,5), Beta(2,3)], false)
            # v = n.val

            # build element constructor
            n2 = m.NConst(:abcd, [], [], v[1], false)
            ge = zeronode(n2)
            # m.tocode(ge)

            # build loop sub-graph
            fg = m.ExGraph()
            ni = m.addnode!(fg, m.NExt(:i)) ; fg.exti[ni] = :i
            nv = m.addnode!(fg, m.NExt(:v)) ; fg.exti[nv] = :v
            nr = m.addgraph!(ge, fg, Dict( :tv => nv))
            ns = m.addnode!(fg, m.NSRef(:setidx, [nv, nr, ni]) )
            fg.seti[ns] = :v
            # fg
            # m.tocode(fg)

            # build final graph
            g  = m.tograph( :( cell( $(length(v)) ) ) )
            nv = m.getnode(g.seti, nothing) ; fg.exto[nv] = :v
            nt = m.addnode!(g, m.NExt(:tv)) ; g.exti[nv] = :tv
            nr = m.addgraph!( :( 1:size(tv) ), g, Dict( :tv => nt) )
            nf = m.addnode!(g, m.NFor( Any[:i, fg], [ nr, nv ]) )
            ns = m.addnode!(g, m.NIn( :v, [ nf ]) ) ; fg.seto[ns] = :v
            g.seti[ns] = nothing

            return g
        
        elseif isa(v, Array) && (eltype(v) == Any) # general Array
            error("[zeronode] to be implemented !")

        elseif isa(v, Tuple) # general Tuple
            error("[zeronode] to be implemented !")

        elseif isleaftype(typeof(v)) # composite type
            g  = m.tograph( :( cell( $(length(names(v))) ) ) )
            nv = m.addnode!(g, m.NExt(:tv))
            g.exti[nv] = :tv
            # TODO : optimize to an array{Float64} instead of array{Any} if all fields are Reals

            for (i, n2) in enumerate(names(typeof(v)))  # i, n2 = 1, :nrmd
                # create node for holding field value
                nf      = m.addnode!(g, m.NDot(QuoteNode(n2), [ m.getnode(g.exti, :tv) ], [], getfield(v, n2), false) ) 

                ng      = zeronode( nf )
                nn      = m.addgraph!(ng, g, Dict( :tv => nf ))
                ni      = m.addnode!(g, m.NConst(i))
                ns      = m.addnode!(g, m.NSRef(:setidx, [m.getnode(g.seti, nothing), nn, ni]))
                g.seti[ns] = nothing
            end

            return m.prune!(g)

        else
            error("[zeronode] Unable to build diff accumulator for node $(repr(n)[1:min(40, end)])")

        end
    end

###################### issue #8   ######################################
    reload("ReverseDiffSource") ; m = ReverseDiffSource

    m.drules[(+,1)]


    ex = :( (1 - x[1])^2 + 100(x[2] - x[1]^2)^2 )
    res = m.rdiff(ex, x=zeros(2), order=2)   
    res = m.rdiff(ex, x=zeros(2), order=3)   # 72  lines (devl3)
    res = m.rdiff(ex, x=zeros(2), order=4)   # 200 lines

    @eval foo(x) = $res
    foo([0.5, 2.])

    (306.5,[-351.0,350.0],
    2x2 Array{Float64,2}:
     -498.0  -200.0
     -200.0   200.0,

    2x2x2 Array{Float64,3}:
    [:, :, 1] =
     1200.0  -400.0
     -400.0     0.0

    [:, :, 2] =
     -400.0  0.0
        0.0  0.0)

    δ = 1e-8
    1/δ * (foo([0.5+δ, 2.])[1] - foo([0.5, 2.])[1])  # - 351, ok
    1/δ * (foo([0.5+δ, 2.])[2] - foo([0.5, 2.])[2])  # ok
    1/δ * (foo([0.5+δ, 2.])[3] - foo([0.5, 2.])[3])  # ok
    #=    2x2 Array{Float64,2}:
         1200.0  -400.0
         -400.0     0.0=#

    1/δ * (foo([0.5, 2.+δ])[1] - foo([0.5, 2.])[1])  # 350, ok
    1/δ * (foo([0.5, 2.+δ])[2] - foo([0.5, 2.])[2])  # ok
    1/δ * (foo([0.5, 2.+δ])[3] - foo([0.5, 2.])[3])  # ok
    # 2x2 Array{Float64,2}:
    #  -400.0  0.0
    #     0.0  0.0

###################### issue #8   ######################################
    reload("ReverseDiffSource") ; m = ReverseDiffSource

    # ex = :( x[1]^3 )  # ok
    # ex = :( x[2]^3 )  # ok
    # ex = :( x[1]^2 + x[2]^2 ) # ok
    ex = :( x[1]^3 + x[2]^3 ) # pb cf plus bas
    # ex = :( x[1]^3 + x[1]*x[2]^2 )
    # ex = :( sum(x * x'))

    x0 = ones(2)
    res = m.rdiff(ex, x=x0, order=2)
    res = m.rdiff(ex, x=x0, order=4)

    #=ERROR: key not found: [subref] :setidx ([0.0 0.0
     0.0 0.0]), from = :_dtmp1 / 0.0 / :colon
     in rev at D:\frtestar\.julia\v0.4\ReverseDiffSource\src\reversegraph.jl:164=#

    @eval foo(x) = $res
    foo(x0)
    # (2.0,[3.0,3.0],
    # 2x2 Array{Float64,2}:
    #  6.0  0.0
    #  0.0  6.0,

    # 2x2x2 Array{Float64,3}:
    # [:, :, 1] =
    #  6.0  0.0
    #  0.0  0.0

    # [:, :, 2] =
    #  0.0  0.0
    #  0.0  6.0)

    δ = 1e-8
    1/δ * (foo(x0+[δ, 0])[1] - foo(x0)[1])  # 3, ok
    1/δ * (foo(x0+[δ, 0])[2] - foo(x0)[2])  # ok
    1/δ * (foo(x0+[δ, 0])[3] - foo(x0)[3])  # faux
    # 2x2 Array{Float64,2}:
    #  6.0  0.0
    #  0.0  0.0

    1/δ * (foo(x0+[0, δ])[1] - foo(x0)[1])  # 3, ok
    1/δ * (foo(x0+[0, δ])[2] - foo(x0)[2])  # ok
    1/δ * (foo(x0+[0, δ])[3] - foo(x0)[3])  # faux
    # 2x2 Array{Float64,2}:
    #  0.0  0.0
    #  0.0  6.0
