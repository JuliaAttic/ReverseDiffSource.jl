######################  setup   ################################
    Pkg.status()

    cd(joinpath(Pkg.dir("ReverseDiffSource"), "test"))
    include("runtests.jl")

###################### rules rewrite   ######################################
    reload("ReverseDiffSource") ; m = ReverseDiffSource
    @deriv_rule reverse(x)   x     0.

    m.rdiff( :(sin(x)) , order=3, x=2.)


    reload("ReverseDiffSource") ; m = ReverseDiffSource
    using Distributions

    function zeronode(n)  
        # n = m.NConst(:abcd, [], [], LogNormal(4,5), false)
        n = m.NConst(:abcd, [], [], [1., [1., [1., 3.]]] , false)
        v = n.val

        isa(v, Real)   && return m.tograph( :(0.) )
        isa(v, Symbol) && return m.tograph( :(0.) )
        isa(v, Range)  && return m.tograph( :( zeros(2) ) )

        if isa(v, Array) && (eltype(v) <: Real)  # is it an array of Reals ?
            g        = m.ExGraph()
            exitnode = m.addnode!(g, m.NCall(zeros, [n]))
            g.seti[exitnode] = nothing
            return g

        ## TODO : add Array{Any}  (needs to find size() trick for arrays containing arrays)
        else # composite type
            g        = m.ExGraph()
            g.exto[n]                            = :tv
            g.exti[ m.addnode!(g, m.NExt(:tv)) ] = :tv

            exitnode = m.addnode!(g, m.NCall(vcat))
            g.seti[exitnode] = nothing

            for n2 in names(typeof(v))  # n2 = :nrmd
                nf      = m.addnode!(g, m.NConst( 10, [n]) )  # create node for this field
                nf.val  = getfield(v, n2)
                ng      = zeronode( nf )
                nn      = m.addgraph!( ng, g, Dict( :tv => nf ))
                # g.nodes = vcat(g.nodes, ng.nodes)
                push!(exitnode.parents, nn)
            end

            return m.prune!(g)

        end
    end

    m.tocode( zeronode(  m.NConst(:abcd, [], [], 12      , false) ))
    m.tocode( zeronode(  m.NConst(:abcd, [], [], 12.2    , false) ))
    m.tocode( zeronode(  m.NConst(:abcd, [], [], 12.2-4im, false) ))

    m.tocode( zeronode(  m.NConst(:abcd, [], [], 5:6        , false) ))
    m.tocode( zeronode(  m.NConst(:abcd, [], [], 5:0.2:10   , false) ))
    m.tocode( zeronode(  m.NConst(:abcd, [], [], Beta(4,5)  , false) ))
    m.tocode( zeronode(  m.NConst(:abcd, [], [], Normal(4,5), false) ))
    m.tocode( zeronode(  m.NConst(:abcd, [], [], LogNormal(4,5), false) ))

    m.tocode( zeronode(  m.NConst(:abcd, [], [], [1,2 ]        , false) ))
    m.tocode( zeronode(  m.NConst(:abcd, [], [], [Beta(4,5), Beta(2,3)], false) ))
    m.tocode( zeronode(  m.NConst(:abcd, [], [], [1., 3.] , false) ))
    m.tocode( zeronode(  m.NConst(:abcd, [], [], Any[1., Any[1., [1., 3.]]] , false) ))


    m.tocode( zeronode(  m.NConst(:abcd, [], [], 12.2-4im, false) ))

    m.tocode(equivzeronode(  5:6 ))

    m.tocode(equivzeronode(  Beta(2,3) ))

    m.tocode(equivzeronode(  Bernoulli(0.5) ))

    m.tocode(equivzeronode(  Bernoulli(0.5) ))
    m.tocode(equivzeronode(  TDist(0.5) ))
    m.tocode(equivzeronode(  Exponential(0.5) ))
    m.tocode(equivzeronode(  Poisson(0.5) ))

    m.tocode(equivzeronode( Normal(0.5,0.3) ) )
    m.tocode(equivzeronode( Uniform(0.5,0.87) ) )
    m.tocode(equivzeronode( Weibull(0.5,0.3) ) )
    m.tocode(equivzeronode( Gamma(0.5,0.3) ) )
    m.tocode(equivzeronode( Cauchy(0.5,0.3) ) )
    m.tocode(equivzeronode( LogNormal(0.5,0.3) ) )
    m.tocode(equivzeronode( Binomial(5,0.3) ) )  
    m.tocode(equivzeronode( Beta(0.5,0.3) ) )
    m.tocode(equivzeronode( Laplace(0.5,0.3) ) )

###################### issue #8   ######################################
    reload("ReverseDiffSource") ; m = ReverseDiffSource

    m.drules[(+,1)]


    ex = :( (1 - x[1])^2 + 100(x[2] - x[1]^2)^2 )
    res = m.rdiff(ex, x=zeros(2), order=2)   
    res = m.rdiff(ex, x=zeros(2), order=3)   # 66/69 lines  72  (devl2)
    res = m.rdiff(ex, x=zeros(2), order=4)   #  error

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

#################   debug  ###################
    ex = :( a = x ; b = a )
    ex = :( a = x ; b = a ; a + b)
    ex = :( a = x ; b = a ; c = b ; b)
    ex = :( b = 0 ; a=x ; b = 3a ; b*x ) 

    res = m.rdiff(ex, x=1.)
    g = m.tograph(ex)
    m.tocode(g)

    @eval foo(x) = $res
    foo(1)
    foo(1.01)


    #### function rdiff(ex; outsym=nothing, order::Int=1, evalmod=Main, params...)
    reload("ReverseDiffSource") ; m = ReverseDiffSource
    begin
        order = 1
        paramsym    = Symbol[ :x ]
        paramvalues = Any[ 1. ]
        parval      = Dict(paramsym, paramvalues)
        g = m.tograph(ex)
        # reduce to variable of interest
        g.seti = m.BiDict{m.ExNode,Any}([g.seti.vk[nothing]], [ nothing ])    

        g |> m.splitnary! |> m.prune! |> m.simplify!
        m.calc!(g, params=parval, emod=Main)

        voi = Any[ nothing ]

        dg = m.reversegraph(g, g.seti.vk[nothing], paramsym)
        append!(g.nodes, dg.nodes)
        nn = m.addnode!( g, m.NCall(:tuple, [ dg.seti.vk[m.dprefix(p)] for p in paramsym] ) )
        ns = m.newvar("_dv")
        g.seti[nn] = ns
        push!(voi, ns)
    end

    # g |> m.splitnary! |> m.prune! |> m.simplify!

    m.tocode(g)
    m.prune!(g) 

    g2 = g.nodes[8].main[2]
    m.tocode(g2)  

    m.ispivot(g2.nodes[5], g2)

    g3 = m.copy(g2)
    g2 =m.copy(g3)

    m.fusenodes(g2, g2.nodes[4], g2.nodes[5])
    g2
    m.fusenodes(g2, g2.nodes[2], g2.nodes[5])

    m.simplify!(g2)
    m.tocode(g2)

    # initial
        node | symbol    | ext ? | type       | parents | precedence | main    | value          | 
        ---- | --------- | ----- | ---------- | ------- | ---------- | ------- | -------------- | 
        1    |           |       | [constant] |         |            | 4       | Int64 4        | 
        2    | _dtmp1 >> | +     | [external] |         |            | :_dtmp1 | Float64 NaN    | 
        3    | _dtmp2 >> | +     | [external] |         |            | :_dtmp2 | Float64 NaN    | 
        4    |           |       | [constant] |         |            | 0.0     | Float64 0.0    | 
        5    |           |       | [constant] |         |            | 0.0     | Float64 0.0    | 
        6    |           |       | [call]     | 4, 2    |            | :+      | Symbol :_tmp4  | 
        7    | _dtmp1 << |       | [subref]   | 2, 5    | 6          | :setidx | Float64 NaN    | 
        8    |           |       | [call]     | 1, 6    |            | :*      | Expr :(4_tmp4) | 
        9    | _dtmp2 << | +     | [call]     | 3, 8    |            | :+      | Float64 NaN    | 

    # identical
        node | symbol    | ext ? | type       | parents | precedence | main    | value          | 
        ---- | --------- | ----- | ---------- | ------- | ---------- | ------- | -------------- | 
        1    |           |       | [constant] |         |            | 4       | Int64 4        | 
        2    | _dtmp1 >> | +     | [external] |         |            | :_dtmp1 | Float64 NaN    | 
        3    | _dtmp2 >> | +     | [external] |         |            | :_dtmp2 | Float64 NaN    | 
        4    |           |       | [constant] |         |            | 0.0     | Float64 0.0    | 
        5    |           |       | [call]     | 4, 2    |            | :+      | Symbol :_tmp4  | 
        6    | _dtmp1 << |       | [subref]   | 2, 4    | 5          | :setidx | Float64 NaN    | 
        7    |           |       | [call]     | 1, 5    |            | :*      | Expr :(4_tmp4) | 
        8    | _dtmp2 << | +     | [call]     | 3, 7    |            | :+      | Float64 NaN    | 


        m.evalconstants(g2.nodes[1], g2, Main)
        m.evalconstants(g2.nodes[2], g2, Main)
        m.evalconstants(g2.nodes[3], g2, Main)
        m.evalconstants(g2.nodes[4], g2, Main)
        m.evalconstants(g2.nodes[5], g2, Main)  # true
            node | symbol    | ext ? | type       | parents | precedence | main    | value          | 
            ---- | --------- | ----- | ---------- | ------- | ---------- | ------- | -------------- | 
            1    |           |       | [constant] |         |            | 4       | Int64 4        | 
            2    | _dtmp1 >> | +     | [external] |         |            | :_dtmp1 | Symbol :_tmp3  | 
            3    | _dtmp2 >> | +     | [external] |         |            | :_dtmp2 | Symbol :_tmp2  | 
            4    |           |       | [constant] |         |            | 0.0     | Float64 0.0    | 
            5    | _dtmp1 << |       | [subref]   | 2, 4    | 8          | :setidx | Symbol :_tmp3  | 
            6    |           |       | [call]     | 1, 8    |            | :*      | Expr :(4_tmp5) | 
            7    | _dtmp2 << | +     | [call]     | 3, 6    |            | :+      | Symbol :_tmp2  | 
            8    |           |       | [constant] |         |            | 1.0     | Float64 NaN    | 


    # identical + evalconstants
        node | symbol    | ext ? | type       | parents | precedence | main    | value       | 
        ---- | --------- | ----- | ---------- | ------- | ---------- | ------- | ----------- | 
        1    |           |       | [constant] |         |            | 4       | Int64 4     | 
        2    | _dtmp1 >> | +     | [external] |         |            | :_dtmp1 | Float64 NaN | 
        3    | _dtmp2 >> | +     | [external] |         |            | :_dtmp2 | Float64 NaN | 
        4    |           |       | [constant] |         |            | 0.0     | Float64 0.0 | 
        5    | _dtmp1 << |       | [subref]   | 2, 4    | 7          | :setidx | Float64 NaN | 
        6    | _dtmp2 << | +     | [call]     | 3, 1    |            | :+      | Float64 NaN | 
        7    |           |       | [constant] |         |            | 1.0     | Float64 NaN | 

    # sans rules
        node | symbol    | ext ? | type       | parents | precedence | main    | value       | 
        ---- | --------- | ----- | ---------- | ------- | ---------- | ------- | ----------- | 
        1    |           |       | [constant] |         |            | 4       | Int64 4     | 
        2    | _dtmp1 >> | +     | [external] |         |            | :_dtmp1 | Float64 NaN | 
        3    | _dtmp2 >> | +     | [external] |         |            | :_dtmp2 | Float64 NaN | 
        4    |           |       | [constant] |         |            | 0.0     | Float64 0.0 | 
        5    |           |       | [constant] |         |            | 1.0     | Float64 1.0 | 
        6    | _dtmp1 << |       | [subref]   | 2, 4    | 5          | :setidx | Float64 NaN | 
        7    | _dtmp2 << | +     | [call]     | 3, 1    |            | :+      | Float64 NaN | 


    ########## simplify  ############
    m.eval(quote 
        function simplify!(g::ExGraph, emod = Main)
            i = 1
            markalloc!(g)
            while i <= length(g.nodes)
                restart = false
                n = g.nodes[i]

                restart = any(n2 -> identical(n, n2, g), g.nodes[i+1:end]) ||
                    evalconstants(n, g, emod) ||
                    rule1(n, g) ||
                    rule2(n, g) ||
                    rule3(n, g) #||
                    #rule4(n, g) ||
                    #rule5(n, g) ||
                    #rule6(n, g) ||
                    #rule7(n, g) ||
                    #rule8(n, g) ||
                    #rule9(n, g) ||
                    #rule10(n, g)
                
                if restart
                    markalloc!(g)
                    i = 1
                else
                    i += 1
                end
            end
            g
        end
    end )

    ##### rule 3  ###################
    n = g2.nodes[6]
    function rule3(n, g)
        !isa(n, m.NCall)             && return false
        (length(n.parents) != 2)   && return false # restricted to binary ops
        val = m.constequiv(n.parents[1], g2)
        (val == nothing)           && return false
        # !isa(n.parents[1], NConst) && return false

        if val == 0 && in(n.main, [:+, :.+])
            m.fusenodes(g2, n.parents[2], n)
            return true

        elseif val == 1 && in(n.main, [:*, :.*])
            fusenodes(g, n.parents[2], n)
            return true

        else
            return false
        end
    end

############## debug 2   ##################
    ex = :( x[1]^3 + x[1]*x[2] )
    res = m.rdiff(ex, x=zeros(2), order=3)   

    ex = :( a = x ; b = a * 3 ; b * x )


    #### function rdiff(ex; outsym=nothing, order::Int=1, evalmod=Main, params...)
    reload("ReverseDiffSource") ; m = ReverseDiffSource
    begin
        order = 3
        paramsym    = Symbol[ :x ]
        paramvalues = Any[ [1., 0] ]
        parval      = Dict(paramsym, paramvalues)
        g = m.tograph(ex)
        # reduce to variable of interest
        g.seti = m.BiDict{m.ExNode,Any}([g.seti.vk[nothing]], [ nothing ])    

        g |> m.splitnary! |> m.prune! |> m.simplify!
        m.calc!(g, params=parval, emod=Main)

        voi = Any[ nothing ]
    end


    #### pass 1 
        begin
            dg = m.reversegraph(g, g.seti.vk[nothing], paramsym)
            append!(g.nodes, dg.nodes)
            ns = m.newvar(:_dv)
            g.seti[ collect(keys(dg.seti))[1] ] = ns
            push!(voi, ns)

            g |> m.splitnary! |> m.prune! |> m.simplify!
        end
    #### pass 2 
        i = 2
        # now order 2 to n
        begin  
            # launch derivation on a single value of the preceding
            #   derivation vector
            no = g.seti.vk[voi[i]]
            si = m.newvar(:_idx)
            ni = m.addnode!(g, m.NExt(si))
            ns = m.addnode!(g, m.NRef(:getidx, [ no, ni ]))

            m.calc!(g, params=Dict([paramsym, si], [paramvalues, 1.]), emod=Main)
            dg = m.reversegraph(g, ns, paramsym)

            #### We will now wrap dg in a loop scanning all the elements of 'no'
            # first create ext nodes to make dg a complete subgraph
            dg2 = m.ExNode[]
            nmap = Dict()
            for n in dg.nodes  # n = dg.nodes[2]
                for (j, np) in enumerate(n.parents)  # j,np = 1, n.parents[1]
                    if haskey(nmap, np) # already remapped
                        n.parents[j] = nmap[np]

                    elseif np == ni # it's the loop index
                        nn = m.NExt(si)
                        push!(dg2, nn)
                        dg.exti[nn] = si
                        n.parents[j] = nn
                        nmap[np] = nn

                    elseif np == ns # it's the selected element of the deriv vector
                        # create 'no' ref if needed
                        if !haskey(nmap, no)
                            sn = m.newvar()
                            nn = m.NExt(sn)
                            push!(dg2, nn)
                            dg.exti[nn] = sn
                            dg.exto[no] = sn
                            nmap[no] = nn
                        end

                        nn = m.NRef(:getidx, [ nmap[no], nmap[ni] ])
                        push!(dg2, nn)
                        nmap[ns] = nn                            

                    elseif !(np in dg.nodes) # it's not in dg (but in g)
                        sn = m.newvar()
                        nn = m.NExt(sn)
                        push!(dg2, nn)
                        dg.exti[nn] = sn
                        dg.exto[np] = sn
                        n.parents[j] = nn
                        nmap[np] = nn

                    end    
                end

                # update onodes in for loops
                if isa(n, m.NFor)
                    g2 = n.main[2]
                    for (o,s) in g2.exto
                        if haskey(nmap, o)
                            g2.exto[ nmap[o] ] = s  # replace
                        end
                    end
                end
            end
            append!(dg.nodes, dg2)    
            # dg |> prune! |> simplify!

            # create for loop node
            nf = m.addnode!(g, m.NFor(Any[ si, dg ] ) )

            # create param size node
            nsz = m.addgraph!( :( length( x ) ), g, [ :x => g.exti.vk[paramsym[1]] ] )

            # create (n-1)th derivative size node
            ndsz = m.addgraph!( :( sz ^ $(i-1) ), g, [ :sz => nsz ] )

            # create index range node
            nid = m.addgraph!( :( 1:dsz ),  g, [ :dsz => ndsz ] )
            push!(nf.parents, nid)

            # pass size node inside subgraph
            sst = m.newvar()
            inst = m.addnode!(dg, m.NExt(sst))
            dg.exti[inst] = sst
            dg.exto[nsz]  = sst
            push!(nf.parents, nsz)

            # create result node (alloc in parent graph)
            nsa = m.addgraph!( :( zeros( $( Expr(:tuple, [:sz for j in 1:i]...) ) ) ), g, [ :sz => nsz ] )
            ssa = m.newvar()
            insa = m.addnode!(dg, m.NExt(ssa))
            dg.exti[insa] = ssa
            dg.exto[nsa]  = ssa
            push!(nf.parents, nsa)

            # create result node update (in subgraph)
            nres = m.addgraph!( :( res[ ((sidx-1)*st+1):(sidx*st) ] = dx ; res ), dg, 
                                [ :res  => insa,
                                  :sidx => nmap[ni],
                                  :st   => inst,
                                  :dx   => collect(dg.seti)[1][1] ] )
            dg.seti = m.NSMap(Dict([nres], [ssa]))

            # create exit node for result
            nex = m.addnode!(g, m.NIn(ssa, [nf]))
            dg.seto = m.NSMap(Dict([nex], [ssa]))

            # update parents of for loop
            append!( nf.parents, setdiff(collect( keys(dg.exto)), nf.parents[2:end]) )

            ns = m.newvar(:_dv)
            g.seti[nex] = ns
            push!(voi, ns)

            g |> m.splitnary! |> m.prune! |> m.simplify!
            
            m.calc!(g, params=Dict(paramsym, paramvalues), emod=Main)
        end

    #### pass 3 
        # now order 2 to n
        begin  
            i = 3
            # launch derivation on a single value of the preceding
            #   derivation vector
            no = g.seti.vk[voi[i]]
            si = m.newvar(:_idx)
            ni = m.addnode!(g, m.NExt(si))
            ns = m.addnode!(g, m.NRef(:getidx, [ no, ni ]))

            m.calc!(g, params=Dict([paramsym, si], [paramvalues, 1.]), emod=Main)
            dg = m.reversegraph(g, ns, paramsym)

            #### We will now wrap dg in a loop scanning all the elements of 'no'
            # first create ext nodes to make dg a complete subgraph
            dg2 = m.ExNode[]
            nmap = Dict()
            for n in dg.nodes  # n = dg.nodes[2]
                for (j, np) in enumerate(n.parents)  # j,np = 1, n.parents[1]
                    if haskey(nmap, np) # already remapped
                        n.parents[j] = nmap[np]

                    elseif np == ni # it's the loop index
                        nn = m.NExt(si)
                        push!(dg2, nn)
                        dg.exti[nn] = si
                        n.parents[j] = nn
                        nmap[np] = nn

                    elseif np == ns # it's the selected element of the deriv vector
                        # create 'no' ref if needed
                        if !haskey(nmap, no)
                            sn = m.newvar()
                            nn = m.NExt(sn)
                            push!(dg2, nn)
                            dg.exti[nn] = sn
                            dg.exto[no] = sn
                            nmap[no] = nn
                        end

                        nn = m.NRef(:getidx, [ nmap[no], nmap[ni] ])
                        push!(dg2, nn)
                        nmap[ns] = nn                            

                    elseif !(np in dg.nodes) # it's not in dg (but in g)
                        sn = m.newvar()
                        nn = m.NExt(sn)
                        push!(dg2, nn)
                        dg.exti[nn] = sn
                        dg.exto[np] = sn
                        n.parents[j] = nn
                        nmap[np] = nn

                    end    
                end

                # update onodes in for loops
                if isa(n, m.NFor)
                    g2 = n.main[2]
                    for (o,s) in g2.exto
                        if haskey(nmap, o)
                            g2.exto[ nmap[o] ] = s  # replace
                        end
                    end
                end
            end
            append!(dg.nodes, dg2)    
            # dg |> prune! |> simplify!

            # create for loop node
            nf = m.addnode!(g, m.NFor(Any[ si, dg ] ) )

            # create param size node
            nsz = m.addgraph!( :( length( x ) ), g, [ :x => g.exti.vk[paramsym[1]] ] )

            # create (n-1)th derivative size node
            ndsz = m.addgraph!( :( sz ^ $(i-1) ), g, [ :sz => nsz ] )

            # create index range node
            nid = m.addgraph!( :( 1:dsz ),  g, [ :dsz => ndsz ] )
            push!(nf.parents, nid)

            # pass size node inside subgraph
            sst = m.newvar()
            inst = m.addnode!(dg, m.NExt(sst))
            dg.exti[inst] = sst
            dg.exto[nsz]  = sst
            push!(nf.parents, nsz)

            # create result node (alloc in parent graph)
            nsa = m.addgraph!( :( zeros( $( Expr(:tuple, [:sz for j in 1:i]...) ) ) ), g, [ :sz => nsz ] )
            ssa = m.newvar()
            insa = m.addnode!(dg, m.NExt(ssa))
            dg.exti[insa] = ssa
            dg.exto[nsa]  = ssa
            push!(nf.parents, nsa)

            # create result node update (in subgraph)
            nres = m.addgraph!( :( res[ ((sidx-1)*st+1):(sidx*st) ] = dx ; res ), dg, 
                                [ :res  => insa,
                                  :sidx => nmap[ni],
                                  :st   => inst,
                                  :dx   => collect(dg.seti)[1][1] ] )
            dg.seti = m.NSMap(Dict([nres], [ssa]))

            # create exit node for result
            nex = m.addnode!(g, m.NIn(ssa, [nf]))
            dg.seto = m.NSMap(Dict([nex], [ssa]))

            # update parents of for loop
            append!( nf.parents, setdiff(collect( keys(dg.exto)), nf.parents[2:end]) )

            ns = m.newvar(:_dv)
            g.seti[nex] = ns
            push!(voi, ns)
        end
            gggggg = m.copy(g)
            # g = m.copy(gggggg)
            m.splitnary!(g)
            m.prune!(g)
            m.simplify!(g)  #  ERROR: key not found: :_tmp25

    voin = map( s -> g.seti.vk[s], voi)
    ex = m.addnode!(g, m.NCall(:tuple, voin))
    g.seti = m.BiDict(Dict{m.ExNode,Any}( [ex], [nothing]) )

    m.splitnary!(g)
    m.prune!(g)
    m.simplify!(g)  #  ERROR: key not found: :_tmp55

    m.resetvar()
    res = m.tocode(g)

    ex = :( x[1]^3 + x[1]*x[2] )
    res = m.rdiff(ex, x=zeros(2), order=3)   

    @eval foo(x) = $res
    foo([1., 2.])
    (3.0,[5.0,1.0],
    2x2 Array{Float64,2}:
     6.0  1.0
     1.0  0.0,

    2x2x2 Array{Float64,3}:
    [:, :, 1] =
     6.0  0.0
     0.0  0.0

    [:, :, 2] =
     0.0  0.0
     0.0  0.0)


##############   tests for composite types    #####################
    reload("ReverseDiffSource") ; tm = ReverseDiffSource
    type Test1
        x
        y
    end

    a = Test1(1,2)

    x = 1.5

    tm.type_decl(Test1, 2)
    tm.@type_decl Main.Test1 2 
    tm.@deriv_rule    Test1(x,y)   x  ds[1]
    tm.@deriv_rule    Test1(x,y)   y  ds[2]

    tm.reversediff(:( x * a.x), x=1)

    tm.reversediff(:( x ^ a.x), x=1)
    tm.reversediff(:( x ^ c.x), c=Test1(2,2))  # doesn't throw error but incorrect

    norm(t::Test1) = t.x*t.x + t.y*t.y
    tm.@deriv_rule    norm(t::Test1)  t  { 2t.x*ds , 2t.y*ds }

    ex = :( c = Test1(x, 2x) ; norm(c) )
    res = tm.reversediff(ex, x=1.)
    @eval exref(x) = ($ex )
    @eval exrds(x) = ($res ; (out, dx))

    exref(1.)
    exref(1.001)  # dx = 10
    exrds(1.)     # (5.0, 10.0)   ok

    using Distributions
    reload("ReverseDiffSource") ; tm = ReverseDiffSource

    tm.type_decl(Normal, 2)    
    tm.@deriv_rule    Normal(mu, sigma)     mu     ds[1]
    tm.@deriv_rule    Normal(mu, sigma)     sigma  ds[2]
    tm.@deriv_rule    mean(d::Normal)       d      { ds , 0. }

    ex = :( d = Normal( x, sin(x)) ; mean(d) )
    res = tm.reversediff(ex, x=1.)
    @eval exref(x) = ($ex )
    @eval exrds(x) = ($res ; (out, dx))

    exref(2.)
    exref(2.001)
    exrds(2.)

    foo( d::Array{Normal} ) = [ mean(de) for de in d ]
    tm.@deriv_rule    foo(d::Array{Normal})   d      (nds=zeros(2); 
                                                        for i in 1:length(d) ;
                                                            nds[i] = ds[i] ;
                                                        end ; { nds , zeros(2) } )
    # tm.@deriv_rule    foo(d::Array{Normal})   d      { copy(ds) , zeros(size(d)) } 
    tm.@deriv_rule    vcat(a,b)               a      ds[1]
    tm.@deriv_rule    vcat(a,b)               b      ds[2]
    tm.@deriv_rule    vcat(a::Normal,b::Normal)  a   { ds[1][1], ds[2][1] }
    tm.@deriv_rule    vcat(a::Normal,b::Normal)  b   { ds[1][2], ds[2][2] }

    foo([Normal(1,1), Normal(2,1)])

    foo([Normal(1,1), Normal(2,1)])

    ex = :( ns = [Normal(x,1.), Normal(2x,1)] ; z = sum(foo(ns)) )
    res = tm.reversediff(ex, :z, x=1.)


##############   tests for composite types 2   #####################

    include("src/ReverseDiffSource.jl")

    type Foo
        x::Float64
        y::Float64
    end
    bar(t::Foo) = t.x*t.x + t.y*t.y
    bar(ta::Array{Foo}) = Float64[ t.x*t.x + t.y*t.y for t in ta]


    x = Foo(1.,2.)
    bar(x)
    bar([x,x])

    tm.@type_decl    Foo             2   
    tm.@deriv_rule   Foo(x,y)        x      ds[1]
    tm.@deriv_rule   Foo(x,y)        y      ds[2]
    ReverseDiffSource.@deriv_rule   vcat(x,y)                          x      ds[1]
    ReverseDiffSource.@deriv_rule   vcat(x,y)                          y      ds[2]

    # ReverseDiffSource.@deriv_rule   Main.Sandbox.Foo(x,y)              x      ds[1]
    # ReverseDiffSource.@deriv_rule   Main.Sandbox.Foo(x,y)              y      ds[2]

    tm.@deriv_rule   bar(t::Foo)           t      [ 2*t.x*ds , 2*t.y*ds ]
    tm.@deriv_rule   bar(ta::Array{Foo})   ta   [(na=length(ta);res=zeros(na);for i in 1:na;res[i]=2ta[i].x*ds[i];end;res) ,
                                                 (na=length(ta);res=zeros(na);for i in 1:na;res[i]=2ta[i].y*ds[i];end;res) ]



    

    import Base.getfield

    getfield(af::Array{Foo}, f::Symbol) = [ getfield(t, f) for t in af]

    t = [ Foo(1.,2.), Foo(3.,4.)]
    Foo.names
    fieldoffsets(Foo)
    x[1]
    x.data



    t2 = reinterpret(Float64,t)


    ex = quote
        v = [ Foo(1., y), Foo(0.,1.)]
        res = sum( bar(v) )
    end
