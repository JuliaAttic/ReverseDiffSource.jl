######################  setup   ################################
    Pkg.status()

    cd(joinpath(Pkg.dir("ReverseDiffSource"), "test"))

    include("runtests.jl")

    m.rdiff(:( a = fill(0.,4) ; a[1]+x^2 ), x=1., order=5)

###################### issue #8   ######################################
    reload("ReverseDiffSource") ; m = ReverseDiffSource

    ex = :( (1 - x[1])^2 + 100(x[2] - x[1]^2)^2 )
    res = m.rdiff(ex, x=zeros(2), order=2)   
    res = m.rdiff(ex, x=zeros(2), order=3)   # 75 lines

    g2 = g.nodes[38].main[2]
    g3 = g2.nodes[36].main[2]

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
    ex = quote
        a = zeros(2)
        for i in 1:2
            a[1] = x
        end
        sum(a)
    end
    m.rdiff(ex, x=1.)
    m.rdiff( :(a=zeros(2) ; a[1]=x ; sum(a)), x=1.)

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
    end

    g
    g.nodes[6].main[2]

    dg = m.reversegraph(g, g.seti.vk[nothing], paramsym)
    dg.nodes[9].main[2]

    # m.tocode(dg)
    # collect(keys(dg.seti))
    # collect(keys(dg.exti))

    # fdg = dg.nodes[9].main[2]
    # m.tocode(fdg)
    # collect(keys(fdg.seti))
    # collect(keys(fdg.exti))


    append!(g.nodes, dg.nodes)
    nn = m.addnode!( g, m.NCall(:tuple, [ dg.seti.vk[m.dprefix(p)] for p in paramsym] ) )
    ns = m.newvar("_dv")
    g.seti[nn] = ns
    push!(voi, ns)

    g
    g2 = g.nodes[26].main[2]
    ns2 = [ g.nodes[28] ]
    m.tocode(g)

    m.prune!(g) 

      # list of g2 nodes whose outer node is in ns2
      exitnodes2 = m.ExNode[]
      for (k, sym) in g2.seto.kv
        k in ns2 || continue
        push!(exitnodes2, g2.seti.vk[sym])
      end
      # don't forget reentrant variables
      collect(g2.exti.kv)
      for (n2, sym) in g2.exti.kv
        haskey(g2.seti.vk, sym)        || continue
        isancestor(n2, exitnodes2, g2) || continue
        push!(exitnodes2, g2.seti.vk[sym])
      end



    m.tocode(g)
    m.simplify!(g)
    m.tocode(g)


############## debug 2   ##################
    ex = :( x[1]^3 + x[1]*x[2] )
    res = m.rdiff(ex, x=zeros(2), order=3)   



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
i = 9

m.eval( quote 
            function simplify!(g::ExGraph, emod = Main)

                i = 1
                markalloc!(g)
                println("+")
                while i <= length(g.nodes)
                    println("s in")
                    restart = false
                    n = g.nodes[i]

                    restart = any(n2 -> identical(n, n2, g), g.nodes[i+1:end]) #=||
                        evalconstants(n, g, emod) ||
                        rule1(n, g) ||
                        rule2(n, g) ||
                        rule3(n, g) ||
                        rule4(n, g) ||
                        rule5(n, g) ||
                        rule6(n, g) ||
                        rule7(n, g) ||
                        rule8(n, g) ||
                        rule9(n, g) ||
                        rule10(n, g)=#
                    
                    if restart
                        markalloc!(g)
                        i = 1
                        println("sout reset $i")
                    else
                        i += 1
                        println("sout +  $i")
                    end
                end

                # separate pass on subgraphs
                map( n -> simplify!(n.main[2], emod), 
                    filter(n->isa(n, NFor), g.nodes))

                
                g
            end

            function fusenodes(g::ExGraph, nk::ExNode, nr::ExNode)  # nk = n ; nr = n2
              println("fuse in")
              # this should not happen...
              # @assert !haskey(g.exti, nr) "[fusenodes] attempt to fuse ext_inode $nr"

              # test if nr is associated to a variable
              # if true, we create an NIn on nk, and associate var to it
              if haskey(g.seti, nr)
                nn = addnode!(g, NIn(g.seti[nr], [nk]))
                nn.val = "fuse #1"
                g.seti[nn] = g.seti[nr]  # nn replaces nr as set_inode

                if haskey(g.seto, nr)   # change onodes too (if we are in a subgraph)
                  g.seto[nn] = g.seto[nr]  # nn replaces nr as set_onode
                end  
              end

              # replace references to nr by nk in parents of other nodes
              for n in filter(n -> n != nr && n != nk, g.nodes)
                if isa(n, NFor)
                  g2 = n.main[2]

                  # this should not happen...
                  @assert !haskey(g2.seto, nr) "[fusenodes (for)] attempt to fuse set_onode $nr"

                  if haskey(g2.exto, nr)
                      println("fuse f for 1")
                    symr = g2.exto[nr]
                      println("fuse f for 2")
                    if haskey(g2.exto, nk)  # both nr and nk are used by the for loop
                        println("fuse f for 3")
                      symk = g2.exto[nk]
                        println("fuse f for 4 $symk  <- $symr")
                      fusenodes(g2, g2.exti.vk[symk], g2.exti.vk[symr])
                        println("fuse f for 5")
                    end

                    # nn = addnode!(g, NIn(g2.exto[nr], [nk]))
                    # nn.val = "fuse #2"
                    # g2.exto[nn] = g2.exto[nr]  # nn replaces nr as g2.exto
                    # for (i, n2) in enumerate(n.parents)
                    #   n2 == nr && (n.parents[i] = nn)
                    # end
                      g2.exto[nk] = g2.exto[nr]  # nk replaces nr as g2.exto
                      for (i, n2) in enumerate(n.parents)
                        n2 == nr && (n.parents[i] = nk)
                      end

                      for (i, n2) in enumerate(n.precedence)
                        n2 == nr && (n.parents[i] = nk)
                      end


                  end  
                end

                for (i, n2) in enumerate(n.parents)
                  n2 == nr && (n.parents[i] = nk)
                end
                for (i, n2) in enumerate(n.precedence)
                  n2 == nr && (n.precedence[i] = nk)
                end

              end

              println("fuse out")
              # remove node nr in g
              filter!(n -> n != nr, g.nodes)
            end

            function identical(n,n2,g)
                n.main != n2.main       && return false
                n.parents != n2.parents && return false
                n.alloc                 && return false
                n2.alloc                && return false
                # isa(n, NConst) && isa(n.main, Real) && return false # no need for small constants

                fusenodes(g, n, n2)
                true
            end
end)

            g
            g2 = g.nodes[33].main[2]
            collect( g2.exti )
            g2.exti.vk[ :_tmp25 ]
            collect( g2.exto )
            g2.exto.vk[ :_tmp25 ]
            findfirst(g.nodes .== g2.exto.vk[ :_tmp25 ])
            g
            
            m.calc!(g, params=Dict(paramsym, paramvalues), emod=Main)
        end

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
