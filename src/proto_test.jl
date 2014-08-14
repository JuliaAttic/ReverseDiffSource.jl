#################### function diff ###################

    function test1(x)
        a = 0
        for i in 1:10
            for j in 1:20
                a += x
            end
        end
        a
    end

    reload("ReverseDiffSource") ; m = ReverseDiffSource
    f = test1 ; sig0=(ones(10),) ; order=1 ; evalmod=Main

    sig = map( typeof, sig0 )
    fs = methods(f, sig)
    length(fs) == 0 && error("no function '$f' found for signature $sig")
    length(fs) > 1  && error("several functions $f found for signature $sig")  # is that possible ?

    fdef  = fs[1].func.code
    fcode = Base.uncompressed_ast(fdef)
    fargs = fcode.args[1]  # function parameters

    cargs = [ (fargs[i], sig0[i]) for i in 1:length(sig0) ]
    dex = rdiff(fcode.args[3]; order=order, evalmod=evalmod, cargs...)

    removetop(e::Expr)    = Expr(e.head, map(removetop, e.args)...)
    removetop(e::TopNode) = e.name
    removetop(e::Any)     = e

    replacetupleref(e::Expr)    = Expr(e.head, map(replacetupleref, e.args)...)
    replacetupleref(e::Symbol)  = e == :tupleref ? :getindex : e
    replacetupleref(e::Any)     = e

    nc2 = removetop(fcode.args[3])
    nc3 = replacetupleref(nc2)

    dump(nc3)

################## setindex  #######################
    reload("ReverseDiffSource") ; m = ReverseDiffSource
    include(joinpath(Pkg.dir("ReverseDiffSource"), "test/unit_tests.jl"))

    m.tograph(:(  a[5] ))
    m.tograph(:(  a[5] = 3 ))
    m.tograph(:( z = zeros(2) ; z[1] = x ; sum(z) ) )

    g = m.tograph(:(  α = x ; β = α ; γ = α)) 
    m.plot(g)



    m.reversediff( :(  α = x ; β = α ; γ = α), :α, x=1)  # correct
    m.reversediff( :(  α = x ; β = α ; γ = α), :β, x=1)  # ok, corrigé
    m.reversediff( :(  α = x ; β = α ; γ = α), :γ, x=1)  # ok, corrigé
    res = m.reversediff( :(  α = zeros(5,2) ; α[2:4,1:2] = x ; sum(α)), x=1) 
    @eval myf(x) = ($res ; (out, dx))
    myf(3)

    res = m.reversediff( :(  α = 1. ; for i in 1:3 ; α *= x ; end; α), x=1) 
    @eval myf(x) = ($res ; (out, dx))
    myf(2)
    myf(2.001)
    myf(3)

    (x = 1 ; α = x ; α += 1 ; x + α)
    (x = [1] ; α = x ; α += 1 ; x + α)

    g = m.tograph(:(  α = fill(x,5) ; β = α[2] ))
    [collect(values(g.set_inodes))]

    ex = quote
        a=zeros(5)
        a[3:5] = x 
        sum(a)
    end
    m.reversediff(ex, x=2.0)

    ex = quote
        a = ones(5)
        b = sum(a)*x
        a[2] += x
        c = sum(a)
        b + c
    end
    m.reversediff(ex, x=1)

    ex = quote
        a=zeros(1+6)
        z=sum(a)
        for i in 1:4
            t = 4+3+2
            a[i] += b[i]+t
        end
        sum(a) + z
    end
    m.reversediff(ex, b=ones(6))

    ex = quote
        a=0.
        b=0.
        for i in 1:4
            a += x
            b += 2x
        end
        a + b
    end
    m.reversediff(ex, x=1.0)

    reload("ReverseDiffSource") ; m = ReverseDiffSource

    g = m.tograph(ex)
    m.splitnary!(g)
    m.simplify!(g)
    m.prune!(g, {g.set_inodes.vk[:α]})
    m.tocode(g)
    m.calc!(g, params={:b => ones(4), :x => 2.0})
    g

    g2 = m.reversegraph(g, g.set_inodes.vk[nothing], [:x])
    for (i,n) in enumerate(g2.nodes)
        all( x -> in(x, g.nodes) || in(x, g2.nodes), n.parents) && continue
        println("#$i  $n")
    end

    g2
    g.nodes = [ g.nodes, g2.nodes]
    collect(g.set_inodes)
    collect(g2.set_inodes)
    g.set_inodes = m.BiDict(merge(g.set_inodes.kv, g2.set_inodes.kv))
    g
    g.nodes
    m.tocode(g)
    m.prune!(g); m.tocode(g)
    m.simplify!(g); m.tocode(g)

    res = fullcycle(ex)
    @eval myf(x) = ($res; a)
    myf(3)

################## for loops  #######################

    reload("ReverseDiffSource") ; m = ReverseDiffSource

    ex = quote
        a=0
        for i in 1:2
            a += 2x    
        end
    end

    ex = quote
        a=0
        for i in 1:2
            a += x^2    
        end
    end

    ex = quote
        a=zeros(1+4)
        for i in 1:4
            t = 4+3+2
            a[i] += b[i]+t
        end
        z=sum(a)
    end

    sigma = 2.
    ex = quote
        a = 0.
        for i = 1:3
            a += x[i]
        end
        a / (sigma^2)
    end
    m.rdiff(ex, x=ones(3))

    g = m.tograph(ex)
    m.splitnary!(g)
    m.simplify!(g)
    m.prune!(g, {g.set_inodes.vk[nothing]})
    m.tocode(g)

    m.calc!(g, params={:x => ones(3)})
    g

    g2 = m.reversegraph(g, g.set_inodes.vk[nothing], [:x])
    g.nodes = [ g.nodes, g2.nodes]
    g.set_inodes = m.BiDict(merge(g.set_inodes.kv, g2.set_inodes.kv))
    g.nodes
    g.nodes[8].main[2]
    collect(g.nodes[6].main[2].set_inodes)  # Nin pour dx pas recrée
    m.tocode(g)
    m.prune!(g); m.tocode(g)

    g
    n = g.nodes[17]
    if n.parents[1].main == 0 && in(n.main, [:+, :.+])
       # fusenodes(g, n.parents[2], n)
       nk, nr = n.parents[2], n

  # same for references to nr in subgraphs
        n = g.nodes[19] 
          for n in filter(n -> isa(n, m.NFor) && n != nr && n != nk, g.nodes)
            g2 = n.main[2]

            # this should not happen...
            @assert !haskey(g2.set_onodes, nr) "[fusenodes (for)] attempt to fuse set_onode $nr"

            if haskey(g2.ext_onodes, nr)
              nn = addnode!(g, NIn(g2.ext_onodes[nr], [nk])) # create a ref node to replace
              g2.ext_onodes[nn] = g2.ext_onodes[nr]  # nn replaces nr as g2.ext_onodes
            end  
          end

          # replace references to nr by nk in parents of other nodes
          for n in filter(n -> n != nr && n != nk, g.nodes)
            for (i, n2) in enumerate(n.parents)
              n2 == nr && (n.parents[i] = nk)
            end
            for (i, n2) in enumerate(n.precedence)
              n2 == nr && (n.precedence[i] = nk)
            end
          end

          # remove node nr in g
          filter!(n -> n != nr, g.nodes)



    m.simplify!(g); m.tocode(g)
    m.markalloc!(g)
    [ (n, n.alloc) for n in g.nodes]


    ex = quote
        a=zeros(1+3)
        for i in 1:4
            t = 4+3+2
            a[i] += b[i]+t
        end
        z=sum(a)
    end

    reload("ReverseDiffSource") ; m = ReverseDiffSource
    m.tograph(ex)
    res = m.reversediff(ex, :z, b=ones(5))
    @eval exref(b) = ($ex ; (z,))
    @eval exrds(b) = ($res ; (z, db))

    exref(ones(6))
    exrds(ones(6))
    exref(2ones(5))
    exrds(2ones(5))
    exref(ones(6))
    exrds(ones(6))


    g = tm.tograph(ex)
    tm.splitnary!(g)
    tm.simplify!(g)
    tm.prune!(g, {g.set_inodes.vk[:z]})
    tm.tocode(g)

    tm.calc!(g, params={:b => ones(5)})
    g.nodes

    b = ones(5)
    res = tm.tocode(g)
    @eval myfunc(b) = ($res ; (z, db))
    myfunc(3ones(6))


    g2 = tm.reversegraph(g, g.set_inodes.vk[:z], [:b])
    g.nodes = [ g.nodes, g2.nodes]
    g.set_inodes = tm.BiDict(merge(g.set_inodes.kv, g2.set_inodes.kv))
    g.nodes
    g.nodes[6].main[2].nodes
    collect(g.nodes[6].main[2].set_inodes)  # Nin pour dx pas recrée
    tm.tocode(g)
    tm.prune!(g); tm.tocode(g)
    tm.simplify!(g); tm.tocode(g)
    tm.markalloc!(g)
    [ (n, n.alloc) for n in g.nodes]




    res = tm.tocode(g)
    @eval myfunc(x) = ($res ; (res,dx))
    myfunc(2)
    myfunc(3)
    myfunc(2)

    g.nodes[4].main[2].nodes[5]

    g3 = g.nodes[4].main[2]
  g2 = tm.ExGraph()
  nmap = Dict{tm.ExNode, tm.ExNode}()
  tm.evalsort!(g3)
  for n in g3.nodes

  n = g3.nodes[5]  
    println("node :     $n")
    n2 = tm.addnode!(g2, copy(n))
    for n3 in n2.parents
        println("    parents   $n3")
    end
    println("    $(typeof(n2.parents))")
    n2.parents
    map((x) -> (x.val), n2.parents);
    [ nmap[n] for n in n2.parents ]

    dump(nmap)
    typeof(n2.parents)
    nmap[ n2.parents[1] ]
    nmap[ n2.parents[2] ]
    println("    $(typeof(xxxx))")
    n2.parents = map(n->nmap[n], n2.parents)
    nmap[n] = n2
  end

    ex = quote
        a=zeros(10)
        for i in 1:10
            t = x+z
            a[i] = b[i]+t
        end
        aa = sum(a)
    end

    g = tm.tograph(ex);
    tm.evalconstants!(g); tm.tocode(g)
    tm.prune!(g); tm.tocode(g)
    tm.simplify!(g); tm.tocode(g)
    tm.calc!(g, params = {:x => 1, :b => ones(10), :z => 0})

    g2 = tm.reversegraph(g, g.setmap[:aa], [:x])
    g.nodes = [ g.nodes, g2.nodes]
    g.setmap = merge(g.setmap, g2.setmap)
    collect(keys(g.setmap))
    tm.evalconstants!(g); tm.tocode(g)
    tm.prune!(g); tm.tocode(g)
    tm.simplify!(g); tm.tocode(g)

    tm.tocode(g)
    tm.evalconstants!(g)
    tm.prune!(g)
    tm.simplify!(g)
    tm.tocode(g)

############### double loop   ################################

    reload("ReverseDiffSource") ; m = ReverseDiffSource

    ex = quote
        a=0
        for i in 1:10
            for j in 1:10
                a += x
            end
        end
        a
    end

    v, b = 2., ones(10)
    res = m.reversediff(ex, x=1.0)

    @eval exref(x) = ($ex )
    @eval exrds(x) = ($res ; (out, dx))

    exref(3)
    exref(4)
    exrds(3)


    ex = quote                #  issue here, a is rewritten but derivation sums over all loops
        a=zeros(10) ; z = 12 
        for i in 1:10
            t = x+z
            for j in 1:10
                u = t+z+v
                a[i] = b[i]+u
            end
        end
        sum(a)
    end

    v, b = 2., ones(10)
    res = m.reversediff(ex, x=1.0)

    @eval exref(x) = ($ex )
    @eval exrds(x) = ($res ; (out, dx))

    exref(3)
    exref(4)
    exrds(3)

##############   tests for composite types    #####################
    reload("ReverseDiffSource") ; m = ReverseDiffSource
    type Test1
        x
        y
    end

    a = Test1(1,2)
    x = 1.5

    m.@typeequiv Test1 2
    m.@deriv_rule    Test1(x,y)   x  ds[1]
    m.@deriv_rule    Test1(x,y)   y  ds[2]

    m.reversediff(:( x * a.x), x=1)
    m.reversediff(:( x + a.x), x=1)

    ex = :( (nds=zeros(size(d)); for i in 1:length(d) ; nds[i] = ds[i] ; end ; nds)  )
    g = m.tograph(ex)
    m.simplify!(g)
    m.prune!(g, [g.set_inodes.vk[nothing]])
    m.tocode(g)

    m.reversediff(:( x ^ a.x), x=1)
    m.reversediff(:( x ^ a.x), x=1)
    m.reversediff(:( x ^ c.x), c=Test1(2,2))  # doesn't throw error but incorrect

    norm(t::Test1) = t.x*t.x + t.y*t.y
    m.@deriv_rule    norm(t::Test1)  t  [ 2t.x*ds , 2t.y*ds ]

    ex = :( c = Test1(x, 2x) ; norm(c) )
    res = m.reversediff(ex, x=1.)
    @eval exref(x) = ($ex )
    @eval exrds(x) = ($res ; (out, dx))

    exref(1.)
    exref(1.001)  # dx = 10
    exrds(1.)     # (5.0, 10.0)   ok

    using Distributions
    Normal(0,1)
    reload("ReverseDiffSource") ; m = ReverseDiffSource

    m.@typeequiv Normal 2
    m.@deriv_rule    Normal(mu, sigma)     mu     ds[1]
    m.@deriv_rule    Normal(mu, sigma)     sigma  ds[2]
    m.@deriv_rule    mean(d::Normal)       d      [ ds , 0. ]

    ex = :( d = Normal( x, sin(x)) ; mean(d) )
    res = m.reversediff(ex, x=1.)
    @eval exref(x) = ($ex )
    @eval exrds(x) = ($res ; (out, dx))

    exref(2.)
    exref(2.001)
    exrds(2.)

    foo( d::Array{Normal} ) = [ mean(de) for de in d ]
    m.@deriv_rule    foo(d::Array{Normal})   d      (nds=Base.cell(size(d)); 
                                                        for i in 1:length(d) ;
                                                            nds[i] = [ ds[i], 0.] ;
                                                        end ;  nds )
    # m.@deriv_rule    foo(d::Array{Normal})   d      { copy(ds) , zeros(size(d)) } 
    m.@deriv_rule    vcat(a,b)               a      ds[1]
    m.@deriv_rule    vcat(a,b)               b      ds[2]
    # m.@deriv_rule    vcat(a::Normal,b::Normal)  a   { ds[1][1], ds[2][1] }
    # m.@deriv_rule    vcat(a::Normal,b::Normal)  b   { ds[1][2], ds[2][2] }

    foo([Normal(1,1), Normal(2,1)])

    foo([Normal(1,1), Normal(2,1)])

    ex = :( ns = [Normal(x,1.), Normal(2x,1)] ; z = sum(foo(ns)) )
    res = m.reversediff(ex, :z, x=1.)
    @eval exref(x) = ($ex ; z)
    @eval exrds(x) = ($res ; (z, dx))

    exref(1.0)
    exref(1.001)
    exrds(1.)

##############   tests for nth order derivation    #################

    reload("ReverseDiffSource") ; m = ReverseDiffSource
    include(joinpath(Pkg.dir("ReverseDiffSource"), "test/unit_tests.jl"))

    function rdiff(ex; outsym=nothing, order::Int=1, evalmod=Main, params...)

        # ex = :(sin(x))      ; order=2 ; evalmod=Main ; params = [(:x, 1.)]      ; outsym=nothing
        # ex = :( x[1]*x[2] ) ; order=2 ; evalmod=Main ; params = [(:x, [1.,2.])] ; outsym=nothing
        # ex = :( x[1]^3 ) ; order=3 ; evalmod=Main ; params = [(:x, [1.,2.])] ; outsym=nothing
        # ex = :( x[1]^3+x[2] ) ; order=2 ; evalmod=Main ; params = [(:x, [1.,2.])] ; outsym=nothing
        # ex = :( z = zeros(2) ; z[1] = x ; z[2] = 2x ; sum(z)) ; order=1 ; evalmod=Main ; params = [(:x, 1.)] ; outsym=nothing
        # ex = :( z = zeros(2) ; z[1] = x ; sum(z) ) ; order=1 ; evalmod=Main ; params = [(:x, 1.)] ; outsym=nothing
        m.resetvar()

        length(params) >= 1 || error("There should be at least one parameter specified, none found")
        
        order <= 1 || 
        length(params) == 1 || error("Only one param allowed for order >= 2")
        
        order <= 1 || 
        isa(params[1][2], Vector) || 
        isa(params[1][2], Real)   || error("Param should be a real or vector for order >= 2")

        paramsym    = Symbol[ e[1] for e in params]
        paramvalues = [ e[2] for e in params]
        parval      = Dict(paramsym, paramvalues)

        println("=== tograph")
        g = m.tograph(ex)

        haskey(g.set_inodes.vk, outsym) || 
            error("can't find output var $( outsym==nothing ? "" : outsym)")

        # reduce to variable of interest
        g.set_inodes = m.BiDict{m.ExNode,Any}([g.set_inodes.vk[outsym]], [ outsym ])    

        g |> m.splitnary! |> m.prune! |> m.simplify!
        m.calc!(g, params=parval, emod=evalmod)

        ov = g.set_inodes.vk[outsym].val 
        isa(ov, Real) || error("output var should be a Real, $(typeof(ov)) found")

        voi = { outsym }

        if order == 1
            dg = m.reversegraph(g, g.set_inodes.vk[outsym], paramsym)
            m.append!(g.nodes, dg.nodes)
            nn = m.addnode!( g, m.NCall(:tuple, [ dg.set_inodes.vk[m.dprefix(p)] for p in paramsym] ) )
            ns = m.newvar("_dv")
            g.set_inodes[nn] = ns
            push!(voi, ns)

            g |> m.splitnary! |> m.prune! |> m.simplify!

        elseif order > 1 && isa(paramvalues[1], Real)
            for i in 1:order  # i=1
                println("=== reversegraph  $i")

                dg = m.reversegraph(g, g.set_inodes.vk[voi[i]], paramsym)
                append!(g.nodes, dg.nodes)
                nn = collect(keys(dg.set_inodes))[1]  # only a single node produced
                ns = m.newvar("_dv")
                g.set_inodes[nn] = ns
                push!(voi, ns)

                g |> m.splitnary! |> m.prune! |> m.simplify!
                
                m.calc!(g, params=parval, emod=evalmod)
            end

        elseif order > 1 && isa(paramvalues[1], Vector)
            # do first order as usual
            dg = m.reversegraph(g, g.set_inodes.vk[outsym], paramsym)
            m.append!(g.nodes, dg.nodes)
            ns = m.newvar(:_dv)
            g.set_inodes[ collect(keys(dg.set_inodes))[1] ] = ns
            push!(voi, ns)

            g |> m.splitnary! |> m.prune! |> m.simplify!

            # now order 2 to n
            for i in 2:order  
            # i=2
            # i=3
                println("=== reversegraph  $i")

                # launch derivation on a single value of the preceding
                #   derivation vector
                no = g.set_inodes.vk[voi[i]]
                si = m.newvar(:_idx)
                ni = m.addnode!(g, m.NExt(si))
                ns = m.addnode!(g, m.NRef(:getidx, [ no, ni ]))

                m.calc!(g, params=Dict([paramsym, si], [paramvalues, 1.]), emod=evalmod)
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
                            dg.ext_inodes[nn] = si
                            n.parents[j] = nn
                            nmap[np] = nn

                        elseif np == ns # it's the selected element of the deriv vector
                            # create 'no' ref if needed
                            if !haskey(nmap, no)
                                sn = m.newvar()
                                nn = m.NExt(sn)
                                push!(dg2, nn)
                                dg.ext_inodes[nn] = sn
                                dg.ext_onodes[no] = sn
                                nmap[no] = nn
                            end

                            nn = m.NRef(:getidx, [ nmap[no], nmap[ni] ])
                            push!(dg2, nn)
                            nmap[ns] = nn                            

                        elseif !(np in dg.nodes) # it's not in dg (but in g)
                            sn = m.newvar()
                            nn = m.NExt(sn)
                            push!(dg2, nn)
                            dg.ext_inodes[nn] = sn
                            dg.ext_onodes[np] = sn
                            n.parents[j] = nn
                            nmap[np] = nn

                        end    
                    end

                    # update onodes in for loops
                    if isa(n, m.NFor)
                        g2 = n.main[2]
                        for (o,s) in g2.ext_onodes
                            if haskey(nmap, o)
                                g2.ext_onodes[ nmap[o] ] = s  # replace
                            end
                        end
                    end
                end
                append!(dg.nodes, dg2)    
                dg |> m.prune! |> m.simplify!

                # dg.ext_onodes =  m.BiDict{m.ExNode, Any}()
                # collect(dg.ext_inodes)
                # collect(dg.set_inodes)
                # collect(nmap)
                m.tocode(dg)

                # collect(dg.nodes[13].parents)
                # dg.nodes[45].main[2]
                # ln

                # create for loop node
                nf = m.addnode!(g, m.NFor({si, dg}) )

                # create size node
                nsz = m.addgraph!( m.tograph(:( length( x ) )), g, { :x => g.ext_inodes.vk[paramsym[1]] } )

                # create index range node
                nid = m.addgraph!( m.tograph(:( 1:sz )),  g, { :sz => nsz } )
                push!(nf.parents, nid)

                # create stride size node
                nst = m.addgraph!( m.tograph(:( sz ^ $(i-1) )),  g, { :sz => nsz } )
                sst = m.newvar()
                inst = m.addnode!(dg, m.NExt(sst))
                dg.ext_inodes[inst] = sst
                dg.ext_onodes[nst]  = sst
                push!(nf.parents, nst)

                # create result node (alloc in parent graph)
                nsa = m.addgraph!( m.tograph(:( zeros( $( Expr(:tuple, [:sz for j in 1:i]...) ) ) )), g, { :sz => nsz } )
                ssa = m.newvar()
                insa = m.addnode!(dg, m.NExt(ssa))
                dg.ext_inodes[insa] = ssa
                dg.ext_onodes[nsa]  = ssa
                push!(nf.parents, nsa)

                # create result node update (in subgraph)
                nres = m.addgraph!( m.tograph(:( res[ ((sidx-1)*st+1):(sidx*st) ] = dx ; res )), dg, 
                                    { :res  => insa,
                                      :sidx => nmap[ni],
                                      :st   => inst,
                                      :dx   => collect(dg.set_inodes)[1][1] } )
                dg.set_inodes = m.BiDict{m.ExNode, Any}(Dict([nres], [ssa]))

                # create exit node for result
                nex = m.addnode!(g, m.NIn(ssa, [nf]))
                dg.set_onodes = m.BiDict{m.ExNode, Any}(Dict([nex], [ssa]))

                # update parents of for loop
                # collect( dg.ext_inodes )
                append!( nf.parents, setdiff(collect( keys(dg.ext_onodes)), nf.parents[2:end]) )

                ns = m.newvar(:_dv)
                g.set_inodes[nex] = ns
                push!(voi, ns)

        # g
        # g.nodes[21].main[2]

                g |> m.splitnary! |> m.prune! |> m.simplify!
                
                m.calc!(g, params=Dict(paramsym, paramvalues), emod=evalmod)
            end

        end

        voin = map( s -> g.set_inodes.vk[s], voi)
        ex = m.addnode!(g, m.NCall(:tuple, voin))
        g.set_inodes = m.BiDict(Dict{m.ExNode,Any}( [ex], [nothing]) )

        # m.resetvar()
        println("=== tocode")
        m.tocode(g)
    end

###################### zoom order 3

    reload("ReverseDiffSource") ; m = ReverseDiffSource


        # ex = :(sin(x))      ; order=2 ; evalmod=Main ; params = [(:x, 1.)]      ; outsym=nothing
        # ex = :( x[1]^3 ) ; order=3 ; evalmod=Main ; params = [(:x, [1.,2.])] ; outsym=nothing
        # ex = :( x[1]^3+x[2] ) ; order=2 ; evalmod=Main ; params = [(:x, [1.,2.])] ; outsym=nothing
        # ex = :( z = zeros(2) ; z[1] = x ; z[2] = 2x ; sum(z)) ; order=1 ; evalmod=Main ; params = [(:x, 1.)] ; outsym=nothing
        # ex = :( z = zeros(2) ; z[1] = x ; sum(z) ) ; order=1 ; evalmod=Main ; params = [(:x, 1.)] ; outsym=nothing

        ex = :( x[1]*x[2] ) ; order=2 ; evalmod=Main ; params = [(:x, [1.,2.])] ; outsym=nothing

        length(params) >= 1 || error("There should be at least one parameter specified, none found")
        
        order <= 1 || 
        length(params) == 1 || error("Only one param allowed for order >= 2")
        
        order <= 1 || 
        isa(params[1][2], Vector) || 
        isa(params[1][2], Real)   || error("Param should be a real or vector for order >= 2")

        paramsym    = Symbol[ e[1] for e in params]
        paramvalues = [ e[2] for e in params]
        parval      = Dict(paramsym, paramvalues)

        println("=== tograph")
        g = m.tograph(ex)

        haskey(g.set_inodes.vk, outsym) || 
            error("can't find output var $( outsym==nothing ? "" : outsym)")

        # reduce to variable of interest
        g.set_inodes = m.BiDict{m.ExNode,Any}([g.set_inodes.vk[outsym]], [ outsym ])    

        g |> m.splitnary! |> m.prune! |> m.simplify!
        m.calc!(g, params=parval, emod=evalmod)

        ov = g.set_inodes.vk[outsym].val 
        isa(ov, Real) || error("output var should be a Real, $(typeof(ov)) found")

        voi = { outsym }

        #####

            # do first order as usual
            dg = m.reversegraph(g, g.set_inodes.vk[outsym], paramsym)
            m.append!(g.nodes, dg.nodes)
            ns = m.newvar(:_dv)
            g.set_inodes[ collect(keys(dg.set_inodes))[1] ] = ns
            push!(voi, ns)

            g |> m.splitnary! |> m.prune! |> m.simplify!

    # m.tocode(g)

            # now order 2 to n
            # for i in 2:order  
             i=2
             i=3
                println("=== reversegraph  $i")

                # launch derivation on a single value of the preceding
                #   derivation vector
                no = g.set_inodes.vk[voi[i]]
                si = m.newvar(:_idx)
                ni = m.addnode!(g, m.NExt(si))
                ns = m.addnode!(g, m.NRef(:getidx, [ no, ni ]))

                m.calc!(g, params=Dict([paramsym, si], [paramvalues, 1.]), emod=evalmod)
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
                            dg.ext_inodes[nn] = si
                            n.parents[j] = nn
                            nmap[np] = nn

                        elseif np == ns # it's the selected element of the deriv vector
                            # create 'no' ref if needed
                            if !haskey(nmap, no)
                                sn = m.newvar()
                                nn = m.NExt(sn)
                                push!(dg2, nn)
                                dg.ext_inodes[nn] = sn
                                dg.ext_onodes[no] = sn
                                nmap[no] = nn
                            end

                            nn = m.NRef(:getidx, [ nmap[no], nmap[ni] ])
                            push!(dg2, nn)
                            nmap[ns] = nn                            

                        elseif !(np in dg.nodes) # it's not in dg (but in g)
                            sn = m.newvar()
                            nn = m.NExt(sn)
                            push!(dg2, nn)
                            dg.ext_inodes[nn] = sn
                            dg.ext_onodes[np] = sn
                            n.parents[j] = nn
                            nmap[np] = nn

                        end    
                    end

                    # update onodes in for loops
                    if isa(n, m.NFor)
                        g2 = n.main[2]
                        for (o,s) in g2.ext_onodes
                            if haskey(nmap, o)
                                g2.ext_onodes[ nmap[o] ] = s  # replace
                            end
                        end
                    end
                end


                # [ (sum(x -> !(x in dg), n.parents) , sum(x -> !(x in dg2), n.parents) ) for n in dg.nodes]
                # test = [ collect(n.parents) for n in dg.nodes]
                # test = test[ map(x->length(x)>0, test) ]
                # [ (sum(x -> !(x in dg) & , ns) , sum(x -> !(x in dg2), ns) ) for n in dg.nodes]


                append!(dg.nodes, dg2)    
                dg |> m.prune! |> m.simplify!

                # dg.ext_onodes =  m.BiDict{m.ExNode, Any}()
                # collect(dg.ext_inodes)
                # collect(dg.set_inodes)
                # collect(nmap)
                m.tocode(dg)

                # collect(dg.nodes[13].parents)
                # dg.nodes[45].main[2]
                # ln

                # create for loop node
                nf = m.addnode!(g, m.NFor({si, dg}) )

                # create size node
                nsz = m.addgraph!( m.tograph(:( length( x ) )), g, { :x => g.ext_inodes.vk[paramsym[1]] } )

                # create index range node
                nid = m.addgraph!( m.tograph(:( 1:sz )),  g, { :sz => nsz } )
                push!(nf.parents, nid)

                # create stride size node
                nst = m.addgraph!( m.tograph(:( sz ^ $(i-1) )),  g, { :sz => nsz } )
                sst = m.newvar()
                inst = m.addnode!(dg, m.NExt(sst))
                dg.ext_inodes[inst] = sst
                dg.ext_onodes[nst]  = sst
                push!(nf.parents, nst)

                # create result node (alloc in parent graph)
                nsa = m.addgraph!( m.tograph(:( zeros( $( Expr(:tuple, [:sz for j in 1:i]...) ) ) )), g, { :sz => nsz } )
                ssa = m.newvar()
                insa = m.addnode!(dg, m.NExt(ssa))
                dg.ext_inodes[insa] = ssa
                dg.ext_onodes[nsa]  = ssa
                push!(nf.parents, nsa)

                # create result node update (in subgraph)
                nres = m.addgraph!( m.tograph(:( res[ ((sidx-1)*st+1):(sidx*st) ] = dx ; res )), dg, 
                                    { :res  => insa,
                                      :sidx => nmap[ni],
                                      :st   => inst,
                                      :dx   => collect(dg.set_inodes)[1][1] } )
                dg.set_inodes = m.BiDict{m.ExNode, Any}(Dict([nres], [ssa]))

                # create exit node for result
                nex = m.addnode!(g, m.NIn(ssa, [nf]))
                dg.set_onodes = m.BiDict{m.ExNode, Any}(Dict([nex], [ssa]))

                # update parents of for loop
                # collect( dg.ext_inodes )
                append!( nf.parents, setdiff(collect( keys(dg.ext_onodes)), nf.parents[2:end]) )

                ns = m.newvar(:_dv)
                g.set_inodes[nex] = ns
                push!(voi, ns)

    g
    g2 = g.nodes[21].main[2]
    collect(g2.ext_inodes)
    collect(g2.ext_onodes)
    [ n in g.nodes for n in collect(keys(g2.ext_onodes))]

                g |> m.splitnary! |> m.prune! |> m.simplify!
                
                m.calc!(g, params=Dict(paramsym, paramvalues), emod=evalmod)
            end

        end

        voin = map( s -> g.set_inodes.vk[s], voi)
        ex = m.addnode!(g, m.NCall(:tuple, voin))
        g.set_inodes = m.BiDict(Dict{m.ExNode,Any}( [ex], [nothing]) )

        # m.resetvar()
        println("=== tocode")
        m.tocode(g)

#################### function diff ###################

    function test1(x)
        a = 0
        for i in 1:10
            for j in 1:20
                a += x
            end
        end
        a
    end


    macro ~(a,b)
        Main.acc += logpdf(b,a)
    end

    macro ~+(a,b)
        println(a,b)
    end

    acc = 0.

    function test2(x)
        a > 2 ? 0 : 1
            # x ~ Normal(4,5)
    end



    reload("ReverseDiffSource") ; m = ReverseDiffSource
    f = test2 ; sig0=(ones(10),) ; order=1 ; evalmod=Main

    sig = map( typeof, sig0 )
    fs = methods(f, sig)
    length(fs) == 0 && error("no function '$f' found for signature $sig")
    length(fs) > 1  && error("several functions $f found for signature $sig")  # is that possible ?

    fdef  = fs[1].func.code
    fcode = Base.uncompressed_ast(fdef)
    fargs = fcode.args[1]  # function parameters

    cargs = [ (fargs[i], sig0[i]) for i in 1:length(sig0) ]
    dex = rdiff(fcode.args[3]; order=order, evalmod=evalmod, cargs...)

    removetop(e::Expr)    = Expr(e.head, map(removetop, e.args)...)
    removetop(e::TopNode) = e.name
    removetop(e::Any)     = e

    replacetupleref(e::Expr)    = Expr(e.head, map(replacetupleref, e.args)...)
    replacetupleref(e::Symbol)  = e == :tupleref ? :getindex : e
    replacetupleref(e::Any)     = e

    function removeline(e::Expr)
        if e.head == :line
           return nothing
        else
           return Expr(e.head, filter(e -> e != nothing, map(removeline, e.args))...)
        end
    end
    removeline(e::LineNumberNode) = nothing
    removeline(e::Any)     = e


    nc2 = removetop(fcode.args[3])
    nc3 = replacetupleref(nc2)
    nc4 = removeline(nc3)

    dump(nc4)

    recreateforloops(e::Expr)



  head: Symbol body
  args: Array(Any,(18,))
    1: Expr 
      head: Symbol line
      args: Array(Any,(2,))
        1: Int64 2
        2: Symbol none
      typ: Any
    2: Expr 
      head: Symbol =
      args: Array(Any,(2,))
        1: Symbol a
        2: Int64 0
      typ: Any
    3: LineNumberNode 
      line: Int64 3
    4: Expr 
      head: Symbol =
      args: Array(Any,(2,))
        1: Symbol #s757
        2: Expr 
          head: Symbol call
          args: Array(Any,(3,))
          typ: Any
      typ: Any
    5: Expr 
      head: Symbol =
      args: Array(Any,(2,))
        1: Symbol #s758
        2: Expr 
          head: Symbol call
          args: Array(Any,(2,))
          typ: Any
      typ: Any
    ...
    14: Expr 
      head: Symbol gotoifnot
      args: Array(Any,(2,))
        1: Expr 
          head: Symbol call
          args: Array(Any,(2,))
          typ: Any
        2: Int64 2
      typ: Any
    15: LabelNode 
      label: Int64 1
    16: LabelNode 
      label: Int64 0
    17: LineNumberNode 
      line: Int64 6
    18: Expr 
      head: Symbol return
      args: Array(Any,(1,))
        1: Symbol a
      typ: Any
  typ: Any

##############  check  ####################
    reload("ReverseDiffSource") ; m = ReverseDiffSource

    ex = :(sum(x))
    y = 3
    ex = :(sum(x+y))

    m.rdiff(ex, x=2.)
    m.rdiff(ex, x=2., y=1.)

tuple({1.,30})

    g = m.tograph(ex)
    m.splitnary!(g)
    m.simplify!(g)
    m.prune!(g, {g.set_inodes.vk[nothing]})
    m.tocode(g)

    m.calc!(g, params={:x => 2.})
    g

    g2 = m.reversegraph(g, g.set_inodes.vk[nothing], [:x])
    g.nodes = [ g.nodes, g2.nodes]
    g.set_inodes = m.BiDict(merge(g.set_inodes.kv, g2.set_inodes.kv))
    g.nodes

    m.tocode(g)
    m.prune!(g); m.tocode(g)
    m.simplify!(g)

    ##
    outsym=nothing; order=1; evalmod=Main; params = [(:x, 2.)]

    length(params) >= 1 || error("There should be at least one parameter specified, none found")
    
    order <= 1 || 
    length(params) == 1 || error("Only one param allowed for order >= 2")
    
    order <= 1 || 
    isa(params[1][2], Vector) || 
    isa(params[1][2], Real)   || error("Param should be a real or vector for order >= 2")

    paramsym    = Symbol[ e[1] for e in params]
    paramvalues = [ e[2] for e in params]
    parval      = Dict(paramsym, paramvalues)

    g = m.tograph(ex)

    haskey(g.set_inodes.vk, outsym) || 
        error("can't find output var $( outsym==nothing ? "" : outsym)")

    # reduce to variable of interest
    g.set_inodes = m.BiDict{m.ExNode,Any}([g.set_inodes.vk[outsym]], [ outsym ])    

    g |> m.splitnary! |> m.prune! |> m.simplify!
    m.calc!(g, params=parval, emod=evalmod)

    ov = g.set_inodes.vk[outsym].val 
    isa(ov, Real) || error("output var should be a Real, $(typeof(ov)) found")

    voi = { outsym }

        dg = m.reversegraph(g, g.set_inodes.vk[outsym], paramsym)
        append!(g.nodes, dg.nodes)
        nn = m.addnode!( g, m.NCall(:tuple, [ dg.set_inodes.vk[m.dprefix(p)] for p in paramsym] ) )
        ns = m.newvar("_dv")
        g.set_inodes[nn] = ns
        push!(voi, ns)

        g |> m.splitnary! |> m.prune! |> m.simplify!


    voin = map( s -> g.set_inodes.vk[s], voi)
    ex = addnode!(g, NCall(:tuple, voin))
    g.set_inodes = BiDict(Dict{ExNode,Any}( [ex], [nothing]) )

    resetvar()
    tocode(g)
