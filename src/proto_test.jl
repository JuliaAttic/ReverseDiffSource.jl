################## setindex  #######################
    reload("ReverseDiffSource") ; m = ReverseDiffSource
    include(joinpath(Pkg.dir("ReverseDiffSource"), "test/unit_tests.jl"))

    m.tograph(:(  a[5] ))
    m.tograph(:(  a[5] = 3 ))

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

    reload("ReverseDiffSource") ; m = ReverseDiffSource

    g = m.tograph(ex)
    m.splitnary!(g)
    m.simplify!(g)
    m.prune!(g, {g.set_inodes.vk[:a]})
    m.tocode(g)

    m.calc!(g, params={:b => ones(5), :x => 2.})
    g

    g2 = m.reversegraph(g, g.set_inodes.vk[:a], [:x])
    g.nodes = [ g.nodes, g2.nodes]
    g.set_inodes = m.BiDict(merge(g.set_inodes.kv, g2.set_inodes.kv))
    g.nodes
    g.nodes[8].main[2]
    collect(g.nodes[6].main[2].set_inodes)  # Nin pour dx pas recrée
    m.tocode(g)
    m.prune!(g); m.tocode(g)
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

    function diff(ex; outsym=nothing, order::Int=1, evalmod=Main, params...)

        # ex = :(sin(x)) ; order=2 ; evalmod=Main ; params = [(:x, 1.)]; outsym=nothing
        ex = :( x[1]*x[2] ) ; order=2 ; evalmod=Main ; params = [(:x, [1.,2.])]; outsym=nothing

        length(params) >= 1 || error("There should be at least one parameter specified, none found")
        order <= 1 || 
            length(params) == 1 || error("Only one param allowed for order >= 2")
        order <= 1 || 
            isa(params[1][2], Vector) || 
            isa(params[1][2], Real) || 
            error("Param should be a real or vector for order >= 2")

        paramsym    = Symbol[ e[1] for e in params]
        paramvalues = [ e[2] for e in params]
        parval      = Dict(paramsym, paramvalues)

        println("=== tograph")
        g = m.tograph(ex)

        haskey(g.set_inodes.vk, outsym) || 
            error("can't find output var $( outsym==nothing ? "" : outsym)")
        exitnode = g.set_inodes.vk[outsym]

        m.splitnary!(g)
        m.prune!(g, [ g.set_inodes.vk[outsym] ])
        m.simplify!(g)
        m.calc!(g, params=parval, emod=evalmod)

        ov = g.set_inodes.vk[outsym].val 
        isa(ov, Real) || error("output var should be a Real, $(typeof(ov)) found")

        voi = { outsym }
        if order == 1
            dg = m.reversegraph(g, g.set_inodes.vk[outsym], paramsym)
            m.append!(g.nodes, dg.nodes)
            nn = m.addnode!( g, m.NCall(:tuple, collect(keys(dg.set_inodes))) )
            ns = m.newvar("_out")
            g.set_inodes[nn] = ns
            push!(voi, ns)

            m.splitnary!(g)
            m.prune!(g)
            m.simplify!(g)

        elseif order > 1 && isa(paramvalues[1], Real)
            for i in 1:order  # i=1
                println("=== reversegraph  $i")

                dg = m.reversegraph(g, g.set_inodes.vk[voi[i]], paramsym)
                append!(g.nodes, dg.nodes)
                nn = collect(keys(dg.set_inodes))[1]  # only a single node produced
                ns = m.newvar("_out")
                g.set_inodes[nn] = ns
                push!(voi, ns)

                m.splitnary!(g)
                m.prune!(g)
                m.simplify!(g)
                
                m.calc!(g, params=parval, emod=evalmod)
            end

        elseif order > 1 && isa(paramvalues[1], Vector)
            sz = length( paramvalues[1] )

            # do first order as usual
            dg = m.reversegraph(g, g.set_inodes.vk[outsym], paramsym)
            m.append!(g.nodes, dg.nodes)
            ns = m.newvar()
            g.set_inodes[ collect(keys(dg.set_inodes))[1] ] = ns
            push!(voi, ns)

            m.splitnary!(g)
            m.prune!(g)
            m.simplify!(g)

            for i in 2:order  # i=2
                println("=== reversegraph  $i")

                # launch derivation on a single value of the preceding
                #   derivation vector
                no = g.set_inodes.vk[voi[i]]
                si = m.newvar()
                ni = m.addnode!(g, m.NExt(si))
                ns = m.addnode!(g, m.NRef(:select, [ no, ni ]))

                m.calc!(g, params=Dict([paramsym, si], [paramvalues, 1.]), emod=evalmod)
                dg = m.reversegraph(g, ns, paramsym)
m.tocode(g)
                #### We will now wrap dg in a loop scanning all the elements of 'no'
                
                # first create nodes to make dg a complete subgraph
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

                            nn = m.NRef(:select, [ nmap[no], nmap[ni] ])
                            push!(dg2, nn)
                            nmap[ns] = nn                            

                        elseif np in g.nodes # it's not in dg (but in g)
                            sn = m.newvar()
                            nn = m.NExt(sn)
                            push!(dg2, nn)
                            dg.ext_inodes[nn] = sn
                            dg.ext_onodes[np] = sn
                            n.parents[j] = nn
                            nmap[np] = nn

                        end    
                    end
                end
                append!(dg.nodes, dg2)    
                dg
                m.prune!(dg)
                m.simplify!(dg)
                collect(dg.ext_inodes)
                collect(dg.ext_onodes)
                collect(dg.set_inodes)
                collect(dg.set_onodes)

m.tocode(dg)

                # m.tograph( :( for i in 1:sz ; end ) )
                sa = m.newvar()
                fex = quote
                    sz = length( x )
                    st = sz ^ $(i-1)
                    $sa = zeros( $( Expr(:tuple, [:sz for j in 1:i]...) ) )
                    for $si in 1:sz
                        ($sa)[ (($si-1)*st+1):($si*st) ] = 1.
                    end
                    $sa
                end

test = m.tograph(fex)
sg = test.nodes[9].main[2]

                collect(sg.ext_inodes)
                collect(sg.ext_onodes)
                collect(dg.set_inodes)
                collect(dg.set_onodes)


                nr = m.addgraph!( m.tograph(fex), g, 
                    { :x => g.ext_inodes.vk[paramsym[1]] } )
                
                sg = nr.parents[1].main[2].set_onodes[nr]
                nr.parents[1].main[2].set_inodes.vk[sg].parents[2] = 

                blah blah  
                
                append!(g.nodes, dg.nodes)
                nn = collect(keys(dg.set_inodes))[1]  # only a single node produced
                ns = m.newvar("_out")
                g.set_inodes[nn] = ns
                push!(voi, ns)

                m.splitnary!(g)
                m.prune!(g)
                m.simplify!(g)
                
                m.calc!(g, params=Dict(paramsym, paramvalues), emod=evalmod)
            end

        end

        voin = map( s -> g.set_inodes.vk[s], voi)
        ex = m.addnode!(g, m.NCall(:tuple, voin))
        g.set_inodes = m.BiDict(Dict{m.ExNode,Any}( [ex], [:out]) )

        m.resetvar()
        println("=== tocode")
        m.tocode(g)
    end


diff( :(sin(x^2-log(y))) ,       x=2.,  y=1.)
diff( :(sin(x^2-log(y))) ,       x=2.,  y=1., order=0)
diff( :(sin(x))          ,   order=10,  x=2.)
diff( :(x^3)             ,    order=6,  x=2.)
diff( :(exp(x))          ,    order=6,  x=2.)
diff( :(exp(-2x))        ,    order=6,  x=2.)


