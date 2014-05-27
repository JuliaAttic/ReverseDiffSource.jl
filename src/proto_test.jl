################## setindex  #######################
    reload("ReverseDiffSource") ; m = ReverseDiffSource

    m.tograph(:(  a[5] ))
    m.tograph(:(  a[5] = 3 ))

    g = m.tograph(:(  α = x ; β = α ; γ = α)) 
    m.reversediff( :(  α = x ; β = α ; γ = α), :α, x=1)  # correct
    m.reversediff( :(  α = x ; β = α ; γ = α), :β, x=1)  # ok, corrigé
    m.reversediff( :(  α = x ; β = α ; γ = α), :γ, x=1)  # ok, corrigé

    (x = 1 ; α = x ; α += 1 ; x + α)
    (x = [1] ; α = x ; α += 1 ; x + α)

    g = m.tograph(:(  α = fill(x,5) ; β = α[2] ))
    [collect(values(g.set_inodes))]


    include(joinpath(Pkg.dir("ReverseDiffSource"), "test/unit_tests.jl"))

    ex = quote
        a=zeros(5)
        a[3:5] = x 
        sum(a)
    end
    # TODO : deriv incorrect when broadcasting,  a[3:5] = x and x scalar
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

