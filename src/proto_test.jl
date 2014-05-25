################## setindex  #######################
    reload("ReverseDiffSource") ; m = ReverseDiffSource

    m.tograph(:(  a[5] ))
    m.tograph(:(  a[5] = 3 ))

    g = m.tograph(:(  α = 3.0 ; β = α ))
    g.set_inodes


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

    reload("ReverseDiffSource") ; m = ReverseDiffSource

    g = m.tograph(ex)
    m.splitnary!(g)
    m.simplify!(g)
    m.prune!(g, {g.set_inodes.vk[nothing]})
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

        ex = :( x * a.x)
        g = tm.tograph(ex)
        exitnode = g.set_inodes.vk[nothing]
            # g.set_inodes[ exitnode] = :out

        tm.splitnary!(g)
        println("=== prune")
        tm.prune!(g, [exitnode])
        println("=== simplify")
        tm.simplify!(g)
        tm.tocode(g)

        println("=== calc")
        tm.calc!(g, params={:x => 1.})
        
        g.nodes
        println("=== reversegraph")
        dg = tm.reversegraph(g, exitnode, [:x])
        g.nodes = [g.nodes, dg.nodes]
        g.set_inodes = tm.BiDict(merge(g.set_inodes.kv, dg.set_inodes.kv))

        g.nodes[30:40]
        g2 = g.nodes[33].main[2]
        collect(g2.set_inodes.kv)
        collect(g2.set_onodes.kv)
        collect(g2.ext_onodes.kv)
        collect(g2.ext_inodes.kv)
        g2.nodes

        tm.splitnary!(g)
        println("=== prune2")
        tm.prune!(g)
        println("=== simplify2")
        tm.simplify!(g)

        resetvar()

        println("=== tocode")
        tm.tocode(g)
        collect(g.nodes)

        ex = :( (nds=zeros(size(d)); for i in 1:length(d) ; nds[i] = ds[i] ; end ; nds)  )
        g = tm.tograph(ex)
        tm.simplify!(g)
        tm.prune!(g, [g.set_inodes.vk[nothing]])
        tm.tocode(g)

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

        ex2 = :(nds=zeros(2); for i in 1:2 ;
                nds[i] = ds[i] ; end ; { nds , zeros(2) } )
        g = tm.tograph(ex2)
        collect(g.set_inodes.kv)
        collect(g.ext_inodes.kv)
        g.nodes


        g = tm.tograph(ex)
        exitnode = g.set_inodes.vk[:z]

        tm.splitnary!(g)
        tm.prune!(g, [exitnode])
        tm.simplify!(g)
        tm.tocode(g)

        println("=== calc")
        tm.calc!(g, params={:x => 1.})
        g.nodes        

        println("=== reversegraph")
        dg = tm.reversegraph(g, exitnode, [:x])
        g.nodes = [g.nodes, dg.nodes]
        g.set_inodes = tm.BiDict(merge(g.set_inodes.kv, dg.set_inodes.kv))

        g.nodes[20:40]
        g2 = g.nodes[33].main[2]
        collect(g2.set_inodes.kv)
        collect(g2.set_onodes.kv)
        collect(g2.ext_inodes.kv)
        collect(g2.ext_onodes.kv)
        dsn = g2.ext_onodes.vk[:ds]
        dsn in g.nodes
        g2.nodes

        tm.splitnary!(g)
        tm.prune!(g)
        tm.simplify!(g)

        resetvar()

        println("=== tocode")
        tm.tocode(g)
        collect(g.nodes)

        ex = :( (nds=zeros(size(d)); for i in 1:length(d) ; nds[i] = ds[i] ; end ; nds)  )
        g = tm.tograph(ex)
        tm.simplify!(g)
        tm.prune!(g, [g.set_inodes.vk[nothing]])
        tm.tocode(g)

        d2 = 

    @eval exref(x) = ($ex  ; (z,) )
    @eval exrds(x) = ($res ; (z, dx))

    exref(2.)
    exref(2.001)
    exrds(2.)

    x = 2.
        _tmp1 = zeros(2)
        _tmp2 = vcat(Normal(x,1.0),Normal(2x,1.0))
        _tmp3 = foo(_tmp2)
        z = sum(_tmp3)
        _tmp4 = zeros(size(_tmp2))
        _tmp5 = size(_tmp3)
        _tmp6 = Base.cell_1d(_tmp4,_tmp4) + Base.cell_1d(zeros(_tmp5) + ones(_tmp5),_tmp4)
        _tmp7 = _tmp1 + _tmp6[1]
        _tmp8 = _tmp1 + _tmp6[2]
        dx = 2 * _tmp8[1] + _tmp7[1]



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


############ higher order  ndiff #####################

    function ndiff(ex, order::Int, paramsym::Symbol, outsym=nothing)
        # ex = :( 2^x ) ; paramsym = [:x] ; outsym = nothing
        g, d, exitnode = Proto.tograph(ex)
        (outsym != nothing) && !haskey(d, outsym) && error("can't find output var $outsym")
        (outsym == nothing) && (exitnode == nothing) && error("can't identify expression's output")
        
        (exitnode, outsym) = outsym == nothing ? (exitnode, :res) : ( d[outsym], outsym) 

        ndict = { outsym => exitnode, :exitnode => exitnode}
        Proto.splitnary!(g)
        Proto.dedup!(g, ndict)
        Proto.simplify!(g, ndict)
        Proto.prune!(g, ndict)
        Proto.evalsort!(g)

        for i in 1:order
            Proto.calc!(g)
            dg, dnodes = Proto.reversegraph(g, ndict[:exitnode], [paramsym])
            g = [g, dg]
            ndict[ Proto.dprefix("$i$paramsym") ] = dnodes[1]
            ndict[ :exitnode ] = dnodes[1]

            Proto.splitnary!(g)
            Proto.dedup!(g, ndict)
            Proto.evalconstants!(g)
            Proto.simplify!(g, ndict)
            Proto.prune!(g, ndict)
            Proto.evalsort!(g)

            exitnode = dnodes[1]
        end
        #  (g2, length(g2))
        delete!(ndict, :exitnode)
        Proto.tocode(g, ndict)
    end

    a = 1.5
    ndiff(:( sin(x) ), 14, :x) 
    ex = ndiff(:( exp(-x*x) ), 3, :x) 

    @eval function bar(x)
                    $ex
                    (res, d1x, d2x, d3x)
            end

            
    bar(1.0)
    bar(0.1)

    # check that d(bar)/dx is correct
    [ bar(1.0)[2] (bar(1.001)[1]-bar(1.)[1]) / 0.001 ]

