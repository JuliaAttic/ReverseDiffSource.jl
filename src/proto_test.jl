################## setindex  #######################
    ex = quote
        a=zeros(2)
        a[2] = x 
        res = sum(a)
    end

    reload("ReverseDiffSource") ; tm = ReverseDiffSource
    g = tm.tograph(ex)
    tm.splitnary!(g)
    tm.simplify!(g)
    tm.tocode(g)

    tm.prune!(g, {g.set_inodes.vk[:res]})

    begin
        g.nodes[2]
        ns2[4]
        ns2[4] == g.nodes[2]
        g.nodes[5] in ns2
        dump(ns2[4])
        dump(g.nodes[2])

        g.nodes[4].parents[2] in ns2
        mx = g.map.vk[(:x, :in_inode)]
        mx in g.nodes
        ma = g.map.vk[(:a, :in_inode)]
        ma in g.nodes

        g.nodes[4].parents[2] == mx
        g.nodes[3] == mx
        hash(g.nodes[3])

        g.map[mx]
        haskey(g.map.vk, (:x, :out_inode))
        haskey(g.map.vk, (:x, :in_inode))


        map(hash, g.nodes)
        map(hash, keys(g.map.kv))
        # map(hash, values(g.map.vk))
        g.nodes

        collect(g.map.kv)

        map(hash, g.nodes[3].parents)
        ex = :( a[2] = x )
        ex.head
    end

    tm.calc!(g, params = {:x => 1})
    g.nodes
    g2 = tm.reversegraph(g, g.map.vk[(:res, :out_inode)], [:x])
    g.nodes = [ g.nodes, g2.nodes]
    collect(g.map.kv)
    collect(g2.map.kv)
    g.map[ g2.map.vk[(:dx, :out_inode)] ] = (:dx, :out_inode)
    nn = g.map.vk[(:a, :out_inode)]
    delete!(g.map.kv, nn)
    delete!(g.map.vk, (:a, :out_inode))
    g.nodes
    tm.tocode(g)
    tm.prune!(g); tm.tocode(g)
    tm.simplify!(g); tm.tocode(g)
    g.map = tm.BiDict([nn], )


################## for loops  #######################

    reload("ReverseDiffSource") ; tm = ReverseDiffSource

    ex = quote
        a=0
        for i in 1:2
            a += 2x    
        end
    end

    ex = quote
        a=zeros(10+6)
        for i in 1:10
            t = 4+3+2
            a[i] += b[i]+t
        end
        z=sum(a)
    end

    # reversediff(ex, :a, x = 1)
    reload("ReverseDiffSource") ; tm = ReverseDiffSource

    g = tm.tograph(ex);
    tm.splitnary!(g)
    collect(g.set_inodes)

    tm.prune!(g, [g.set_inodes.vk[:z]])
    tm.simplify!(g)
    tm.calc!(g, params = {:b => ones(10), :x => 1})
    g.nodes
    g2 = tm.reversegraph(g, g.set_inodes.vk[:z], [:x])
    g.nodes = [ g.nodes, g2.nodes ]

    tm.tocode(g)
    tm.splitnary!(g); tm.tocode(g)
    tm.prune!(g); tm.tocode(g)
    tm.simplify!(g); tm.tocode(g)


    reload("ReverseDiffSource") ; tm = ReverseDiffSource
    ex = quote
        a=zeros(2)
        for i in 1:2
            a[i] = x 
        end
        res = sum(a)
    end
    g = tm.tograph(ex); tm.tocode(g)
    tm.prune!(g); tm.tocode(g)
    tm.simplify!(g); tm.tocode(g)
    tm.calc!(g, params = {:x => 1})
    g2 = tm.reversegraph(g, g.setmap[:res], [:x])
    g.nodes = [ g.nodes, g2.nodes]
    g.setmap = merge(g.setmap, g2.setmap)
    tm.tocode(g)
    tm.prune!(g); tm.tocode(g)
    tm.simplify!(g); tm.tocode(g)





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

    reload("ReverseDiffSource") ; tm = ReverseDiffSource
    ex = quote
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

    reversediff(ex, nothing, v=1, b=[1], x=4)

    g = tm.tograph(ex);
    tm.splitnary!(g)
    tm.simplify!(g) ; tm.tocode(g)
    tm.calc!(g, params = {:x => 4, :v => 1, :b =>zeros(10)})

    g2 = tm.reversegraph(g, g.setmap[nothing], [:x])
    g.nodes = [ g.nodes, g2.nodes]
    g.setmap = merge(g.setmap, g2.setmap)
    tm.tocode(g)

    tm.prune!(g); tm.tocode(g)

    g.nodes

    g2 = tm.copy(g)

    tm.simplify!(g); tm.tocode(g)

    g = tm.tograph(ex)
    collect(keys(g.inmap))
    tm.calc!(g, params= {:v => 1., :b => -1, :x => 4})
    g.setmap[:a].val

    collect(keys(g.inmap))
    collect(keys(g.outmap))
    collect(keys(g.setmap))
    g.nodes
    tm.evalsort!(g)
    tm.tocode(g)
    g.setmap[:a]


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

    reversediff(:( sin(x * a.x)), nothing, x=1)
    reversediff(:( x * a.x), [:x])
    reversediff(:( x * a.x), [:a])

    norm(t::Test1) = t.x*t.x + t.y*t.y
    norm(a)
    Proto.@deriv_rule norm(t::Main.Test1) t    [ 2*t.x*ds , 2*t.y*ds ]

    out = reversediff( :( norm(a) ), [:a] )

    @eval function myf(a)
            $out
            (res, da)
        end

    myf(a)
    myf(Test1(3,3))


    using Distributions

    testedmod.type_decl(Normal,2)    

    testedmod.@deriv_rule mean(d::Main.Normal)          d      [ ds , 0. ]
    testedmod.@deriv_rule mean(d::Array{Main.Normal})   d      [ ds , zeros(size(ds)) ]
    testedmod.@deriv_rule Normal(mu, sigma)      mu     ds[1]
    testedmod.@deriv_rule Normal(mu, sigma)      sigma  ds[2]

    ex = quote
        d = Normal(x,y)
        res = mean(d)
    end

    @eval function exf(x,y)
        $(testedmod.reversediff(ex, :res, x=1.0, y=1.0))
        (res, dx, dy)
    end

    exf(2,2)

    ex = quote
        d = [ Normal(x,y), Normal(2x, y/2)]
        res = mean(d)
    end

    mean( d::Array{Main.Normal}) = [ mean(de) for de in d]
    testedmod.reversediff(ex, :res, x=1.0, y=1.0)

    @eval function exf(x,y)
        $(testedmod.reversediff(ex, :res, x=1.0, y=1.0))
        (res, dx, dy)
    end

    exf(2,2)

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

    ReverseDiffSource.@type_decl    Foo                                2   
    ReverseDiffSource.@deriv_rule   Foo(x,y)                           x      ds[1]
    ReverseDiffSource.@deriv_rule   Foo(x,y)                           y      ds[2]
    ReverseDiffSource.@deriv_rule   vcat(x,y)                          x      ds[1]
    ReverseDiffSource.@deriv_rule   vcat(x,y)                          y      ds[2]

    # ReverseDiffSource.@deriv_rule   Main.Sandbox.Foo(x,y)              x      ds[1]
    # ReverseDiffSource.@deriv_rule   Main.Sandbox.Foo(x,y)              y      ds[2]

    ReverseDiffSource.@deriv_rule   bar(t::Foo)                        t      [ 2*t.x*ds , 2*t.y*ds ]
    ReverseDiffSource.@deriv_rule   bar(ta::Array{Foo})                ta     [ 2*ta.x .* ds , 2*ta[2].*ds ]


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

    out = ReverseDiffSource.reversediff(ex, :res, y=1.0)
    @eval myf(y) = ( $out ; (res, dy) )
    myf(1.)

    g, d, exitnode  = ReverseDiffSource.tograph(ex)
    g.nodes

    # ReverseDiffSource.reversediff(ex, :res, y=1.)

        g.exitnodes = { :res => d[:res] }
        ReverseDiffSource.splitnary!(g)
        ReverseDiffSource.dedup!(g)
        ReverseDiffSource.simplify!(g)
        ReverseDiffSource.prune!(g)
        ReverseDiffSource.evalsort!(g)
        ReverseDiffSource.calc!(g, params={ :y => 1.})
    g.nodes

        dg, dnodes = ReverseDiffSource.reversegraph(g, g.exitnodes[:res], [:y])
        g.nodes = [g.nodes, dg]
        g.exitnodes[:dy] = dnodes[1]
        for i in 1:length(paramsym)
            g.exitnodes[ReverseDiffSource.dprefix(paramsym[i])] = dnodes[i]
        end
        # println(g.nodes)

        # g.exitnodes[:dy] == g.nodes[4]
        # g.exitnodes[:dx] == g.nodes[4]
        # g.exitnodes[:res] == g.nodes[3]
        # println(g.nodes)

        # (g2, length(g2))
        ReverseDiffSource.splitnary!(g)
        ReverseDiffSource.dedup!(g)
        ReverseDiffSource.evalconstants!(g)
        ReverseDiffSource.simplify!(g)
        ReverseDiffSource.prune!(g)
        #  (g2, length(g2))
        ReverseDiffSource.evalsort!(g)
    g.nodes

        # println(g.nodes)
        ReverseDiffSource.tocode(g)








    module Test1
        type Foo
            a
        end

        f(x::Foo) = x.a

        export f
    end


    object_id(Test1)
    object_id(Test1.Foo)
    object_id(Test1.f)

    X = Test1.Foo
    "$X"
    names(X)

    Base.exprtype(:a)
    Base.resolve_relative(:Foo, Test1, Main, DataType, "blah")
    Base.resolve_globals(:(a+5 ; sin(x)), Main, Test1, [], [])
    Base.resolve_globals(:( bar(x::Foo, y::Real) ), Main, Test1, [], [])

    typeof(X)
    object_id(X)

    f2 = Test1.f
    object_id(f2)
    object_id(Real)
    object_id(Union(Float64, Int))
    object_id(Union(Int, Float64))
    object_id(Union(Int, Float64, Complex))
    object_id(Array{Int})
    object_id(Array{Int,2})

    import Test1.f

    object_id(Test1.f)

    f2(x::Int) = 3x
    Test1.f(x::Float64) = 2x

        dump(f2.env.defs)

    methods(Test1.Foo)
    methodswith(Test1.Foo)

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

