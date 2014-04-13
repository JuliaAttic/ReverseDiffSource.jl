reload("ReverseDiffSource") ; tm = ReverseDiffSource
# include(joinpath(Pkg.dir("ReverseDiffSource"), "test/unit_tests.jl"))

################## setindex  #######################

    reload("ReverseDiffSource") ; tm = ReverseDiffSource
    ex = quote
        a=zeros(2)
        a[2] = x 
        res = sum(a)
    end
    g = tm.tograph(ex); tm.tocode(g)
    tm.evalconstants!(g); tm.tocode(g)
    tm.prune!(g); tm.tocode(g)
    tm.simplify!(g); tm.tocode(g)
    tm.calc!(g, params = {:x => 1})
    g2 = tm.reversegraph(g, g.setmap[:res], [:x])
    g.nodes = [ g.nodes, g2.nodes]
    g.setmap = merge(g.setmap, g2.setmap)
    tm.evalconstants!(g); tm.tocode(g)
    tm.prune!(g); tm.tocode(g)
    tm.simplify!(g); tm.tocode(g)



################## for loops  #######################

    ex = quote
        a=0
        for i in 1:2
            a += x
        end
    end
    g = tm.tograph(ex);
    tm.evalconstants!(g); tm.tocode(g)
    tm.calc!(g, params = {:x => 1})
    g2 = tm.reversegraph(g, g.setmap[:a], [:x])
    g.nodes = [ g.nodes, g2.nodes]
    g.setmap = merge(g.setmap, g2.setmap)
    tm.evalconstants!(g); tm.tocode(g)
    tm.prune!(g); tm.tocode(g)
    tm.simplify!(g); tm.tocode(g)

    g.nodes

    g.nodes[6].main[2].nodes
    # for (k,v) in g.nodes[8].main[2].inmap  ; println("$k => $v : pos = $(findin(g.nodes, [v])[1])") ; end
    # for p in g.nodes[8].parents  ; println("$p : pos = $(findin(g.nodes, [p])[1])") ; end
    # for (k,v) in g.nodes[8].main[2].outmap ; println("$k => $v") ; end

    # g2 = g
    # g2 = g.nodes[3].main[2]
    # g2 = g.nodes[8].main[2]
    # for (i,n) in zip(1:length(g2.nodes), g2.nodes)
    #     println("#$i - $n : ")
    #     for p in n.parents ; println("    $p : pos = $(findin(g2.nodes, [p])[1])") ; end
    # end

    reload("ReverseDiffSource") ; tm = ReverseDiffSource
    ex = quote
        a=zeros(2)
        for i in 1:2
            a[i] = x 
        end
        res = sum(a)
    end
    g = tm.tograph(ex); tm.tocode(g)
    tm.evalconstants!(g); tm.tocode(g)
    tm.prune!(g); tm.tocode(g)
    tm.simplify!(g); tm.tocode(g)
    tm.calc!(g, params = {:x => 1})
    g2 = tm.reversegraph(g, g.setmap[:res], [:x])
    g.nodes = [ g.nodes, g2.nodes]
    g.setmap = merge(g.setmap, g2.setmap)
    tm.evalconstants!(g); tm.tocode(g)
    tm.prune!(g); tm.tocode(g)
    tm.simplify!(g); tm.tocode(g)

    g.nodes[4].main[2].nodes[5]

    g3 = g.nodes[4].main[2]
  g2 = tm.ExGraph()
  nmap = Dict{tm.ExNode, tm.ExNode}()
  tm.evalsort!(g3)
  for n in g3.nodes

  n = g3.nodes[5]  
    println("node :     $n")
    n2 = tm.add_node(g2, copy(n))
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




#
    g = tm.tograph(:(a=0 ; a += x));
    tm.calc!(g, params = {:x => 1})
    g2 = tm.reversegraph(g, g.setmap[:a], [:x])
    g.nodes = [ g.nodes, g2.nodes]
    g.setmap = merge(g.setmap, g2.setmap)
    tm.tocode(g)

    g = tm.tograph(:(a=0 ; a += 2x));
    tm.calc!(g, params = {:x => 1})
    g2 = tm.reversegraph(g, g.setmap[:a], [:x])
    g.nodes = [ g.nodes, g2.nodes]
    g.setmap = merge(g.setmap, g2.setmap)
    tm.tocode(g)



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

    tm.reversegraph(g, g.setmap[:aa], [:b, :x])
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

#############################################################
    g = tm.tograph(:(a[2]=x ; res=a[2]));
    tm.calc!(g, params = {:a => ones(3), :x => 0})
    g2 = tm.reversegraph(g, g.setmap[:res], [:x])
    g.nodes = [ g.nodes, g2.nodes]
    g.setmap = merge(g.setmap, g2.setmap)
    tm.tocode(g)
    g2.nodes

#############################################################

    reload("ReverseDiffSource") ; tm = ReverseDiffSource
    ex = quote
        a = 2 * b
        aa = sum(a)
    end

    g = tm.tograph(ex);
    tm.evalconstants!(g)
    g.setmap
    tm.prune!(g)
    tm.simplify!(g)
    tm.calc!(g, params = {:b => ones(10)})
    g2 = tm.reversegraph(g, g.setmap[:aa], [:b])
    g2.nodes
    g2.setmap
    g.nodes = [ g.nodes, g2.nodes]
    g.setmap = merge(g.setmap, g2.setmap)
    collect(keys(g.setmap))
    tm.tocode(g)
    tm.evalconstants!(g)
    tm.prune!(g)
    tm.simplify!(g)
    tm.tocode(g)


g.nodes
    g2 = tm.tograph(:( a = ))

    # collect(keys(g.inmap))
    # collect(keys(g.outmap))
    # collect(keys(g.setmap))

    ex = quote
        a=zeros(10) ; z = 12 
        for i in 1:10
            t = x+z
            for j in 1:10
                u = t+z+v
                a[i] = b[i]+u
            end
        end
    end
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


########### testing big func  ##########
    ex = quote
        y = x * a * 1
        y2 = (x * a) + 0 + 3
        x += 1
        y3 = x * a
        y + y2 + y3 + 12
    end

    g = tm.tograph(ex)
    tm.calc!(g, params = {:x => 1.5, :a => -4})
    g.setmap[nothing].val


    out = ReverseDiffSource.reversediff(ex, x=1.5, a=-4 )

    @eval function myf(x)
            $out
            (res, dx)
        end

    myf(1.5)
    myf(1.5001)

########### testing big func 2 ##########
    ex = quote
        a = x * y + exp(-sin(4x))
        b = 1 + log(-a)
        b ^ a 
    end

    out = tm.reversediff(ex, [:x, :y])

    @eval function myf(x, y)
            $out
            (res, dx, dy)
    end
    y
    myf(1.5, -4)

    x0 = 1.5
    y0 = -4
    delta = 1e-6
    [ myf(x0,y0)[2]  (myf(x0+delta,y0)[1]- myf(x0,y0)[1])/delta ; 
        myf(x0,y0)[3]  (myf(x0,y0+delta)[1]- myf(x0,y0)[1])/delta]

##############   tests for composite types    #####################
    type Test1
        x
        y
    end

    a = Test1(1,2)

    x = 1.5
    Proto.type_decl(Test1, 2)
    Proto.@type_decl Main.Test1 2  # fails

    reversediff(:( sin(x * a.x)), [:x])
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


######################" misc "  ########################
pwd()
    reload("ReverseDiffSource")
    tm = ReverseDiffSource


    g, sv, ext, outsym = tm.tograph(:(  foo(x,y)))

    g.nodes

    g, sv, ext, outsym = tm.tograph(:(  foo(x::Real,y)))




 tm.ispivot(g.nodes[1], g)

n = g.nodes[1]
tm.ispivot(n, g)


    nbref = 0
    for n2 in g.nodes  # n2 = g.nodes[3]
        np = sum(i -> is(i, n), n2.parents)
        (np == 0) && continue

        isa(n2, tm.NFor) && return true    # force assignment if used in for loops
        isa(n2, Union(tm.NSRef, tm.NSDot)) && 
            is(n2.parents[1], n) && return true  # force if setindex/setfield applies to it

        nbref += np
        (nbref >= 2) && return true  # if used more than once
    end

nbref