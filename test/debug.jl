######################  setup   ################################
    Pkg.status()

    cd(joinpath(Pkg.dir("ReverseDiffSource"), "test"))

    include("runtests.jl")

    using DataFrames

ex = quote
    a = x
    b = a
    c = b
    a += x
    c
end
m.rdiff(ex, x=1)

###################### issue #8   ######################################
    reload("ReverseDiffSource")
    m = ReverseDiffSource

    ex = :( (1 - x[1])^2 + 100(x[2] - x[1]^2)^2 )
    # res = m.rdiff(ex, x=zeros(2), order=3)   # 75 lines
    res = m.rdiff(ex, x=zeros(2), order=2)   # 75 lines
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
    reload("ReverseDiffSource")
    m = ReverseDiffSource

    # ex = :( x[1]^3 )  # ok
    # ex = :( x[2]^3 )  # ok
    # ex = :( x[1]^2 + x[2]^2 ) # ok
    ex = :( x[1]^3 + x[2]^3 ) # pb cf plus bas
    # ex = :( x[1]^3 + x[1]*x[2]^2 )
    # ex = :( sum(x * x'))

    x0 = ones(2)
    res = m.rdiff(ex, x=x0, order=3)
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

######################  loops    #######################################
    reload("ReverseDiffSource")
    m = ReverseDiffSource

    ex = quote
    	a = 0.
    	for i in 1:length(x)
    		a += x[i]^i
    	end
    	a+1
    end
    x0 = ones(3)

    res = m.rdiff(ex, x=x0, order=1)
    res = m.rdiff(ex, x=x0, order=1, debug=true)
    res.nodes[11].main[2]
    @eval foo(x) = $res
    foo(x0)

    ex = quote
        a = 0.
        for i in 1:length(x)
            a += x[1]^i
        end
        a+1
    end
    x0 = ones(3)

    res = m.rdiff(ex, x=x0, order=1)
    @eval foo(x) = $res
    foo(x0)

    res = m.rdiff(ex, x=x0, order=2)
    @eval foo(x) = $res
    foo(x0)

    ex = quote
        a = 0.
        for i in 1:3
            a += x^i
        end
        a
    end
    res = m.rdiff(ex, x=2., order=3)
    @eval foo(x) = $res
    foo(2.)


############# loops ##################
    reload("ReverseDiffSource")
    m = ReverseDiffSource

    function check(ex)
        const δ = 1e-8
        x0 = 1.
        res = m.rdiff(ex, x=x0);
        nfoo = @eval foo(x) = $res
        println(" calc = $(nfoo(x0)[2][1]) vs vrai = $(round((nfoo(x0+δ)[1] - nfoo(x0)[1]) / δ,2)) ")
    end


    ex4 = quote
        a = zeros(2)
        for i in 1:2
            a[i] = x
        end
        sum(a)
    end
    check(ex4)  #  calc = 2.0 vs vrai = 2.0 
    m.rdiff(ex4, x=1.)


    #### function rdiff(ex; outsym=nothing, order::Int=1, evalmod=Main, params...)
        order = 1

        paramsym    = Symbol[ :x ]
        paramvalues = [ 1. ]
        parval      = Dict(paramsym, paramvalues)

        g = m.tograph(ex)

        # reduce to variable of interest
        g.seti = m.BiDict{m.ExNode,Any}([g.seti.vk[nothing]], [ nothing ])    

        g |> m.splitnary! |> m.prune! |> m.simplify!
        m.calc!(g, params=parval, emod=Main)

        voi = Any[ nothing ]

        g.nodes[6].main[2]

            dg = m.reversegraph(g, g.seti.vk[nothing], paramsym)

            m.tocode(dg)
            collect(keys(dg.seti))
            collect(keys(dg.exti))

            fdg = dg.nodes[9].main[2]
            m.tocode(fdg)
            collect(keys(fdg.seti))
            collect(keys(fdg.exti))


            append!(g.nodes, dg.nodes)
            nn = m.addnode!( g, m.NCall(:tuple, [ dg.seti.vk[m.dprefix(p)] for p in paramsym] ) )
            ns = m.newvar("_dv")
            g.seti[nn] = ns
            push!(voi, ns)

            g
            m.tocode(g)

            m.ancestors()

            m.prune!(g)     #  il disparait à cette étape !!!
            m.tocode(g)
            m.simplify!(g)
            m.tocode(g)


################## for loops  #######################
    function check(ex)
        const δ = 1e-8
        x0 = 1.
        res = m.rdiff(ex, x=x0);
        nfoo = @eval foo(x) = $res
        println(" calc = $(nfoo(x0)[2][1]) vs vrai = $(round((nfoo(x0+δ)[1] - nfoo(x0)[1]) / δ,2)) ")
    end

    ex = quote
        a=0
        for i in 1:2
            a += 2x    
        end
        a
    end
    check(ex)

    ex = quote
        a=0
        for i in 1:2
            a += x^2    
        end
        a
    end
    check(ex)

    b = [1:4]
    ex = quote
        a=zeros(1+4)
        for i in 1:4
            t = 4+3+2
            a[i] += b[i]+t-x
        end
        sum(a)
    end
    check(ex)

    ex = quote
        a=zeros(1+3)
        for i in 1:4
            t = 4+3+2
            a[i] += b[i]*x+t
        end
        sum(a)
    end
    check(ex)

    ex = quote
        a = 0
        for i in 1:4
           a = x
        end
        sum(a)
    end
    check(ex)  # 0 (1 expected)

    ex = quote
        a = zeros(1)
        for i in 1:4
           a[1] = x
        end
        a[1]
    end
    check(ex)  # 4.0 (1 expected)
    m.rdiff(ex, x=1.)



############### double loop   ################################

    reload("ReverseDiffSource") ; m = ReverseDiffSource
    function check(ex)
        const δ = 1e-8
        x0 = 1.
        res = m.rdiff(ex, x=x0);
        nfoo = @eval foo(x) = $res
        println(" calc = $(nfoo(x0)[2][1]) vs vrai = $(round((nfoo(x0+δ)[1] - nfoo(x0)[1]) / δ,2)) ")
    end

    ex = quote
        a=0
        for i in 1:10
            for j in 1:10
                a += (j < 4) * log(x) * sin(j)
            end
        end
        a
    end
    check(ex)


#################   debug  ###################
    ex = quote
        a = 0.
        for i in 1:4
            a += 2+x
        end
        a
    end

    #### function rdiff(ex; outsym=nothing, order::Int=1, evalmod=Main, params...)
    reload("ReverseDiffSource")
    m = ReverseDiffSource
    begin
        order = 1
        paramsym    = Symbol[ :x ]
        paramvalues = [ 1. ]
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

    m.tocode(dg)
    collect(keys(dg.seti))
    collect(keys(dg.exti))

    fdg = dg.nodes[9].main[2]
    m.tocode(fdg)
    collect(keys(fdg.seti))
    collect(keys(fdg.exti))


    append!(g.nodes, dg.nodes)
    nn = m.addnode!( g, m.NCall(:tuple, [ dg.seti.vk[m.dprefix(p)] for p in paramsym] ) )
    ns = m.newvar("_dv")
    g.seti[nn] = ns
    push!(voi, ns)

    g
    m.tocode(g)

    m.ancestors()

    m.prune!(g)     #  il disparait à cette étape !!!
    m.tocode(g)
    m.simplify!(g)
    m.tocode(g)


###############  slowness #################################
    ex = quote
        a = zeros(2,2)
        b = zeros(2)
        b[1] = b[1] + x[1]
        b[2] = b[2] + x[2]
        a[1] = b[1]
        a[2] = b[2]
        b[2] = b[2] + x[2]
        b[1] = b[1] + x[1]
        a[3] = b[1]
        a[4] = b[2]
        a[_idx2]
    end

    m.rdiff(ex, x=ones(2))
    @profile m.rdiff(ex, x=ones(2))
    Profile.print()

    s = open("c:/temp/prof.txt","w")
    Profile.print(s,cols = 500)
    close(s)

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
