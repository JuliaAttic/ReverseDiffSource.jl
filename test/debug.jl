######################  setup   ################################
    Pkg.status()

    cd(joinpath(Pkg.dir("ReverseDiffSource"), "test"))

    include("runtests.jl")

    using DataFrames

###################### issue #8   ######################################
    reload("ReverseDiffSource")
    m = ReverseDiffSource

    ex = :( (1 - x[1])^2 + 100(x[2] - x[1]^2)^2 )
    res = m.rdiff(ex, x=zeros(2), order=3)
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
    2x2 Array{Float64,2}:
     1200.0  -400.0
     -400.0     0.0

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
    ex = quote
    	a = 0.
    	for i in 1:length(x)
    		a += x[i]^i
    	end
    	a+1
    end
    # ERROR: assertion failed: [fusenodes] attempt to fuse ext_inode [external] :da (NaN)

    x0 = ones(3)
    res = m.rdiff(ex, x=x0, order=1)
    @eval foo(x) = $res
    foo(x0)


################  inconsistent NSRef   ###############
    reload("ReverseDiffSource")
    m = ReverseDiffSource


    function check(ex)
        const δ = 1e-8
        x0 = [1., 1.]
        res = m.rdiff(ex, x=x0);
        nfoo = @eval foo(x, _idx2) = $res
        println( mapreduce(i -> nfoo(x0,i)[2][1], hcat, 1:4) )
        println( Float64[ round((nfoo(x0+δ*eye(2)[:,v],i)[1] - nfoo(x0,i)[1]) / δ)  for v in 1:2, i in 1:4 ])
    end


    _idx2 = 1
    ex3 = quote
        a = zeros(2,2)
        b = zeros(2)

        b[2] = b[2] + x[2]
        b[1] = b[1] + x[1]
        a[1:2] = b

        b[2] = b[2] + x[2]
        b[1] = b[1] + x[1]
        a[3:4] = b

        a[_idx2]
    end
    check(ex3)
    # [1.0 0.0 2.0 0.0
    #  0.0 1.0 0.0 2.0]
    # [1.0 0.0 2.0 0.0
    #  0.0 1.0 0.0 2.0]


    ex3 = quote
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
    check(ex3)
    # [1.0 0.0 2.0 0.0
    #  0.0 1.0 0.0 2.0]
    # [1.0 0.0 2.0 0.0
    #  0.0 1.0 0.0 2.0]

    ex3 = quote
        a = zeros(4)
        b = zeros(2)

        b += x
        a[1:2] = b
        b[1] = b[1] + x[1]
        # b[2] = b[2] + x[2]
        a[3:4] = b

        a[_idx2]
    end
    check(ex3)
    # [1.0 0.0 2.0 0.0
    #  0.0 1.0 0.0 1.0]
    # [1.0 0.0 2.0 0.0
    #  0.0 1.0 0.0 1.0]



    ex3 = quote
        a = zeros(2,2)

        a[1] = x[1]
        a[3] = x[1]
        a[4] = x[1]
        a[1] = x[1]  
        a[_idx2]
    end
    check(ex3)
    # [1.0 0.0 1.0 1.0
    #  0.0 0.0 0.0 0.0]
    # [1.0 0.0 1.0 1.0
    #  0.0 0.0 0.0 0.0] ok






    ex3 = quote
        a = zeros(2,2)
        b = fill(0.0, size(x))

        b[2] = b[2] + x[2]
        b[1] = b[1] + x[1]
        a[1:2] = b
        a[_idx2]
    end
    check(ex3)
    # [1.0 0.0 0.0 0.0
    #  0.0 1.0 0.0 0.0]
    # Any[1.0 0.0 0.0 0.0
    #     0.0 1.0 0.0 0.0]  , OK

    ex3 = quote
        a = zeros(2,2)
        b = zeros(2)

        b[2] = b[2] + x[2]
        b[1] = b[1] + x[1]
        # a[1:2] = b
        a[1] = b[1]
        a[2] = b[2]
        a[_idx2]
    end
    check(ex3) # OK

    ex3 = quote
        a = zeros(2,2)
        a[1:2] = x
        a[_idx2]
    end
    check(ex3)  # ok

    ex3 = quote
        a = zeros(2,2)
        a[1:2] = x[1]
        a[_idx2]
    end
    check(ex3)  # OK

    # x0 = [1., 1.]
    # res = m.rdiff(ex3, x=x0);
    # @eval foo(x, _idx2) = $res
    # # map(i -> foo(x0,i), 1:4)
    # δ = 1e-8
    # [ (foo(x0+δ*eye(2)[:,v],i)[1] - foo(x0,i)[1]) / δ  for v in 1:2, i in 1:4 ]

################  inconsistent NSRef   ###############
    reload("ReverseDiffSource")
    m = ReverseDiffSource

    function check(ex)
        const δ = 1e-8
        x0 = 1.
        res = m.rdiff(ex, x=x0);
        nfoo = @eval foo(x) = $res
        println(" calc = $(nfoo(x0)[2][1]) vs vrai = $(round((nfoo(x0+δ)[1] - nfoo(x0)[1]) / δ)) ")
    end

    ex4 = quote
        a = zeros(2)
        a[1] = x
        a[2] = x
        sum(a)
    end
    check(ex4) #    calc = 2.0 vs vrai = 2.0 

    m.rdiff(ex4, x=1.)

    ex4 = quote
        a = zeros(2)
        a[1] = x
        a[1] = x
        sum(a)
    end
    check(ex4) #  calc = 1.0 vs vrai = 1.0 
    m.rdiff(ex4, x=1.)

    ex4 = quote
        a = zeros(2)
        a[1] = 3x
        a[1] = x
        sum(a)
    end
    check(ex4)     #  calc = 1.0 vs vrai = 1.0 

    ex4 = quote
        a = zeros(2)
        a[1] = x
        a[1] = 3x
        sum(a)
    end
    check(ex4)     #   calc = 3.0 vs vrai = 3.0 

############# loops ##################
    reload("ReverseDiffSource")
    m = ReverseDiffSource

    function check(ex)
        const δ = 1e-8
        x0 = 1.
        res = m.rdiff(ex, x=x0);
        nfoo = @eval foo(x) = $res
        println(" calc = $(nfoo(x0)[2][1]) vs vrai = $(round((nfoo(x0+δ)[1] - nfoo(x0)[1]) / δ)) ")
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

    ex4 = quote
        a = zeros(2)
        for i in 1:2
            a[1] = x
        end
        sum(a)
    end
    check(ex4)  #  calc = 1.0 vs vrai = 1.0 
    m.rdiff(ex4, x=1.)


    #### function rdiff(ex; outsym=nothing, order::Int=1, evalmod=Main, params...)
                
                ex = ex4
                order = 1

                paramsym    = Symbol[ :x ]
                paramvalues = [ 1. ]
                parval      = Dict(paramsym, paramvalues)

                g = m.tograph(ex)

                # reduce to variable of interest
                g.seti = m.BiDict{m.ExNode,Any}([g.seti.vk[nothing]], [ nothing ])    

                g |> m.splitnary! |> m.prune! |> m.simplify!
                m.calc!(g, params=parval, emod=Main)

                ov = g.seti.vk[nothing].val 

                voi = Any[ nothing ]

                    dg = m.reversegraph(g, g.seti.vk[nothing], paramsym)
                    append!(g.nodes, dg.nodes)
                    nn = m.addnode!( g, m.NCall(:tuple, [ dg.seti.vk[m.dprefix(p)] for p in paramsym] ) )
                    ns = m.newvar("_dv")
                    g.seti[nn] = ns
                    push!(voi, ns)

                    dg.nodes[18].main[2]
                    g.nodes[26].main[2]

                    g
                    m.tocode(g)

                    m.ancestors()

                    m.prune!(g)     #  il disparait à cette étape !!!
                    m.tocode(g)
                    g |> simplify!



    m.@deriv_rule %(x,y)      x     0
    m.@deriv_rule %(x,y)      y     0

    ex4 = quote
        a = zeros(2)

        for i in 1:4
            a[1 + i % 2] = x
        end

        sum(a)
    end
    check(ex4)  #  calc = 2.0 vs vrai = 2.0 
    m.rdiff(ex4, x=1.)

    ex4 = quote
        a = zeros(2)

        for i in 1:4
            a[1] = x
        end

        sum(a)
    end
    check(ex4)  # calc = 1.0 vs vrai = 1.0 

    ex4 = quote
        a = 0.
        a = 3x
        a = x
        a
    end
    check(ex4)  #  calc = 1.0 vs vrai = 1.0 

    ex4 = quote
        a = zeros(2)
        a += 3x
        a = x
        a
    end
    check(ex4)  #  calc = 1.0 vs vrai = 1.0 

################## setindex  #######################
    reload("ReverseDiffSource") ; m = ReverseDiffSource

    ex = quote
        a=zeros(5)
        a[3:5] = x 
        sum(a)
    end
    check(ex)

    ex = quote
        a = ones(5)
        b = sum(a)*x
        a[2] += x
        c = sum(a)
        b + c
    end
    check(ex)

    x = ones(4)
    ex = quote
        a=zeros(1+6)
        z=sum(a)
        for i in 1:4
            t = 4+3+2
            a[i] += b[i]+t
        end
        sum(a) + z
    end
    check(ex)
    m.reversediff(ex, b=ones(6))

    ex = quote
        a=zeros(3)
        b=zeros(3)
        b[2]=x
        a[1]=x
        sum(a)+sum(b)
    end
    m.reversediff(ex, x=1)


################## for loops  #######################

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


    ex = quote
        a=zeros(1+3)
        for i in 1:4
            t = 4+3+2
            a[i] += b[i]+t
        end
        z=sum(a)
    end

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
