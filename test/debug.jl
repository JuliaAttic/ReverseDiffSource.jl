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
    #=(2.0,[3.0,3.0],
    2x2 Array{Float64,2}:
     6.0  0.0
     0.0  6.0,

    2x2x2 Array{Float64,3}:
    [:, :, 1] =
     0.0  0.0
     0.0  0.0

    [:, :, 2] =
     0.0  0.0
     0.0  0.0)=#

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
    	a
    end

    x0 = ones(3)
    res,g = m.rdiff(ex, x=x0, order=3)
    @eval foo(x) = $res
    foo(x0)

    ex = quote
        a = 0.
        for i in 1:length(x)
            a += i+i
        end
        a
    end
    res,g = m.rdiff(ex, x=x0, order=3)
    @eval foo(x) = $res
    foo(x0)


    foo (generic function with 1 method)
    (3.0,[1.0,2.0,3.0],
    3x3 Array{Float64,2}:
     0.0  0.0  0.0
     0.0  2.0  0.0
     0.0  0.0  6.0,

    3x3x3 Array{Float64,3}:
    [:, :, 1] =
     0.0  0.0  0.0
     0.0  0.0  0.0
     0.0  0.0  0.0

    [:, :, 2] =
     0.0  0.0  0.0
     0.0  0.0  0.0
     0.0  0.0  0.0

    [:, :, 3] =
     0.0  0.0  0.0
     0.0  0.0  0.0
     6.0  6.0  6.0)    <<<<<<

############# loops ##################
    reload("ReverseDiffSource")
    m = ReverseDiffSource

    ex = quote
        a = 0.
        for i in 1:4
            a += 2+x
        end
        a
    end

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


