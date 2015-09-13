######################  setup   ################################
    Pkg.status()
    Pkg.test("ReverseDiffSource")

    cd(joinpath(Pkg.dir("ReverseDiffSource"), "test"))
    reload("ReverseDiffSource") ; m = ReverseDiffSource
    include("runtests.jl")
    include("loops.jl")


    module Sandbox
        type Bar
            x
            y
        end

        norm(z::Bar) = z.x*z.x + z.y*z.y
    end

    ex = quote
        z = Sandbox.Bar(2^a, sin(a))
        Sandbox.norm(z)
    end

    m.@deriv_rule  Sandbox.Bar(x,y)      x  ds[1]   # Derivative accumulator of x is increased by ds[1]
    m.@deriv_rule  Sandbox.Bar(x,y)      y  ds[2]   # Derivative accumulator of y is increased by ds[2]

    m.@deriv_rule  Sandbox.norm(z::Sandbox.Bar)  z  Any[ 2*z.x*ds , 2*z.y*ds ]  # Note : produces a 2-vector since z is a Bar

    res = m.rdiff(ex, a=0.)

    reload("ReverseDiffSource") ; m = ReverseDiffSource
    include(joinpath(Pkg.dir("ReverseDiffSource"), "src/ReverseDiffSource.jl")) ; m = ReverseDiffSource


    collect(keys(m.drules))[40:58]
    lst = collect(keys(m.drules))
    length(m.drules)
    lst[43][1] == sin
    lst[43] == (sin,1)
    dr2 = Dict(lst, collect(values(m.drules)))
    haskey(m.drules, (sin,1))
    m.drules[(sin,1)]
    m.__init__()

    haskey(m.drules, (cos,1))
    haskey(m.drules, (sin,1))
    haskey(m.drules, (similar,1))
    haskey(m.drules, (^,1))
    haskey(m.drules, (cos,1))

    include(joinpath(Pkg.dir("ReverseDiffSource"), "test/syntax.jl")) 


    Base.sin == sin
    ReverseDiffSource.sin == sin

    m.@deriv_rule sin(x) x cos(x) * ds

################ pb with latest julia (sept 15) ##########################

    cd(joinpath(Pkg.dir("ReverseDiffSource"), "test"))
    include("defs.jl")  # testing setup

    my_tests = [
                "unit_tests.jl",
                "parsing.jl",
                "syntax.jl",
                "firstorder.jl",
                "indexing.jl",
                "types.jl",
                "loops.jl",
                "functions.jl",
                "more.jl"
               ]

    println("Running tests:")

    for my_test in my_tests
        println("  * $(my_test) *")
        include(my_test)
    end

    rosenbrock(x) = (1 - x[1])^2 + 100(x[2] - x[1]^2)^2   # function to be derived
    rosenbrock([1,2])
    rosenbrock([0.5,2])
    rosen2 = m.rdiff(rosenbrock, (ones(2),), order=2)       # orders up to 2
    rosen2 = m.rdiff(rosenbrock, (ones(2),), order=1)       # orders up to 2
    rosen2([1,2])
    rosen2([0.5,2])

    println("Finished")

    methods(getindex)

    current_module()
    Main.getindex
    eval( :( (Main.(+)) ) )

    m.tograph(:( 1+x[2] ))

    m.rdiff( :( (1 - x[1])^2 + 100(x[2] - x[1]^2)^2 ) , x=ones(2), order=2, evalmod=Main)

    Main.sin(1.2)

    isdefined(:(+))
    Main.isdefined(:(+))

    function rdiff(f::Function, sig0::Tuple; args...)

        # f = rosenbrock ; sig0 = (ones(2),)
        sig = map( typeof, sig0 )
        fs = methods(f, sig)
        length(fs) == 0 && error("no function '$f' found for signature $sig")
        length(fs) > 1  && error("several functions $f found for signature $sig")  # is that possible ?

        fdef  = fs[1].func.code
        fcode = Base.uncompressed_ast(fdef)

        fargs = fcode.args[1]  # function parameters
        cargs = [ (fargs[i], sig0[i]) for i in 1:length(sig0) ]

        ex  = m.transform(fcode.args[3]) # TODO : add error messages if not parseable
        dex = rdiff(ex; args..., cargs...)

            ex₁ = fcode.args[3]
            ex₂ = m.streamline(ex₁)
            ex₃ = m.e2s(ex₂)

        # Note : new function is created in the same module as original function
        myf = fdef.module.eval( :( $(Expr(:tuple, fargs...)) -> $dex ) )
    end




    quote 
        (Main.+)( (Main.^)((Main.-)(1,(Main.getindex)(x,1)) ,2),(Main.*)(100,(Main.^)((Main.-)((Main.getindex)(x,2),(Main.^)((Main.getindex)(x,1),2)),2)))
    end

    x = ones(2)
    eval(ex)

        ex.args[1].args[1]

################ pb with latest julia  ##########################
    lsk = collect(keys(m.drules))

    lsk[56][1] == ^
    m.getrule(^, 1)

    function getrule(f, pos) # f = ^  ; pos=1
        if isa(f, Function) && isa(f.env, Symbol) # non generic functions are matched by their name
            haskey(m.drules, (f.env, pos)) && return m.drules[(f.env, pos)]
        else
            haskey(m.drules, (f, pos)) && return m.drules[(f, pos)]  
        end
        error("no derivation rule for $(f) at arg #$(pos)")
    end

############## external symbols resolution  #########################



###################### issue #8   ######################################
    reload("ReverseDiffSource") ; m = ReverseDiffSource

    m.drules[(+,1)]


    ex = :( (1 - x[1])^2 + 100(x[2] - x[1]^2)^2 )
    res = m.rdiff(ex, x=zeros(2), order=2)   # 29 lines
    res = m.rdiff(ex, x=zeros(2), order=3)   # 73  lines (devl)
    res = m.rdiff(ex, x=zeros(2), order=4)   # 211 lines

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


