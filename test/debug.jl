######################  setup   ################################
    Pkg.status()
    Pkg.test("ReverseDiffSource")

    cd(joinpath(Pkg.dir("ReverseDiffSource"), "test"))
    include("runtests.jl")
    include("loops.jl")

################ pb with latest julia  ##########################

    dump(fullcycle(:(a = [1,2])))

    a = fullcycle(:(a = [1,2]))
    eval(a)

    dump(:(a = [1,2]))


    op = zeros
    dump(op)
    methods(op)
    methods(methods)

    m = methods(op, (Int,))
    isempty(m) && error("[tocode] cannot find module of function $op")
    m[1].func.code.module

        # default translation
        if isa(op, DataType)
            mods =  try
                        fullname(op.name.module)
                    catch e
                        error("[tocode] cannot find module of DataType $op")
                    end                
            mt = tuple( mods..., op.name.name )

        elseif isa(op, Function)
            mods =  try
                        fullname(Base.function_module(op, (Any...)))
                    catch e
                        error("[tocode] cannot find module of function $op")
                    end                
            mt = tuple( mods..., symbol(string(op)) )

        else
            error("[tocode] call using neither a DataType nor a Function : $op")
        end

        # try to strip module names for brevity
        try
            mt2 = (:Base, mt[end])
            eval(:( $(mexpr(mt)) == $(mexpr(mt2)) )) &&  (mt = mt2)
            mt2 = (mt[end],)
            eval(:( $(mexpr(mt)) == $(mexpr(mt2)) )) &&  (mt = mt2)
        end

        Expr(:call, mexpr( mt ), Any[ valueof(x,n) for x in n.parents[2:end] ]...)



############## external symbols resolution  #########################
    reload("ReverseDiffSource") ; m = ReverseDiffSource

    m.tograph( :( sin(x) ))
    g = m.tograph( :( Base.sin(x) ))
    m.simplify!( g )

    g = m.tograph( :( Base.sin(4.) ))
    m.simplify!( g )

    ###################### modules ########################################
        module Abcd
            module Abcd2
                type Argf ; end
                function probe()
                    println(current_module())
                    eval( :( a = 1 ))
                    current_module().eval( :( a = 2 ) )
                end
                function probe2()
                    println(repr(Argf))
                end
            end
        end

        Abcd.Abcd2.probe()


        Abcd.Abcd2.probe2()
        a

        t = Abcd.Abcd2.Argf
        tn = t.name
        tn.module
        fullname(tn.module)

        t = Abcd.Abcd2
        names(t)
        typeof(t)

        tn = t.name
        tn.module
        fullname(tn.module)


        t = Abcd.Abcd2.probe2


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

