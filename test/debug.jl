######################  setup   ################################
    Pkg.status()
    Pkg.test("ReverseDiffSource")

    cd(joinpath(Pkg.dir("ReverseDiffSource"), "test"))
    reload("ReverseDiffSource") ; m = ReverseDiffSource
    include("runtests.jl")
    include("loops.jl")

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
    Pkg.test("ReverseDiffSource")

    m.tograph( :( sin(x) ))
    g = m.tograph( :( Base.sin(x) ))
    m.simplify!( g )

    m.tograph( :( a + b ))



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

##############   loops in functions  #################################

    function tf(x)
        a = zeros(2)
        for i in 1:2
            a[i] = x
        end
        sum(a)
    end

    function tf(x)
        a=0
        for i in 1:10
            for j in 1:10
                a += log(3.+sin(j*x)+cos(i))
            end
        end
        a
    end
    dtf = m.rdiff(tf, (0,))
    dtf(1.)
    dtf(1.1)
    dtf(1.5)
    dtf(2.0)

    using Bokeh
    Bokeh.plot( sin , -2:2, title="tf(x)", plotfile="c:/temp/tf.html")
    showplot()

    Pkg.update()

    function tf(x)
        z = 0
        for i in 1:length(x)
            z = i * x[i]
        end
        return z
    end

    function tf(x)
        a=0
        for i in 1:10
            for j in 1:10
                a += log(x) * sin(j)
            end
        end
        a
    end
    # (f::Function, sig0::Tuple; order::Int=1, evalmod=Main, debug=false, allorders=true)
    f=tf;sig0=(1.,);order=1;evalmod=Main;debug=false;allorders=true

    sig = map( typeof, sig0 )
    fs = methods(f, sig)
    length(fs) == 0 && error("no function '$f' found for signature $sig")
    length(fs) > 1  && error("several functions $f found for signature $sig")  # is that possible ?

    fdef  = fs[1].func.code
    fcode = Base.uncompressed_ast(fdef)
    fargs = fcode.args[1]  # function parameters
    mex = fcode.args[3]

    mex2 = m.streamline(mex)
    dump( mex.args[4] )

    dump(mex2)
    s = "↑=→:a→0↓↑=→:__gensym0→↑call→:colon→1→10↓↓↑=→symbol(\"#s6957\")→↑call→:start→:__gensym0↓↓↑gotoifnot→↑call→:!→↑call→:done→:__gensym0→symbol(\"#s6957\")↓↓→1↓:(2: )↑=→:__gensym1→↑call→:next→:__gensym0→symbol(\"#s6957\")↓↓↑=→:i→↑call→:getfield→:__gensym1→1↓↓↑=→symbol(\"#s6957\")→↑call→:getfield→:__gensym1→2↓↓↑=→:__gensym2→↑call→:colon→1→10↓↓↑=→symbol(\"#s6958\")→↑call→:start→:__gensym2↓↓↑gotoifnot→↑call→:!→↑call→:done→:__gensym2→symbol(\"#s6958\")↓↓→5↓:(6: )↑=→:__gensym3→↑call→:next→:__gensym2→symbol(\"#s6958\")↓↓↑=→:j→↑call→:getfield→:__gensym3→1↓↓↑=→symbol(\"#s6958\")→↑call→:getfield→:__gensym3→2↓↓↑=→:a→↑call→:+→:a→↑call→:*→↑call→:log→:x↓→↑call→:sin→:j↓↓↓↓:(7: )↑gotoifnot→↑call→:!→↑call→:!→↑call→:done→:__gensym2→symbol(\"#s6958\")↓↓↓→6↓:(5: ):(4: ):(3: )↑gotoifnot→↑call→:!→↑call→:!→↑call→:done→:__gensym0→symbol(\"#s6957\")↓↓↓→2↓:(1: ):(0: )↑:a↓"

    ex  = m.transform(mex)
    dump(ex)

    #######
    s = m.e2s(streamline(mex))
    tex = m._transform(s)

    exreg = quote
        rg"(?<pre>.*?)"
        rg"(?<g0>:[#_].+?)" = rg"(?<range>.+?)"
        rg"(?<iter>.+)" = start(rg"\g{g0}")
        gotoifnot( !(done(rg"\g{g0}", rg"\g{iter}" )) , rg"(?<lab1>\d+)" )
        rg":\((?<lab2>\d+): \)"
        rg"(?<g1>.+?)" = next(rg"\g{g0}", rg"\g{iter}")
        rg"(?<idx>.+?)" = rg":(?:getfield|tupleref)"(rg"\g{g1}", 1)
        rg"\g{iter}"    = rg":(?:getfield|tupleref)"(rg"\g{g1}", 2)
        rg"(?<in>.*)"
        rg":\((?<lab3>\d+): \)"
        gotoifnot( !(!(done(rg"\g{g0}", rg"\g{iter}"))) , rg"\g{lab2}" )
        rg":\(\g{lab1}: \)"
        rg"(?<post>.*)"
    end
    sexp = e2s(m.streamline(exreg), true)
    rexp = Regex(e2s(streamline(rexp), true))
    mm = match(Regex(sexp), s)
    sexp2 = "(?<pre>.*?)\u2191=\u2192(?<g0>:[#_].+?)\u2192(?<range>.+?)\u2193\u2191=\u2192(?<iter>.+)\u2192\u2191call\u2192:start\u2192\\g{g0}\u2193\u2193\u2191gotoifnot\u2192\u2191call\u2192:!\u2192\u2191call\u2192:done\u2192\\g{g0}\u2192\\g{iter}\u2193\u2193\u2192(?<lab1>\\d+)\u2193:\\((?<lab2>\\d+): \\)\u2191=\u2192(?<g1>.+?)\u2192\u2191call\u2192:next\u2192\\g{g0}\u2192\\g{iter}\u2193\u2193\u2191=\u2192(?<idx>.+?)\u2192\u2191call\u2192:(?:getfield)|(?:tupleref)\u2192\\g{g1}\u21921\u2193\u2193\u2191=\u2192\\g{iter}\u2192\u2191call\u2192:(?:getfield)|(?:tupleref)\u2192\\g{g1}\u21922\u2193\u2193(?<in>.*):\\((?<lab3>\\d+): \\)\u2191gotoifnot\u2192\u2191call\u2192:!\u2192\u2191call\u2192:!\u2192\u2191call\u2192:done\u2192\\g{g0}\u2192\\g{iter}\u2193\u2193\u2193\u2192\\g{lab2}\u2193:\\(\\g{lab1}: \\)(?<post>.*)"
    sexp2 = "(?<pre>.*?)\u2191=\u2192(?<g0>:[#_].+?)\u2192(?<range>.+?)\u2193\u2191=\u2192(?<iter>.+)\u2192\u2191call\u2192:start\u2192\\g{g0}\u2193\u2193\u2191gotoifnot\u2192\u2191call\u2192:!\u2192\u2191call\u2192:done\u2192\\g{g0}\u2192\\g{iter}\u2193\u2193\u2192(?<lab1>\\d+)\u2193:\\((?<lab2>\\d+): \\)\u2191=\u2192(?<g1>.+?)\u2192\u2191call\u2192:next\u2192\\g{g0}\u2192\\g{iter}\u2193\u2193\u2191=\u2192(?<idx>.+?)\u2192\u2191call\u2192:(?:getfield|tupleref)\u2192\\g{g1}\u21921\u2193\u2193\u2191=\u2192\\g{iter}\u2192\u2191call\u2192:(?:getfield|tupleref)\u2192\\g{g1}\u21922\u2193\u2193(?<in>.*):\\((?<lab3>\\d+): \\)\u2191gotoifnot\u2192\u2191call\u2192:!\u2192\u2191call\u2192:!\u2192\u2191call\u2192:done\u2192\\g{g0}\u2192\\g{iter}\u2193\u2193\u2193\u2192\\g{lab2}\u2193:\\(\\g{lab1}: \\)(?<post>.*)"
   
    sexp2 = "(?<pre>.*?)\u2191=\u2192(?<g0>:[#_].+?)\u2192(?<range>.+?)\u2193\u2191=\u2192(?<iter>.+)\u2192\u2191call\u2192:start\u2192\\g{g0}\u2193\u2193\u2191gotoifnot\u2192\u2191call\u2192:!\u2192\u2191call\u2192:done\u2192\\g{g0}\u2192\\g{iter}\u2193\u2193\u2192(?<lab1>\\d+)\u2193:\\((?<lab2>\\d+): \\)\u2191=\u2192(?<g1>.+?)\u2192\u2191call\u2192:next\u2192\\g{g0}\u2192\\g{iter}\u2193\u2193\u2191=\u2192(?<idx>.+?)\u2192\u2191call\u2192:(?:tupleref|getfield)\u2192"
    sexp2 = "(?<pre>.*?)\u2191=\u2192(?<g0>:[#_].+?)\u2192(?<range>.+?)\u2193\u2191=\u2192(?<iter>.+)\u2192\u2191call\u2192:start\u2192\\g{g0}\u2193\u2193\u2191gotoifnot\u2192\u2191call\u2192:!\u2192\u2191call\u2192:done\u2192\\g{g0}\u2192\\g{iter}\u2193\u2193\u2192(?<lab1>\\d+)\u2193:\\((?<lab2>\\d+): \\)\u2191=\u2192(?<g1>.+?)\u2192\u2191call\u2192:next\u2192\\g{g0}\u2192\\g{iter}\u2193\u2193\u2191=\u2192(?<idx>.+?)\u2192\u2191call\u2192"
    sexp2 = "(?<pre>.*?)\u2191=\u2192(?<g0>:[#_].+?)\u2192(?<range>.+?)\u2193\u2191=\u2192(?<iter>.+)\u2192\u2191call\u2192:start\u2192\\g{g0}\u2193\u2193\u2191gotoifnot\u2192\u2191call\u2192:!\u2192\u2191call\u2192:done\u2192\\g{g0}\u2192\\g{iter}"

    mm = match(Regex(sexp2), s)
    mm.captures
s
    rexp
    s



    function _transform(s::AbstractString)
        # s = inside
        mm = match(m.rexp, s)
        if mm != nothing && length(mm.captures) >= 11
            pre, rg, idx, inside, post = mm.captures[[1,3,8,9,11]]
            exin = m._transform(inside)
            ef = Expr(:for, Expr(:(=), symbol(idx[2:end]), m.s2e(rg)[1] ), exin)

            return Expr(:block, [ m.s2e(pre) ; ef ; m.s2e(post)]...)
        else
            return Expr(:block, s2e(s)...)
        end
    end



    dex = rdiff(ex; args..., cargs...)



    ###
    pr = r"""
    (.*)\n
    \W+ (GenSym\(\d+\))\ =\ (.*)\n
    \W+ (.*)\ =\ start\(\g{-3}\)\n
    \W+ unless\ \!\(done\(\g{-3},\g{-1}\)\)\ goto\ (\d+)\n
    \W+ (\d+):
    \W+ (GenSym\(\d+\))\ =\ next\(\g{-6},\g{-4}\)\n
    \W+ (.*)\ =\ tupleref\(\g{-2},1\)\n
    \W+ \g{-5}\ =\ tupleref\(\g{-2},2\) # line 4:\n
    \W+ (.*)\n
    \W+ unless\ !\(!\(done\(\g{-8},\g{-6}\)\)\)\ goto\ \g{-4}\n
    \W+ \g{-5}: 
    (.*)
    """sx
    rc = match(pr, mes) ; rc.captures


    tf(x) = sum(x)
    dtf = m.rdiff(tf, (zeros(3),))
    dtf(zeros(3))
    dtf(ones(3))

    @compare tf v0ref
    @compare tf v1ref
    @compare tf v2ref


    function tf(x)
        a=zeros(1+3)
        for i in 1:4
            t = 4+3+2
            a[i] += b[i]*x+t
        end
        sum(a)
    end
    compare(:tf, 0.)
    @compare tf v0ref
    @compare tf 0.
    dtf = m.rdiff(tf, (0.,))
    dtf(0.)

    macro abcd(a,b)
        println(typeof(a), " ", a)
        println(typeof(b), " ", b)
    end
    @abcd tf 0.

    # f = tf ; sig0 = (0.,)
    sig = map( typeof, sig0 )
    fs = methods(f, sig)
    length(fs) == 0 && error("no function '$f' found for signature $sig")
    length(fs) > 1  && error("several functions $f found for signature $sig")  # is that possible ?

    fdef  = fs[1].func.code
    fcode = Base.uncompressed_ast(fdef)

    fargs = fcode.args[1]  # function parameters
    cargs = [ (fargs[i], sig0[i]) for i in 1:length(sig0) ]

    ex  = m.transform(fcode.args[3])
    dex = m.rdiff(ex; cargs...)


    ex = tex ; outsym=nothing; order=1; evalmod=Main; debug=false; allorders=true; params = cargs
    length(params) >= 1 || error("There should be at least one parameter specified, none found")
    
    order <= 1 || 
    length(params) == 1 || error("Only one param allowed for order >= 2")
    
    order <= 1 || 
    isa(params[1][2], Vector) || 
    isa(params[1][2], Real)   || error("Param should be a real or vector for order >= 2")

    paramsym    = Symbol[ e[1] for e in params]
    paramvalues = [ e[2] for e in params]
    parval      = Dict(zip(paramsym, paramvalues))

    g = m.tograph(ex, evalmod)

        tex = quote
            a = zeros(10)
            for i in 1:length(a)
                a[i] += 1
            end
            sum(a)
        end
        g = m.tograph(tex)
        g.seti = m.NSMap([m.getnode(g.seti, outsym)], [ outsym ])    
        g |> m.splitnary! |> m.prune! |> m.simplify!
        m.tocode(g)

        tex = quote
            a = zeros(10)
            for i in 1:length(a)
                c = a[i] + 1
                setindex!(a, c, i)
                z = 3
            end
            sum(a)
        end
        g = m.tograph(tex)

        tex = quote
            a = zeros(10)
            for i in 1:length(a)
                c = a[i] + 1
                setindex!(a, c, i)
                # z = 3
            end
            sum(a)
        end
        g2 = m.tograph(tex)

        g
        g2


        g.seti = m.NSMap([m.getnode(g.seti, outsym)], [ outsym ])    
        g |> m.splitnary! |> m.prune! |> m.simplify!
        m.tocode(g)

        tex = quote
                c = a[i] + 1
                setindex!(a, c, i)
                z = 3
        end
        g = m.tograph(tex, Main, [:a])
        m.syms(g.seti)

        tex = quote  # marche pas
            setindex!(a, c, i)
        end
        g = m.tograph(tex, Main, [:a])
        m.syms(g.seti)

        tex = quote  # ok
            a[i] = c
        end
        g = m.tograph(tex, Main, [:a])
        m.syms(g.seti)

        tex = quote  # ok
            setindex!(a, c, i)
            c = 2
        end
        g = m.tograph(tex, Main, [:a])
        m.syms(g.seti)


    ###################################
    m.hassym(g.seti, outsym) || 
        error("can't find output var $( outsym==nothing ? "" : outsym)")

    # reduce to variable of interest
    g.seti = m.NSMap([m.getnode(g.seti, outsym)], [ outsym ])    

    g |> m.splitnary! |> m.prune! |> m.simplify!
    m.calc!(g, params=parval, emod=evalmod)
    m.tocode(g)


    ov = getnode(g.seti, outsym).val 
    isa(ov, Real) || error("output var should be a Real, $(typeof(ov)) found")

    voi = Any[ outsym ]

    if order == 1
        dg = reversegraph(g, getnode(g.seti, outsym), paramsym)
        append!(g.nodes, dg.nodes)

        for p in paramsym
            nn = getnode(dg.seti, dprefix(p))  # find the exit node of deriv of p
            ns = newvar("_dv")
            g.seti[nn] = ns
            push!(voi, ns)
        end

        g |> splitnary! |> prune! |> simplify!


############ loops in func, v0.3 ###########################################

