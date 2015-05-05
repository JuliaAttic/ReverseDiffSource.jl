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

##############   loops in functions  #################################

    function tt(x)
        a = zeros(2)
        for i in 1:2
            a[i] = x
        end
        sum(a)
    end

    # (f::Function, sig0::Tuple; order::Int=1, evalmod=Main, debug=false, allorders=true)
    f=tt;sig0=(1.,);order=1;evalmod=Main;debug=false;allorders=true

    sig = map( typeof, sig0 )
    fs = methods(f, sig)
    length(fs) == 0 && error("no function '$f' found for signature $sig")
    length(fs) > 1  && error("several functions $f found for signature $sig")  # is that possible ?

    fdef  = fs[1].func.code
    fcode = Base.uncompressed_ast(fdef)
    fargs = fcode.args[1]  # function parameters
    mex = fcode.args[3]

    function streamline(ex::Expr)
        ex.head == :call && isa(ex.args[1], TopNode) && (ex.args[1] = ex.args[1].name)
        args = Any[]
        for a in ex.args
            isa(a, LineNumberNode) && continue
            isa(a, Expr) && a.head==:line && continue

            push!(args, isa(a,Expr) ? streamline(a) : a )
        end
        Expr(ex.head, args...)   
    end

    mex2 = streamline(mex)
    mex2.head = :block
    :(begin 
        a = zeros(2)
        GenSym(0) = colon(1,2)
        #s136 = start(GenSym(0))
        unless !(done(GenSym(0),#s136)) goto 1
        2: 
        GenSym(1) = next(GenSym(0),#s136)
        i = getfield(GenSym(1),1)
        #s136 = getfield(GenSym(1),2)
        setindex!(a,x,i)
        3: 
        unless !(!(done(GenSym(0),#s136))) goto 2
        1: 
        0: 
        return sum(a)
    end)

    mex2.args[2]
    dump( mex2.args[2] )
    typeof( mex2.args[2].args[1] )
    fieldnames( mex2.args[2].args[1] )
    typeof( mex2.args[2].args[1].id )
    dump(mex2.args[end])

    function e2s(ex::Expr, escape=false)
        ex.head == :macrocall && ex.args[1] == Symbol("@regexp_str") && return(ex.args[2])

        if ex.head == :call && ex.args[1] == :gotoifnot
            es = "↑gotoifnot"
            ra = 2:length(ex.args)
        else
            es = "↑$(ex.head)"
            ra = 1:length(ex.args)
        end

        for a in ex.args[ra]
            es *= "→" * e2s(a, escape)
        end
        return es * "↓"
    end

    function e2s(thing, escape=false)
        res = repr(thing)
        escape || return(res)
        # now escape characters that would otherwise have a meaning in regex
        i = start(res)
        res2 = ""
        while !done(res,i)
            c, j = next(res,i)
            c in "()+*.\$^[]|" && (res2 *= "\\")
            res2 *= string(c)
            i = j
        end
        res2
    end
    e2s(:(4+bcd))


    rexp = quote
        regexp"(?<pre>.*)"
        regexp"(?<g0>GenSym\(\d+\))" = regexp"(?<range>.+)"
        regexp"(?<iter>.+)" = start(regexp"\g{g0}")
        gotoifnot( !(done(regexp"\g{g0}", regexp"\g{iter}" )) , regexp"(?<lab1>\d+)" )
        regexp":\((?<lab2>\d+): \)"
        regexp"(?<g1>GenSym\(\d+\))" = next(regexp"\g{g0}", regexp"\g{iter}")
        regexp"(?<idx>.+)" = getfield(regexp"\g{g1}", 1)
        regexp"\g{iter}"   = getfield(regexp"\g{g1}", 2)
        regexp"(?<in>.*)"
        regexp":\((?<lab3>\d+): \)"
        gotoifnot( !(!(done(regexp"\g{g0}", regexp"\g{iter}"))) , regexp"\g{lab2}" )
        regexp":\(\g{lab1}: \)"
        regexp"(?<post>.*)"
    end
    r1 = Regex(e2s(streamline(rexp), true))

    r1
    e2s(mex2)
    mm = match(r1, e2s(mex2))
    pre, rg, idx, inside, post = mm.captures[[1,3,8,9,11]]


        for c in s
         println(c)
       end
       iter = start(s)
       next(iter, true)

    function _s2e(s::AbstractString, pos=1) # s = pre ; pos = 1
        cap = match( r"↑([^→↓]*)(.*)↓$", s, pos )
        cap == nothing && error("[s2e] unexpected string (1)")

        he  = symbol(cap.captures[1])
        ar  = Any[]
        pos = cap.offsets[2]
        while s[pos] == '→' && !done(s, pos)
            cap = match( r"→([^→↓]*)(.*)↓$", s, pos )  # s[pos:end]
            cap == nothing && error("[s2e] unexpected string (2)")
            if cap.captures[1][1] == '↑'
                ex, pos2 = _s2e(s, cap.offsets[1])
            elseif cap.captures[1][1] == ':'
                ex = symbol(cap.captures[1][2:end])
                pos2 = cap.offsets[2]
            else
                ex = parse(cap.captures[1])
                pos2 = cap.offsets[2]
            end
            push!(ar, ex)
            pos = pos2
        end

        c, pos = next(s, pos)
        return Expr(he, ar...), pos
    end
    _s2e(pre,1)

    function s2e(s::AbstractString) # s = pre
        res = Expr[]
        pos = 1
        while !done(s, pos)
            ex, pos = _s2e(s, pos)
            push!(res, ex)
        end
        res
    end
    s2e(pre)
    s2e(post)

pre, rg, idx, inside, post

    dump( :(for i in 1:2 ; end))

    fex = copy( s2e(pre) )
    ef = Expr(:for, Expr(:(=), symbol(idx[2:end]), s2e(rg)[1] ), Expr(:block, s2e(inside)...))

    Expr(:block, [ s2e(pre) ; ef ; s2e(post)]...)


    mm2 = match(rexp, inside)
    mm2 = match(rexp, "↑block→" * inside)

    rexp
    inside

    r2 = r"↑block→(?<pre>.*?)→↑=→(?<g0>:__gensym\d+)→(?<range>.+)↓→↑=→(?<iter>.+)→↑call→:start→\g{g0}↓↓→↑gotoifnot→↑call→:!→↑call→:done→\g{g0}→\g{iter}↓↓→(?<lab1>\d+)↓→:\((?<lab2>\d+): \)→↑=→(?<g1>:__gensym\d+)→↑call→:next→\g{g0}→\g{iter}↓↓→↑=→(?<idx>.+)→↑call→:getfield→\g{g1}→1↓↓→↑=→\g{iter}→↑call→:getfield→\g{g1}→2↓↓→(?<in>.*)→:\((?<lab3>\d+): \)→↑gotoifnot→↑call→:!→↑call→:!→↑call→:done→\g{g0}→\g{iter}↓↓↓→\g{lab2}↓→:\(\g{lab1}: \)→(?<post>.*)↓"
    r2 = r"↑block→(?<pre>.*?)→↑=→(?<g0>:__gensym\d+)→(?<range>.+)↓→↑=→(?<iter>.+)→↑call→:start→\g{g0}↓↓→↑"
    mm2 = match(r2, inside)


    mm2.captures[[1,3,8,9,11]]

"↑block→(?<pre>.*?)→↑=→(?<g0>:__gensym\d+)→(?<range>.+)     ↓→↑=→(?<iter>.+)     →↑call→:start→\g{g0}    ↓↓→↑gotoifnot→↑call→:!→↑call→:done→\g{g0}    →\g{iter}        ↓↓→(?<lab1>\d+)↓→:\((?<lab2>\d+): \)→↑=→(?<g1>:__gensym\d+)→↑call→:next→\g{g0}    →\g{iter}        ↓↓→↑=→(?<idx>.+)→↑call→:getfield→\g{g1}    →1↓↓→↑=→\g{iter}        →↑call→:getfield→\g{g1}    →2↓↓→(?<in>.*)                                                                                                                                                                                      →:\((?<lab3>\d+): \)→↑gotoifnot→↑call→:!→↑call→:!→↑call→:done→\g{g0}    →\g{iter}        ↓↓↓→\g{lab2}↓→:\(\g{lab1}: \)→(?<post>.*)↓"
"↑=→:d→3↓→↑=→                :__gensym2   →↑call→:colon→1→2↓↓→↑=→symbol(\"#s13\")→↑call→:start→:__gensym2↓↓→↑gotoifnot→↑call→:!→↑call→:done→:__gensym2→symbol(\"#s13\")↓↓→5           ↓→:(6: )             →↑=→:__gensym3         →↑call→:next→:__gensym2→symbol(\"#s13\")↓↓→↑=→:j        →↑call→:getfield→:__gensym3→1↓↓→↑=→symbol(\"#s13\")→↑call→:getfield→:__gensym3→2↓↓→↑call→:setindex!→:a→:x→:i↓→↑=→:__gensym4→↑call→:+→↑call→:getindex→:a→:i↓→↑call→:getindex→:b→↑call→:+→:i→1↓↓↓↓→↑call→:setindex!→:a→:__gensym4→:i↓→↑=→:d→↑call→:*→:d→↑call→:sin→↑call→:*→2→:i↓↓↓↓→:(7: )             →↑gotoifnot→↑call→:!→↑call→:!→↑call→:done→:__gensym2→symbol(\"#s13\")↓↓↓→6       ↓→:(5: )         →:(4: )→↑=→:c→↑call→:+→:c→:d↓↓"



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

    mes2 = "$(rc.captures[1]) ; 
                for $(rc.captures[8]) in $(rc.captures[3]);
                $(rc.captures[9]);
            end ;
            $(rc.captures[10]) "


    println("""
            $(rc.captures[1])
            for $(rc.captures[8]) in $(rc.captures[3])
                $(rc.captures[9])
            end
            $(rc.captures[10]) """)

#######################################################


    type Abcd
        val
    end
    tf(x) = (x.val=0. ; x)

    tf(Abcd(3.))
    a = Abcd(3.)
    tf(a)
    a
    
tf(4+3im)

