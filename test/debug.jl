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

    mes = repr(mex)
    println(mes)

    match(r"^\s*(?:\#|$)"sx, mes)
    match(r"\#\ .*$"sx, mes)

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

    mes = repr(streamline(mex))
    dump(mex.args[5].args[2].args[1])

    mex2 = streamline(mex)
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

    mex2.args[4]

:(begin 
        a = zeros(2)
        GenSym(0) = colon(1,2)
        _s1 = start(GenSym(0))
        # unless !(done(GenSym(0),_s1)) goto 1
2:         GenSym(1) = next(GenSym(0),_s1)
        i = getfield(GenSym(1),1)
        _s1 = getfield(GenSym(1),2)
        setindex!(a,x,i)
# 3:         unless !(!(done(GenSym(0),_s1))) goto 2
1: 0:         return sum(a)
    end)

    dump( mex2.args[4] )
    dump( :( gotoifnot (done(lst, s1),1 ) ) )

    rexp = quote
        GenSym(regexp"(\d+)\k{g0}") = regexp"(.*)\k{range}"
        regexp"(\d+)\k{iter}" = start(GenSym(regexp"\g{g0}"))
        gotoifnot( !(done(GenSym(regexp"\g{g0}"), regexp"\g{iter}" )) , 1 )
        2: GenSym(regexp"(\d+)\k{g1}") = next(GenSym(regexp"\g{g0}"), regexp"\g{iter}")
        i = getfield(GenSym(1),1)
        regexp"\g{iter}" = getfield(GenSym(regexp"\g{g0}"),2)
        setindex!(a,x,i)
        3: gotoifnot( !(!(done(GenSym(regexp"\g{g0}"), regexp"\g{iter}"))) , 2 )
    end

    e2s(streamline(rexp))

    dump( mex2.args[10] )
    ln = mex2.args[10]
    isa(ln, LabelNode)
    isa(ln, Expr)
    repr(ln)


    dump(:( regexp"(\d+)" ))
   dump(:( 0: ))

    function e2s(ex::Expr)
        ex.head == :macrocall && ex.args[1] == Symbol("@regexp_str") && return(ex.args[2])

        if ex.head == :call && ex.args[1] == :gotoifnot
            es = "↑:gotoifnot"
            ra = 2:length(ex.args)
        else
            es = "↑$(ex.head)"
            ra = 1:length(ex.args)
        end
        for a in ex.args[ra]
            es *= "→" * e2s(a)
        end
        return es * "↓"
    end

    function e2s(thing)
        res = repr(thing)
        # now escape regex special characters
        i = start(res)
        res2 = ""
        while !done(res,i)
            c, j = next(res,i)
            c in ['(', ')', '+', '*', '.', '$', '^', '[', ']', '|'] && res2 *= string('\\')
            res2 *= string(c)
            i = j
        end
        res2
    end
    e2s(:(4+bcd))

    rexp2 = Regex(e2s(streamline(rexp)))
    tr = Regex(".*")
    dump(tr)

    match(rexp2)

    rexp = quote
        regexp"\("
        GenSym(regexp"(\d+)") = regexp"(.*)"
        _s136 = start(GenSym(regexp"\g{0}"))
        gotoifnot( !(done(GenSym(0),_s136)), 1)
    end
    e2s(streamline(rexp))

    e2s(mex2)
    dump(mex2.args)

    ### 
    function loopextract(ex::Vector{Expr})
        for e in ex
            


        return (before, idx, inside, after)
    end    



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

 