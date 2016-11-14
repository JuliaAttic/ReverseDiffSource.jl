#########################################################################
#
#   rdiff differentiation for functions
#
#########################################################################

### main function

"""
Generates the derivative function for a given function

	rdiff( func::Function, init::Tuple; kwargs...)

Arguments:

- func: is a Julia generic function.
- init: is a tuple containing the types for each parameter of ``func``. These types are necessary to pick a the right method of the given function. By default the generated expression will yield the derivative for each variable given unless the variable is listed in the ``ignore`` argument.
- order: (keyword arg, default = 1) is an integer indicating the derivation order (1 for 1st order, etc.). Order 0 is allowed and will produce a function that is a processed version of ``ex`` with some variables names rewritten and possibly some optimizations.
- evalmod: (keyword arg, default=Main) module where the expression is meant to be evaluated. External variables and functions should be evaluable in this module.
- debug: (keyword arg, default=false) if true ``rdiff`` dumps the graph of the generating expression, instead of the expression.
- allorders: (keyword arg, default=true) tells rdiff whether to generate the code for all orders up to ``order`` (true) or only the last order.
- ignore: (keyword arg, default=[]) do not differentiate against the listed variables (identified by their position index), useful if you are not interested in having the derivative of one of several variables in ``init``.

```julia
julia> rosenbrock(x) = (1 - x[1])^2 + 100(x[2] - x[1]^2)^2       # function to be derived
julia> rosen2 = rdiff(rosenbrock, (Vector{Float64},), order=2)   # orders up to 2
```

```julia
julia> df = rdiff((x,y) -> 2x^y, (Float64, Float64), ignore=[2], allorders=false)
# derivation against x only (y omitted), order  1 only
```

"""
function rdiff(f::Function, sig::Tuple; args...)
    fs = methods(f, sig)
    length(fs) == 0 && error("no function '$f' found for signature $sig")
    length(fs) > 1  && error("several functions $f found for signature $sig")  # is that possible ?

	if VERSION >= v"0.6.0-"
                fdef  = fs.ms[1]
                fcode = Base.uncompressed_ast(fdef, fdef.source)
                fcode = fcode.code[2:end]
                fcode = Expr(:block, fcode...)
		fargs = [ Symbol("(_$i)") for i in 2:(length(sig)+1) ]
		cargs = [ (fargs[i], sig[i]) for i in 1:length(sig) ]

	elseif VERSION >= v"0.5.0-"
		fdef  = fs.ms[1]
		fcode = Base.uncompressed_ast(fdef.lambda_template)[2:end]
		fcode = Expr(:block, fcode...)

		fargs = [ Symbol("(_$i)") for i in 2:(length(sig)+1) ]
		cargs = [ (fargs[i], sig[i]) for i in 1:length(sig) ]
	else
		fdef  = fs[1].func.code
		ast   = Base.uncompressed_ast(fdef)
		fcode = ast.args[3]

		fargs = ast.args[1]  # function parameters
		cargs = [ (fargs[i], sig[i]) for i in 1:length(sig) ]
	end

	ex  = transform(fcode) # TODO : add error messages if not parseable
    dex = rdiff(ex; args..., cargs...)

    # Note : new function is created in the same module as original function
    myf = fdef.module.eval( :( $(Expr(:tuple, fargs...)) -> $dex ) )
end


### translation functions to recover a workable expression that can be differentiated

# Simplifies expressions for processing
#  - removes Topnodes and linenumbers,
#  - replaces GenSym() with actual symbol
#  - replaces SSAValue() with actual symbol
function streamline(ex0::Expr)
    ex = copy(ex0)

    ex.head == :call && isdefined(:TopNode) && # julia 0.5 compatibility
			isa(ex.args[1], TopNode) &&
			(ex.args[1] = ex.args[1].name)

    args = Any[]
    for a in ex.args
        isa(a, LineNumberNode) && continue
        isa(a, Expr) && a.head==:line && continue

        ar = if isa(a,Expr)
                streamline(a)
 			 elseif isdefined(:SSAValue) && isa(a, SSAValue)
                Symbol("__ssavalue$(a.id)")
             elseif isdefined(:GenSym) && isa(a, GenSym)
                Symbol("__gensym$(a.id)")
             else
                a
             end
        push!(args, ar)
    end
    Expr(ex.head, args...)
end

# converts expression to searchable strings
function _e2s(ex::Expr, escape=false)
    ex.head == :macrocall && ex.args[1] == Symbol("@rg_str") && return(ex.args[2])

    if ex.head == :call && ex.args[1] == :gotoifnot
        es = "↑gotoifnot"
        ra = 2:length(ex.args)
    else
        es = "↑$(ex.head)"
        ra = 1:length(ex.args)
    end

    for a in ex.args[ra]
        es *= "→" * _e2s(a, escape)
    end
    return es * "↓"
end

function _e2s(thing, escape=false)
    # res = isa(thing, Symbol) ? ":" * string(thing) : repr(thing)
    if isa(thing, Symbol)
        res = ":" * string(thing)
    elseif isdefined(:GlobalRef) && isa(thing, GlobalRef)
        res = _e2s(Expr(:., Symbol(thing.mod), QuoteNode(thing.name)))
	elseif isdefined(:LabelNode) && isa(thing, LabelNode)
        res = "↑" * repr(thing) * "↓"
	elseif isdefined(:GotoNode) && isa(thing, GotoNode)
        res = "↑" * repr(thing) * "↓"
    else
        res = repr(thing)
    end

    escape || return(res)
    # now escape characters that would otherwise have a meaning in regex
    i = start(res)
    res2 = ""
    while !done(res,i)
        c, j = next(res,i)
        c in "()+*.\$^[]|?" && (res2 *= "\\")
        res2 *= string(c)
        i = j
    end
    res2
end

function e2s(ex::Expr, escape=false)
    if ex.head in [:body, :block]
        return mapreduce(e -> _e2s(e, escape), *, "", ex.args)
    else
        return _e2s(ex, escape)
    end
end


# converts searchable strings back to expressions
function _s2e(s::AbstractString, pos=1)
    cap = match( r"↑([^→↓]*)(.*)", s, pos )
    if cap == nothing # skip junk characters (Labelnodes,..) and return
        cap = match( r".*?↑(.*)", s, pos )
        cap == nothing && return nothing, endof(s)+1
        return nothing, cap.offsets[1]
    end

	if (mm = match(r":\(goto (\d+)\)", cap.captures[1])) != nothing # Goto node
		pos = cap.offsets[2]
		return GotoNode(parse(mm.captures[1])), pos

	elseif (mm=match(r":\((\d+): \)", cap.captures[1])) != nothing # Labelnodes
		pos = cap.offsets[2]
		return LabelNode(parse(mm.captures[1])), pos

	else  # probably an expression
	    he  = Symbol(cap.captures[1])
	    ar  = Any[]
	    pos = cap.offsets[2]
	    while s[pos] == '→' && !done(s, pos)
	        cap = match( r"→([^→↓]*)(.*)↓$", s, pos )  # s[pos:end]
	        cap == nothing && error("[s2e] unexpected string (2)")
	        cap1 = cap.captures[1]
	        if cap1[1] == '↑'
	            ex, pos2 = _s2e(s, cap.offsets[1])
	        elseif length(cap1) > 4 && cap1[1:3] == ":(:"    # Quotenodes
	            ex = QuoteNode(Symbol(cap1[4:end-1]))
	            pos2 = cap.offsets[2]
	        elseif cap1[1] == ':'        # symbols
	            ex = Symbol(cap1[2:end])
	            pos2 = cap.offsets[2]
	        else
	            ex = parse(cap1)
	            pos2 = cap.offsets[2]
	        end
	        push!(ar, ex)
	        pos = pos2
	    end

	    c, pos = next(s, pos)
	    return Expr(he, ar...), pos
	end
end

function s2e(s::AbstractString)
    res = Any[]
    pos = 1
    while !done(s, pos)
        ex, pos = _s2e(s, pos)
        ex != nothing && push!(res, ex)
    end
    res
end


if VERSION >= v"0.5.0-"
	# `for` loop search regex string (julia v0.5)
	function formatch(s::AbstractString)
		exreg = quote
			rg"(?<pre>.*?)"
	        rg"(?<g0>:[#_].+?)" = rg"(?<range>.+?)"
	        rg"(?<iter>.+)" = Base.start(rg"\g{g0}")
	        rg"↑:\((?<lab2>\d+): \)↓"
	        gotoifnot( Base.:!(Base.done(rg"\g{g0}", rg"\g{iter}" )) , rg"(?<lab1>\d+)" )
	        rg"(?<g1>.+?)" = Base.next(rg"\g{g0}", rg"\g{iter}")
	        rg"(?<idx>.+?)" = Core.getfield(rg"\g{g1}", 1)
	        rg"\g{iter}"    = Core.getfield(rg"\g{g1}", 2)
	        rg"(?<in>.*)"
	        rg"↑:\(goto \g{lab2}\)↓"
	        rg"↑:\(\g{lab1}: \)↓"
	        rg"(?<post>.*)"
		end
		rexp = Regex(e2s(streamline(exreg), true))

		mm = match(rexp, s)
		if mm != nothing && length(mm.captures) >= 10
			return mm.captures[[1, 3, 8, 9, 10]] # pre, rg, idx, inside, post
		else
			return nothing, nothing, nothing, nothing, nothing
		end
	end
else
	# `for` loop search regex string (julia v0.3.3 + 0.4 latest)
	function formatch(s::AbstractString)
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
		rexp = Regex(e2s(streamline(exreg), true))

		mm = match(rexp, s)
		if mm != nothing && length(mm.captures) >= 11
	        return mm.captures[[1,3,8,9,11]] # pre, rg, idx, inside, post
		else
			return nothing, nothing, nothing, nothing, nothing
		end
	end
end


function _transform(s::AbstractString)
	pre, rg, idx, inside, post = formatch(s)
    if pre != nothing
        exin = _transform(inside)
        ef = Expr(:for, Expr(:(=), Symbol(idx[2:end]), s2e(rg)[1] ), exin)
        return Expr(:block, [ s2e(pre) ; ef ; s2e(post)]...)
    else
        return Expr(:block, s2e(s)...)
    end
end

function transform(ex::Expr)
    s = e2s(streamline(ex))
    tex = _transform(s)

    # remove return statement at the end
    rex = tex.args[end]
    if rex.head == :return
        tex.args[end] = rex.args[1]
    else
        error("[transform] no return statement found at the end")
    end

    tex
end
