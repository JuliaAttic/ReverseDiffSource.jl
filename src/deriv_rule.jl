#########################################################################
#
#   Derivation rule and type equivalence declaration functions / macros
#
#########################################################################

drules = Dict() # holds the derivation expressions

#### function derivation rules declaration functions/macros

# macro version
macro deriv_rule(func::Expr, dv::Symbol, diff)
    # func = :(  colon(x,y) ) ; dv = :x ; diff = 0.
    emod = current_module()

    ss = Symbol[]
    ts = Type[]
    for e in func.args[2:end]
        if isa(e, Symbol)
            push!(ss, e)
            push!(ts, Any)

        elseif isa(e, Expr) && e.head== :(::)
            push!(ss, e.args[1])
            push!(ts, emod.eval(e.args[2]))

        else
            error("[deriv_rule] cannot parse $e")
        end
    end

    deriv_rule(emod.eval(func.args[1]), collect(zip(ss, ts)), dv, diff)
end

function deriv_rule(func::Union{Function, Type},
    args::Vector, dv::Symbol, diff::Union{Expr, Symbol, Real})
    emod = current_module()

    sig = Tuple{ Type[ e[2] for e in args ]... }

    ss  = Symbol[ e[1] for e in args ]

    index = findfirst(dv .== ss)
    (index == 0) && error("[deriv_rule] cannot find $dv in function arguments")

    # non generic functions are matched by their name
    fidx = isa(func, Function) && isbuiltin(func) ?
                  builtin_name(func) : func

    haskey(drules, (fidx, index)) || (drules[(fidx, index)] = Dict())

    g = tograph(diff, emod)  # make the graph
    push!(ss, :ds)

    drules[(fidx, index)][sig] = (g, ss)
    nothing
end

isbuiltin(f) = VERSION >= v"0.5.0-" ? isa(f, Core.Builtin) : isa(f.env, Symbol)
builtin_name(f) = VERSION >= v"0.5.0-" ? Base.function_name(f) : f.env

function hasrule(f, pos)
    haskey(drules, (f, pos)) && return true
    # non generic functions are matched by their name
    if isa(f, Function) && isbuiltin(f)
        fname = builtin_name(f)
        return haskey(drules, (fname, pos))
    end
    false
end

function getrule(f, pos)
    if isa(f, Function) && isbuiltin(f) # non generic functions are matched by their name
        fname = builtin_name(f)
        haskey(drules, (fname, pos)) && return drules[(fname, pos)]
    else
        haskey(drules, (f, pos)) && return drules[(f, pos)]
    end
    error("no derivation rule for $(f) at arg #$(pos)")
end

#### Type tuple matching  (naive multiple dispatch)
function tmatch(sig, keys)
    keys2 = filter(k -> length(k.parameters) == length(sig.parameters), keys)
    sort!(keys2, lt=issubtype)
    idx = findfirst(s -> sig <: s, keys2)
    return idx==0 ? nothing : keys2[idx]
end
