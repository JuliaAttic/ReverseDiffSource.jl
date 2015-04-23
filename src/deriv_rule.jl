#########################################################################
#
#   Derivation rule and type equivalence declaration functions / macros
#
#########################################################################

drules = Dict() # holds the derivation expressions
trules = Dict() # holds the type equivalence expressions

#### function derivation rules declaration functions/macros

# macro version
macro deriv_rule(func::Expr, dv::Symbol, diff)
    # func = :(  float(x, y::Real, z::Array{String}, t::Union(Float64,Int)))
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

# function deriv_rule{T<:Type}(func::Union(Function, Type), args::Vector{Tuple{Symbol, T}}, dv::Symbol, diff::Union(Expr, Symbol, Real))
function deriv_rule(func::Union(Function, Type),
    args::Vector, dv::Symbol, diff::Union(Expr, Symbol, Real))

    emod = current_module()

    sig = VERSION < v"0.4.0-dev+4319" ?
            tuple( Type[ e[2] for e in args ]... ) :
            Tuple{ Type[ e[2] for e in args ]... }

    ss  = Symbol[ e[1] for e in args ]

    index = findfirst(dv .== ss)
    (index == 0) && error("[deriv_rule] cannot find $dv in function arguments")

    haskey(drules, (func, index)) || (drules[(func, index)] = Dict())

    g = tograph(diff, emod)  # make the graph
    push!(ss, :ds)

    drules[(func, index)][sig] = (g, ss) 
    nothing
end


#### Type tuple matching  (naive multiple dispatch)

# function tmatch(sig, keys)
#     if VERSION < v"0.4.0-dev+4319"
#         keys2 = filter(k -> length(k) == length(sig), keys)
#         tcp(a,b) = a <: b
#         sort!(keys2, lt=tcp)
#         for k in keys2
#             all( t -> t[1] <: t[2], zip(sig, k)) && return k
#         end
#         return nothing
#     else
#         keys2 = filter(k -> length(k.parameters) == length(sig.parameters), keys)
#     end
# end

function tmatch(sig, keys)
    if VERSION < v"0.4.0-dev+4319"
        keys2 = filter(k -> length(k) == length(sig), keys)
    else
        keys2 = filter(k -> length(k.parameters) == length(sig.parameters), keys)
    end
    sort!(keys2, lt=issubtype)
    idx = findfirst(s -> sig <: s, keys2)
    return idx==0 ? nothing : keys2[idx]
end
