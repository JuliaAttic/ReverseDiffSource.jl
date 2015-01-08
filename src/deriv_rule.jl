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

function deriv_rule{T<:Type}(func::Union(Function, Type), args::Vector{(Symbol, T)}, dv::Symbol, diff::Union(Expr, Symbol, Real))
    sig = tuple( Type[ e[2] for e in args ]... )
    ss  = Symbol[ e[1] for e in args ]

    index = findfirst(dv .== ss)
    (index == 0) && error("[deriv_rule] cannot find $dv in function arguments")

    haskey(drules, (func, index)) || (drules[(func, index)] = Dict())

    g = tograph(diff, current_module())  # make the graph
    push!(ss, :ds)

    drules[(func, index)][sig] = (g, ss) 
end


#### composite type - vector equivalence declaration function ######
# TODO : implement type hierarchy (only leaf types work)

macro typeequiv(typ::Union(Symbol, Expr), n::Int)
    emod = current_module()
    typeequiv( emod.eval(typ), n)
end

function typeequiv(typ::DataType, n::Int)
    g = tograph( n==1 ? 0. : Expr(:vcat, zeros(n)...) )
    trules[typ] = ( g, Symbol[] )
end 

#### Type tuple matching  (naive multiple dispatch)

function tmatch(sig, keys)
    keys2 = filter(k -> length(k) == length(sig), keys)
    tcp(a,b) = a <: b
    sort!(keys2, lt=tcp)
    for k in keys2
        all( t -> t[1] <: t[2], zip(sig, k)) && return k
    end
    return nothing
end

