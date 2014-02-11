#########################################################################
#
#   Derivation rule declaration
#
#########################################################################

rdict = Dict()


# this is really crappy, should use the function as key, not a symbol
# that is namespace dependant
function dfuncname(nam::Union(Expr, Symbol), ind::Int)
    if isa(nam, Symbol)
        return symbol("d_$(nam)_$(ind)")
    elseif nam.head == :. 
        st = "$nam"[3:end-1]
        return symbol("d_$(st)_$(ind)")
    else
        error("[dfuncname] cannot parse expr $nam")
    end
end

function deriv_rule(func::Expr, dv::Symbol, diff::Union(Expr, Symbol, Real))
    argsn = map(e-> isa(e, Symbol) ? e : e.args[1], func.args[2:end])
    push!(argsn, :ds)  # add special symbol ds

    index = find(dv .== argsn)[1] # TODO : add error message if not found

    g, vd, exitnode = tograph( diff )
    vmap = Union(ExNode, Nothing)[]
    for v in argsn
        pos = find( n -> (n.nodetype==:external) & (n.name == v) , g.nodes)
        push!(vmap, length(pos) == 0 ? nothing : g.nodes[pos[1]])
    end

    rn = gensym("rule")
    rdict[rn] = (g, vmap, exitnode)

    # diff function name
    fn = dfuncname(func.args[1], index)

    # create function returning applicable rule # for this signature
    eval( :( $(Expr(:call, fn, func.args[2:end]...)) = $(Expr(:quote, rn)) ) )
end

# macro version
macro deriv_rule(func::Expr, dv::Symbol, diff)
    deriv_rule(func, dv, diff)
end

#####  composite type - vector equivalence declaration  ######

tdict = Dict()

type_decl(typ::DataType, n::Real) = tdict[typ] = n

# macro version
macro type_decl(typ::Union(Symbol, Expr), n::Int)
    type_decl(eval(typ), n)
end



####################  rules  ######################################

# addition
@deriv_rule +(x::Real         , y::Real )            x     ds
@deriv_rule +(x::Real         , y::AbstractArray)    x     sum(ds)
@deriv_rule +(x::AbstractArray, y       )            x     ds
@deriv_rule +(x::Real         , y::Real )            y     ds
@deriv_rule +(x::AbstractArray, y::Real )            y     sum(ds)
@deriv_rule +(x               , y::AbstractArray)    y     ds

# unary substraction
@deriv_rule -(x::Real )                              x     -ds
@deriv_rule -(x::AbstractArray)                      x     -ds

# binary substraction
@deriv_rule -(x::Real         , y::Real )            x     ds
@deriv_rule -(x::Real         , y::AbstractArray)    x     sum(ds)
@deriv_rule -(x::AbstractArray, y       )            x     ds
@deriv_rule -(x::Real         , y::Real )            y     -ds
@deriv_rule -(x::AbstractArray, y::Real )            y     -sum(ds)
@deriv_rule -(x               , y::AbstractArray)    y     -ds

# sum()
@deriv_rule sum(x::Real )                            x     ds
@deriv_rule sum(x::AbstractArray)                    x     ones(size(x)).*ds

# dot()
@deriv_rule dot(x::AbstractArray, y::AbstractArray)  x     y.*ds
@deriv_rule dot(x::AbstractArray, y::AbstractArray)  y     x.*ds

# log() and exp()
@deriv_rule log(x::Real )                            x     ds / x
@deriv_rule log(x::AbstractArray)                    x     ds ./ x

@deriv_rule exp(x::Real )                            x     exp(x) * ds 
@deriv_rule exp(x::AbstractArray)                    x     exp(x) .* ds

# sin() and cos()
@deriv_rule sin(x::Real )                            x     cos(x) * ds
@deriv_rule sin(x::AbstractArray)                    x     cos(x) .* ds

@deriv_rule cos(x::Real )                            x     -sin(x) * ds
@deriv_rule cos(x::AbstractArray)                    x     -sin(x) .* ds

# abs, max(), min()
@deriv_rule abs(x::Real )                            x     sign(x) * ds
@deriv_rule abs(x::AbstractArray)                    x     sign(x) .* ds

@deriv_rule max(x::Real         , y::Real )          x     (x > y) * ds
@deriv_rule max(x::Real         , y::AbstractArray)  x     sum((x .> y) .* ds)
@deriv_rule max(x::AbstractArray, y::Real )          x     (x .> y) .* ds
@deriv_rule max(x::AbstractArray, y::AbstractArray)  x     (x .> y) .* ds

@deriv_rule max(x::Real         , y::Real )          y     (x < y) * ds
@deriv_rule max(x::Real         , y::AbstractArray)  y     (x .< y) .* ds
@deriv_rule max(x::AbstractArray, y::Real )          y     sum((x .< y) .* ds)
@deriv_rule max(x::AbstractArray, y::AbstractArray)  y     (x .< y) .* ds

@deriv_rule min(x::Real         , y::Real )          x     (x < y) * ds
@deriv_rule min(x::Real         , y::AbstractArray)  x     sum((x .< y) .* ds)
@deriv_rule min(x::AbstractArray, y::Real )          x     (x .< y) .* ds
@deriv_rule min(x::AbstractArray, y::AbstractArray)  x     (x .< y) .* ds

@deriv_rule min(x::Real         , y::Real )          y     (x > y) * ds
@deriv_rule min(x::Real         , y::AbstractArray)  y     (x .> y) .* ds
@deriv_rule min(x::AbstractArray, y::Real )          y     sum((x .> y) .* ds)
@deriv_rule min(x::AbstractArray, y::AbstractArray)  y     (x .> y) .* ds

# multiplication
@deriv_rule *(x::Real         , y::Real )            x     y * ds
@deriv_rule *(x::Real         , y::AbstractArray)    x     sum(y .* ds)
@deriv_rule *(x::AbstractArray, y::Real )            x     y .* ds
# @deriv_rule *(x::AbstractArray, y::Vector)          x     gemm!('N', 'T', 1., ds, reshape(y, length(y), 1), 1., dx)  # reshape needed 
@deriv_rule *(x::AbstractArray, y::AbstractArray)    x     ds * y'
 
@deriv_rule *(x::Real         , y::Real )            y     x * ds
@deriv_rule *(x::Real         , y::AbstractArray)    y     x .* ds
@deriv_rule *(x::AbstractArray, y::Real )            y     sum(x .* ds)
# @deriv_rule *(x::AbstractArray, y::Vector)          y     gemm!('T', 'N', 1., x, reshape(ds, length(ds), 1), 1., dy)
@deriv_rule *(x::AbstractArray, y::AbstractArray)    y     x' * ds

# dot multiplication
@deriv_rule .*(x::Real         , y::Real )           x     y .* ds
@deriv_rule .*(x::Real         , y::AbstractArray)   x     sum(y .* ds)
@deriv_rule .*(x::AbstractArray, y::Real )           x     y .* ds
@deriv_rule .*(x::AbstractArray, y::AbstractArray)   x     y .* ds

@deriv_rule .*(x::Real         , y::Real )           y     x * ds
@deriv_rule .*(x::Real         , y::AbstractArray)   y     x .* ds
@deriv_rule .*(x::AbstractArray, y::Real )           y     sum(x .* ds)
@deriv_rule .*(x::AbstractArray, y::AbstractArray)   y     x .* ds

# power  (both args reals)
@deriv_rule ^(x::Real, y::Real)                      x     y * x ^ (y-1) * ds
@deriv_rule ^(x::Real, y::Real)                      y     log(x) * x ^ y * ds

# dot power
@deriv_rule .^(x::Real         , y::Real )           x     y * x ^ (y-1) * ds
@deriv_rule .^(x::Real         , y::AbstractArray)   x     sum(y .* x .^ (y-1) .* ds)
@deriv_rule .^(x::AbstractArray, y::Real )           x     y * x .^ (y-1) .* ds
@deriv_rule .^(x::AbstractArray, y::AbstractArray)   x     y .* x .^ (y-1) .* ds

@deriv_rule .^(x::Real         , y::Real )           y     log(x) * x ^ y * ds
@deriv_rule .^(x::AbstractArray, y::Real )           y     sum( log(x) .* x .^ y .* ds)
@deriv_rule .^(x::Real         , y::AbstractArray)   y     log(x) .* x .^ y .* ds
@deriv_rule .^(x::AbstractArray, y::AbstractArray)   y     log(x) .* x .^ y .* ds

# division
@deriv_rule /(x::Real          , y::Real )           x     ds / y
@deriv_rule /(x::Real          , y::AbstractArray)   x     sum(ds ./ y)
@deriv_rule /(x::AbstractArray , y::Real )           x     ds ./ y

@deriv_rule /(x::Real          , y::Real )           y     -x * ds / (y * y)
@deriv_rule /(x::Real          , y::AbstractArray)   y     -x * ds ./ (y .* y) 
@deriv_rule /(x::AbstractArray , y::Real )           y     sum(-x .* ds) / (y * y)

# dot division
@deriv_rule ./(x::Real         , y::Real )           x     ds / y
@deriv_rule ./(x::Real         , y::AbstractArray)   x     sum(ds ./ y)
@deriv_rule ./(x::AbstractArray, y::Real )           x     ds ./ y
@deriv_rule ./(x::AbstractArray, y::AbstractArray)   x     ds ./ y

@deriv_rule ./(x::Real         , y::Real )           y     -x * ds / (y * y)
@deriv_rule ./(x::Real         , y::AbstractArray)   y     -x * ds ./ (y .* y)
@deriv_rule ./(x::AbstractArray, y::Real )           y     -sum(x .* ds) / (y * y)
@deriv_rule ./(x::AbstractArray, y::AbstractArray)   y     -x .* ds ./ (y .* y)

# transpose
@deriv_rule transpose(x::Real )                      x     ds
@deriv_rule transpose(x::AbstractArray)              x     transpose(ds)

