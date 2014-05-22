#########################################################################
#
#   Derivation rule declaration
#
#########################################################################

rdict = Dict()

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
    #### list variable symbols and annotate type names with "Main." 
    argsn = Symbol[]
    sig = {}
    for e in func.args[2:end]
        if isa(e, Symbol)
            push!(argsn, e)
            push!(sig, e)

        elseif isa(e, Expr) && e.head== :(::)  # FIXME : will fail for complex definitions
            push!(argsn, e.args[1])
            e2 = e.args[2]
            if isa(e2, Symbol) || (isa(e2, Expr) && e2.head == :.)
                ne = Expr(:., :Main, Expr(:quote, e2))
            elseif isa(e2, Expr) && e2.head == :curly
                ne = Expr(:curly, [ Expr(:., :Main, Expr(:quote, ei)) for ei in e2.args]...)
            elseif isa(e2, Expr) && e2.head == :call && e2.args[1] == :Union
                ne = Expr(:call, :Union, [ Expr(:., :Main, Expr(:quote, ei)) for ei in e2.args[2:end]]...)
            
            else
                error("[deriv_rule] cannot parse $e")
            end

            push!(sig, Expr(:(::), e.args[1], ne))

        else
            error("[deriv_rule] cannot parse $e")
        end
    end
    # argsn = map(e-> isa(e, Symbol) ? e : e.args[1], func.args[2:end])
    push!(argsn, :ds)  # add special symbol ds

    index = find(dv .== argsn)[1] # TODO : add error message if not found

    #### make the graph
    g = tograph( diff )

    #### store graph, build proxy function
    rn = gensym("rule")
    rdict[rn] = (g, argsn, g.set_inodes.vk[nothing])

    # diff function name
    fn = dfuncname(func.args[1], index)

    # create function returning applicable rule # for this signature
    eval( :( $(Expr(:call, fn, sig...)) = $(Expr(:quote, rn)) ) )
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
    type_decl(eval(Main, typ), n)
end

macro typeequiv(typ::Union(Symbol, Expr), n::Int)
    ie = n==1 ? 0. : Expr(:vcat, zeros(n)...)
    deriv_rule(:( equivnode(x::$(typ))) , :x, ie)
end


####################  rules  ######################################

# @type_decl UnitRange 2    # to allow parsing of things like a[1:5], for i in 2:7
# @type_decl UnitRange 2    # to allow parsing of things like a[1:5], for i in 2:7

@typeequiv    Real     1    # derivatives of scalars are scalars

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

