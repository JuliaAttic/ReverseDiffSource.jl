####################  derivation rule declaration  ##############

rdict = Dict()

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
    fn = symbol("d_$(func.args[1])_x$index")

    # create function returning applicable rule # for this signature
    eval( :( $(Expr(:call, fn, func.args[2:end]...)) = $(Expr(:quote, rn)) ) )
end


# macro version
macro deriv_rule(func::Expr, dv::Symbol, diff)
    deriv_rule(func, dv, diff)
end

####################  composite type conversion rules declaration  ##############

tdict = Dict()

type_decl(typ::DataType, n::Int) = tdict[typ] = n

# macro version
macro type_decl(typ::DataType, n::Int)
    deriv_rule(typ, n)
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
@deriv_rule sum(x::AbstractArray)                    x     ones(length(x)).*ds

# dot()
@deriv_rule dot(x::AbstractArray, y::AbstractArray)  x     y.*ds
@deriv_rule dot(x::AbstractArray, y::AbstractArray)  y     x.*ds

# log() and exp()
@deriv_rule log(x::Real )            x     ds / x
@deriv_rule log(x::AbstractArray)    x     ds ./ x

@deriv_rule exp(x::Real )            x     exp(x) * ds 
@deriv_rule exp(x::AbstractArray)    x     exp(x) .* ds

# sin() and cos()
@deriv_rule sin(x::Real )            x     cos(x) * ds
@deriv_rule sin(x::AbstractArray)    x     cos(x) * ds

@deriv_rule cos(x::Real )            x     -sin(x) * ds
@deriv_rule cos(x::AbstractArray)    x     -sin(x) * ds

# abs, max(), min()
@deriv_rule abs(x::Real )            x     sign(x) * ds
@deriv_rule abs(x::AbstractArray)    x     sign(x) * ds

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
@deriv_rule transpose(x::Real )                      x   ds
@deriv_rule transpose(x::AbstractArray)              x   transpose(ds)



# # addition
# @deriv_rule +(x::Real         , y::Real )            x     ds
# @deriv_rule +(x::Real         , y::AbstractArray)    x     sum(ds)
# @deriv_rule +(x::AbstractArray, y       )            x     ds
# @deriv_rule +(x::Real         , y::Real )            y     ds
# @deriv_rule +(x::AbstractArray, y::Real )            y     sum(ds)
# @deriv_rule +(x               , y::AbstractArray)    y     ds

# # dot multiplication
# @deriv_rule .*(x::Real         , y::Real )           x     y*ds
# @deriv_rule .*(x::Real         , y::AbstractArray)   x     sum(ds)
# @deriv_rule .*(x::AbstractArray, y::Real )           x     y*ones(size(x))
# @deriv_rule .*(x::AbstractArray, y::AbstractArray)   x     y

# @deriv_rule .*(x::Real         , y::Real )           y     x
# @deriv_rule .*(x::Real         , y::AbstractArray)   y     x*ones(size(y))
# @deriv_rule .*(x::AbstractArray, y::Real )           y     length(x)
# @deriv_rule .*(x::AbstractArray, y::AbstractArray)   y     x

# # unary substraction
# @deriv_rule -(x::Real )              x     -1.0
# @deriv_rule -(x::AbstractArray)      x     -ones(size(x))

# # binary substraction
# @deriv_rule -(x::Real         , y::Real )            x     1.
# @deriv_rule -(x::Real         , y::AbstractArray)    x     length(y)
# @deriv_rule -(x::AbstractArray, y       )            x     ones(size(x))
# @deriv_rule -(x::Real         , y::Real )            y     -1.
# @deriv_rule -(x::AbstractArray, y::Real )            y     -length(x)
# @deriv_rule -(x               , y::AbstractArray)    y     -ones(size(y))

# # sum()
# @deriv_rule sum(x::Real )           x     1.
# @deriv_rule sum(x::AbstractArray)   x     ones(size(x))

# # dot()
# @deriv_rule dot(x::AbstractArray, y::AbstractArray)    x     y
# @deriv_rule dot(x::AbstractArray, y::AbstractArray)    y     x

# # log() and exp()
# @deriv_rule log(x::Real )            x     1. / x
# @deriv_rule log(x::AbstractArray)    x     1. ./ x

# @deriv_rule exp(x::Real )            x     exp(x)
# @deriv_rule exp(x::AbstractArray)    x     exp(x)

# # sin() and cos()
# @deriv_rule sin(x::Real )            x     cos(x)
# @deriv_rule sin(x::AbstractArray)    x     cos(x)

# @deriv_rule cos(x::Real )            x     -sin(x)
# @deriv_rule cos(x::AbstractArray)    x     -sin(x)

# # abs, max(), min()
# @deriv_rule abs(x::Real )            x     sign(x)
# @deriv_rule abs(x::AbstractArray)    x     sign(x)

# # @deriv_rule max(x::Real         , y::Real )          x     x > y
# # @deriv_rule max(x::Real         , y::AbstractArray)  x     sum(x > y)for i in 1:length(ds) ; dx += (x > y[i]) * ds[i] ; end
# # @deriv_rule max(x::AbstractArray, y::Real )          x     for i in 1:length(ds) ; dx[i] += (x[i] > y) * ds[i] ; end
# # @deriv_rule max(x::AbstractArray, y::AbstractArray)  x     for i in 1:length(ds) ; dx[i] += (x[i] > y[i]) * ds[i] ; end
# # @deriv_rule max(x::Real         , y::Real )          y     (x < y)
# # @deriv_rule max(x::Real         , y::AbstractArray)  y     for i in 1:length(ds) ; dy[i] += (x < y[i]) * ds[i] ; end
# # @deriv_rule max(x::AbstractArray, y::Real )          y     for i in 1:length(ds) ; dy += (x[i] < y) * ds[i] ; end
# # @deriv_rule max(x::AbstractArray, y::AbstractArray)  y     for i in 1:length(ds) ; dy[i] += (x[i] < y[i]) * ds[i] ; end

# # @deriv_rule min(x::Real         , y::Real )          x     (x < y)
# # @deriv_rule min(x::Real         , y::AbstractArray)  x     for i in 1:length(ds) ; dx += (x < y[i]) * ds[i] ; end
# # @deriv_rule min(x::AbstractArray, y::Real )          x     for i in 1:length(ds) ; dx[i] += (x[i] < y) * ds[i] ; end
# # @deriv_rule min(x::AbstractArray, y::AbstractArray)  x     for i in 1:length(ds) ; dx[i] += (x[i] < y[i]) * ds[i] ; end
# # @deriv_rule min(x::Real         , y::Real )          y     (x > y)
# # @deriv_rule min(x::Real         , y::AbstractArray)  y     for i in 1:length(ds) ; dy[i] += (x > y[i]) * ds[i] ; end
# # @deriv_rule min(x::AbstractArray, y::Real )          y     for i in 1:length(ds) ; dy += (x[i] > y) * ds[i] ; end
# # @deriv_rule min(x::AbstractArray, y::AbstractArray)  y     for i in 1:length(ds) ; dy[i] += (x[i] > y[i]) * ds[i] ; end

# # # multiplication
# @deriv_rule *(x::Real         , y::Real )           x     y
# # @deriv_rule *(x::Real         , y::AbstractArray)   x     for i in 1:length(ds) ; dx += y[i] * ds[i] ; end
# # @deriv_rule *(x::AbstractArray, y::Real )           x     for i in 1:length(ds) ; dx[i] += y * ds[i] ; end
# # @deriv_rule *(x::AbstractArray, y::Vector)          x     gemm!('N', 'T', 1., ds, reshape(y, length(y), 1), 1., dx)  # reshape needed 
# # @deriv_rule *(x::AbstractArray, y::AbstractArray)   x     gemm!('N', 'T', 1., ds, y, 1., dx)

# @deriv_rule *(x::Real         , y::Real )           y     x
# # @deriv_rule *(x::Real         , y::AbstractArray)   y     for i in 1:length(ds) ; dy[i] += x * ds[i] ; end
# # @deriv_rule *(x::AbstractArray, y::Real )           y     for i in 1:length(ds) ; dy += x[i] * ds[i] ; end
# # @deriv_rule *(x::AbstractArray, y::Vector)          y     gemm!('T', 'N', 1., x, reshape(ds, length(ds), 1), 1., dy)
# # @deriv_rule *(x::AbstractArray, y::AbstractArray)   y     gemm!('T', 'N', 1., x, ds, 1., dy)

# # # dot multiplication
# @deriv_rule .*(x::Real         , y::Real )           x     y
# # @deriv_rule .*(x::Real         , y::AbstractArray)   x     for i in 1:length(ds) ; dx += y[i] * ds[i] ; end
# # @deriv_rule .*(x::AbstractArray, y::Real )           x     for i in 1:length(ds) ; dx[i] += y * ds[i] ; end
# # @deriv_rule .*(x::AbstractArray, y::AbstractArray)   x     for i in 1:length(ds) ; dx[i] += y[i] * ds[i] ; end

# @deriv_rule .*(x::Real         , y::Real )           y     x
# # @deriv_rule .*(x::Real         , y::AbstractArray)   y     for i in 1:length(ds) ; dy[i] += x * ds[i] ; end
# # @deriv_rule .*(x::AbstractArray, y::Real )           y     for i in 1:length(ds) ; dy += x[i] * ds[i] ; end
# # @deriv_rule .*(x::AbstractArray, y::AbstractArray)   y     for i in 1:length(ds) ; dy[i] += x[i] * ds[i] ; end

# # # power  (both args reals)
# @deriv_rule ^(x::Real, y::Real)     x     y * x ^ (y-1)
# @deriv_rule ^(x::Real, y::Real)     y     log(x) * x ^ y

# # # dot power
# # @deriv_rule .^(x::Real         , y::Real )            x     dx += y * x ^ (y-1) * ds
# # @deriv_rule .^(x::Real         , y::AbstractArray)    x     for i in 1:length(ds) ; dx += y[i] * x ^ (y[i]-1) * ds[i] ; end
# # @deriv_rule .^(x::AbstractArray, y::Real )            x     for i in 1:length(ds) ; dx[i] += y * x[i] ^ (y-1) * ds[i] ; end
# # @deriv_rule .^(x::AbstractArray, y::AbstractArray)    x     for i in 1:length(ds) ; dx[i] += y[i] * x[i] ^ (y[i]-1) * ds[i] ; end

# # @deriv_rule .^(x::Real         , y::Real )            y     dy += log(x) * x ^ y * ds
# # @deriv_rule .^(x::AbstractArray, y::Real )            y     for i in 1:length(ds) ; dy += log(x[i]) * x[i] ^ y * ds[i] ; end
# # @deriv_rule .^(x::Real         , y::AbstractArray)    y     for i in 1:length(ds) ; dy[i] += log(x) * x ^ y[i] * ds[i] ; end
# # @deriv_rule .^(x::AbstractArray, y::AbstractArray)    y     for i in 1:length(ds) ; dy[i] += log(x[i]) * x[i] ^ y[i] * ds[i] ; end

# # # division
# # @deriv_rule /(x::Real         , y::Real )           x     dx += ds / y
# # @deriv_rule /(x::Real         , y::AbstractArray)   x     for i in 1:length(ds) ; dx += ds[i] / y[i] ; end
# # @deriv_rule /(x::AbstractArray, y::Real )           x     for i in 1:length(ds) ; dx[i] += ds[i] / y ; end

# # @deriv_rule /(x::Real         , y::Real )           y     dy -= x * ds / (y * y)
# # @deriv_rule /(x::Real         , y::AbstractArray)   y     for i in 1:length(ds) ; dy[i] -= x * ds[i] / (y[i]*y[i]) ; end
# # @deriv_rule /(x::AbstractArray, y::Real )           y     for i in 1:length(ds) ; dy -= x[i] * ds[i] / (y * y); end

# # # dot division
# # @deriv_rule ./(x::Real         , y::Real )           x     dx += ds / y
# # @deriv_rule ./(x::Real         , y::AbstractArray)   x     for i in 1:length(ds) ; dx += ds[i] / y[i] ; end
# # @deriv_rule ./(x::AbstractArray, y::Real )           x     for i in 1:length(ds) ; dx[i] += ds[i] / y ; end
# # @deriv_rule ./(x::AbstractArray, y::AbstractArray)   x     for i in 1:length(ds) ; dx[i] += ds[i] / y[i] ; end

# # @deriv_rule ./(x::Real         , y::Real )           y     dy -= x * ds / (y * y)
# # @deriv_rule ./(x::Real         , y::AbstractArray)   y     for i in 1:length(ds) ; dy[i] -= x * ds[i] / (y[i]*y[i]) ; end
# # @deriv_rule ./(x::AbstractArray, y::Real )           y     for i in 1:length(ds) ; dy -= x[i] * ds[i] / (y * y); end
# # @deriv_rule ./(x::AbstractArray, y::AbstractArray)   y     for i in 1:length(ds) ; dy[i] -= x[i] * ds[i] / (y[i] * y[i]); end

# # # transpose
# # @deriv_rule transpose(x::Real )           x   1.
# # @deriv_rule transpose(x::AbstractArray)   x   dx += transpose(ds)
