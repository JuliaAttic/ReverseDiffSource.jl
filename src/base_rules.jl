#########################################################################
#
#   Base rules
#
#########################################################################

@typeequiv    Real     1    # derivatives of scalars are scalars
@typeequiv    Range    2    # usualy not derived against but useful for reversegraph anyway


# derivation neutral functions
@deriv_rule colon(x,y)   x     0.
@deriv_rule colon(x,y)   y     0.
@deriv_rule length(x)    x     0.
@deriv_rule fill(x,y)    x     0.
@deriv_rule fill(x,y)    y     0.
@deriv_rule zeros(x)     x     0.
@deriv_rule ones(x)      x     0.
@deriv_rule size(x)      x     0.


#  tuple  TODO : add definitions of the type  @deriv_tuple  func(x...)
@deriv_rule tuple(x)        x     ds[1]
@deriv_rule tuple(x,y)      x     ds[1]
@deriv_rule tuple(x,y)      y     ds[2]

#  vcat
# @deriv_rule vcat(x,y)       x     ds[1]

# square root
@deriv_rule sqrt(x)    x     0.5 * x ^ (-0.5) * ds

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

>>>>>>> b783b3653551a64376d4c907aec16e0127b51bc2
