#########################################################################
#
#   Base rules
#
#########################################################################

# derivation neutral functions
@deriv_rule colon(x,y)   x     0.
@deriv_rule colon(x,y)   y     0.

@deriv_rule length(x)    x     0.

@deriv_rule size(x)      x     0.
@deriv_rule size(x,y)    x     0.
@deriv_rule size(x,y)    y     0.

@deriv_rule fill(x,y)    x     0.
@deriv_rule fill(x,y)    y     0.

@deriv_rule similar(x,y) x     0.
@deriv_rule similar(x,y) y     0.

@deriv_rule zeros(x)     x     0.

@deriv_rule ones(x)      x     0.

@deriv_rule cell(x)      x     0.

@deriv_rule sign(x)      x     0.

@deriv_rule reverse(x)   x     0.

#  tuple  TODO : specialized macro for this kind of function
@deriv_rule tuple(x)        x     ds[1]
@deriv_rule tuple(x,y)      x     ds[1]
@deriv_rule tuple(x,y)      y     ds[2]
@deriv_rule tuple(x,y,z)    x     ds[1]
@deriv_rule tuple(x,y,z)    y     ds[2]
@deriv_rule tuple(x,y,z)    z     ds[3]
@deriv_rule tuple(x,y,z,t)  x     ds[1]
@deriv_rule tuple(x,y,z,t)  y     ds[2]
@deriv_rule tuple(x,y,z,t)  x     ds[3]
@deriv_rule tuple(x,y,z,t)  t     ds[4]

#  vcat
# @deriv_rule vcat(x)        x     ds[1]
# @deriv_rule vcat(x,y)      x     ds[1]
# @deriv_rule vcat(x,y)      y     ds[2]
# @deriv_rule vcat(x,y,z)    x     ds[1]
# @deriv_rule vcat(x,y,z)    y     ds[2]
# @deriv_rule vcat(x,y,z)    z     ds[3]
# @deriv_rule vcat(x,y,z,t)  x     ds[1]
# @deriv_rule vcat(x,y,z,t)  y     ds[2]
# @deriv_rule vcat(x,y,z,t)  x     ds[3]
# @deriv_rule vcat(x,y,z,t)  t     ds[4]

# reshape
@deriv_rule reshape(x::AbstractArray, a, b)        x    reshape(ds, size(x))
@deriv_rule reshape(x::AbstractArray, a, b)        a    0.
@deriv_rule reshape(x::AbstractArray, a, b)        b    0.
@deriv_rule reshape(x::AbstractArray, d::Tuple)    x    reshape(ds, size(x))
@deriv_rule reshape(x::AbstractArray, d::Tuple)    d    0.

# copy
@deriv_rule     copy(x)     x   ds

# copy!
@deriv_rule_mut copy!(x,y)  x   fill!(ds, 0.)
@deriv_rule     copy!(x,y)  y   ds

# fill!
@deriv_rule_mut fill!(x,y)  x   fill!(ds, 0.)
@deriv_rule     fill!(x,y)  y   sum(ds)

# getindex
@deriv_rule getindex(x, i)               x     (tmp = zeros(x); tmp[i]=ds ; tmp)
@deriv_rule getindex(x, i)               i     0.

@deriv_rule getindex(x, i1, i2)          x     (tmp = zeros(x); tmp[i1, i2]=ds ; tmp)
@deriv_rule getindex(x, i1, i2)          i1    0.
@deriv_rule getindex(x, i1, i2)          i2    0.

@deriv_rule getindex(x, i1, i2, i3)      x     (tmp = zeros(x); tmp[i1, i2, i3]=ds ; tmp)
@deriv_rule getindex(x, i1, i2, i3)      i1    0.
@deriv_rule getindex(x, i1, i2, i3)      i2    0.
@deriv_rule getindex(x, i1, i2, i3)      i3    0.

@deriv_rule getindex(x, i1, i2, i3, i4)  x     (tmp = zeros(x); tmp[i1, i2, i3, i4]=ds ; tmp)
@deriv_rule getindex(x, i1, i2, i3, i4)  i1    0.
@deriv_rule getindex(x, i1, i2, i3, i4)  i2    0.
@deriv_rule getindex(x, i1, i2, i3, i4)  i3    0.
@deriv_rule getindex(x, i1, i2, i3, i4)  i4    0.

# setindex
@deriv_rule_mut setindex!(x, y, i)          x     ds[i] = 0.
@deriv_rule     setindex!(x, y, i)          y     ds[i]
@deriv_rule     setindex!(x, y::Real, i::Range)    y     sum(ds[i])
@deriv_rule     setindex!(x, y, i)          i     0.

@deriv_rule_mut setindex!(x, y, i1, i2)    x     ds[i1,i2] = 0.
@deriv_rule     setindex!(x, y, i1, i2)    y     ds[i1,i2]
@deriv_rule     setindex!(x, y, i1, i2)    i1    0.
@deriv_rule     setindex!(x, y, i1, i2)    i2    0.


# square root
@deriv_rule sqrt(x::Real)              x     0.5 * x ^ (-0.5) * ds
@deriv_rule sqrt(x::AbstractVector)    x     0.5 .* x .^ (-0.5) .* ds

# addition
@deriv_rule +(x::Real         , y::Real )            x     ds
@deriv_rule +(x::AbstractArray, y::AbstractArray)    x     ds
@deriv_rule +(x::Real         , y::Real )            y     ds
@deriv_rule +(x::AbstractArray, y::AbstractArray)    y     ds

@deriv_rule +(x::Real         , y::AbstractArray)    x     sum(ds)
@deriv_rule +(x::AbstractArray, y       )            x     ds
@deriv_rule +(x::AbstractArray, y::Real )            y     sum(ds)
@deriv_rule +(x               , y::AbstractArray)    y     ds

# dot addition
@deriv_rule .+(x::Real         , y::Real )            x     ds
@deriv_rule .+(x::Real         , y::AbstractArray)    x     sum(ds)
@deriv_rule .+(x::AbstractArray, y       )            x     ds
@deriv_rule .+(x::Real         , y::Real )            y     ds
@deriv_rule .+(x::AbstractArray, y::Real )            y     sum(ds)
@deriv_rule .+(x               , y::AbstractArray)    y     ds

# unary substraction
@deriv_rule -(x::Real )                              x     -ds
@deriv_rule -(x::AbstractArray)                      x     -ds

# binary substraction
@deriv_rule -(x::Real         , y::Real )            x     ds
@deriv_rule -(x::AbstractArray, y::AbstractArray)    x     ds
@deriv_rule -(x::Real         , y::Real )            y     -ds
@deriv_rule -(x::AbstractArray, y::AbstractArray)    y     -ds

# dot binary substraction
@deriv_rule .-(x::Real         , y::Real )            x     ds
@deriv_rule .-(x::Real         , y::AbstractArray)    x     sum(ds)
@deriv_rule .-(x::AbstractArray, y       )            x     ds
@deriv_rule .-(x::Real         , y::Real )            y     -ds
@deriv_rule .-(x::AbstractArray, y::Real )            y     -sum(ds)
@deriv_rule .-(x               , y::AbstractArray)    y     -ds

# sum()
@deriv_rule sum(x::Real )                            x     ds
@deriv_rule sum(x::AbstractArray)                    x     ones(x).*ds

# dot()
@deriv_rule dot(x::Real         , y::Real )          x     y * ds
@deriv_rule dot(x::Real         , y::Real )          y     x * ds

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

# maximum, minimum
@deriv_rule maximum(x::Real         )     x     ds
@deriv_rule maximum(x::AbstractArray)     x     (x .== maximum(x)) .* ds

@deriv_rule minimum(x::Real         )     x     ds
@deriv_rule minimum(x::AbstractArray)     x     (x .== minimum(x)) .* ds


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
@deriv_rule /(x::AbstractArray , y::Real )           x     ds ./ y

@deriv_rule /(x::Real          , y::Real )           y     -x * ds / (y * y)
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
