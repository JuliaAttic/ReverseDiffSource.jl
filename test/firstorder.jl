#################################################################
#
#    1st order derivation testing
#
#################################################################

###### sum()
@compare sum(x)  v0ref
@compare sum(x)  v1ref
@compare sum(x)  v2ref

###### abs()
@compare abs(x)        v0ref
@compare sum(abs(x))   v1ref .+ 0.1(sign(v1ref).==0.)
@compare sum(abs(x))   v2ref .+ 0.1(sign(v2ref).==0.)

###### sin()
@compare sin(x)      v0ref
@compare sum(sin(x)) v1ref
@compare sum(sin(x)) v2ref

###### cos()
@compare cos(x)      v0ref
@compare sum(cos(x)) v1ref
@compare sum(cos(x)) v2ref

###### exp()
@compare exp(x)      v0ref
@compare sum(exp(x)) v1ref
@compare sum(exp(x)) v2ref

###### log()
@compare log(x)      v0ref
@compare sum(log(x)) abs( v1ref .+ 0.1(sign(v1ref).==0.) )
@compare sum(log(x)) abs( v2ref .+ 0.1(sign(v2ref).==0.) )

###### sqrt()
@compare sqrt(x)      v0ref
@compare sum(sqrt(x)) abs( v1ref .+ 0.1(sign(v1ref).==0.))

###### - (unary)
@compare -x      v0ref
@compare sum(-x) v1ref
@compare sum(-x) v2ref

###### ' transpose
@compare x'                v0ref
@compare sum(x')           v1ref
@compare sum(x')           v2ref
@compare transpose(x)      v0ref
@compare sum(transpose(x)) v1ref
@compare sum(transpose(x)) v2ref

###### +
@compare x + 1.    v0ref
@compare 1. + x    v0ref

@compare sum(x + v1ref) v1ref
@compare sum(v1ref + x) v1ref
@compare sum(x + v2ref) v2ref
@compare sum(v2ref + x) v2ref

###### -
@compare x - 1.    v0ref
@compare 1. - x    v0ref

@compare sum(x - v1ref)    v1ref
@compare sum(v1ref - x)    v1ref
@compare sum(x - 0.5v2ref) v2ref
@compare sum(3v2ref - x)   v2ref

###### .+
@compare x  .+ 1.   v0ref
@compare 1. .+ x    v0ref

@compare sum(x  .+ 2.)   v1ref
@compare sum(3. .+ x)    v1ref
@compare sum(x .+ v1ref) v1ref
@compare sum(v1ref .+ x) v1ref

@compare sum(x  .+ 2.)   v2ref
@compare sum(3. .+ x)    v2ref
@compare sum(x .+ v2ref) v2ref
@compare sum(v2ref .+ x) v2ref

###### .-
@compare x  .- 1.   v0ref
@compare 1. .- x    v0ref

@compare sum(x  .- 2.)   v1ref
@compare sum(3. .- x)    v1ref
@compare sum(x .- v1ref) v1ref
@compare sum(v1ref .- x) v1ref

@compare sum(x  .- 2.)   v2ref
@compare sum(3. .- x)    v2ref
@compare sum(x .- v2ref) v2ref
@compare sum(v2ref .- x) v2ref

###### .*
@compare x  .* 1.   v0ref
@compare 1. .* x    v0ref

@compare sum(x  .* 2.)   v1ref
@compare sum(3. .* x)    v1ref
@compare sum(x .* v1ref) v1ref
@compare sum(v1ref .* x) v1ref

@compare sum(x  .* 2.)   v2ref
@compare sum(3. .* x)    v2ref
@compare sum(x .* v2ref) v2ref
@compare sum(v2ref .* x) v2ref

###### ./
@compare x  ./ 1.   v0ref
@compare 1. ./ x    v0ref

@compare sum(x      ./ 2.)             v1ref
@compare sum(3.     ./  x)             v1ref .+ 0.1(sign(v1ref) .== 0.)
@compare sum(x      ./ (v1ref .+ 0.1)) v1ref
@compare sum(2v1ref ./  x)             v1ref .+ 0.1(sign(v1ref) .== 0.)

@compare sum(x      ./ 2.)             v2ref
@compare sum(3.     ./  x)             v2ref .+ 0.1(sign(v2ref) .== 0.)
@compare sum(x      ./ (v2ref .+ 0.1)) v2ref
@compare sum(2v2ref ./  x)             v2ref .+ 0.1(sign(v2ref) .== 0.)

###### /
@compare   x / 3.   v0ref
@compare -1. / x    v0ref

@compare sum(x /   2.)     v1ref
@compare sum(x / -0.5)     v2ref

###### max
@compare max( x, 1.)   v0ref
@compare max(1.,  x)   v0ref

@compare sum(max(            x,                   2.1))  v1ref
@compare sum(max(          -3.,                     x))  v1ref
@compare sum(max(            x, 1. .- abs(v1ref)./2.1))  v1ref
@compare sum(max(0.5abs(v1ref),                x.+0.1))  v1ref

@compare sum(max(            x      , -1.1                )) v2ref
@compare sum(max(          0.1      , x                   )) v2ref
@compare sum(max(            x      , -abs(v2ref)./2.+0.1 )) v2ref
@compare sum(max(0.5abs(v2ref).+0.1 , x                   )) v2ref

###### min
@compare min( x, 1.)   v0ref
@compare min(1.,  x)   v0ref

@compare sum(min(            x,              2.))  v1ref
@compare sum(min(          -3.,               x))  v1ref
@compare sum(min(            x, -abs(v1ref)./2.))  v1ref
@compare sum(min(0.5abs(v1ref),               x))  v1ref

@compare sum(min(            x,             -1.))  v2ref
@compare sum(min(           0.,               x))  v2ref
@compare sum(min(            x, -abs(v2ref)./2.))  v2ref
@compare sum(min(0.5abs(v2ref),               x))  v2ref

###### maximum
@compare maximum(x)   v0ref
@compare maximum(x)   v1ref
@compare maximum(x)   v2ref

###### minimum
@compare minimum(x)   v0ref
@compare minimum(x)   v1ref
@compare minimum(x)   v2ref

###### dot
@compare dot(  x, 3.)   v0ref
@compare dot(-1.,  x)   v0ref

@compare dot(            x, -abs(v1ref)./2.)  v1ref
@compare dot(0.5abs(v1ref),               x)  v1ref

###### ^
@compare x ^ 3.   v0ref
@compare 3. ^ x   v0ref

###### .^
@compare x .^ 3.    v0ref
@compare 3 .^ x     v0ref
@compare x .^ -3.   v0ref

@compare sum( x .^  2)  v1ref
@compare sum( x .^ -1)  v1ref .+ 0.1(sign(v1ref) .== 0.)
@compare sum( 2 .^  x)  v1ref

@compare sum( x .^  2)  v2ref
@compare sum( x .^ -1)  v2ref .+ 0.1(sign(v2ref) .== 0.)
@compare sum( 2 .^  x)  v2ref

###### *
@compare  x * 3.    v0ref
@compare -2 * x     v0ref

@compare  sum(ones(5)'   * x)            v1ref
@compare  sum(x'         * v1ref)        v1ref
@compare  sum(ones(5)    * x')           v1ref
@compare  sum(x          * (v1ref./2.)') v1ref

@compare  sum(2ones(5,2) * x)            v2ref
@compare  sum(zeros(1,2) * x)            v2ref
@compare  sum(x          * ones(3,2))    v2ref
@compare  sum(x          * -1ones(3,1))  v2ref

###### tan
@compare tan(x)      v0ref
@compare sum(tan(x)) v1ref
@compare sum(tan(x)) v2ref

###### sinh
@compare sinh(x)      v0ref
@compare sum(sinh(x)) v1ref
@compare sum(sinh(x)) v2ref

###### cosh
@compare cosh(x)      v0ref
@compare sum(cosh(x)) v1ref
@compare sum(cosh(x)) v2ref

###### tanh
@compare tanh(x)      v0ref
@compare sum(tanh(x)) v1ref
@compare sum(tanh(x)) v2ref

###### asin
@compare asin(x)      clamp(v0ref, -.99, .99)
@compare sum(asin(x)) clamp(v1ref, -.99, .99)
@compare sum(asin(x)) clamp(v2ref, -.99, .99)

###### acos
@compare acos(x)      clamp(v0ref, -.99, .99)
@compare sum(acos(x)) clamp(v1ref, -.99, .99)
@compare sum(acos(x)) clamp(v2ref, -.99, .99)

###### atan
@compare atan(x)      v0ref
@compare sum(atan(x)) v1ref
@compare sum(atan(x)) v2ref

###### round
@compare round(x)      v0ref
@compare sum(round(x)) v1ref
@compare sum(round(x)) v2ref

###### ceil
@compare ceil(x)      v0ref + 1e-5
@compare sum(ceil(x)) v1ref + 1e-5
@compare sum(ceil(x)) v2ref + 1e-5

###### floor
@compare floor(x)      v0ref + 1e-5
@compare sum(floor(x)) v1ref + 1e-5
@compare sum(floor(x)) v2ref + 1e-5

###### trunc
@compare trunc(x)      v0ref + 1e-5
@compare sum(trunc(x)) v1ref + 1e-5
@compare sum(trunc(x)) v2ref + 1e-5

###### mod2pi
@compare mod2pi(x)      v0ref

###### log1p
@compare log1p(x)      clamp(v0ref, -0.99, Inf)
@compare sum(log1p(x)) clamp(v1ref, -0.99, Inf)
@compare sum(log1p(x)) clamp(v2ref, -0.99, Inf)

###### expm1
@compare expm1(x)      v0ref
@compare sum(expm1(x)) v1ref
@compare sum(expm1(x)) v2ref

###### erf
@compare erf(x)      v0ref
@compare sum(erf(x)) v1ref
@compare sum(erf(x)) v2ref

###### erfc
@compare erfc(x)      v0ref
@compare sum(erfc(x)) v1ref
@compare sum(erfc(x)) v2ref

###### gamma
@compare gamma(x)      v0ref + 0.1 # to stay away from negative integers
@compare sum(gamma(x)) v1ref + 0.1
@compare sum(gamma(x)) v2ref + 0.1

###### lgamma
@compare lgamma(x)      v0ref + 0.1
@compare sum(lgamma(x)) v1ref + 0.1
@compare sum(lgamma(x)) v2ref + 0.1

###### beta
@compare beta(x, 2.)             v0ref + 0.1
@compare sum(beta(x, x .* 0.2))  v1ref + 0.1
@compare sum(beta(x, x))         v2ref + 0.1

###### lbeta
@compare lbeta(x, 2.)             v0ref + 0.1
@compare sum(lbeta(x, x .* 0.2))  v1ref + 0.1
@compare sum(lbeta(x, x))         v2ref + 0.1
