
cd(joinpath(Pkg.dir("ReverseDiffSource"), "test"))


include("runtests.jl")

using DataFrames

######################  improved show    ################################
a = rand(20)
ex = :( (a[1] - x[1])^2 + 100(x[2] - x[1]^2)^2 )
g = ReverseDiffSource.tograph(ex)
ReverseDiffSource.calc!(g, params=[:x => [1.,1.]])


######################   ERROR: `zero` has no method matching zero(::Type{Union(NAtype,Bool)})  #####
reload("ReverseDiffSource")
m = ReverseDiffSource
ex = quote
    a=zeros(10)
    for i in 1:10
        t = x+z
        a[i] = b[i]+t
    end
end

g = m.tograph(ex)
m.evalsort!(g)
m.resetvar()
    m.tocode(g) ## error
g

sum(x -> sum(Bool[ p == n for p in x.parents ]), g.nodes) 


sum(x -> sum([ p == n for p in x.parents ]), g.nodes) 





sum(x -> sum(n .== x.parents), g.nodes) 


sum(x -> sum(x.parents .== n), g.nodes[1]) 

sum(x.parents .== n)
x = g.nodes[1]
n = g.nodes[2]

tmp = [ p == n for p in x.parents ]
sum(tmp)

n = g.nodes[7]
np = n.parents[1]
typeof(np.val)
zerosAny[ np.val ]
Any[ valueof(x,n) for x in n.parents ]
ev = Expr(:call, :zeros, Any[ np.val ]...)
eval(ev)

n = g.nodes[8]
pvals = Any[ x.val for x in n.parents ]
ev = Expr(  :(:), pvals...)
eval(ev)

###################### issue #8   ######################################

reload("ReverseDiffSource")
m = ReverseDiffSource

ex = :( (1 - x[1])^2 + 100(x[2] - x[1]^2)^2 )
res = m.rdiff(ex, x=zeros(2), order=3)
@eval foo(x) = $res
foo([0.5, 2.])

(306.5,[-351.0,350.0],
2x2 Array{Float64,2}:
 -498.0  -200.0
 -200.0   200.0,

2x2x2 Array{Float64,3}:
[:, :, 1] =
  400.0  -400.0
 -400.0     0.0

[:, :, 2] =
 0.0  0.0
 0.0  0.0)

δ = 1e-8
1/δ * (foo([0.5+δ, 2.])[1] - foo([0.5, 2.])[1])  # - 351, ok
1/δ * (foo([0.5+δ, 2.])[2] - foo([0.5, 2.])[2])  # ok
1/δ * (foo([0.5+δ, 2.])[3] - foo([0.5, 2.])[3])  # pas ok
# 2x2 Array{Float64,2}:
#  1200.0  -400.0
#  -400.0     0.0

1/δ * (foo([0.5, 2.+δ])[1] - foo([0.5, 2.])[1])  # 350, ok
1/δ * (foo([0.5, 2.+δ])[2] - foo([0.5, 2.])[2])  # ok
1/δ * (foo([0.5, 2.+δ])[3] - foo([0.5, 2.])[3])  # pas ok
# 2x2 Array{Float64,2}:
#  -400.0  0.0
#     0.0  0.0


reload("ReverseDiffSource")
m = ReverseDiffSource

# ex = :( x[1]^3 )  # ok
# ex = :( x[2]^3 )  # ok
# ex = :( x[1]^2 + x[2]^2 ) # ok
ex = :( x[1]^3 + x[2]^3 ) # pb cf plus bas
# ex = :( x[1]^3 + x[1]*x[2]^2 )
# ex = :( sum(x * x'))

x0 = ones(2)
res = m.rdiff(ex, x=x0, order=3)
@eval foo(x) = $res
foo(x0)
2x2x2 Array{Float64,3}:
[:, :, 1] =
 6.0  0.0
 0.0  0.0

[:, :, 2] =
 0.0  0.0
 6.0  6.0)


#######################

ex = quote
	a = 0.
	for i in 1:length(x)
		a += x[i]^i
	end
	a
end

x0 = ones(3)
res = m.rdiff(ex, x=x0, order=3)
@eval foo(x) = $res
foo(x0)

x = copy(x0)
quote 
    _tmp1 = 0.0
    _tmp2 = length(x)
    _tmp3 = 1
    _tmp4 = 1:_tmp2
    _tmp5 = fill(0.0,size(x))
    _tmp6 = _tmp2
    _tmp7 = _tmp2
    for i = _tmp4
        _tmp1 = _tmp1 + x[i]^i
    end
    for i = _tmp4
        _tmp5[i] = _tmp5[i] + i * (x[i]^(i - 1) * _tmp3)
    end
    _tmp8 = zeros((_tmp2,_tmp2))
    _tmp9 = zeros((_tmp2,_tmp2,_tmp2))
    _tmp10 = _tmp5
    _tmp11 = _tmp10
    for _idx1 = _tmp4
        _tmp12 = 0.0
        _tmp13 = fill(0.0,size(x))
        _tmp14 = fill(0.0,size(_tmp11))
        _tmp14[_idx1] = _tmp14[_idx1] + 1.0
        _tmp15 = _tmp14 + fill(0.0,size(_tmp10))
        for i = _tmp4
            _tmp16 = i - 1
            _tmp13[i] = _tmp13[i] + _tmp16 * (x[i]^(_tmp16 - 1) * (_tmp3 * (i * _tmp15[i])))
        end
        _tmp17 = _tmp13
        for i = _tmp4
            _tmp17[i] = _tmp17[i] + i * (x[i]^(i - 1) * _tmp12)
        end
        _tmp8[(_idx1 - 1.0) * _tmp6 + 1.0:_idx1 * _tmp6] = _tmp17
    end
    _tmp18 = _tmp8
    for _idx2 = 1:_tmp2^2  # _idx2 = 7
        _tmp19 = 0.0
        _tmp20 = fill(0.0,size(x))
        _tmp21 = fill(0.0,size(_tmp10))
        _tmp22 = fill(0.0,size(_tmp11))
        _tmp23 = fill(0.0,size(_tmp18))
        _tmp23[_idx2] = _tmp23[_idx2] + 1.0
        for _idx1 = _tmp4
            _tmp24 = 0.0
            _tmp25 = fill(0.0,size(x))
            _tmp26 = fill(0.0,size(_tmp11))
            _tmp26[_idx1] = _tmp26[_idx1] + 1.0
            _tmp27 = _tmp26 + fill(0.0,size(_tmp10))
            for i = _tmp4
                _tmp28 = i - 1
                _tmp25[i] = _tmp25[i] + _tmp28 * (x[i]^(_tmp28 - 1) * (_tmp3 * (i * _tmp27[i])))
            end
            _tmp29 = _tmp25
            for i = _tmp4
                _tmp29[i] = _tmp29[i] + i * (x[i]^(i - 1) * _tmp24)
            end
            _tmp30 = fill(0.0,size(_tmp29))
            _tmp31 = fill(0.0,size(_tmp29)) + sum(_tmp23[(_idx1 - 1.0) * _tmp6 + 1.0:_idx1 * _tmp6])
            for i = _tmp4
                _tmp32 = i - 1
                _tmp30[i] = _tmp30[i] + _tmp31[i]
                _tmp20[i] = _tmp20[i] + _tmp32 * (x[i]^(_tmp32 - 1) * (_tmp24 * (i * _tmp31[i])))
            end
            _tmp33 = _tmp20
            _tmp34 = _tmp30
            for i = _tmp4
                _tmp35 = i - 1
                _tmp36 = _tmp35 - 1
                _tmp33[i] = _tmp33[i] + _tmp36 * (x[i]^(_tmp36 - 1) * ((_tmp3 * (i * _tmp27[i])) * (_tmp35 * _tmp34[i])))
            end
            _tmp20 = _tmp33
        end
        _tmp37 = _tmp20
        _tmp38 = _tmp22 + _tmp21
        for i = _tmp4
            _tmp39 = i - 1
            _tmp37[i] = _tmp37[i] + _tmp39 * (x[i]^(_tmp39 - 1) * (_tmp3 * (i * _tmp38[i])))
        end
        _tmp40 = _tmp37
        for i = _tmp4
            _tmp40[i] = _tmp40[i] + i * (x[i]^(i - 1) * _tmp19)
        end
        _tmp9[(_idx2 - 1.0) * _tmp7 + 1.0:_idx2 * _tmp7] = _tmp40
    end
    (_tmp1,_tmp11,_tmp18,_tmp9)
end

foo (generic function with 1 method)
(3.0,[1.0,2.0,3.0],
3x3 Array{Float64,2}:
 0.0  0.0  0.0
 0.0  2.0  0.0
 0.0  0.0  6.0,

3x3x3 Array{Float64,3}:
[:, :, 1] =
 0.0  0.0  0.0
 0.0  0.0  0.0
 0.0  0.0  0.0

[:, :, 2] =
 0.0  0.0  0.0
 0.0  0.0  0.0
 0.0  0.0  0.0

[:, :, 3] =
 0.0  0.0  0.0
 0.0  0.0  0.0
 6.0  6.0  6.0)    <<<<<<

