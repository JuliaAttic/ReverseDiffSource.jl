######################  setup   ################################
    Pkg.status()

    cd(joinpath(Pkg.dir("ReverseDiffSource"), "test"))

    include("runtests.jl")

    using DataFrames

###################### issue #8   ######################################
    reload("ReverseDiffSource")
    m = ReverseDiffSource

    ex = :( (1 - x[1])^2 + 100(x[2] - x[1]^2)^2 )
    res = m.rdiff(ex, x=zeros(2), order=3)
    @eval foo(x) = $res
    foo([0.5, 2.])

    #=(306.5,[-351.0,350.0],
    2x2 Array{Float64,2}:
     -498.0  0.0
     -200.0  0.0,

    2x2x2 Array{Float64,3}:
    [:, :, 1] =
     1200.0  0.0
     -400.0  0.0

    [:, :, 2] =
     0.0  0.0
     0.0  0.0)=#

    δ = 1e-8
    1/δ * (foo([0.5+δ, 2.])[1] - foo([0.5, 2.])[1])  # - 351, ok
    1/δ * (foo([0.5+δ, 2.])[2] - foo([0.5, 2.])[2])  # ok
    1/δ * (foo([0.5+δ, 2.])[3] - foo([0.5, 2.])[3])  # ok ?
    #=2x2 Array{Float64,2}:
     1200.0  0.0
     -400.0  0.0=#

    1/δ * (foo([0.5, 2.+δ])[1] - foo([0.5, 2.])[1])  # 350, ok
    1/δ * (foo([0.5, 2.+δ])[2] - foo([0.5, 2.])[2])  # faux  !!! 
    1/δ * (foo([0.5, 2.+δ])[3] - foo([0.5, 2.])[3])  # pas ok
    # 2x2 Array{Float64,2}:
    #  -400.0  0.0
    #     0.0  0.0

###################### issue #8   ######################################
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
    (2.0,[3.0,3.0],
    2x2 Array{Float64,2}:
     6.0  0.0
     0.0  0.0,

    2x2x2 Array{Float64,3}:
    [:, :, 1] =
     6.0  0.0
     0.0  0.0

    [:, :, 2] =
     0.0  0.0
     0.0  0.0)

    δ = 1e-8
    1/δ * (foo(x0+[δ, 0])[1] - foo(x0)[1])  # 3, ok
    1/δ * (foo(x0+[δ, 0])[2] - foo(x0)[2])  # ok
    1/δ * (foo(x0+[δ, 0])[3] - foo(x0)[3])  # ok
    # 2x2 Array{Float64,2}:
    #  6.0  0.0
    #  0.0  0.0

    1/δ * (foo(x0+[0, δ])[1] - foo(x0)[1])  # 3, ok
    1/δ * (foo(x0+[0, δ])[2] - foo(x0)[2])  # pas ok
    1/δ * (foo(x0+[0, δ])[3] - foo(x0)[3])  # ok
    # 2x2 Array{Float64,2}:
    #  0.0  0.0
    #  0.0  6.0



    res

    x = copy(x0)
    quote 
        _tmp1 = 1
        _tmp2 = 3
        _tmp3 = 2
        _tmp4 = length(x)
        _tmp5 = fill(0.0,size(x))
        _tmp6 = _tmp1:_tmp4
        _tmp7 = _tmp4
        _tmp8 = zeros((_tmp4,_tmp4))
        _tmp9 = zeros((_tmp4,_tmp4,_tmp4))
        _tmp5[_tmp3] = _tmp5[_tmp3] + _tmp2 * x[_tmp3]^_tmp3
        _tmp5[_tmp1] = _tmp5[_tmp1] + _tmp2 * x[_tmp1]^_tmp3
        for _idx1 = _tmp6
            _tmp10 = _tmp3 - 1.0
            _tmp11 = fill(0.0,size(x))
            _tmp12 = fill(0.0,size(_tmp5))
            _tmp12[_idx1] = _tmp12[_idx1] + 1.0
            _tmp11[_tmp3] = _tmp11[_tmp3] + _tmp3 * (x[_tmp3]^_tmp10 * (_tmp2 * _tmp12[_tmp3]))
            _tmp11[_tmp1] = _tmp11[_tmp1] + _tmp3 * (x[_tmp1]^_tmp10 * (_tmp2 * _tmp12[_tmp1]))
            _tmp8[(_idx1 - 1.0) * _tmp4 + 1.0:_idx1 * _tmp4] = _tmp11
        end
        _tmp13 = _tmp8
        for _idx2 = _tmp1:_tmp4^_tmp3  # _idx2 = 3
            _tmp14 = 0.0
            _tmp15 = 0.0
            _tmp16 = _tmp3 - 1.0
            _tmp17 = _tmp2 - 1.0
            _tmp18 = fill(0.0,size(x))
            _tmp19 = fill(0.0,size(_tmp5))
            _tmp20 = fill(0.0,size(_tmp13))
            _tmp20[_idx2] = _tmp20[_idx2] + 1.0
            for _idx1 = _tmp6
                _tmp21 = _tmp3 - 1.0
                _tmp22 = fill(0.0,size(x))
                _tmp23 = fill(0.0,size(_tmp5))
                _tmp24 = (_tmp2 - 1.0) - 1.0
                _tmp25 = _tmp21 - 1.0
                _tmp23[_idx1] = _tmp23[_idx1] + 1.0
                _tmp26 = _tmp2 * _tmp23[_tmp1]
                _tmp27 = _tmp2 * _tmp23[_tmp3]
                _tmp22[_tmp3] = _tmp22[_tmp3] + _tmp3 * (x[_tmp3]^_tmp21 * _tmp27)
                _tmp14 = _tmp14 + _tmp21 * (x[_tmp1]^_tmp25 * (_tmp26 * (_tmp3 * _tmp20[_tmp1])))
                _tmp22[_tmp1] = _tmp22[_tmp1] + _tmp3 * (x[_tmp1]^_tmp21 * _tmp26)
                _tmp28 = fill(0.0,size(_tmp22)) + sum(_tmp20[(_idx1 - 1.0) * _tmp4 + 1.0:_idx1 * _tmp4])
                _tmp15 = _tmp15 + _tmp21 * (x[_tmp3]^_tmp25 * (_tmp27 * (_tmp3 * _tmp28[_tmp3])))
            end
            _tmp18[_tmp3] = _tmp18[_tmp3] + (_tmp15 + _tmp3 * (x[_tmp3]^_tmp16 * (_tmp2 * _tmp19[_tmp3])))
            _tmp18[_tmp1] = _tmp18[_tmp1] + (_tmp14 + _tmp3 * (x[_tmp1]^_tmp16 * (_tmp2 * _tmp19[_tmp1])))
            _tmp9[(_idx2 - 1.0) * _tmp7 + 1.0:_idx2 * _tmp7] = _tmp18
        end
        (x[_tmp1]^_tmp2 + x[_tmp3]^_tmp2,_tmp5,_tmp13,_tmp9)
    end

######################  loops    #######################################
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

###################### issue #8   ######################################
    reload("ReverseDiffSource")
    m = ReverseDiffSource

    ex = :( x[1]^3 + x[2]^3)
    x0 = ones(2)
    res = m.rdiff(ex, x=x0, order=3)
    @eval foo(x) = $res
    foo(x0)
    # (2.0,[3.0,3.0],
    # 2x2 Array{Float64,2}:
    #  6.0  0.0
    #  0.0  6.0,

    # 2x2x2 Array{Float64,3}:
    # [:, :, 1] =
    #  6.0  0.0
    #  0.0  6.0

    # [:, :, 2] =
    #  0.0  0.0
    #  0.0  0.0)

    δ = 1e-8
    (foo(x0+[δ, 0.])[1] - foo(x0)[1]) / δ  # 3, ok
    (foo(x0+[δ, 0.])[2] - foo(x0)[2]) / δ  # ok
    (foo(x0+[δ, 0.])[3] - foo(x0)[3]) / δ  # not ok
    # 2.999999981767587
    # 2-element Array{Float64,1}:
    #  6.0
    #  0.0
    # 2x2 Array{Float64,2}:
    #  6.0  0.0
    #  0.0  0.0

    (foo(x0+[0., δ])[1] - foo(x0)[1]) / δ  # 3, ok
    (foo(x0+[0., δ])[2] - foo(x0)[2]) / δ  # ok
    (foo(x0+[0., δ])[3] - foo(x0)[3]) / δ  # not ok
    # 2.999999981767587
    # 2-element Array{Float64,1}:
    #  0.0
    #  6.0
    # 2x2 Array{Float64,2}:
    #  0.0  0.0
    #  0.0  6.0

    let x=x0 
        # _tmp1 = 1
        # _tmp2 = 3
        # 2 = 2
        _tmp4 = length(x)
        _tmp5 = fill(0.0,size(x))
        _tmp6 = 1:_tmp4
        _tmp7 = _tmp4
        _tmp8 = zeros((_tmp4,_tmp4))
        _tmp9 = zeros((_tmp4,_tmp4,_tmp4))
        _tmp5[2] = _tmp5[2] + 3 * x[2]^2
        _tmp5[1] = _tmp5[1] + 3 * x[1]^2
        for _idx1 = _tmp6
            _tmp11 = fill(0.0,size(x))
            _tmp12 = fill(0.0,size(_tmp5))
            _tmp12[_idx1] = _tmp12[_idx1] + 1.0
            _tmp11[2] = _tmp11[2] + 2 * (x[2] * (3 * _tmp12[2]))
            _tmp11[1] = _tmp11[1] + 2 * (x[1] * (3 * _tmp12[1]))
            _tmp8[(_idx1 - 1.0) * _tmp4 + 1.0:_idx1 * _tmp4] = _tmp11
        end
        _tmp13 = _tmp8
        for _idx2 = 1:_tmp4^2  
            _idx2 = 3
            _tmp14 = 0.0
            _tmp15 = 0.0
            # _tmp16 = 2 - 1.0
            # _tmp17 = 3 - 1.0
            _tmp18 = fill(0.0,size(x))
            _tmp19 = fill(0.0,size(_tmp5))
            _tmp20 = fill(0.0,size(_tmp13))
            _tmp20[_idx2] = _tmp20[_idx2] + 1.0
            for _idx1 = _tmp6
                # _tmp21 = 2 - 1.0
                _tmp22 = fill(0.0,size(x))
                _tmp23 = fill(0.0,size(_tmp5))
                # _tmp25 = 0.
                _tmp23[_idx1] = _tmp23[_idx1] + 1.0
                _tmp26 = 3 * _tmp23[1]
                _tmp27 = 3 * _tmp23[2]
                _tmp22[2] = _tmp22[2] + 2 * (x[2] * _tmp27)
                _tmp14 = _tmp14 + (_tmp26 * (2 * _tmp20[1]))
                _tmp22[1] = _tmp22[1] + 2 * (x[1] * _tmp26)
                _tmp28 = fill(0.0,size(_tmp22)) + sum(_tmp20[(_idx1 - 1.0) * _tmp4 + 1.0:_idx1 * _tmp4])
                _tmp15 = _tmp15 + (_tmp27 * (2 * _tmp28[2]))
            end
            # _tmp18[2] = _tmp18[2] + (_tmp15 + 2 * (x[2] * (3 * _tmp19[2])))
            # _tmp18[1] = _tmp18[1] + (_tmp14 + 2 * (x[1] * (3 * _tmp19[1])))
            _tmp18[2] = _tmp18[2] + _tmp15
            _tmp18[1] = _tmp18[1] + _tmp14
            _tmp9[(_idx2 - 1.0) * _tmp7 + 1.0:_idx2 * _tmp7] = _tmp18
        end
        (x[1]^3 + x[2]^3,_tmp5,_tmp13,_tmp9)
    end


    ex2 = quote
        _tmp4 = length(x)
        _tmp5 = fill(0.0,size(x))
        _tmp6 = 1:_tmp4
        _tmp7 = _tmp4
        _tmp8 = zeros((_tmp4,_tmp4))
        _tmp9 = zeros((_tmp4,_tmp4,_tmp4))
        _tmp5[2] = _tmp5[2] + 3 * x[2]^2
        _tmp5[1] = _tmp5[1] + 3 * x[1]^2
        for _idx1 = _tmp6  
            _tmp11 = fill(0.0,size(x))
            _tmp12 = fill(0.0,size(_tmp5))
            _tmp12[_idx1] = _tmp12[_idx1] + 1.0
            _tmp11[2] = _tmp11[2] + 2 * (x[2] * (3 * _tmp12[2]))
            _tmp11[1] = _tmp11[1] + 2 * (x[1] * (3 * _tmp12[1]))
            _tmp8[(_idx1 - 1.0) * _tmp4 + 1.0:_idx1 * _tmp4] = _tmp11
        end
        _tmp8[_idx2]
    end

    @eval foo0(x) = $ex2
    _idx2 = 1 ; foo0(x0)
    _idx2 = 2 ; foo0(x0)
    _idx2 = 3 ; foo0(x0)
    _idx2 = 4 ; foo0(x0)
    foo0(x0)


    res = m.rdiff(ex2, x=x0, order=1)
    @eval foo(x) = $res
    foo(x0)
    _idx2 = 1 ; foo(x0)[2][1]
    _idx2 = 2 ; foo(x0)[2][1]
    _idx2 = 3 ; foo(x0)[2][1]
    _idx2 = 4 ; foo(x0)[2][1]

    _idx2 = 3
    x = copy(x0)
    quote 
        _tmp1 = length(x)
        _tmp2 = size(x)
        _tmp3 = fill(0.0,_tmp2)
        _tmp4 = 1:_tmp1
        _tmp5 = fill(0.0,_tmp2)
        _tmp6 = zeros((_tmp1,_tmp1))
        _tmp3[2] = _tmp3[2] + 3 * x[2]^2
        _tmp3[1] = _tmp3[1] + 3 * x[1]^2
        for _idx1 = _tmp4
            _tmp7 = fill(0.0,size(x))
            _tmp8 = fill(0.0,size(_tmp3))
            _tmp8[_idx1] = _tmp8[_idx1] + 1.0
            _tmp7[2] = _tmp7[2] + 2 * (x[2] * (3 * _tmp8[2]))
            _tmp7[1.0] = _tmp7[1.0] + 2 * (x[1.0] * (3 * _tmp8[1.0]))
            _tmp6[(_idx1 - 1.0) * _tmp1 + 1.0:_idx1 * _tmp1] = _tmp7
        end
        _tmp9 = fill(0.0,size(_tmp3))
        _tmp10 = fill(0.0,size(_tmp6))
        _tmp10[_idx2] = _tmp10[_idx2] + 1
        for _idx1 = _tmp4 # _idx1 = 1
            _tmp11 = fill(0.0,size(x))
            _tmp12 = fill(0.0,size(_tmp3))
            _tmp12[_idx1] = _tmp12[_idx1] + 1.0
            _tmp13 = 3 * _tmp12[2]
            _tmp14 = 3 * _tmp12[1.0]
            _tmp5[1] = _tmp5[1] + _tmp14 * (2 * _tmp10[1])  <<<<  pourquoi indice fixe ??
            _tmp11[2] = _tmp11[2] + 2 * (x[2] * _tmp13)
            _tmp11[1] = _tmp11[1] + 2 * (x[1] * _tmp14)
            _tmp15 = fill(0.0,size(_tmp11)) + sum(_tmp10[(_idx1 - 1) * _tmp1 + 1:_idx1 * _tmp1])
            _tmp5[2] = _tmp5[2] + _tmp13 * (2 * _tmp15[2])
        end
        _tmp5[1] = _tmp5[1] + 2 * (x[1] * (3 * _tmp9[1]))
        _tmp5[2] = _tmp5[2] + 2 * (x[2] * (3 * _tmp9[2]))
    end

###################### indexing pb ?  #######################

    ex3 = quote
        _tmp4 = length(x)
        _tmp5 = fill(0.0,size(x))
        _tmp6 = 1:_tmp4
        _tmp7 = _tmp4
        _tmp8 = zeros((_tmp4,_tmp4))
        # _tmp9 = zeros((_tmp4,_tmp4,_tmp4))
        _tmp5[1] = _tmp5[1] + 3 * x[1]^2
        _tmp5[2] = _tmp5[2] + 3 * x[2]^2
        for _idx1 = _tmp6  
            _tmp11 = fill(0.0,size(x))
            _tmp12 = fill(0.0,size(_tmp5))
            _tmp12[_idx1] = _tmp12[_idx1] + 1.0
            _tmp11[1] = _tmp11[1] + 2 * (x[1] * (3 * _tmp12[1]))
            _tmp11[2] = _tmp11[2] + 2 * (x[2] * (3 * _tmp12[2]))
            _tmp8[(_idx1 - 1.0) * _tmp4 + 1.0:_idx1 * _tmp4] = _tmp11
        end
        _tmp8[_idx2]
    end

    x0 = [1., 1.]
    res = m.rdiff(ex3, x=x0);
    @eval foo(x, _idx2) = $res
    mapreduce(i -> foo(x0,i)[2][1], hcat, 1:4)
    δ = 1e-8
    [ (foo(x0+δ*eye(2)[:,v],i)[1] - foo(x0,i)[1]) / δ  for v in 1:2, i in 1:4 ]

###################### indexing pb ?  #######################

    ex4 = quote
        _tmp11 = fill(0.0,size(x))
        _tmp12 = fill(0.0,size(x))
        _tmp12[_idx1] = _tmp12[_idx1] + 1.0
        _tmp11[1] = _tmp11[1] + 2 * (x[1] * (3 * _tmp12[1]))
        _tmp11[2] = _tmp11[2] + 2 * (x[2] * (3 * _tmp12[2]))
        _tmp11[_idx2]
    end

    x0 = [1., 1.]
    res = m.rdiff(ex4, x=x0);
    @eval foo(x, _idx2) = $res
    mapreduce(i -> foo(x0,i)[2][1], hcat, 1:4)
    δ = 1e-8
    [ (foo(x0+δ*eye(2)[:,v],i)[1] - foo(x0,i)[1]) / δ  for v in 1:2, i in 1:4 ]


###################### indexing pb ?  #######################
    reload("ReverseDiffSource")
    m = ReverseDiffSource

    function check(ex)
        global _idx2

        _idx2 = 1
        x0 = [1., 1.]
        res = m.rdiff(ex, x=x0);
        @eval foo(x) = $res
        cres = Array(Float64, 4, 2)
        _idx2 = 1 ; cres[1,:] = foo(x0)[2][1]
        _idx2 = 2 ; cres[2,:] = foo(x0)[2][1]
        _idx2 = 3 ; cres[3,:] = foo(x0)[2][1]
        _idx2 = 4 ; cres[4,:] = foo(x0)[2][1]
        cres
    end

    ex3 = quote
        _tmp8 = zeros(2,2)
        _tmp11 = fill(0.0, size(x))
        for _idx1 = 1:2  
            _tmp11[2] = _tmp11[2] + x[2]
            _tmp11[1] = _tmp11[1] + x[1]
            _tmp8[((_idx1 - 1) * 2 + 1):(_idx1 * 2)] = _tmp11
            # _tmp8[2 * _idx1]     = _tmp11[2]
            # _tmp8[2 * _idx1 - 1] = _tmp11[1]
            # _tmp8[2 * _idx1]     = _tmp11[2] + x[2]
            # _tmp8[2 * _idx1 - 1] = _tmp11[1] + x[1]
        end
        _tmp8[_idx2]
    end

    check(ex3)

################  inconsistent NSRef   ###############
    reload("ReverseDiffSource")
    m = ReverseDiffSource


    function check(ex)
        const δ = 1e-8
        x0 = [1., 1.]
        res = m.rdiff(ex, x=x0);
        nfoo = @eval foo(x, _idx2) = $res
        println( mapreduce(i -> nfoo(x0,i)[2][1], hcat, 1:4) )
        println( Float64[ round((nfoo(x0+δ*eye(2)[:,v],i)[1] - nfoo(x0,i)[1]) / δ)  for v in 1:2, i in 1:4 ])
    end


    _idx2 = 1
    ex3 = quote
        a = zeros(2,2)
        b = zeros(2)

        b[2] = b[2] + x[2]
        b[1] = b[1] + x[1]
        a[1:2] = b

        b[2] = b[2] + x[2]
        b[1] = b[1] + x[1]
        a[3:4] = b

        a[_idx2]
    end
    check(ex3)
    # [2.0 0.0 0.0 0.0
    #  0.0 2.0 0.0 0.0]
    # [1.0 0.0 2.0 0.0
    #  0.0 1.0 0.0 2.0], erreur !


    ex3 = quote
        a = zeros(2,2)
        b = zeros(2)

        b[1] = b[1] + x[1]
        b[2] = b[2] + x[2]
        a[1] = b[1]
        a[2] = b[2]

        b[2] = b[2] + x[2]
        b[1] = b[1] + x[1]
        a[3] = b[1]
        a[4] = b[2]

        a[_idx2]
    end
    check(ex3)
    # [0.0 0.0 2.0 0.0
    #  0.0 0.0 0.0 2.0]
    # [1.0 0.0 2.0 0.0
    #  0.0 1.0 0.0 2.0], erreur ! (mais différente)

    ex3 = quote
        a = zeros(4)
        b = zeros(2)

        b += x
        a[1:2] = b
        b[1] = b[1] + x[1]
        # b[2] = b[2] + x[2]
        a[3:4] = b

        a[_idx2]
    end
    check(ex3)
    # [3.0 0.0 0.0 0.0
    #  0.0 1.0 0.0 0.0]
    # [1.0 0.0 2.0 0.0
    #  0.0 1.0 0.0 1.0]

    function test(_idx2)
        da = zeros(4)
        da[_idx2] = da[_idx2] + 1
        db1 = zeros(2)
        db1 = db1 + da[3:4]
        dx = zeros(2)
        dx[1] = dx[1] + db1[1]
        db2 = zeros(2)
        db2[1] = db2[1] + db1[1]
        dx  = dx + db2
        db2 = db2 + da[1:2]
        dx
    end    
    mapreduce(test, hcat, 1:4)

    g = m.tograph(ex3)
    m.calc!(g, params=[ :x => x0])
    m.simplify!(g)
    # m.tocode(g)

    m.rdiff(ex3, x=x0)
    quote # x = x0 
        _tmp1 = zeros(2,2)
        _tmp2 = 1:2

        _tmp3 = zeros(2) + x

        _tmp1[_tmp2] = _tmp3

        _tmp3[1] = _tmp3[1] + x[1]
        _tmp3[2] = _tmp3[2] + x[2]

        _tmp1[3:4] = _tmp3

        _tmp5 = fill(0.0,size(_tmp1))
        _tmp5[_idx2] = _tmp5[_idx2] + 1
        
        _tmp4 = fill(0.0,size(_tmp3))
        _tmp4[2] = _tmp4[2] + _tmp5[2]

        _tmp6 = fill(0.0,size(x)) + (_tmp4 + _tmp5[_tmp2])
        _tmp6[1] = _tmp6[1] + _tmp5[1]
        _tmp6[2] = _tmp6[2] + _tmp5[2]

        (_tmp1[_idx2],(_tmp6,))
    end


    ex3 = quote
        a = zeros(2,2)

        a[1] = x[1]
        a[3] = x[1]
        a[4] = x[1]
        a[1] = x[1]    # l'ajout de ça fait planter, pb des affectation multiples sur le même indice ?
        a[_idx2]
    end
    check(ex3)

    # [1.0 0.0 1.0 1.0
    #  0.0 0.0 0.0 0.0]
    # [1.0 0.0 1.0 1.0
    #  0.0 0.0 0.0 0.0], ok






    ex3 = quote
        a = zeros(2,2)
        b = fill(0.0, size(x))

        b[2] = b[2] + x[2]
        b[1] = b[1] + x[1]
        a[1:2] = b
        a[_idx2]
    end
    check(ex3)
    # [1.0 0.0 0.0 0.0
    #  0.0 1.0 0.0 0.0]
    # Any[1.0 0.0 0.0 0.0
    #     0.0 1.0 0.0 0.0]  , OK

    ex3 = quote
        a = zeros(2,2)
        b = zeros(2)

        b[2] = b[2] + x[2]
        b[1] = b[1] + x[1]
        # a[1:2] = b
        a[1] = b[1]
        a[2] = b[2]
        a[_idx2]
    end
    check(ex3) # OK

    ex3 = quote
        a = zeros(2,2)
        a[1:2] = x
        a[_idx2]
    end
    check(ex3)  # ok

    ex3 = quote
        a = zeros(2,2)
        a[1:2] = x[1]
        a[_idx2]
    end
    check(ex3)  # OK

    # x0 = [1., 1.]
    # res = m.rdiff(ex3, x=x0);
    # @eval foo(x, _idx2) = $res
    # # map(i -> foo(x0,i), 1:4)
    # δ = 1e-8
    # [ (foo(x0+δ*eye(2)[:,v],i)[1] - foo(x0,i)[1]) / δ  for v in 1:2, i in 1:4 ]

################  inconsistent NSRef   ###############
    reload("ReverseDiffSource")
    m = ReverseDiffSource

    function check(ex)
        const δ = 1e-8
        x0 = 1.
        res = m.rdiff(ex, x=x0);
        nfoo = @eval foo(x) = $res
        println(" calc = $(nfoo(x0)[2][1]) vs vrai = $(round((nfoo(x0+δ)[1] - nfoo(x0)[1]) / δ)) ")
    end

    ex4 = quote
        a = zeros(2)

        a[1] = x
        a[2] = x

        sum(a)
    end
    check(ex4) #    calc = 1.0 vs vrai = 2.0 

    m.rdiff(ex4, x=1.)

    #  calc = 2.0 vs vrai = 2.0 # ok
    # calc = 1.0 vs vrai = 2.0 

    ex4 = quote
        a = zeros(2)

        a[1] = x
        a[1] = x

        sum(a)
    end
    check(ex4) #  calc = 1.0 vs vrai = 1.0 
    m.rdiff(ex4, x=1.)

    ex4 = quote
        a = zeros(2)

        a[1] = 3x
        a[1] = x

        sum(a)
    end
    check(ex4)     #  calc = 1.0 vs vrai = 1.0 

    ex4 = quote
        a = zeros(2)

        a[1] = x
        a[1] = 3x

        sum(a)
    end
    check(ex4)     #   calc = 3.0 vs vrai = 3.0 



    m.@deriv_rule %(x,y)      x     0
    m.@deriv_rule %(x,y)      y     0

    ex4 = quote
        a = zeros(2)

        for i in 1:4
            a[1 + i % 2] = x
        end

        sum(a)
    end
    check(ex4)  #  calc = 4.0 vs vrai = 2.0 , faux

    ex4 = quote
        a = zeros(2)

        for i in 1:4
            a[1] = x
        end

        sum(a)
    end
    check(ex4)  # faux
    # calc = 4.0 vs vrai = 1.0 

    ex4 = quote
        a = 0.
        a = 3x
        a = x
        a
    end
    check(ex4)  #  calc = 1.0 vs vrai = 1.0 

    ex4 = quote
        a = zeros(2)
        a += 3x
        a = x
        a
    end
    check(ex4)  #  calc = 1.0 vs vrai = 1.0 

