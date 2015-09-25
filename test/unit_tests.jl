#################################################################
#
#    Internal function testing
#
#    m = ReverseDiffSource module
#################################################################

@test m.isSymbol(:a)            == true
@test m.isSymbol(:(a[1]))       == false
@test m.isSymbol(:(a.b))        == false
@test m.isSymbol(:(exp(a)))     == false

@test m.isRef(:a)            == false
@test m.isRef(:(a[1]))       == true
@test m.isRef(:(a[x]))       == true
@test m.isRef(:(a.b))        == false
@test m.isRef(:(a.b[end]))   == false
@test m.isRef(:(a[end].b))   == false
@test m.isRef(:(exp(a)))     == false

@test m.isDot(:a)           == false
@test m.isDot(:(a[1]))      == false
@test m.isDot(:(a[x]))      == false
@test m.isDot(:(a.b))       == true
@test m.isDot(:(a.b[end]))  == false
@test m.isDot(:(a[end].b))  == false
@test m.isDot(:(exp(a)))    == false

@test m.dprefix("coucou")            == :dcoucou
@test m.dprefix(:tr)                 == :dtr


#### accumulator variable generation testing ####
    function zerocode(v)
        zex = m.zeronode( m.NConst(:abcd, [], [], v, false) )
        m.resetvar()
        m.tocode(zex)
    end

    @test zerocode(12)       == :(0.0;)
    @test zerocode(12.2)     == :(0.0;)
    @test zerocode(5:6)      == :(zeros(2); )
    @test zerocode(5:0.2:10) == :(zeros(2); )

    @test zerocode( [2., 2 , 3, 0] )    == :(zeros(size(tv)); )
    @test zerocode( [2.; [1,2 ]; 3.] )  == :(zeros(size(tv)); )

    @test zerocode( [false, true] )    == :(zeros(size(tv)); )

    @test zerocode(12.2-4im) == striplinenumbers( quote
                                    _tmp1 = cell(2)
                                    _tmp1[1] = 0.0
                                    _tmp1[2] = 0.0
                                end )

    type Abcd
        a::Float64
        b
        c::Vector{Float64}
    end
    @test zerocode(Abcd(2., 3., [1,2 ]))  == striplinenumbers( quote
                                                    _tmp1 = cell(3)
                                                    _tmp1[1] = 0.0
                                                    _tmp1[2] = 0.0
                                                    _tmp1[3] = zeros(size(tv.c))
                                                end )

    @test zerocode([Abcd(2., 3., [1,2 ]), Abcd(2., 3., [1,2 ])])  == 
            striplinenumbers( quote
                                  _tmp1 = cell(size(tv))
                                  for i = 1:length(_tmp1)
                                      _tmp2 = cell(3)
                                      _tmp2[1] = 0.0
                                      _tmp2[2] = 0.0
                                      _tmp2[3] = zeros(size(tv[i].c))
                                      _tmp1[i] = _tmp2
                                  end
                                  _tmp1
                              end )


    @test zerocode( (2., 3., [1,2 ]) )    == striplinenumbers( quote 
                                                    _tmp1 = cell(3)
                                                    _tmp1[1] = 0.0
                                                    _tmp1[2] = 0.0
                                                    _tmp1[3] = zeros(size(tv[3]))
                                                end )


    @test zerocode( Any[2., [1,2 ], 3.] )    == striplinenumbers( quote 
                                                    _tmp1 = cell(3)
                                                    _tmp1[1] = 0.0
                                                    _tmp1[2] = zeros(size(tv[2]))
                                                    _tmp1[3] = 0.0
                                                end )


    @test zerocode( [2.+im, 3.-2im] )    == striplinenumbers( quote 
                                                        _tmp1 = cell(size(tv))
                                                        for i = 1:length(_tmp1)
                                                            _tmp2 = cell(2)
                                                            _tmp2[1] = 0.0
                                                            _tmp2[2] = 0.0
                                                            _tmp1[i] = _tmp2
                                                        end
                                                        _tmp1
                                                    end )


### tmatch (naive multiple dispatch) testing  ###
    if VERSION < v"0.4.0-dev+4319"
      tts = Any[ (Real,), 
                 (Real, Real), 
                 (Float64, Float64), 
                 (Float64, Int), 
                 (Float64, Int64), 
                 (Float64,), 
                 (Int64,), 
                 (AbstractString,),
                 (Any,),
                 (Any, Any) ]

      @test m.tmatch((Float64,), tts)          == (Float64,)
      @test m.tmatch((Int64,), tts)            == (Int64,)
      @test m.tmatch((Float64,Int64), tts)     == (Float64, Int64)
      @test m.tmatch((Float64,Int32), tts)     == (Real,Real)
      @test m.tmatch((Any, Real), tts)         == (Any,Any)
      @test m.tmatch((Float64, Int, Int), tts) == nothing
      @test m.tmatch((AbstractString,), tts)   == (AbstractString,)
      @test m.tmatch((Float32,), tts)          == (Real,)
      @test m.tmatch((Float64,Real), tts)      == (Real,Real)
      @test m.tmatch((Float64,Float64), tts)   == (Float64,Float64)
      @test m.tmatch((Float64,Vector), tts)    == (Any,Any)
      @test m.tmatch((Real,Float64), tts)      == (Real,Real)
    else
      tts = Any[ Tuple{Real}, 
                 Tuple{Real, Real}, 
                 Tuple{Float64, Float64}, 
                 Tuple{Float64, Int}, 
                 Tuple{Float64, Int64}, 
                 Tuple{Float64}, 
                 Tuple{Int64}, 
                 Tuple{AbstractString},
                 Tuple{Any},
                 Tuple{Any, Any} ]

      @test m.tmatch(Tuple{Float64}, tts)           == Tuple{Float64}
      @test m.tmatch(Tuple{Int64}, tts)             == Tuple{Int64}
      @test m.tmatch(Tuple{Float64,Int64}, tts)     == Tuple{Float64, Int64}
      @test m.tmatch(Tuple{Float64,Int32}, tts)     == Tuple{Real,Real}
      @test m.tmatch(Tuple{Any, Real}, tts)         == Tuple{Any,Any}
      @test m.tmatch(Tuple{Float64, Int, Int}, tts) == nothing
      @test m.tmatch(Tuple{AbstractString}, tts)    == Tuple{AbstractString}
      @test m.tmatch(Tuple{Float32}, tts)           == Tuple{Real}
      @test m.tmatch(Tuple{Float64,Real}, tts)      == Tuple{Real,Real}
      @test m.tmatch(Tuple{Float64,Float64}, tts)   == Tuple{Float64,Float64}
      @test m.tmatch(Tuple{Float64,Vector}, tts)    == Tuple{Any,Any}
      @test m.tmatch(Tuple{Real,Float64}, tts)      == Tuple{Real,Real}
    end

### testing conversions for functions diff  ###
  ex = quote 
    a = zeros(2)
    for i in 1:2
        a[i] = x
    end
    sum(a)
  end
  @test m.e2s(m.streamline(ex)) == "↑=→:a→↑call→:zeros→2↓↓↑for→↑=→:i→↑:→1→2↓↓" *
                               "→↑block→↑=→↑ref→:a→:i↓→:x↓↓↓↑call→:sum→:a↓"

  ex = quote
      a = zeros(2)
      c = 0
      for i in 1:2
          d = 3
          for j in 1:2
              a[i] = x
              a[i] += b[i+1]
              d = d * sin(2i)
          end
          c += d
      end
      z = c * sum(a)
      return z - sum(a)
  end
  @test m.e2s(m.streamline(ex)) == "↑=→:a→↑call→:zeros→2↓↓↑=→:c→0↓↑for→↑=→:i→↑:→1→2↓↓→↑block→" *
                               "↑=→:d→3↓→↑for→↑=→:j→↑:→1→2↓↓→↑block→↑=→↑ref→:a→:i↓→:x↓→" *
                               "↑+=→↑ref→:a→:i↓→↑ref→:b→↑call→:+→:i→1↓↓↓→↑=→:d→↑call→:*→:d→" *
                               "↑call→:sin→↑call→:*→2→:i↓↓↓↓↓↓→↑+=→:c→:d↓↓↓↑=→:z→↑call→:*→:c→" *
                               "↑call→:sum→:a↓↓↓↑return→↑call→:-→:z→↑call→:sum→:a↓↓↓"

  aex = Expr[ :(a = zeros(2)),
              striplinenumbers(:(for i = 1:2 ; a[i] = x ; end)),
              :(sum(a)) ]

  @test m.s2e("↑=→:a→↑call→:zeros→2↓↓↑for→↑=→:i→↑:→1→2↓↓" *
              "→↑block→↑=→↑ref→:a→:i↓→:x↓↓↓↑call→:sum→:a↓" ) == aex
