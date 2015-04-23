#################################################################
## expression to graph testing
#################################################################

function striplinenumbers(ex::Expr)
    args = Any[]
    for a in ex.args
        isa(a, LineNumberNode) && continue
        isa(a, Expr) && a.head==:line && continue
        push!(args, isa(a,Expr) ? striplinenumbers(a) : a )
    end
    Expr(ex.head, args...)
end 

### for loop #1
ex = quote
    a=zeros(10)
    for i in 1:10
        t = x+z
        a[i] = b[i]+t
    end
end

g = m.tograph(ex);
@test sort(collect(values(g.exti.kv))) == [:b, :x, :z]
@test sort(collect(values(g.seti.kv))) == [:a]
@test length(g.seto.kv) == 0
@test length(g.exto.kv) == 0

exout = striplinenumbers(quote 
    _tmp1 = zeros(10)
    for i = 1:10
        t = x + z
        _tmp1[i] = b[i] + t
    end
    a = _tmp1
end)
m.resetvar()
@test m.tocode(g) == exout   


### for loop #2
ex = quote
    a=zeros(10)
    z = 12 
    for i in 1:10
        t = x+z
        for j in 1:10
            u = t+z+v
            a[i] = b[i]+u
        end
    end
end
g = m.tograph(ex)

@test sort(collect(values(g.exti.kv))) == [:b, :v, :x]
@test sort(collect(values(g.seti.kv))) == [:a, :z]
@test length(g.seto.kv) == 0
@test length(g.exto.kv) == 0

g2 = g.nodes[7].main[2]  # first level loop
@test sort(collect(values(g2.exti.kv))) == [:a, :b, :i, :v, :x, :z]
@test sort(collect(values(g2.seti.kv))) == [:a, :t]
@test sort(collect(values(g2.exto.kv))) == [:a, :b, :v, :x, :z]
@test sort(collect(values(g2.seto.kv))) == [:a]

g3 = g2.nodes[7].main[2] # second level loop
@test sort(collect(values(g3.exti.kv))) == [:a, :b, :i, :t, :v, :z]
@test sort(collect(values(g3.seti.kv))) == [:a, :u]
@test sort(collect(values(g3.exto.kv))) == [:a, :b, :i, :t, :v, :z]
@test sort(collect(values(g3.seto.kv))) == [:a]

m.resetvar()
exout = striplinenumbers(quote         
        z = 12
        _tmp1 = zeros(10)
        for i = 1:10
            t = x + z
            for j = 1:10
                u = t + z + v
                _tmp1[i] = b[i] + u
            end
        end
        a = _tmp1
    end)
exout2 = striplinenumbers(quote  # alternate possible result       
        _tmp1 = zeros(10)
        z = 12
        for i = 1:10
            t = x + z
            for j = 1:10
                u = t + z + v
                _tmp1[i] = b[i] + u
            end
        end
        a = _tmp1
    end)
@test m.tocode(g) == exout || m.tocode(g) == exout2




#################################################################
## full cycle  : tograph -> splitnary -> simplify -> prune -> tocode 
#################################################################
function fullcycle(ex) # ex = :( y = a[2] ; x = 2 ; y )
    g = m.tograph(ex)

    length(g.seti.kv) == 0 && error("nothing defined here")

    # isolate a single variable of interest
    if haskey( g.seti.vk, nothing) # last statement has priority
        lastnode = g.seti.vk[nothing]
    else # find last evaluated otherwise
        m.evalsort!(g)
        nvars = collect(keys(g.seti))
        lastnode = g.nodes[ maximum(indexin(nvars, g.nodes)) ]
    end
    # lastnode = (g.nodes[1], :out)
    # for n in reverse(g.nodes)
    #     haskey(g.seti, n) || continue 
    #     lastnode = n
    #     break
    # end

    sym = g.seti[lastnode]
    # g.seti = m.BiDict{m.ExNode,Any}([lastnode], [ sym==nothing ? :out : sym ])
    g.seti = m.BiDict{m.ExNode,Any}([lastnode], [ sym ])

    g |> m.splitnary! |> m.simplify! |> m.prune!
    m.simplify!(g)
    m.prune!(g)

    m.resetvar()
    m.tocode(g)
end



@test fullcycle(:(a = b+6 ))        == Expr(:block, :(a = b+6) ) 
@test fullcycle(:(sin(y);a=3))      == Expr(:block, :(a = 3) )
@test fullcycle(:(a += b+6))        == Expr(:block, :(a = a + (b+6)) )
@test fullcycle(:(a -= b+6))        == Expr(:block, :(a = a - (b+6)) )
@test fullcycle(:(a *= b+6))        == Expr(:block, :(a = a * (b+6)) )
@test fullcycle(:(a = b'))          == Expr(:block, :(a = b') )
@test fullcycle(:(a = [1,2]))       == Expr(:block, :(a = [1,2]) )
@test fullcycle(:(a = 4:5 ))        == Expr(:block, :(a = 4:5) )

@test fullcycle(:(a = b+4+5))       == Expr(:block, :(a = b+9) )
@test fullcycle(:(a = b+0))         == Expr(:block, :(a = b) )    
@test fullcycle(:(a = b*0))         == Expr(:block, :(a = 0) ) 
@test fullcycle(:(a = b*1))         == Expr(:block, :(a = b) )   
@test fullcycle(:(a = b*(0.5+0.5))) == Expr(:block, :(a = b) )   
@test fullcycle(:(a = b/1))         == Expr(:block, :(a = b) )   

@test fullcycle(:(5))                          == Expr(:block, :(5) )
@test fullcycle(:(a = 2 ; b = 2 ; a:b))        == Expr(:block, :(2:2) )
@test fullcycle(:(a = 2 ; a))                  == Expr(:block, :(2) )
@test fullcycle(:(a = x ; b = a ))             == Expr(:block, :( b = x ) )
@test fullcycle(:(a = x ; b = a ; a + b))      == Expr(:block, :( x+x ) )
@test fullcycle(:(a = x ; b = a ; c = b ; b))  == Expr(:block, :( x ) )


@test fullcycle(:( a[2] ))                        == Expr(:block, :( a[2]) )
@test fullcycle(:( y = a[2] ; y ))                == Expr(:block, :( a[2]) )
@test fullcycle(:( y = a[2] ; y[1] ))             == Expr(:block, :( a[2][1]) )
@test fullcycle(:( y[1] = a[2] ; y[1] ))          == :(y[1] = a[2]; y[1])
@test fullcycle(:( y = a+1 ; y[2]+y[1] ))         == :(_tmp1 = a+1 ; _tmp1[2]+_tmp1[1])
@test fullcycle(:( a[2] = x ; a[3] ))             == :(a[2] = x ; a[3])
@test fullcycle(:( a[2] = x ; y=a[3] ; y ))       == :(a[2] = x ; a[3])
@test fullcycle(:( b = a ; b[2] = x; 1 + b[2] ))  == :(a[2] = x ; 1+a[2])
@test fullcycle(:( b = a ; b[2] = x; 1 + b[1] ))  == :(a[2] = x ; 1+a[1])
@test fullcycle(:( a[1] + a[2] ))                 == Expr(:block, :( a[1] + a[2]) )
@test fullcycle(:( a[1:2] ))                      == Expr(:block, :( a[1:2]) )
@test fullcycle(:( a = x ; a[1:2] = a[1:2] ))     == Expr(:block, :( a = x ) )
@test fullcycle(:( a = x ; a[1:2,3] = a[1:2,3] )) == Expr(:block, :( a = x ) )
@test fullcycle(:( a = x ; a[1:2,3] = a[1:2,4] )) == :( _tmp1 = 1:2 ; x[_tmp1,3] = x[_tmp1,4])

@test fullcycle(:( x[:] ))               == Expr(:block, :( x[1:length(x)] ) )
@test fullcycle(:( x[a+b, c:d] ))        == Expr(:block, :( x[a + b,c:d] ) )
@test fullcycle(:( x[1:4] ))             == Expr(:block, :( x[1:4] ) )
@test fullcycle(:( x[1:end] ))           == Expr(:block, :( x[1:length(x)]) )
@test fullcycle(:( a[1:end, :, 10:15] )) == Expr(:block, :( a[1:size(a,1),1:size(a,2),10:15]) )

@test fullcycle(:( a.x ))                     == Expr(:block, :( a.x) )
@test fullcycle(:( y = a.x ))                 == Expr(:block, :(y = a.x) )
@test fullcycle(:( y = a.x + 1 ; y.b + y.c )) == :(_tmp1 = +(a.x,1) ; _tmp1.b+_tmp1.c)
@test fullcycle(:( a.x = x ; a[3] ))          == :(a.x = x; a[3])
@test fullcycle(:( a.x = x ; y = a.y ; y ))   == :(a.x = x ; a.y )
@test fullcycle(:( b = a; b.x = x; 1 + b.y )) == :(a.x = x ; 1+a.y )
@test fullcycle(:( a.x + a.y ))               == Expr(:block, :( a.x+a.y ) )

@test fullcycle(:( a = b.f[i]))        == Expr(:block, :(a = b.f[i]) )
@test fullcycle(:( a = b[j].f[i]))     == Expr(:block, :(a = b[j].f[i]) )



ex = quote
    for i in 1:10
        a[i] = b[i]+2
    end
    c=3
end
exout = Expr(:block, :( c=3 ))
@test fullcycle(ex) == striplinenumbers(exout)


ex = quote
    a=0
    for i in 1:10
        a += x
    end
end
exout = quote
    _tmp1=0
    for i in 1:10
        _tmp1 = _tmp1+x
    end
    a=_tmp1
end
@test fullcycle(ex) == striplinenumbers(exout)


ex = quote
    a=zeros(10)
    for i in 1:10
        a[i] = b[i]+2
    end
end 
exout = quote 
    _tmp1 = zeros(10)
    for i = 1:10
        _tmp1[i] = b[i] + 2
    end
    a = _tmp1
end
@test fullcycle(ex) == striplinenumbers(exout)



ex = quote
    a=zeros(10)
    z=sum(a)
    for i in 1:10
        a[i] = b[i]+2
    end
end 
exout = quote 
    _tmp1 = zeros(10)
    for i = 1:10
        _tmp1[i] = b[i] + 2
    end
    a = _tmp1
end
@test fullcycle(ex) == striplinenumbers(exout)


ex = quote
    a=zeros(10)
    for i in 1:10
        a[i] = b[i]+2
    end 
    z=sum(a) 
end
exout = quote 
    _tmp1 = zeros(10)
    for i = 1:10
        _tmp1[i] = b[i] + 2
    end
    z=sum(_tmp1)
end
@test fullcycle(ex) == striplinenumbers(exout)


ex = quote
    a=zeros(10+6)
    for i in 1:10
        t = 4+3+2
        a[i] += b[i]+t
    end
    z=sum(a)
end
exout = quote 
    _tmp1 = zeros(16)
    for i = 1:10
        _tmp1[i] = _tmp1[i] + (b[i] + 9)
    end
    z=sum(_tmp1)
end
@test fullcycle(ex) == striplinenumbers(exout) 



###  test evalconstants, simplify
ex = quote
    y = x * a * 1
    y2 = (x * a) + 0 + 3
    x += 1
    y3 = x * a
    y + y2 + y3 + 12
end
exout = :( _tmp1 = *(x,a) ; +(_tmp1,+(+(_tmp1,3),+(*(+(x,1),a),12))) )
@test fullcycle(ex) == exout 

###  test respect of allocations
ex = quote
    a=zeros(2)
    a[2] = x 
    sum(a)
end
exout = quote 
    _tmp1 = zeros(2)
    _tmp1[2] = x
    sum(_tmp1)
end
@test fullcycle(ex) == striplinenumbers(exout)


ex = quote
    a = zeros(5)
    x = sum(a)
    a[2] = 1
    y = sum(a)
    x + y
end
exout = quote 
    _tmp1 = zeros(5)
    _tmp2 = sum(_tmp1)
    _tmp1[2] = 1
    _tmp2 + sum(_tmp1)
end

@test fullcycle(ex) == striplinenumbers(exout)

