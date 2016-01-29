# locations instead of symbols
# dict symbol -> loc
# (No) : loc have previous states (for mutating functions)
# deriv rules as real functions in a dedicated sub module
# Loc at parent level only

# in module ReverseDiffSource

# check for errors
tograph( :(a = 1) )
tograph( :(a += 1) )
tograph( :(B = 1) )
tograph( :(B[2] = 1) )
tograph( :(Main.B[2] = 1) )
tograph( :(C.x = 1) )
tograph( :(A.C.x = 1) )

# check for loc type propagation
show(tograph( :(x = a) ))  # x regular (copy)
show(tograph( :(x = B[2]) ))   # x regular (copy)
show(tograph( :(x = C.y) ))   # x regular (copy)
show(tograph( :(X = B) ))  # x external (ref)
show(tograph( :(X = B[1:2]) )) # x external => should be regular (getindex is a copy)
show(tograph( :(X = C) ))  # x external (ref)

# other checks
show(tograph( :( sin(24) ) ) )
show(tograph( :( e^x ) ) ) # x undefined
show(tograph( :( e^a ) ) )
show(tograph( :( Base.sin(24) ) ) )

show(tograph( :( Main.A.sin(24) ) ) )
show(tograph( :( x = 2 ; sin(x) ) ) )
show(tograph( :( x = 2 ; y = x ; x+y ) ) )
show(tograph( :( X = ones(2) ; Y = X ; X+Y ) ) )
show(tograph( :( x = y = 2 ) ) )
show(tograph( :( X = Y = ones(2) ) ) )
show(tograph( :( X = ones(2) ; X[2] ) ) )
show(tograph( :( X = ones(2) ; X[2]=a ; X[1] ) ) )

show(tograph( :( C.x ) ) )
show(tograph( :( A.B[2] ) ) )
show(tograph( :( B[2] ) ) )
show(tograph( :( X=copy(B) ; X[2] = 3) ) )

module A
  using ReverseDiffSource
  a = 2
  show(ReverseDiffSource.tograph( :( x = y = a) ) )
end

module A
  using ReverseDiffSource
  a = zeros(2)
  show(ReverseDiffSource.tograph( :( x = y = a) ) )
end


############################ testing #####################################

@test fullcycle(:( x = 5 ; y = a+5 ; z = cos(y) )) == :(cos(a+5))
@test fullcycle(:( x = 5 ; y = x+5 ; z = cos(y) )) == :(cos(5+5))

@test fullcycle(:( x = 5 + 6 + 5)) == :((5+6)+5)
@test fullcycle(:( x = a * b * B)) == :((a*b)*B)

@test fullcycle(:( x = b+6 ))       == :(b+6)
# @test fullcycle(:( x = b+6 ),     keep_var=[:x]) == :(x=b+6)
# @test fullcycle(:( x = y = b+6 ), keep_var=[:x]) == :(x=b+6)
# @test fullcycle(:( x = y = b+6 ), keep_var=[:y]) == :(y=b+6)
# @test fullcycle(:( x = 3.; y = b+6 ), keep_var=[:x]) == :(y=b+6)


@test fullcycle(:(sin(b); x=3))     == :(3)

@test fullcycle(:(x = b+6; x+=1))   == :((b+6)+1)
@test fullcycle(:(x = 1; x -= b+6)) == :(1 - (b+6))
@test fullcycle(:(x = a; x *= b+6)) == :(a * (b+6))
@test fullcycle(:(x = b'))          == :( b')
@test fullcycle(:(x = [1,2]))       == :( [1,2])
@test fullcycle(:(x = 4:5 ))        == :( 4:5)

@test fullcycle(:(x = b+4+5))       == :((b+4)+5)
@test fullcycle(:(x = b+0))         == :(b)

# @test fullcycle(:(x = b*0))         == :(0)
@test fullcycle(:(x = b*1))         == :(b)
# @test fullcycle(:(x = b*(0.5+0.5))) == :(b)
@test fullcycle(:(x = b/1))         == :(b)

@test fullcycle(:(5))                          == :(5)
@test fullcycle(:(x = 2 ; y = 2 ; x:y))        == :(2:2)
@test fullcycle(:(x = 2 ; x))                  == :(2)
@test fullcycle(:(x = a ; y = 0 ))             == :(0)
@test fullcycle(:(x = a ; x))                  == :(a)
@test fullcycle(:(y = a ; y))                  == :(a)

@test fullcycle(:(x = a ; y = a ; y))          == :(a)
@test fullcycle(:(x = a ; y = a ; x + y))      == :(a+a)
@test fullcycle(:(x = a ; y = x ; z = y ; y))  == :(a)

@test fullcycle(:(x = B ; y = B ; y))          == :(B)
@test fullcycle(:(x = B ; y = B ; x + y))      == :(B+B)
@test fullcycle(:(x = B ; y = x ; z = y ; y))  == :(B)


@test fullcycle(:( B[2] ))                      == :( B[2] )
@test fullcycle(:(y = B[2]; y ))                == :( B[2] )
@test fullcycle(:(y = B[2]; y[1] ))             == :( B[2][1] )

@test fullcycle(:(y = zeros(B); y[1] = B[2]; y[1] )) ==
      cleanup(:(_tmp1 = zeros(B); _tmp1[1] = B[2]; _tmp1[1]))
@test fullcycle(:(y = B+1 ; y[2]+y[1] ))        == :(_tmp1 = B+1 ; _tmp1[2]+_tmp1[1])
@test fullcycle(:(Y = zeros(B); Y[2]=a ; a ))   == :(a)
@test fullcycle(:(Y = copy(B); Y[2]=a ; Y[1] )) == cleanup(:(_tmp1=copy(B); _tmp1[2]=a ; _tmp1[1]))

@test fullcycle(:( x = a ; Y = copy(B) ; Y[2] = x; 1 + Y[1] ))  ==
      cleanup(:(_tmp1 = copy(B) ; _tmp1[2] = a; 1 + _tmp1[1]))

@test fullcycle(:( B[1] + B[2] ))   == :(B[1] + B[2])
@test fullcycle(:( B[1:2] ))        == :( B[1:2] )

# @test fullcycle(:( X = copy(B) ; X[1:2] = X[1:2] ))  ==
#       cleanup( :( X = copy(B) ; X[1:2] = X[1:2] ) )
@test fullcycle(:( X = copy(D) ; X[1:2,3] = a ))  ==
      cleanup( :( _tmp2 = copy(D) ; _tmp2[1:2,3] = a ; _tmp2) )
@test fullcycle(:( X = copy(D) ; X[1:2,2] = D[1:2,3] )) ==
      cleanup( :( _tmp2 = copy(D) ; _tmp2[1:2,2] = D[1:2,3] ; _tmp2) )

@test fullcycle(:( B[:] ))                == :( B[1:length(B)] )
@test fullcycle(:( D[:] ))                == :( D[1:length(D)] )
@test fullcycle(:( B[1:2] ))              == :( B[1:2] )
@test fullcycle(:( B[1:end-1] ))          == :( B[1:length(B)-1] )
@test fullcycle(:( B[end-1:end] ))        == cleanup(:(_tmp1=length(B);B[_tmp1-1:_tmp1] ))

@test fullcycle(:( D[4:11] ))             == :( D[4:11] )
@test fullcycle(:( D[:] ))                == :( D[1:length(D)] )
@test fullcycle(:( D[3:end-2] ))          == :( D[3:length(D)-2] )
@test fullcycle(:( D[2,:,2:3] ))          == :( D[2,1:size(D,2),2:3] )
@test fullcycle(:( D[1:end,3,end-2:end-1] )) ==
      cleanup(:(_tmp1=size(D,3) ; D[1:size(D,1), 3, _tmp1-2:_tmp1-1] ))


@test fullcycle(:( C.x ))                       == :( C.x )
@test fullcycle(:( y=C.x ))                     == :( C.x )
@test fullcycle(:( y=C.x + 1 ; C.y + C.x ))     == :(C.y + C.x)
@test fullcycle(:( C.x + C.y ))                 == :( C.x + C.y )
# @test fullcycle(:( X=Z(0,0); X.x=a ))           ==
#       cleanup(:(X=ReverseDiffSource.Z(0,0);X.x=a))
# @test fullcycle(:( X=Z(0,0); X.x=a; y=X.y; y )) ==
#       cleanup(:(X=ReverseDiffSource.Z(0,0);X.x=a; X.y))
# @test fullcycle(:( X=Z(1,1); X.x = a; 1+X.y ))  ==
#       cleanup(:(X=ReverseDiffSource.Z(1,1);X.x=a; 1+X.y))

# z = Z(0,0).x = a

###  test evalconstants, simplify
ex = quote
    x = 3
    y = x * a * 1
    y2 = (x * a) + 0 + 3
    x += 1
    y3 = x * a
    y + y2 + y3 + 12
end
fullcycle(ex)
exout = :( _tmp1 = *(x,a) ; +(_tmp1,+(+(_tmp1,3),+(*(+(x,1),a),12))) )
@test fullcycle(ex) == exout

###  test respect of allocations
ex = quote
    x=zeros(2)
    x[2] = a
    sum(x)
end
exout = quote
    _tmp1 = zeros(2)
    _tmp1[2] = a
    sum(_tmp1)
end
@test fullcycle(ex) == cleanup(exout)


ex = quote
    z = zeros(5)
    x = sum(z)
    z[2] = 1
    y = sum(z)
    x + y
end
exout = quote
    z = zeros(5)
    x = sum(z)
    z[2] = 1
    x + sum(z)
end
@test fullcycle(ex) == cleanup(exout)
# FIXME

############## if blocks  ###################
ex = quote
  x = 0.
  if a > 2
    y = sin(b)
    x = a*y
  else
    z = cos(a)
    x = a^z
  end
  x
end
  exout = quote
    if >(a,2)
      _tmp1 = a*sin(b)
    else
      _tmp1 = a^cos(a)
    end
    _tmp1
  end
  @test fullcycle(ex) == cleanup(exout)

ex = quote
  x = 0.
  if a > 2
    x = 3
    y = sin(b)
  else
    z = cos(a)
    x = 5
  end
  x
end
  exout = quote
    if >(a,2)
      _tmp1 = 3
    else
      _tmp1 = 5
    end
    _tmp1
  end
  @test fullcycle(ex) == cleanup(exout)

ex = quote
  if a > 2
    x = 3
  else
    x = 5
  end
  x
end
  exout = quote
    if >(a,2)
      _tmp1 = 3
    else
      _tmp1 = 5
    end
    _tmp1
  end
  @test fullcycle(ex) == cleanup(exout)

ex = quote
  x = 0.
  if a > 2
    x += 3
  end
  x
end
  exout = quote
    if >(a,2)
      _tmp1 = 3.
    else
      _tmp1 = 0.
    end
    _tmp1
  end
  @test fullcycle(ex) == cleanup(exout)


ex = quote
  x = a
  if a > 2
    x += 3
  end
  x
end
  exout = quote
    if >(a,2)
      _tmp1 = 3.
    else
      _tmp1 = 0.
    end
    _tmp1
  end
  fullcycle(ex)
@test fullcycle(ex) == cleanup(exout)

g = tograph(ex)
simplify!(g)
tocode(g)
show(g)
keep = Set{Loc}([ g.block.symbols[s] for s in [EXIT_SYM;]])
prune!(g, keep)
splitnary!(g)
fusecopies!(g)
removerightneutral!(g)
removeleftneutral!(g)
prune!(g, keep)
show(tocode(g))

show(backtrace())
catch_backtrace()

Base.show_backtrace(STDOUT, catch_backtrace())
Base.show_backtrace(STDOUT, backtrace())


################# for loops  ################
ex = quote
  x = 0.
  for i in 1:3
    x = x + i
  end
  x+2
end
  fullcycle(ex)

g = tograph(ex)
show(g)
tocode(g)
simplify!(g)
show(tocode(g))

o = g.block.ops[1]
isfusable(o.asc[1], o.desc[1], Walk(o,g))

keeps2 = intersect([EXIT_SYM;], keys(g.block.symbols))
keep = Set{Loc}([ g.block.symbols[s] for s in keeps2])

prune!(g, keep)
splitnary!(g)

fusecopies!(g)
removerightneutral!(g)
removeleftneutral!(g)
prune!(g, keep)






ex = quote
  x = 0.
  y = 12.
  for i in 1:10
    x = x + i
    y = y + i*i
  end
  x + 3 + y
end
  fullcycle(ex)

g = tograph(ex)
show(g)
tocode(g)
simplify!(g)

keeps2 = intersect([EXIT_SYM;], keys(g.block.symbols))
keep = Set{Loc}([ g.block.symbols[s] for s in keeps2])
prune!(g, keep)
splitnary!(g)
fusecopies!(g)
removerightneutral!(g)
removeleftneutral!(g)
prune!(g, keep)





ex = quote
  x = a
  for i in 1:10
    x = x + i
  end
  x + 3
end
  fullcycle(ex)

ex = quote
  x = a
  for i in 1:10
    y = x + i*a
  end
  x
end
  fullcycle(ex)



g = tograph(ex)
flatops(g)
prune!(g, [:_result;])
splitnary!(g)
# fusecopies!(g)
show(g)
show(tocode(g))

collect(g.block.symbols)

splitnary!(g)
fusecopies!(g)
removerightneutral!(g)
removeleftneutral!(g)
prune!(g, keep)

g = tograph(:(x=a; x))
prune!(g, [:_result;])
splitnary!(g)
fusecopies!(g)
show(g)
show(tocode(g))


ex = quote
  x = 0.
  for i in 1:10
  	x += i
  end
  x
end
  fullcycle(ex)

g = tograph(ex)
prune!(g, [:_result;])
splitnary!(g)
fusecopies!(g)
removerightneutral!(g)
removeleftneutral!(g)
prune!(g, keep)

show(g)
simplify!(g)
tocode(g)


ex = quote
  N = 12
  X = Array(Float64,N)
  for i in 1:N
    X[i] = i*i
  end
  sum(X)
end
  fullcycle(ex)
show(tograph(ex))

ex = quote
  N = 12
  X = Array(Float64,N)
  y = 0
  for i in 1:N
    y = 13.
    X[i] = i*i
  end
  sum(X)
end
  fullcycle(ex)

g = tograph(ex)
prune!(g, [:_result;])
splitnary!(g)
fusecopies!(g)
removerightneutral!(g)
removeleftneutral!(g)

prune!(g, [:_result;])
show(g)
allops(g)

g.block.ops[4]
g.block.ops[4].f.typ

simplify!(g)
tocode(g)
show(g)

ex = quote
  N = 12
  X = Array(Float64,N,N)
  for i in 1:N
    for j in 1:N
      X[i,j] = i+j
    end
  end
  sum(X)
end
  fullcycle(ex)
g = tograph(ex)
show(g)
tocode(g)
prune!(g, [EXIT_SYM;])

simplify!(g)
show(g)


ex = quote
  N = 12
  X = Array(Float64,N,N)
  for i in 1:N
    x = 4.
    12.
  end
  sum(X)
end
  fullcycle(ex)
