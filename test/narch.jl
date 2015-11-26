# locations instead of symbols
# dict symbol -> loc
# (No) : loc have previous states (for mutating functions)
# deriv rules as real functions in a dedicated sub module
# Loc at parent level only


module A; end

module A

include("../src2/new-main.jl")
include("../src2/new-tograph.jl")
include("../src2/new-tocode.jl")
include("../src2/new-simplify.jl")

# testing vars
a, b = 2, 2.1
B = ones(2)
type Z ; x ; y ; end
C = Z(1,2)

# check for errors
tograph( :(a = 1) )
tograph( :(a += 1) )
tograph( :(B = 1) )
tograph( :(B[2] = 1) )
tograph( :(A.B[2] = 1) )
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
show(g)

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

############################ testing #####################################

function fullcycle(ex; params...)
  psym  = Symbol[ e[1] for e in params]
  pvals = [ e[2] for e in params]
  m = current_module()
  function env(s::Symbol)
    i = findfirst(s .== psym)
    i != 0 && return (true, pvals[i], false, nothing)

    isdefined(m, s) || return (false, nothing, nothing, nothing)

    (true, eval(m,s), isconst(m,s), nothing)
  end

  resetvar()
  g2 =
  tocode(simplify!(tograph(ex, env)))
end

g = tograph(:( x = 5 ; y = x+5 ; z = cos(y) ))
show(g)

ispivot(g.ops[1], 1)
tocode(g)
fullcycle(:( x = 5 ; y = x+5 ; z = cos(y) ))
fullcycle(:( x = 5 ; y = a+5 ; z = cos(y) ))

fullcycle(:( x = 5 + 6 + 5))
fullcycle(:( x = a * b * B))

fullcycle( :( a[3] = 5 ; z = a[2]) )
fullcycle( :( z.x = 2 ; y = z.y ), z=z)
g = tograph(:( z.x = 2))
show(g)

@test fullcycle(:( x = b+6 ))       == Expr(:block, :(a = b+6) )
@test fullcycle(:(sin(b); x=3))     == Expr(:block, :(a = 3) )
@test fullcycle(:(a += b+6))        == Expr(:block, :(a = a + (b+6)) )
show(tograph(:(a += b+6)))

@test fullcycle(:(a -= b+6))        == Expr(:block, :(a = a - (b+6)) )
@test fullcycle(:(a *= b+6))        == Expr(:block, :(a = a * (b+6)) )
@test fullcycle(:(a = b'))          == Expr(:block, :(a = b') )
@test fullcycle(:(a = [1,2]))       == Expr(:block, :(a = [1,2]) )
@test fullcycle(:(a = 4:5 ))        == Expr(:block, :(a = 4:5) )

@test fullcycle(:(a = b+4+5),b=1)       == Expr(:block, :(a = b+9) )
@test fullcycle(:(a = b+0))         == Expr(:block, :(a = b) )
@test fullcycle(:(a = b*0))         == Expr(:block, :(a = 0) )
@test fullcycle(:(a = b*1))         == Expr(:block, :(a = b) )
@test fullcycle(:(a = b*(0.5+0.5)), b=1) == Expr(:block, :(a = b) )
@test fullcycle(:(a = b/1))         == Expr(:block, :(a = b) )

@test fullcycle(:(5))                          == Expr(:block, :(5) )
@test fullcycle(:(a = 2 ; b = 2 ; a:b))        == Expr(:block, :(2:2) )
@test fullcycle(:(a = 2 ; a))                  == Expr(:block, :(2) )
@test fullcycle(:(a = x ; b = a ),x=0)         == Expr(:block, :( b = x ) )
@test fullcycle(:(a = x ; b = a ; a + b),x=0)  == Expr(:block, :( x+x ) )
@test fullcycle(:(a = x ; b = a ; c = b ; b))  == Expr(:block, :( x ) )

@test fullcycle(:( B[2] ))                        == Expr(:block, :( a[2]) )
@test fullcycle(:( y = B[2] ; y ))                == Expr(:block, :( a[2]) )
@test fullcycle(:( y = B[2] ; y[1] ))             == Expr(:block, :( a[2][1]) )
@test fullcycle(:( y[1] = a[2] ; y[1] ))          == :(y[1] = a[2]; y[1])
@test fullcycle(:( y = a+1 ; y[2]+y[1] ))         == :(_tmp1 = a+1 ; _tmp1[2]+_tmp1[1])

@test fullcycle(:( B[2] = a ; a[3] ))             == :(a[2] = x ; a[3])
@test fullcycle(:( A.B[2] = a ; a[3] ))             == :(a[2] = x ; a[3])
isDot(:(A.B))
isDot(:(A.B[2]))
isRef(:(B[2]))
isDot(:(A.B[2]))

@test fullcycle(:( a[2] = x ; y=a[3] ; y ))       == :(a[2] = x ; a[3])
@test fullcycle(:( b = a ; b[2] = x; 1 + b[2] ))  == :(a[2] = x ; 1+a[2])
@test fullcycle(:( b = a ; b[2] = x; 1 + b[1] ))  == :(a[2] = x ; 1+a[1])
@test fullcycle(:( a[1] + a[2] ))                 == Expr(:block, :( a[1] + a[2]) )
@test fullcycle(:( a[1:2] ))                      == Expr(:block, :( a[1:2]) )
@test fullcycle(:( a = x ; a[1:2] = a[1:2] ))     == Expr(:block, :( a = x ) )
@test fullcycle(:( a = x ; a[1:2,3] = a[1:2,3] )) == Expr(:block, :( a = x ) )
@test fullcycle(:( a = x ; a[1:2,3] = a[1:2,4] ), x=ones(4,4)) == :( _tmp1 = 1:2 ; x[_tmp1,3] = x[_tmp1,4])

@test fullcycle(:( x[:] ))               == Expr(:block, :( x[1:length(x)] ) )
@test fullcycle(:( x[a+b, c:d] ))        == Expr(:block, :( x[a + b,c:d] ) )
@test fullcycle(:( x[1:4] ))             == Expr(:block, :( x[1:4] ) )
@test fullcycle(:( x[1:end] ), x=ones(5))           == Expr(:block, :( x[1:length(x)]) )
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






end # of module A
