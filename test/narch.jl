# locations instead of symbols
# dict symbol -> loc
# loc have previous states (for mutating functions)
# deriv rules as real functions in a dedicated sub module

module A; end

module A

type Loc{T}  # regular, constant, external
    typ::DataType
    val::Any
end

typealias CLoc Loc{:constant}  # constants
typealias ELoc Loc{:external}  # external

Loc(typ, val)  = Loc{:regular}(typ,val)
Loc(val)       = Loc(typeof(val),val)
CLoc(val)      = CLoc(typeof(val),val)
ELoc(val)      = ELoc(typeof(val),val)

loctype{T}(l::Loc{T}) = T

type Op
    f::Union{Void, Function}
    asc::Vector{Loc}  # parent Loc (function arguments)
    desc::Vector{Loc} # descendant Loc (Loc modified/created by function)
end

type Graph
  locs::Vector{Loc}
  ops::Vector{Op}
  vars::Dict{Any, Loc}
end

Graph() = Graph(Vector{Loc}(), Vector{Op}(), Dict{Any, Loc}())

import Base.show
function show(io::IO, g::Graph)
  function printtable(t::Array{UTF8String,2})
    sz = maximum(map(length, t),1)
    for i in 1:size(t,1)
      for j in 1:size(t,2)
        l = length(t[i,j])
        print(io, " " ^ (sz[j]-l), t[i,j], " ")
      end
      println()
    end
  end

  slocs = Array(UTF8String, length(g.locs)+1, 5)
  slocs[1,:] = ["#", "type", "symbol(s)", "cat", "val" ]
  for (i,l) in enumerate(g.locs) # i,l = 1, g.locs[1]
    vs = keys(filter((k,v) -> v===l, g.vars))
    slocs[i+1,:] = map(string, Any[i, l.typ, join(vs, ","), loctype(l), l.val])
  end
  printtable(slocs)
  println()

  sops = Array(UTF8String, length(g.ops)+1, 3)
  sops[1,:] = ["f" "parents" "children"]
  for (i,o) in enumerate(g.ops) # i,l = 1, g.ops[1]
    ps = indexin(o.asc, g.locs)
    cs = indexin(o.desc, g.locs)
    sops[i+1,:] = map(string, Any[o.f, join(ps, ","), join(cs, ",")])
  end
  printtable(sops)
end

##########  Parameterized type to ease AST exploration  ############


include("../src2/new-tograph.jl")
include("../src2/new-tocode.jl")


a = ones(10)
a[5:end]
getindex(a, 2)
getindex(a, 5:end)  # does not work

type Z ; x ; y ; end
z = Z(1,2)

g = tograph( :( z.x ) )
show(g)

A.a
getfield(A,:a)
g = tograph( :( A.a[2] ) )
show(g)

g = tograph( :( A.a[2] = 3) )
show(g)


g = tograph( :( sin(24) ) )
show(g)

dump(:( Base.sin(24) ))
g = tograph( :( Base.sin(24) ) )
show(g)

g = tograph( :( a = 2 ; sin(a) ) )
show(g)

g = tograph( :( a = 2 ; b = a ; a+b ) )
show(g)

g = tograph( :( a = ones(2) ; b = a ; a+b ) )
show(g)

g = tograph( :( a = b = 2 ) )
show(g)

g = tograph( :( a = b = ones(2) ) )
show(g)

g = tograph( :( a = ones(2) ; a[2] ) )
show(g)

g = tograph( :( a = ones(2) ; a[2]=2. ; a[1] ) )
show(g)


################################## tocode ##############################
show(dump( :(a[b] = z) ))

show(g)
tocode(g)

isconst(:sin)
typeof(A)
isconst(:A)
typeof(Main)
isconst(:Main)

isconst(:abcd)
isconst(:Op)
isconst(:a)
a

isconst(:a)
isconst(:(3+6))

############################ testing #####################################

function fullcycle(ex; params...)
  psym  = Symbol[ e[1] for e in params]
  pvals = [ e[2] for e in params]
  env(s) = (i = findfirst(s .== psym) ; i==0 ? error("not found") : pvals[i])
  tocode(tograph(ex, env))
end

show(tograph(:( a[3] = 5 )))
type Z ; x ; y ; end
z = Z(1,2)

fullcycle( :( a[3] = 5 ; z = a[2]), a=ones(5))
fullcycle( :( z.x = 2 ; y = z.y ), z=z)
g = tograph(:( z.x = 2))
show(g)

@test fullcycle(:(a = b+6 ), b=3)      == Expr(:block, :(a = b+6) )
@test fullcycle(:(sin(y);a=3), y=1.)   == Expr(:block, :(a = 3) )
@test fullcycle(:(a += b+6), a=2, b=0) == Expr(:block, :(a = a + (b+6)) )
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


@test fullcycle(:( a[2] ))                        == Expr(:block, :( a[2]) )
@test fullcycle(:( y = a[2] ; y ), a=ones(5))     == Expr(:block, :( a[2]) )
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
