# locations instead of symbols
# dict symbol -> loc
# loc have previous states (for mutating functions)
# deriv rules as real functions in a dedicated sub module

module A; end

module A

type Loc{T}  # regular, constant, external
    typ::DataType
    val::Any
    # ext::Bool   # external variable ?
    # cst::Bool   # constant ?
end

Loc(typ, val)  = Loc{:regular}(typ,val)
Loc(val)       = Loc(typeof(val),val)
CLoc(typ, val) = Loc{:constant}(typ,val)
CLoc(val)      = CLoc(typeof(val),val)
ELoc(typ, val) = Loc{:external}(typ,val)
ELoc(val)      = ELoc(typeof(val),val)

loctype{T}(l::Loc{T}) = T

loctype(ELoc(12))


type Op
    f::Union{Void, Function}
    asc::Vector{Loc}  # parent Loc (function arguments)
    desc::Vector{Loc} # descendant Loc (Loc modified/created by function)
end

# constloc(val) = Loc(typeof(val), val, Vector{Loc}(), 0, nothing)
# constloc(12.0)

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
type ExH{H}
  head::Symbol
  args::Vector
  typ::Any
end
toExH(ex::Expr) = ExH{ex.head}(ex.head, ex.args, ex.typ)
toExpr(ex::ExH) = Expr(ex.head, ex.args...)

typealias ExEqual    ExH{:(=)}
typealias ExDColon   ExH{:(::)}
typealias ExColon    ExH{:(:)}
typealias ExPEqual   ExH{:(+=)}
typealias ExMEqual   ExH{:(-=)}
typealias ExTEqual   ExH{:(*=)}
typealias ExTrans    ExH{symbol("'")}
typealias ExCall     ExH{:call}
typealias ExBlock    ExH{:block}
typealias ExLine     ExH{:line}
typealias ExVcat     ExH{:vcat}
typealias ExVect     ExH{:vect}
typealias ExCell1d   ExH{:cell1d}
typealias ExCell     ExH{:cell1d}
typealias ExFor      ExH{:for}
typealias ExRef      ExH{:ref}
typealias ExIf       ExH{:if}
typealias ExComp     ExH{:comparison}
typealias ExDot      ExH{:.}
typealias ExTuple    ExH{:tuple}
typealias ExReturn   ExH{:return}
typealias ExBody     ExH{:body}
typealias ExQuote    ExH{:QuoteNode}

isSymbol(ex)   = isa(ex, Symbol)
isDot(ex)      = isa(ex, Expr) && ex.head == :.   && isa(ex.args[1], Symbol)
isRef(ex)      = isa(ex, Expr) && ex.head == :ref && isa(ex.args[1], Symbol)

function modenv(s::Symbol, m::Module=A)
  isdefined(m, s) && return eval(m, s)
  error("[tograph] undefined symbol $s")
end

function tograph(s, env::Function=modenv)  # env = modenv
  # ex = :( z.x )
  #  this level graph
  g = Graph()
  add!(l::Loc) = (push!(g.locs,l) ; l)
  add!(o::Op)  = (push!(g.ops, o) ; o)

  # AST exploration functions
  explore(ex::Any)       = error("[tograph] unmanaged type $ex ($(typeof(ex)))")
  explore(ex::Expr)      = explore(toExH(ex))
  explore(ex::ExH)       = error("[tograph] unmanaged expr type $(ex.head) in ($ex)")

  explore(ex::ExLine)         = nothing     # remove line info
  explore(ex::LineNumberNode) = nothing     # remove line info

  explore(ex::QuoteNode) = add!(CLoc(ex))  # consider as constant

  explore(ex::ExReturn)  = explore(ex.args[1]) # focus on returned statement

  explore(ex::ExVcat)    = explore(Expr(:call, :vcat, ex.args...) )  # translate to vcat() call, and explore
  explore(ex::ExVect)    = explore(Expr(:call, :vcat, ex.args...) )  # translate to vcat() call, and explore
  explore(ex::ExCell1d)  = explore(Expr(:call, :(Base.cell_1d), ex.args...) )  # translate to cell_1d() call, and explore
  explore(ex::ExTrans)   = explore(Expr(:call, :transpose, ex.args[1]) )  # translate to transpose() and explore
  explore(ex::ExColon)   = explore(Expr(:call, :colon, ex.args...) )  # translate to colon() and explore
  explore(ex::ExTuple)   = explore(Expr(:call, :tuple, ex.args...) )  # translate to tuple() and explore

  explore(ex::ExPEqual)  = (args = ex.args ; explore( Expr(:(=), args[1], Expr(:call, :+, args[1], args[2])) ) )
  explore(ex::ExMEqual)  = (args = ex.args ; explore( Expr(:(=), args[1], Expr(:call, :-, args[1], args[2])) ) )
  explore(ex::ExTEqual)  = (args = ex.args ; explore( Expr(:(=), args[1], Expr(:call, :*, args[1], args[2])) ) )

  explore(ex::Real)      = add!(CLoc(ex))

  explore(ex::ExBlock)   = map( explore, ex.args )[end]
  explore(ex::ExBody)    = map( explore, ex.args )[end]

  # explore(ex::ExComp)    = addnode!(g, NComp(ex.args[2], [explore(ex.args[1]), explore(ex.args[3])]))

  # explore(ex::ExDot)     = addnode!(g, NDot(ex.args[2],     [ explore(ex.args[1]) ]))
  explore(ex::ExDot)     = explore(Expr(:call, :getfield, ex.args...))
  explore(ex::ExRef)     = explore(Expr(:call, :getindex, ex.args...))

  # ex = Expr(:call, :getfield, ex.args...)

  function explore(ex::Symbol)
      haskey(g.vars, ex) && return g.vars[ex]

      val = env(ex)
      nloc = ELoc(val)
      g.vars[ex] = nloc
      add!(nloc)  # return Loc
  end

  function explore(ex::ExCall) # TODO :  treat func that return several values
      sf  = ex.args[1]

      # ex = :( setfield(z, 3, :x) )
      # show(dump(ex))
      # ex = ex2
      # ex2 = ex
      # ex = ex2.args[3]
      # getfield, setfield!, getindex, setindex! need a special treatment
      #   of their arguments
      # if sf == :getfield
      #   fsym = add!(CLoc(ex.args[3])) # symbol constant
      #   largs = [explore(ex.args[2]), fsym]
      # elseif sf == setfield!
      #   fsym = add!(CLoc(ex.args[4])) # symbol constant
      #   largs = [map(explore, ex.args[2:3])..., fsym]
      # elseif sf == getfield  # TODO translate ':' and 'end'
      #   largs = [explore(ex.args[2]), explore(ex.args[3])]
      # elseif sf == setfield! # TODO translate ':' and 'end'
      #   largs = [map(explore, ex.args[2:3])..., map(explore, ex.args[4:end])...]
      # else
      #   largs = map(explore, ex.args[2:end])
      # end
      largs = map(explore, ex.args[2:end])
      val = eval(Expr(:call, sf, [x.val for x in largs]...))

      if string(sf)[end] == '!' # mutating ? TODO : generalize with rules
        nloc = largs[1]
      else
        nloc = Loc(val)
        add!(nloc)
      end

      add!( Op(eval(sf), largs, [nloc;]) )
      nloc  # return Loc of result
  end

  function explore(ex::ExEqual) #  ex = toExH(:(a = sin(2)))
    lhs = ex.args[1]

    if isa(lhs, Symbol)
      rloc = explore(ex.args[2])
      if isbits(rloc.typ)  # bitstype => '=' is a copy
        nloc = Loc(rloc.typ, rloc.val)
        add!( Op(copy, [rloc;], [nloc;]) )
        add!(nloc)
      else  # array or composite type => '=' is a reference copy
        nloc = rloc
      end
      g.vars[lhs] = nloc
      return nloc

    elseif isRef(lhs)   # x[i] = ....
      lhss = lhs.args[1]
      explore( Expr(:call, :setindex!, lhss, ex.args[2], lhs.args[2:end]...) )
      return nothing  # TODO : should return the Array

    elseif isDot(lhs)   # x.field = ....
      lhss = lhs.args[1]
      explore( Expr(:call, :setfield!, lhss, lhs.args[2], ex.args[2]) )
      return nothing  # TODO : should return the composite type

    else
        error("[tograph] $(toExpr(ex)) not allowed on LHS of assigment")
    end

    nothing
      # catch getindex, etc. qualified by a module
      # sf2 = if isa(sf, Expr) && sf.head == :. && isa(sf.args[2], QuoteNode)
      #         sf.args[2].value
      #       else
      #         nothing
      #       end
  end

  exitnode = explore(s)
  # exitnode = nothing if only variable assigments in expression
  #          = ExNode of last calc otherwise

  # id is 'nothing' for unnassigned last statement
  exitnode!=nothing && ( g.vars[nothing] = exitnode )

    # # Resolve external symbols that are Functions, DataTypes or Modules
    # # and turn them into constants
    # for en in filter(n -> isa(n, NExt) & !in(n.main, svars) , keys(g.exti))
    #     if isdefined(evalmod, en.main)  # is it defined
    #         tv = evalmod.eval(en.main)
    #         isa(tv, TypeConstructor) && error("[tograph] TypeConstructors not supported: $ex $(tv), use DataTypes")
    #         if isa(tv, DataType) || isa(tv, Module) || isa(tv, Function)
    #             delete!(g.exti, en)
    #             nc = addnode!(g, NConst( tv ))
    #             fusenodes(g, nc, en)
    #         end
    #     end
    # end

  g  # return graph
end


function indexspec(nv, as)
    p  = ExNode[]
    for (i,na) in enumerate(as)
        if length(as)==1 # single dimension
            ns = addgraph!(:( length(x) ), g, @compat Dict(:x => nv) )
        else # several dimensions
            ns = addgraph!(:( size(x, $i) ), g, @compat Dict(:x => nv) )
        end

        na==:(:) && (na = Expr(:(:), 1, :end) )  # replace (:) with (1:end)

        # explore the dimension expression
        nsvars = union(svars, collect(syms(g.seti)))
        ng = tograph(na, evalmod, nsvars)

        # find mappings for onodes, including :end
        vmap = Dict()
        for (k, sym) in ng.exti.kv
            vmap[sym] = sym == :end ? ns : explore(sym)
        end

        nd = addgraph!(ng, g, vmap)
        push!(p, nd)
    end
    p
end

a = ones(10)
a[5:end]
getindex(a, 2)
getindex(a, 5:end)

type Z ; x ; y ; end
z = Z(1,2)

g = tograph( :( z.x ) )
show(g)

A.a
g = tograph( :( A.a[2] ) )
show(g)

g = tograph( :( A.a[2] = 3) )
show(g)


g = tograph( :( sin(24) ) )
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

function tocode(g::Graph)
  #### creates expression for names qualified by a module
  mexpr(ns) = length(ns) == 1 ? ns[1] : Expr(:., mexpr(ns[1:end-1]), QuoteNode(ns[end]) )

  function translate(o::Op)  # o = g.ops[1]
    vargs = Any[ getexpr(arg) for arg in o.asc ]

    # special translation cases
    if o.f == vcat
      return Expr(:vect, vargs...)
    elseif o.f == colon
      return Expr( :(:), vargs...)
    elseif o.f == transpose
      return Expr(symbol("'"), vargs...)
    elseif o.f == tuple
      return Expr(:tuple, vargs...)
    elseif o.f == getindex
      return Expr( :ref, vargs...)
    elseif o.f == getfield
      return Expr(   :., vargs...)
    elseif o.f == setindex!
      return Expr( :(=), Expr(:ref, vargs[1], vargs[3:end]...), vargs[2])
    elseif o.f == setfield!
      return Expr( :(=), Expr(  :., vargs[1], vargs[3:end]...), vargs[2])
    end

    # default translation
    thing_module(op::DataType) = tuple(fullname(op.name.module)..., op.name.name)

    thing_module(op::Function) =
        tuple(fullname(Base.function_module(op, Tuple{Vararg{Any}}))...,
              op.env.name )
              # symbol(string(op)) )

    mt = try
            thing_module(o.f)
         catch e
            error("[tocode] cannot find module of $op ($(typeof(op)))")
         end

    # try to strip module names for brevity
    try
        mt2 = (:Base, mt[end])
        eval(:( $(mexpr(mt)) == $(mexpr(mt2)) )) &&  (mt = mt2)
        mt2 = (mt[end],)
        eval(:( $(mexpr(mt)) == $(mexpr(mt2)) )) &&  (mt = mt2)
    end

    Expr(:call, mexpr( mt ), Any[ getexpr(arg) for arg in o.asc ]...)
  end

  getexpr(l::Loc{:constant}) = l.val
  function getexpr(l::Loc) # l = g.locs[1]
    haskey(locex, l) && return locex[l]
    sym = 0
    for (k,v) in g.vars ; v!=l && continue ; sym = k ; break ; end
    sym = sym!= 0 ? sym : gensym("_tmp")
    locex[l] = sym
    sym
  end

  function ispivot(o::Op)
    true
  end

  out = Any[]
  opex  = Dict{Op, Any}()
  locex = Dict{Loc, Any}()
  for o in g.ops #
    opex[o] = translate(o)       # translate to Expr

    # TODO : manage multiple assignment
    if o.desc[1] in o.asc   # mutating Function
      push!(out, opex[o])
    elseif ispivot(o) # assignment needed,
      sym = getexpr(o.desc[1])
      locex[o.desc[1]] = sym
      if sym == nothing # terminal calculation
        push!(out, :( $(opex[o])) )
      else
        push!(out, :( $sym = $(opex[o])) )
      end
    else
      locex[o.desc[1]] = opex[o]
    end
  end

  #       stat, lhs = ispivot(n, g, nref1, nref2, nref3)
  #
  #     if stat && isa(n, Union{NSRef, NSDot})
  #         push!(out, n.val)
  #         n.val = n.parents[1].val
  #
  #     elseif stat && isa(n, NFor)
  #         push!(out, n.val)
  #
  #         g2 = n.main[2]
  #         valdict = Dict()
  #         for (k, sym) in g2.seto
  #           valdict[k] = getnode(g2.seti, sym).val
  #         end
  #         n.val = valdict
  #
  #     elseif stat && (lhs != n.val)
  #         if lhs == nothing && n == g.nodes[end] # last statement without assignment
  #             push!(out, :( $(n.val) ))
  #
  #         else
  #             ( lhs in [ nosym, nothing] ) && ( lhs = newvar() )
  #             push!(out, :( $lhs = $(n.val) ))
  #         end
  #
  #         n.val = lhs
  #
  #     end
  #
  # end

  return Expr(:block, out...)
end

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
