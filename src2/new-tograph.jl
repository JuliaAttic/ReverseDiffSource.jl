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


type Env
  eval::Function
  isdefined::Function
  isconst::Function
  mark::Function
end

modenv(m::Module=current_module()) = Env(ex -> eval(m, ex),
                             s -> isdefined(m, s),
                             s -> isconst(m, s),
                             s -> error("can't modify variable outside function ($s)"))

current_module()
env = modenv(A)
env.eval(:a)
env.isconst(:a)
env.isdefined(:z)
env.isdefined(:y)

env = modenv()
env.eval(:a)
env.isconst(:a)
env.isdefined(:z)
env.isdefined(:y)

# function modenv(s::Symbol, m::Module=A)
#   isdefined(m, s) && return eval(m, s)
#   error("[tograph] undefined symbol $s")
# end

function tograph(s, env::Env=modenv())  # env = modenv
  # ex = :( z.x )
  #  this level graph
  g = Graph()
  add!(l::Loc) = (push!(g.locs,l) ; l)
  add!(o::Op)  = (push!(g.ops, o) ; o)

  localenv = Env()

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

#
# function indexspec(nv, as)
#     p  = ExNode[]
#     for (i,na) in enumerate(as)
#         if length(as)==1 # single dimension
#             ns = addgraph!(:( length(x) ), g, @compat Dict(:x => nv) )
#         else # several dimensions
#             ns = addgraph!(:( size(x, $i) ), g, @compat Dict(:x => nv) )
#         end
#
#         na==:(:) && (na = Expr(:(:), 1, :end) )  # replace (:) with (1:end)
#
#         # explore the dimension expression
#         nsvars = union(svars, collect(syms(g.seti)))
#         ng = tograph(na, evalmod, nsvars)
#
#         # find mappings for onodes, including :end
#         vmap = Dict()
#         for (k, sym) in ng.exti.kv
#             vmap[sym] = sym == :end ? ns : explore(sym)
#         end
#
#         nd = addgraph!(ng, g, vmap)
#         push!(p, nd)
#     end
#     p
# end
