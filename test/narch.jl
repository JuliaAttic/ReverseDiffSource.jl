# locations instead of symbols
# dict symbol -> loc
# loc have previous states (for mutating functions)
# deriv rules as real functions in a dedicated sub module


module A; end

module A

type Loc
    typ::DataType
    val::Any
    ext::Bool
end

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

g = Graph()
# g = Vector{Loc}()

addloc!(g::Graph, l::Loc) = (push!(g.locs,l) ; l)

function modenv(s::Symbol, m::Module=Main)
  isdefined(m, s) && return eval(m, s)
  error("[tograph] undefined symbol $s")
end

function tograph(s, env::Function=modenv)  # env = modenv

    explore(ex::Any)       = error("[tograph] unmanaged type $ex ($(typeof(ex)))")
    explore(ex::Expr)      = explore(toExH(ex))
    explore(ex::ExH)       = error("[tograph] unmanaged expr type $(ex.head) in ($ex)")

    explore(ex::ExLine)         = nothing     # remove line info
    explore(ex::LineNumberNode) = nothing     # remove line info

    explore(ex::QuoteNode) = addnode!(g, constloc(ex.value))  # consider as constant

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

    explore(ex::Real)      = addloc!(g, Loc(typeof(ex), ex, false))

    explore(ex::ExBlock)   = map( explore, ex.args )[end]
    explore(ex::ExBody)    = map( explore, ex.args )[end]

    # explore(ex::ExComp)    = addnode!(g, NComp(ex.args[2], [explore(ex.args[1]), explore(ex.args[3])]))

    # explore(ex::ExDot)     = addnode!(g, NDot(ex.args[2],     [ explore(ex.args[1]) ]))
    explore(ex::ExDot)     = explore(Expr(:call, :getfield, ex.args...))
    explore(ex::ExRef)     = explore(Expr(:call, :getindex, ex.args...))

    function explore(ex::Symbol)
        haskey(g.vars, ex) && return g.vars[ex]

        val = env(ex)
        nloc = Loc(typeof(val), val, true)
        g.vars[ex] = nloc
        addloc!(g, nloc)
    end

    function explore(ex::ExCall) # TODO :  treat func that return several values
        sf  = ex.args[1]

        largs = map(explore, ex.args[2:end])
        val = eval(Expr(:call, sf, [x.val for x in largs]...))

        mut = string(sf)[end] == '!' # TODO : needs base rules

        nloc = Loc(typeof(val), val, mut)
        nop = Op(eval(sf), largs, [nloc;])
        push!(g.ops, nop)
        addloc!(g, nloc)
    end

    function explore(ex::ExEqual) #  ex = toExH(:(a = sin(2)))
      lhs = ex.args[1]

      if isa(lhs, Symbol)
        rloc = explore(ex.args[2])
        if isbits(rloc.typ)  # bitstype => '=' is a copy
          nloc = Loc(rloc.typ, rloc.val, false)
          nop = Op(copy, [rloc;], [nloc;])
          push!(g.ops, nop)
        else  # array or composite type => '=' is a reference copy
          nloc = rloc
        end
        addloc!(g, nloc)
        g.vars[lhs] = nloc

      elseif isRef(lhs)   # x[i] = ....
          lhss = lhs.args[1]
          explore( Expr(:call, :setindex!, lhss, ex.args[2], lhs.args[2:end]...) )

      elseif isDot(lhs)   # x.field = ....
          lhss = lhs.args[1]
          explore( Expr(:call, :setfield!, lhss, lhs.args[2], ex.args[2]) )

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

    #  top level graph
    g = Graph()

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

    g
end



g = tograph( :( sin(24) ) )
g.vars
g.ops
g.locs

g = tograph( :( a = 2 ; sin(a) ) )

g = tograph( :( a = 2 ; b = a ; a+b ) )
g.ops

g = tograph( :( a = ones(2) ; b = a ; b[1] ) )
g.ops




g.locs

import Base.show
function show(io::IO, g::Graph)
  locs = Any["#" "type" "val" "var(s)"]
  for (i,l) in enumerate(g.locs) # i,l = 1, g.locs[1]
      vs = collect(keys(filter((k,v) -> v==l, g.vars)))
      # vs = collect(keys(vs))
      locs = vcat(locs, [i l.typ l.val join(vs)])
  end
  show(io, locs)

end



g
showall(g)

end # of module A
