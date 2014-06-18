#########################################################################
#
#   Julia Package for reverse mode automated differentiation (from source)
#
#########################################################################

module ReverseDiffSource

  # using GraphViz
  
  import Base.show, Base.copy

  # naming conventions
  const TEMP_NAME = "_tmp"   # prefix of new variables
  const DERIV_PREFIX = "d"   # prefix of gradient variables

  ## misc functions
  dprefix(v::Union(Symbol, String, Char)) = symbol("$DERIV_PREFIX$v")

  isSymbol(ex)   = isa(ex, Symbol)
  isDot(ex)      = isa(ex, Expr) && ex.head == :.   && isa(ex.args[1], Symbol)
  isRef(ex)      = isa(ex, Expr) && ex.head == :ref && isa(ex.args[1], Symbol)

  ## temp var name generator
  let
    vcount = Dict()
    global newvar
    function newvar(radix::Union(String, Symbol)=TEMP_NAME)
      vcount[radix] = haskey(vcount, radix) ? vcount[radix]+1 : 1
      return symbol("$(radix)$(vcount[radix])")
    end

    global resetvar
    function resetvar()
      vcount = Dict()
    end
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
  typealias ExCell1d   ExH{:cell1d}
  typealias ExFor      ExH{:for}
  typealias ExRef      ExH{:ref}
  typealias ExIf       ExH{:if}
  typealias ExComp     ExH{:comparison}
  typealias ExDot      ExH{:.}
  typealias ExTuple    ExH{:tuple}
  typealias ExReturn   ExH{:return}
  typealias ExBody     ExH{:body}

  # variable symbol sampling functions
  getSymbols(ex::Any)    = Set{Symbol}()
  getSymbols(ex::Symbol) = Set{Symbol}(ex)
  getSymbols(ex::Array)  = mapreduce(getSymbols, union, ex)
  getSymbols(ex::Expr)   = getSymbols(toExH(ex))
  getSymbols(ex::ExH)    = mapreduce(getSymbols, union, ex.args)
  getSymbols(ex::ExCall) = mapreduce(getSymbols, union, ex.args[2:end])  # skip function name
  getSymbols(ex::ExRef)  = setdiff(mapreduce(getSymbols, union, ex.args), Set(:(:), symbol("end")) )# ':'' and 'end' do not count
  getSymbols(ex::ExDot)  = Set{Symbol}(ex.args[1])  # return variable, not fields
  getSymbols(ex::ExComp) = setdiff(mapreduce(getSymbols, union, ex.args), 
    Set(:(>), :(<), :(>=), :(<=), :(.>), :(.<), :(.<=), :(.>=), :(==)) )

  ## variable symbol substitution functions
  substSymbols(ex::Any, smap::Dict)     = ex
  substSymbols(ex::Expr, smap::Dict)    = substSymbols(toExH(ex), smap::Dict)
  substSymbols(ex::Vector, smap::Dict)  = map(e -> substSymbols(e, smap), ex)
  substSymbols(ex::ExH, smap::Dict)     = Expr(ex.head, map(e -> substSymbols(e, smap), ex.args)...)
  substSymbols(ex::ExCall, smap::Dict)  = Expr(:call, ex.args[1], map(e -> substSymbols(e, smap), ex.args[2:end])...)
  substSymbols(ex::ExDot, smap::Dict)   = (ex = toExpr(ex) ; ex.args[1] = substSymbols(ex.args[1], smap) ; ex)
  substSymbols(ex::Symbol, smap::Dict)  = get(smap, ex, ex)




  ######  Includes  ######
  include("node.jl")
  include("bidict.jl")
  include("graph.jl")
  include("plot.jl")
  include("simplify.jl")
  include("tograph.jl")
  include("tocode.jl")
  include("reversegraph.jl")
  include("deriv_rule.jl")
  include("base_rules.jl")
  include("rdiff.jl")

  ######  Exports  ######
  export 
    rdiff,
    @deriv_rule, deriv_rule, 
    @type_decl, type_decl


end # module ReverseDiffSource

