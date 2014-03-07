#########################################################################
#
#   Julia Package for reverse mode automated differentiation (from source)
#
#########################################################################

module ReverseDiffSource

  import Base.show

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

  #####  ExNode type  ######
  type ExNode
    nodetype::Symbol
    name
    parents::Vector
    value
  end

  ExNode(typ::Symbol, name) = ExNode(typ, name, ExNode[], NaN)
  ExNode(typ::Symbol, name, parents) = ExNode(typ, name, parents, NaN)

  function show(io::IO, res::ExNode)
    pl = join( map(x->repr(x.name), res.parents) , " / ")
    print(io, "[$(res.nodetype)] $(repr(res.name)) ($(res.value))")
    length(pl) > 0 && print(io, ", from = $pl")
  end

  typealias ExNodes Vector{ExNode}

  #####  ExGraph type  ######
  type ExGraph
    nodes::ExNodes
    exitnodes::Dict
  end

  ExGraph() = ExGraph(ExNode[], Dict{Symbol, ExNode}())

  ######  Graph functions  ######
  function add_node(g::ExGraph, nargs...)
    v = ExNode(nargs...)
    push!(g.nodes, v)
    v
  end

  ancestors(n::ExNode) = union( Set(n), ancestors(n.parents) )
  ancestors(n::Vector) = union( map(ancestors, n)... )

  ######  Includes  ######
  include("graph_funcs.jl")
  include("graph_code.jl")
  include("reversegraph.jl")
  include("deriv_rules.jl")
  include("reversediff.jl")


  ######  Exports  ######
  export 
    reversediff, ndiff,
    @deriv_rule, deriv_rule, 
    @type_decl, type_decl


end # module ReverseDiffSource
