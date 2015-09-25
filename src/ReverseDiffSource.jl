#########################################################################
#
#   Julia Package for reverse mode automated differentiation (from source)
#
#########################################################################

VERSION >= v"0.4.0-dev+6521" && __precompile__(true)

module ReverseDiffSource

  import Base.show, Base.copy

  
  using Compat  # for Julia v0.x compatibility issues

  # naming conventions
  const TEMP_NAME = "_tmp"   # prefix of new variables
  const DERIV_PREFIX = "d"   # prefix of gradient variables

  ## misc functions
  dprefix(v::@compat Union{Symbol, AbstractString, Char}) = symbol("$DERIV_PREFIX$v")

  isSymbol(ex)   = isa(ex, Symbol)
  isDot(ex)      = isa(ex, Expr) && ex.head == :.   && isa(ex.args[1], Symbol)
  isRef(ex)      = isa(ex, Expr) && ex.head == :ref && isa(ex.args[1], Symbol)

  ## temp var name generator
  let
    vcount = Dict()
    global newvar
    function newvar(radix::@compat Union{AbstractString, Symbol})
      vcount[radix] = haskey(vcount, radix) ? vcount[radix]+1 : 1
      return symbol("$(radix)$(vcount[radix])")
    end
    newvar() = newvar(TEMP_NAME)

    global resetvar
    function resetvar()
      vcount = Dict()
    end
  end

  ######  Includes  ######
  include("node.jl")
  include("bidict.jl")
  include("graph.jl")
  include("plot.jl")
  include("simplify.jl")
  include("tograph.jl")
  include("tocode.jl")
  include("zeronode.jl")
  include("reversegraph.jl")
  include("deriv_rule.jl")
  # include("base_rules.jl")
  include("rdiff.jl")
  include("frdiff.jl")

  ######  Exports  ######
  export 
    rdiff,
    @deriv_rule, deriv_rule, 
    @typeequiv, typeequiv

  ######  Initializations (for the deriv rules)  ######
  __init__() = include(joinpath(Pkg.dir("ReverseDiffSource"), "src/base_rules.jl"))
  
  # VERSION < v"0.3-" && __init__()  # call explicitly __init__ for older julias

  if VERSION < v"0.3-"
    __init__()  # call explicitly __init__ for older julias
  else
    __init__()  # additional warmup for faster execution
    rdiff(:( x^2 - 1), x=2., order=2)
  end

end # module ReverseDiffSource

