################################################################################
#
#   Snippet type definition + related methods
#
################################################################################

"""
type Snippet contains an expression + related info for inclusion in a graph.
It starts in an incomplete state with some fields undefined and can later be
'compiled' to a complete state.
"""
type Snippet
  ex::Union{Expr, Symbol, Real} # expression defining derivation rule
  syms::Vector{Symbol}          # symbols of ex to be substituted
  g::Graph                      # corresponding graph (initially undefined)
  inputs::Vector{Loc}           # Locs in g corresponding to each syms

  function Snippet(ex, syms)
      x = new()
      x.ex, x.syms = ex, syms
      x
  end
end

iscompiled(s::Snippet) = isdefined(s, :g)

function compile!(s::Snippet, args::Vector{Loc})
  length(args) == length(s.syms) ||
    error("[snippet compile] incompatible arguments length")

  s.g = Graph()
  s.inputs = Array(Loc, length(s.syms))

  for (i,(sym,arg)) in enumerate(zip(s.syms, args))
    l = RLoc(arg.val)
    push!(s.g.locs, l)
    s.g.block.symbols[sym] = l
    s.inputs[i] = l
  end

  result = addtoops!(s.ex, s.g.block.ops, s.g.block.symbols, s.g)

  result != nothing && ( s.g.block.symbols[EXIT_SYM] = result )
  s
end



function insertsnippet!(src::Snippet, dest::Graph, args::Vector{Loc})
  iscompiled(src) || compile!(src, args)
  inmap = Dict{Loc,Loc}(zip(src.inputs, args))
  insertgraph!(src.g, dest, inmap)
end
