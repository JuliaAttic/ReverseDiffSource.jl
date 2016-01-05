

module ReverseDiffSource

import Base.show

# name for exit variable
const EXIT_SYM = :_result


######## Location type ##########
"""
Type `Loc` is used to represent a memory location used or modified or created by
an operation.

  - typ : object type
  - val : object value
"""
type Loc{T}  # regular, constant, external
    typ::DataType
    val::Any

    Loc(x) = new(typeof(x), x)
end

# Loc{T}(x) = Loc{T}(typeof(x), x)

typealias CLoc Loc{:constant}  # constants
typealias ELoc Loc{:external}  # external
typealias RLoc Loc{:regular}   # regular

loctype{T}(l::Loc{T}) = T

######## Operation type ##########
"""
Type `Op` is used to represent an operation. The presence of a given Loc in
both `asc`, the operation's input (its arguments), and `desc`n the operation's
results indicate that the operation is mutating the Loc.

  - f : the Loc containing the function
  - asc : a `Vector{Loc}` of the function arguments
  - val : a `Vector{Loc}` of the function results
"""
type Op
    f::Loc
    asc::Vector{Loc}  # parent Loc (function arguments)
    desc::Vector{Loc} # descendant Loc (Loc modified/created by function)
end

######## Block types ##########
abstract AbstractBlock   # abstract type for all block types

# Plain block
"""
Type `Block` contains the fields :

  - ops : a `Vector{Op}` describing the operations
  - symbols : a `Dict{Any, Loc}` giving the mapping between symbols and their
  Loc. Several symbols can point to the same Loc. The Dict is shared with the
  parent Block if the block is not a scope block, and distinct otherwise.
"""
type Block <: AbstractBlock
    ops::Vector{Op}
    symbols::Dict{Any, Loc}
end

Block() = Block(Vector{Op}(), Dict{Any, Loc}())

######## Graph type ##########
"""
Type `Graph` contains the whole function/expression description on which the
differentitation is to take place. It contains the fields :

  - locs : a `Vector{Loc}` of all the Locs used/modified/created by the parent
  block and its children. There are no Locs in the children.
  - block : the parent block
  - isdef : a function indicating if a given symbol is defined in the
  function/expression environment
  - eval  : a function returning the value of a symbol in the
  function/expression environment
  - isconst : a function indicating if a given symbol is a constant in the
  function/expression environment
"""
type Graph
    locs::Vector{Loc}
    block::Block
    isdef::Function
    eval::Function
    isconst::Function
end

function Graph(m::Module=current_module())
   Graph(Vector{Loc}(),
         Block(),
         s -> isdefined(m, s),
         s -> eval(m,s),
         s -> isconst(m,s) )
end


###########  printing methods  ###################

function _printtable(t::Array{UTF8String,2})
  sz = maximum(map(length, t),1)
  for i in 1:size(t,1)
    for j in 1:size(t,2)
      l = length(t[i,j])
      print(io, " " ^ (sz[j]-l), t[i,j], " ")
    end
    println()
  end
end

function show(io::IO, ls::Vector{Loc})
  slocs = Array(UTF8String, length(g.locs)+1, 5)
  slocs[1,:] = ["#", "type", "symbol(s)", "cat", "val" ]
  for (i,l) in enumerate(g.locs) # i,l = 1, g.locs[1]
    vs = keys(filter((k,v) -> v===l, g.symbols))
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

function show(io::IO, g::Graph)
  slocs = Array(UTF8String, length(g.locs)+1, 5)
  slocs[1,:] = ["#", "type", "symbol(s)", "cat", "val" ]
  for (i,l) in enumerate(g.locs) # i,l = 1, g.locs[1]
    vs = keys(filter((k,v) -> v===l, g.block.symbols))
    slocs[i+1,:] = map(string, Any[i, l.typ, join(vs, ","), loctype(l), l.val])
  end
  _printtable(slocs)
  println()

  sops = Array(UTF8String, length(g.block.ops)+1, 3)
  sops[1,:] = ["f" "parents" "children"]
  for (i,o) in enumerate(g.block.ops) # i,l = 1, g.ops[1]
    ps = indexin(o.asc, g.locs)
    cs = indexin(o.desc, g.locs)
    sops[i+1,:] = map(string, Any[o.f, join(ps, ","), join(cs, ",")])
  end
  _printtable(sops)
end

include("tograph.jl")
include("tocode.jl")
include("simplify.jl")

end # module
