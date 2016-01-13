
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
Abstract type `Op` is used to represent an operation. The presence of a given Loc in
both `asc`, the operation's input (its arguments), and `desc`n the operation's
results indicate that the operation is mutating the Loc.

Common fields for this abstract type are :

  - asc : a `Vector{Loc}` of the function arguments
  - val : a `Vector{Loc}` of the function results
"""
abstract Op


"""
Type `FOp <: Op` is used to for simple functions.

  - f : the Loc containing the function
"""
type FOp <: Op
    f::Loc
    asc::Vector{Loc}  # parent Loc (function arguments)
    desc::Vector{Loc} # descendant Loc (Loc modified/created by function)
end



"""
Abstract type `AbstractBlock <: Op` is used to for block types (Block,
ForBlock, etc..)

Common fields for this abstract type are :

  - symbols : a `Dict{Any, Loc}` giving the mapping between symbols and their
  Loc. Several symbols can point to the same Loc. The Dict is shared with the
  parent Block if the block is not a scope block, and distinct otherwise.
"""
abstract AbstractBlock <: Op  # abstract type for all block types


"""
`getops(bl::AbstractBlock)` returns a Vector{Vector{Op}} of the operations vectors
in this block. Usually there is a single vector of ops by block, one
exception being the IfBlock which has 2.
"""
getops(bl::AbstractBlock) = Any[bl.ops]


"""
Type `Block` is for simple blocks :

  - ops : a `Vector{Op}` describing the operations
"""
type Block <: AbstractBlock
    ops::Vector{Op}
    symbols::Dict{Any, Loc}
    asc::Vector{Loc}  # parent Loc (block arguments)
    desc::Vector{Loc} # descendant Loc (Loc modified/created by block)
end

Block() = Block(Vector{Op}(), Dict{Any, Loc}(), Vector{Loc}(), Vector{Loc}())


"""
`allblocks(x)` returns a vector of the blocks in x and below
"""
allblocks(op::Op) = []
allblocks(ops::Vector{Op})   = vcat(map(allblocks, ops)...)
# allblocks(ops::Vector)       = vcat(map(allblocks, ops)...)
allblocks(bl::AbstractBlock) = vcat(bl, map(allblocks, getops(bl))...)
# getops(g.block)
# bl=g.block
# typeof(getops(bl)[1])
# isa(getops(bl)[1], Vector{Op})
# allblocks(getops(bl)[1])
# allblocks(getops(bl)[1][1])
# allblocks(getops(bl)[1][2])
# allblocks(getops(bl)[1][3])
# isa(getops(bl)[1][3], AbstractBlock)
# allblocks(g.block)
# allblocks(getops(bl))
# allblocks(g)
# function allblocks(op::Op)
#   isa(op.f.val, AbstractBlock) || return []
#   vcat((op.f.val, op), allblocks(op.f.val)...)
# end

function summarize(bl::AbstractBlock)
  asc  = Set()
  desc = Set()
  for ops in getops(bl)
    asc  = mapreduce(o ->  o.asc, union, asc, ops)
    desc = mapreduce(o -> o.desc, union, asc, ops)
  end
  collect(asc), collect(desc)
end



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

allblocks(g::Graph) = allblocks(g.block)
# allblocks(g)
getops(g::Graph) = getops(g.block)

# allblocks(g)

###########  printing methods  ###################

function _printtable(io::IO, t::Array{UTF8String,2})
  sz = maximum(map(length, t),1)
  for i in 1:size(t,1)
    for j in 1:size(t,2)
      l = length(t[i,j])
      print(io, " " ^ (sz[j]-l), t[i,j], " ")
    end
    println(io,"")
  end
end

function show(io::IO, g::Graph)  # io=STDOUT
  slocs = Array(UTF8String, length(g.locs)+1, 5)
  slocs[1,:] = ["#", "type", "symbol(s)", "cat", "val" ]
  for (i,l) in enumerate(g.locs) # i,l = 1, g.locs[1]
    vs = keys(filter((k,v) -> v===l, g.block.symbols))
    sio = IOBuffer(true, true) ; show(sio, l.val)
    vals = takebuf_string(sio)[1:min(end,30)]  # 30 char max for value
    vals *= length(vals)==30 ? "..." : ""
    slocs[i+1,:] = map(string, Any[i, l.typ, join(vs, ","), loctype(l), vals])
  end
  _printtable(io, slocs)
  println(io, "")

  for bl in allblocks(g) # bl = allblocks(g)[1]
    for ops in getops(bl) # ops = getops(bl)[1]
      sops = Array(UTF8String, length(ops)+1, 3)
      sops[1,:] = ["operator" "parents" "children"]
      for (i,o) in enumerate(ops) # i,l = 1, g.ops[1]
        sio = IOBuffer(true, true)
        show(sio, o)
        sops[i+1,1] = takebuf_string(sio)
        sops[i+1,2] = join(indexin( o.asc, g.locs), ",")
        sops[i+1,3] = join(indexin(o.desc, g.locs), ",")
      end
      _printtable(io, sops)
      println(io, "")
    end
  end
end

function show(io::IO, l::Loc)
  print(io, "($(l.typ)) ")
  sio = IOBuffer(true, true)
  show(io, l.val)
end

function show(io::IO, bl::AbstractBlock)
  ns = length(bl.symbols)
  no = length(bl.ops)
  print(io, "Block $ns symbols, $no ops")
end

function show(io::IO, op::Op)
  show(io, op.f)
end

##### files to be included

include("tograph.jl")
include("tocode.jl")
include("simplify.jl")
include("forblock.jl")
include("defs.jl")  # testing stuff


end # module
