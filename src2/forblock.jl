################################################################################
#
#   'for' loops material for parsing, differentiating, etc...
#
################################################################################



"""
Type `ForBlock` contains the block inside the for loop and additional
info on the iteration range and the iteration variable :

  - ops : a `Vector{Op}` describing the operations
  - symbols : a `Dict{Any, Loc}` giving the mapping between symbols and their
  Loc. Since for blocks are scope blocks the symbols map is distinct
  from the parent's map.
  - asc  : used Locs, with loop index in pos #1, and range in #2
  - desc : generated Locs
"""
type ForBlock <: AbstractBlock
    ops::Vector{Op}
    symbols::Dict{Any, Loc}
    lops::Vector{Op}
    asc::Vector{Loc}  # parent Loc (block arguments)
    desc::Vector{Loc} # descendant Loc (Loc modified/created by block)
end

getops(bl::ForBlock) = Any[bl.ops, bl.lops]

function summarize(bl::ForBlock)
  asc  = Set()
  desc = Set()
  for ops in getops(bl)
    asc  = mapreduce(o ->  o.asc, union,  asc, ops)
    desc = mapreduce(o -> o.desc, union, desc, ops)
  end
  # keep var and range in correct positions
  asc = vcat(bl.asc[1:2], setdiff(asc, bl.asc[1:2]))
  asc, desc
end

function blockparse!(ex::ExFor, parentops, parentsymbols::AbstractBlock, g::Graph)
  # find the iteration variable
  ixs = ex.args[1].args[1]
  isa(ixs, Symbol) || error("[tograph] for loop using several indices : $ixs ")

  # explore loop iterable in the parentblock
  rgl = addtoblock!(ex.args[1].args[2], parentops, parentsymbols, g)

  # create Loc for iteration variable
  ixl = RLoc( first(rgl.val) ) # first element of iterable
  push!(g.locs, ixl)

  # create ForBlock
  symbols = copy(parentsymbols)
  symbols[ixs] = ixl  # add iteration var symbol in symbols map

  thisblock = ForBlock(Vector{Op}(), symbols, Loc[ixl, rgl], Vector{Loc}())
  addtoblock!(ex.args[2], thisblock, g) # parse loop contents

  # look for symbols that point to a different Loc
  #  - to update the symbols table of the parent block
  #  - to update the lops field marking variables updated and used
  for k in keys(parentsymbols)
    parentsymbols[k] == symbols[k] && continue # not modified => pass
    parentsymbols[k] = symbols[k]  # TODO complete here
  end

  thisblock.asc, thisblock.desc = summarize(thisblock)

  push!(parentops, thisblock)

  nothing  # considers that for loops do not return anything (TODO : check)
end


function blockcode(bl::ForBlock, locex, g::Graph)
  # iteration variable Loc is in pos # 1
  ixl = bl.asc[1]
  if !haskey(locex, ixl) # if no name, create one
      locex[ixl] = newvar()
  end
  ixs = locex[ixl]

  # iterable Loc is in pos # 2
  rgl = bl.asc[2]
  rgs = locex[rgl]

  # expression for inner code
  exits = intersect(bl.asc, bl.desc)  # mutated Locs
  fex = _tocode(bl.ops, collect(exits), bl.symbols, g, locex)

  Expr(:for,
       Expr(:(=), ixs, rgs),
       fex)
end
