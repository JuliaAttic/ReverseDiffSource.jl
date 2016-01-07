################################################################################
#
#   'for' loops material for parsing, differentiating, etc...
#
################################################################################



"""
Type `ForBlock` used to contain the block inside the for loop and additional
info on the iteration range and the iteration variable :

  - ops : a `Vector{Op}` describing the operations
  - symbols : a `Dict{Any, Loc}` giving the mapping between symbols and their
  Loc. Several symbols can point to the same Loc. Since for blocks are scope
  blocks the symbols map is distinct from the parent's map.
  - iter : symbol of the iteration variable
  - iterable : Loc containing the iterable
"""
type ForBlock <: AbstractBlock
    ops::Vector{Op}
    symbols::Dict{Any, Loc}
    iter::Symbol
    iterable
end

Block() = Block(Vector{Op}(), Dict{Any, Loc}())


function blockparse!(ex::ExFor, parentblock::AbstractBlock, g::Graph)
  # find the iteration variable
  ixs = ex.args[1].args[1]
  isa(ixs, Symbol) || error("[tograph] for loop using several indices : $ixs ")

  # explore loop iterable in the parentblock
  rgl = addtoblock!(ex.args[1].args[2], parentblock, g)

  # create Loc for iteration variable
  ixl = RLoc( first(rgl.val) ) # first element of iterable
  push!(g.locs, ixl)

  # create ForBlock
  symbols = copy(parentblock.symbols)
  symbols[ixs] = ixl  # add iteration var symbol in symbols map

  thisblock = ForBlock(Vector{Op}(), symbols, ixs, rgl)
  addtoblock!(ex.args[2], thisblock, g) # parse loop contents

  # update parent block symbols map
  for k in keys(parentblock.symbols)
    haskey(symbols, k) || continue
    parentblock.symbols[k] = symbols[k]
  end

  # create op
  asc  = vcat([ixl, rgl], mapreduce(o ->  o.asc, union, Set{Loc}(), thisblock.ops))
  desc = collect( mapreduce(o -> o.desc, union, Set{Loc}(), thisblock.ops) )
  lb = RLoc(thisblock)
  push!(g.locs, lb)
  op = Op(lb, asc, desc)
  push!(parentblock.ops, op)

  nothing  # considers that for loops do not return anything (TODO : check)
end


function blockcode(bl::ForBlock, locex, asc, g::Graph)
  # iteration variable Loc is in pos # 1
  ixl = asc[1]
  if !haskey(locex, ixl) # if no name, create one
      locex[ixl] = newvar()
  end
  ixs = locex[ixl]

  # iterable Loc is in pos # 2
  rgl = asc[2]
  rgs = locex[rgl]

  # expression for inner code
  fex = _tocode(bl.ops, [], g, locex)

  Expr(:for,
       Expr(:(=), ixs, rgs),
       fex)
end
