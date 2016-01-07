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
  is = ex.args[1].args[1]
  isa(is, Symbol) || error("[tograph] for loop using several indices : $is ")

  # explore loop iterable in the parentblock
  nir = addtoblock!(ex.args[1].args[2], parentblock, g)

  # create Loc for iteration variable
  lis = RLoc( first(nir.val) ) # first element of iterable
  push!(g.locs, lis)

  # create ForBlock
  symbols = copy(parentblock.symbols)
  symbols[is] = lis  # add iteration variable

  thisblock = ForBlock(Vector{Op}(), symbols, is, nir)
  addtoblock!(ex.args[2], thisblock, g) # parse loop contents

  # update parent block symbols map
  for k in keys(parentblock.symbols)
    haskey(symbols, k) || continue
    parentblock.symbols[k] = symbols[k]
  end

  # create op
  asc  = collect( mapreduce(o ->  o.asc, union, Set{Loc}(), thisblock.ops) )
  desc = collect( mapreduce(o -> o.desc, union, Set{Loc}(), thisblock.ops) )
  lb = RLoc(thisblock)
  push!(g.locs, lb)
  op = Op(lb, asc, desc)
  push!(parentblock.ops, op)

  nothing  # considers that for loops do not return anything (TODO : check)
end
