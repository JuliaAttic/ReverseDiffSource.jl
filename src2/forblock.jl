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
    lops::Vector{Op}
    symbols::Dict{Any, Loc}
    asc::Vector{Loc}  # parent Loc (block arguments)
    desc::Vector{Loc} # descendant Loc (Loc modified/created by block)
end

getops(bl::ForBlock) = Any[bl.ops, bl.lops]

function summarize(bl::ForBlock)
  # note : only `ops` vector considered, `lops` is only implicit assignements
  asc  = mapreduce(o ->  o.asc, union, Set(), bl.ops)
  desc = mapreduce(o -> o.desc, union, Set(), bl.ops)
  # keep var and range in correct positions
  asc = vcat(bl.asc[1:2], setdiff(asc, bl.asc[1:2]))
  asc, desc
end

function blockparse!(ex::ExFor, parentops, parentsymbols, g::Graph)
  # find the iteration variable
  ixs = ex.args[1].args[1]
  isa(ixs, Symbol) || error("[tograph] for loop using several indices : $ixs ")

  # explore loop iterable in the parentblock
  rgl = addtoops!(ex.args[1].args[2], parentops, parentsymbols, g)

  # create Loc for iteration variable
  ixl = RLoc( first(rgl.val) ) # first element of iterable
  push!(g.locs, ixl)

  # create ForBlock
  symbols = copy(parentsymbols)
  symbols[ixs] = ixl  # add iteration var symbol in symbols map

  thisblock = ForBlock(Op[], Op[], symbols, Loc[ixl, rgl], Vector{Loc}())
  addtoops!(ex.args[2], thisblock.ops, symbols, g) # parse loop contents

  # look for symbols that point to a different Loc
  #  - to update the symbols table of the parent block
  #  - to update the lops field marking variables updated and used
  for k in keys(parentsymbols)
    parentsymbols[k] == symbols[k] && continue # not modified => pass

    # when looping, we are copying the previous loop result
    # into the original variable
    oloc = parentsymbols[k]
    dloc = symbols[k]
    fcop = CLoc(copy!)
    push!(g.locs, fcop)
    push!(thisblock.lops, FOp(fcop, [oloc, dloc], [oloc;]))

    # update the parents' symbol map
    parentsymbols[k] = dloc
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

  # exits = intersect(bl.asc, bl.desc)  # mutated Locs
  out = Expr[]

  # for each updated variable ( <> mutated variables) : force creation of
  # variable before loop if there isn't one
  for lop in bl.lops
    li, lo = lop.asc

    # find symbol
    ks  = collect(keys(bl.symbols))
    syms = filter(s -> s!=EXIT_SYM && bl.symbols[s]==lo, ks)
    length(syms)==0 && push!(syms, newvar())

    if !haskey(locex, li) # probably a constant
      push!(out, Expr(:(=), syms[1], li.val))
      locex[li] = syms[1]
    elseif !isa(locex[li], Symbol)
      push!(out, Expr(:(=), syms[1], locex[li]))
      locex[li] = syms[1]
    end
  end

  # for updated and mutated variables : mark as exit for code generation
  exits = copy(bl.desc)
  append!(exits, Loc[ op.asc[2] for op in bl.lops])

  # expression for inner code
  fex = _tocode(bl.ops, exits, bl.symbols, g, locex)

  push!(out, Expr(:for, Expr(:(=), ixs, rgs), fex))

  out
end
