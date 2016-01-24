################################################################################
#
#   'for' loops material for parsing, differentiating, etc...
#
################################################################################

# each block should define :
#   - a xxBlock type, children of AbstractBlock
#   - a getops(bl::xxBlock) function
#   - a summarize(bl::xxBlock) function
#   - a remap(bl::xxBlock, lmap) function
#   - a blockparse!(ex::xxExpr, parentops, parentsymbols, g::Graph) function
#   - a blockcode(bl::xxBlock, locex, g::Graph) function
#   - a blockdiff(bl::xxBlock, dmap, g) function

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
flatops(bl::ForBlock) = vcat(flatops(bl.ops), bl)  # ignore var looping block

function summarize(bl::ForBlock)
  # note : only `ops` vector considered, `lops` is only implicit assignements
  asc  = mapreduce(o ->  o.asc, union, Set(), bl.ops)
  desc = mapreduce(o -> o.desc, union, Set(), bl.ops)
  # keep var and range in correct positions
  asc = vcat(bl.asc[1:2], setdiff(asc, bl.asc[1:2]))
  collect(asc), collect(desc)
end



function remap(bl::ForBlock, lmap)
  ForBlock(remap(bl.ops, lmap),
           remap(bl.lops, lmap),
           [ s => lmap[l] for (s,l) in bl.symbols ],
           Loc[ lmap[l] for l in  bl.asc  ],
           Loc[ lmap[l] for l in  bl.desc ] )
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

  thisblock = ForBlock(Op[], Op[], symbols, Loc[ixl, rgl], Loc[])
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

  # for externals found update symbols in parentblock
  for (s,l) in symbols
    loctype(l) == :external || continue
    haskey(parentsymbols, s) && continue
    parentsymbols[s] = l
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

  # for each updated variable ( != mutated variables) : force creation of
  # variable before loop if there isn't one
  for lop in bl.lops
    li, lo = lop.asc

    # find symbol
    ks  = collect(keys(bl.symbols))
    println(bl.symbols)
    syms = filter(s -> s!=EXIT_SYM && (bl.symbols[s]==lo), ks)
    length(syms)==0 && push!(syms, newvar())

    if !haskey(locex, li) # probably a constant
      push!(out, Expr(:(=), syms[1], li.val))
      locex[li] = syms[1]
      println("$syms    $(li.val)")
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

function blockdiff(bl::ForBlock, dmap, g)
  # create Loc for iteration variable
  # ixl = copy( bl.asc[1] )
  # push!(g.locs, ixl)
  ixl = bl.asc[1]

  # create Loc for iteration range
  # rgl = copy( bl.asc[2] )
  # push!(g.locs, rgl)
  rgl = bl.asc[2]

  # create ForBlock
  symbols = Dict{Any, Loc}() # no need for symbols
  thisblock = ForBlock(Op[], Op[], copy(bl.symbols), Loc[ixl, rgl], Loc[])

  pos = length(bl.ops) # start at the last position
  thisblock.ops  = _diff(bl.ops, pos, dmap, g)
  thisblock.lops = remap(bl.lops, dmap)

  thisblock.asc, thisblock.desc = summarize(thisblock)
  thisblock
end