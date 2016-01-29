################################################################################
#
#   'if' block material for parsing, differentiating, etc...
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
Type `IfBlock` contains the `true` and `false` blocks of an if-then-else
statement.

  - trueops : a `Vector{Op}` describing the operations if condition is true
  - falseops : a `Vector{Op}` describing the operations if condition is false
  - collector : a `Vector{Op}` describing the operations if condition is false
  - symbols : a `Dict{Any, Loc}` giving the mapping between symbols and their
  - asc  : used Locs, with condition expression in pos #1
  - desc : generated Locs
"""
type IfBlock <: AbstractBlock
  trueops::Vector{Op}
  falseops::Vector{Op}
  collector::Vector{Op}
  symbols::Dict{Any, Loc}
  asc::Vector{Loc}  # parent Loc (block arguments)
  desc::Vector{Loc} # descendant Loc (Loc modified/created by block)
end

getops(bl::IfBlock) = Any[bl.trueops,bl.falseops,bl.collector]
flatops(bl::IfBlock) = vcat(map(flatops, getops(bl))...)

function summarize(bl::IfBlock)
  fops = flatops(bl)
  asc  = mapreduce(o ->   o.asc, union, Set{Loc}(), fops)
  # keep condition in correct position
  asc  = vcat(bl.asc[1], setdiff(asc, [bl.asc[1];]))

  desc = mapreduce(o ->  o.desc, union, Set{Loc}(), fops)

  collect(asc), collect(desc)
end

function prune!(bl::IfBlock, keep::Set{Loc})
  prune!(bl.collector, keep)  # start with collector
  prune!(bl.trueops, keep)
  prune!(bl.falseops, keep)
	bl.asc, bl.desc = summarize(bl)
end

function show(io::IO, bl::IfBlock)
  nt = length(bl.trueops)
  nf = length(bl.falseops)
  nc = length(bl.collector)
  print(io, "If Block $nt+$nf+$nc ops")
end

function remap(bl::IfBlock, lmap)
  IfBlock(remap(  bl.trueops, lmap),
          remap( bl.falseops, lmap),
          remap(bl.collector, lmap),
          [ s => lmap[l] for (s,l) in bl.symbols ],
          Loc[ lmap[l] for l in  bl.asc  ],
          Loc[ lmap[l] for l in  bl.desc ] )
end

chose(a,b) = a  # dummy function to use in collector block

function blockparse!(ex::ExIf, parentops, parentsymbols, g::Graph)
  # explore condition expression in the parentblock
  condl = addtoops!(ex.args[1], parentops, parentsymbols, g)

  thisblock = IfBlock(Op[], Op[], Op[], parentsymbols, Loc[condl;], Loc[])

  tsyms = copy(parentsymbols)
  exitloc = addtoops!(ex.args[2],  thisblock.trueops, tsyms, g) # parse true block
  exitloc != nothing && ( tsyms[EXIT_SYM] = exitloc )
  # for externals found update symbols in parentblock
  for (s,l) in tsyms
    loctype(l) == :external || continue
    haskey(parentsymbols, s) && continue
    parentsymbols[s] = l
  end

  fsyms = copy(parentsymbols)
  if length(ex.args) == 3 # check that there is an else clause
    exitloc = addtoops!(ex.args[3], thisblock.falseops, fsyms, g) # parse false block
    exitloc != nothing && ( fsyms[EXIT_SYM] = exitloc )
    for (s,l) in fsyms
      loctype(l) == :external || continue
      haskey(parentsymbols, s) && continue
      parentsymbols[s] = l
    end
  end

  ### generate collector, for all changed bindings
  for s in union(keys(tsyms), keys(fsyms))
    s == EXIT_SYM && continue

    oloc = get(parentsymbols, s, nothing)
    tloc = get(tsyms, s, nothing)
    floc = get(fsyms, s, nothing)

    oloc == tloc == floc && continue # binding unchanged, pass

    # new value
    nloc = oloc==nothing ? tloc==nothing ? floc : tloc : oloc

    loctype(nloc) in [:external,:constant] && continue # no impact if external, pass

    tloc==nothing && oloc==nothing && continue # variable cannot be used if cond true, pass
    floc==nothing && oloc==nothing && continue # variable cannot be used if cond false, pass

    fop = CLoc(chose)
    push!(g.locs, fop)
    cloc = RLoc(nloc.val)
    push!(g.locs, cloc)

    tloc==nothing && (tloc = oloc)
    floc==nothing && (floc = oloc)
    println("symbol $s  $oloc $tloc $floc $cloc -- $nloc ($(typeof(nloc)))")
    push!(thisblock.collector, FOp(fop, Loc[tloc,floc], [cloc;]))

    parentsymbols[s] = cloc
  end

  thisblock.asc, thisblock.desc = summarize(thisblock)

  push!(parentops, thisblock)

  nothing  # TODO : treat cases where if return value is exploited
end

function blockcode(bl::IfBlock, locex, symbols, g::Graph)
  tsyms, texits = copy(symbols), Loc[]
  fsyms, fexits = copy(symbols), Loc[]
  for o in bl.collector
    tl, fl, rl = o.asc[1], o.asc[2], o.desc[1]
    push!(texits, tl)
    push!(fexits, fl)
    ns = newvar()
    tsyms = filter!((k,v)-> v != tl, tsyms)
    tsyms[ns] = tl
    fsyms = filter!((k,v)-> v != fl, fsyms)
    fsyms[ns] = fl

    locex[rl] = ns
  end

  trueex  = _tocode( bl.trueops, texits, tsyms, g, locex)
  falseex = _tocode(bl.falseops, fexits, fsyms, g, locex)

  [Expr(:if, locex[bl.asc[1]], trueex, falseex) ;]
end

function blockdiff(bl::IfBlock, dmap, g)
  condl = bl.asc[1] # condition

  # create IfBlock
  thisblock = IfBlock(Op[], Op[], Op[], Dict{Any,Loc}(), Loc[condl;], Loc[])

  l = g.block.symbols[EXIT_SYM]
  tdmap = merge(dmap, [ o.asc[1] => dmap[o.desc[1]] for o in bl.collector])
  # tdmap = copy(dmap)
  # for o in bl.collector
  #   tdmap[o.asc[1]] = dmap[o.desc[1]]
  # end
  thisblock.trueops   = _diff( bl.trueops, length( bl.trueops), tdmap, g)

  fdmap = merge(dmap, [ o.asc[2] => dmap[o.desc[1]] for o in bl.collector])
  thisblock.falseops  = _diff(bl.falseops, length(bl.falseops), fdmap, g)

  # changed dmaps go in the if-collector
  for sloc in union(keys(tdmap), keys(fdmap))
    oloc = get( dmap, sloc, nothing)
    tloc = get(tdmap, sloc, nothing)
    floc = get(fdmap, sloc, nothing)

    oloc == tloc == floc && continue # deriv unchanged, pass

    # new value
    nloc = oloc==nothing ? tloc==nothing ? floc : tloc : oloc

    # FIXME : these cases should be managed instead of ignored
    tloc==nothing && oloc==nothing && continue # variable cannot be used if cond true, pass
    floc==nothing && oloc==nothing && continue # variable cannot be used if cond false, pass

    fop = CLoc(+) ; push!(g.locs, fop)
    cloc = RLoc(nloc.val) ; push!(g.locs, cloc)
    tloc==nothing && (tloc = oloc)
    floc==nothing && (floc = oloc)
    push!(thisblock.collector, FOp(fop, Loc[tloc,floc], [cloc;]))

    dmap[sloc] = cloc
    # println("symbol $sloc  $oloc $tloc $floc $cloc -- $nloc ($(typeof(nloc)))")
  end

  thisblock.asc, thisblock.desc = summarize(thisblock)
  thisblock
end
