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
  prune!(bl.trueops, keep)
  prune!(bl.falseops, keep)
  prune!(bl.collector, keep)
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


function blockparse!(ex::ExIf, parentops, parentsymbols, g::Graph)
  # explore condition expression in the parentblock
  condl = addtoops!(ex.args[1], parentops, parentsymbols, g)

  # Same scope as parent => use symbols of parent
  thisblock = IfBlock(Op[], Op[], Op[], parentsymbols, Loc[condl;], Loc[])

  tsyms = copy(parentsymbols)
  exitloc = addtoops!(ex.args[2],  thisblock.trueops, tsyms, g) # parse true block
  exitloc != nothing && ( tsyms[EXIT_SYM] = exitloc )

  fsyms = copy(parentsymbols)
  exitloc = addtoops!(ex.args[3], thisblock.falseops, fsyms, g) # parse false block
  exitloc != nothing && ( fsyms[EXIT_SYM] = exitloc )

  # generate collector
  for s in union(keys(tsyms), keys(fsyms))
    oloc = get(parentsymbols, s, nothing)
    tloc = get(tsyms, s, nothing)
    floc = get(fsyms, s, nothing)

    oloc == tloc == floc && continue # binding unchanged, pass

    # new value
    nloc = oloc==nothing ? tloc==nothing ? floc : tloc : oloc

    loctype(nloc) in [:external,:constant] && continue # no impact if external, pass

    tloc==nothing && oloc==nothing && continue # variable cannot be used if cond true, pass
    floc==nothing && oloc==nothing && continue # variable cannot be used if cond false, pass

    fop = CLoc(+)
    push!(g.locs, fop)
    cloc = RLoc(nloc.val)
    push!(g.locs, cloc)

    tloc==nothing && (tloc = oloc)
    floc==nothing && (floc = oloc)
    println("symbol $s  $oloc $tloc $floc $cloc -- $nloc ($(typeof(nloc)))")
    push!(thisblock.collector, FOp(fop, Loc[tloc,floc], [cloc;]))

    if s == EXIT_SYM
      exitloc = cloc
    else
      parentsymbols[s] = cloc
    end
  end

  thisblock.asc, thisblock.desc = summarize(thisblock)

  push!(parentops, thisblock)

  exitloc
end


function blockcode(bl::IfBlock, locex, symbols, g::Graph)
  # exits = copy(bl.desc)
  # append!(exits, Loc[ op.asc[2] for op in bl.lops])

  tsyms = copy(symbols)
  fsyms = copy(symbols)
  texits = Loc[]
  fexits = Loc[]
  for o in bl.collector
    tl, fl, rl = o.asc[1], o.asc[2], o.desc[1]
    push!(texits, tl)
    push!(fexits, fl)
    ns = newvar()
    tsyms[ns] = tl
    fsyms[ns] = fl
    # locex[rl] = locex[tl] = locex[fl] = ns

  end

  trueex  = _tocode( bl.trueops, texits, tsyms, g, locex)
  println(trueex)
  falseex = _tocode(bl.falseops, fexits, fsyms, g, locex)
  println(falseex)

  [Expr(:if, locex[bl.asc[1]], trueex, falseex) ;]
end

function blockdiff(bl::IfBlock, dmap, g)
  condl = bl.asc[1] # condition

  # create IfBlock
  thisblock = IfBlock(Op[], Op[], copy(bl.symbols), Loc[condl;], Loc[])

  pos = length(bl.trueops) # start at the last position
  thisblock.trueops   = _diff( bl.trueops, length( bl.trueops), dmap, g)
  thisblock.falseops  = _diff(bl.falseops, length(bl.falseops), dmap, g)

  thisblock.asc, thisblock.desc = summarize(thisblock)
  thisblock
end
