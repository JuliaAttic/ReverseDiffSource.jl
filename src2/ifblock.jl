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
  - symbols : a `Dict{Any, Loc}` giving the mapping between symbols and their
  - asc  : used Locs, with condition expression in pos #1
  - desc : generated Locs
"""
type IfBlock <: AbstractBlock
  trueops::Vector{Op}
  falseops::Vector{Op}
  symbols::Dict{Any, Loc}
  asc::Vector{Loc}  # parent Loc (block arguments)
  desc::Vector{Loc} # descendant Loc (Loc modified/created by block)
end

getops(bl::IfBlock) = Any[bl.trueops, bl.falseops]
flatops(bl::IfBlock) = vcat(flatops(bl.trueops),flatops(bl.falseops))

function summarize(bl::IfBlock)
  # note : only `ops` vector considered, `lops` is only implicit assignements
  asc  = mapreduce(o ->   o.asc, union, Set{Loc}(),  bl.trueops)
  asc  = mapreduce(o ->   o.asc, union,        asc, bl.falseops)
  desc = mapreduce(o ->  o.desc, union, Set{Loc}(),  bl.trueops)
  desc = mapreduce(o ->  o.desc, union,       desc, bl.falseops)

  # keep condition in correct position
  asc = vcat(bl.asc[1], setdiff(asc, [bl.asc[1];]))
  collect(asc), collect(desc)
end

function prune!(bl::IfBlock, keep::Set{Loc})
	del_list = Int64[]
	iop = collect(enumerate(bl.trueops))
	for (i, op) in reverse(iop) # i,op = iop[9]
		if any(l -> l in op.desc, keep)
			isa(op, AbstractBlock) && prune!(op, keep)
			union!(keep, op.asc)
		else
			push!(del_list,i)
		end
	end
	deleteat!(bl.trueops, reverse(del_list))

  del_list = Int64[]
  iop = collect(enumerate(bl.falseops))
	for (i, op) in reverse(iop) # i,op = iop[9]
		if any(l -> l in op.desc, keep)
			isa(op, AbstractBlock) && prune!(op, keep)
			union!(keep, op.asc)
		else
			push!(del_list,i)
		end
	end
	deleteat!(bl.falseops, reverse(del_list))

	bl.asc, bl.desc = summarize(bl)
end


function show(io::IO, bl::IfBlock)
  nt = length(bl.trueops)
  nf = length(bl.falseops)
  print(io, "If Block $nt+$nf ops")
end

function remap(bl::IfBlock, lmap)
  IfBlock(remap( bl.trueops, lmap),
          remap(bl.falseops, lmap),
          [ s => lmap[l] for (s,l) in bl.symbols ],
          Loc[ lmap[l] for l in  bl.asc  ],
          Loc[ lmap[l] for l in  bl.desc ] )
end


function blockparse!(ex::ExIf, parentops, parentsymbols, g::Graph)
  # explore loop iterable in the parentblock
  condl = addtoops!(ex.args[1], parentops, parentsymbols, g)

  # Same scope as parent => use symbols of parent
  symbols = parentsymbols

  thisblock = IfBlock(Op[], Op[], symbols, Loc[condl;], Loc[])

  addtoops!(ex.args[2],  thisblock.trueops, symbols, g) # parse loop contents
  addtoops!(ex.args[3], thisblock.falseops, symbols, g) # parse loop contents

  thisblock.asc, thisblock.desc = summarize(thisblock)

  push!(parentops, thisblock)

  get(symbols, EXIT_SYM, nothing)
end


function blockcode(bl::IfBlock, locex, g::Graph)
  exits = copy(bl.desc)
  # append!(exits, Loc[ op.asc[2] for op in bl.lops])

  # expression for inner code
  trueex  = _tocode( bl.trueops, exits, bl.symbols, g, locex)
  falseex = _tocode(bl.falseops, exits, bl.symbols, g, locex)

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
