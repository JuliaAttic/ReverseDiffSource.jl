#########################################################################
#
#    Graph simplification
#        - keeps ops necessary for exit variables specified in 'keep'
#        - splits n-ary ops into binary ops
#        - calculates constants
#        - applies multiple simplifications heuristics
#
#########################################################################

function simplify!(g, keeps=[EXIT_SYM;]) # g = A.g ; keep = [A.EXIT_SYM;]

	keeps2 = intersect(keeps, keys(g.block.symbols))
	keep = Set{Loc}([ g.block.symbols[s] for s in keeps2])

	prune!(g, keep)
	splitnary!(g)

	fusecopies!(g)
	removerightneutral!(g)
	removeleftneutral!(g)
	prune!(g, keep)
	g
end



#######################################################

flatops(op::Op)            = [op;]
flatops(bl::AbstractBlock) = vcat(map(flatops, getops(bl))..., bl)
flatops(ops::Vector{Op})   = vcat(map(flatops, ops)...)
flatops(g::Graph)          = flatops(g.block)


type Walk
  start::Union{Op, Void}
  g::Graph
end

Walk(g::Graph) = Walk(nothing, g) # start from top

function start(w::Walk) # w = Walk(g.block.ops[2], g)
  ops = flatops(w.g)
  pos = w.start==nothing ? 1 : findfirst(o -> o == w.start, ops) + 1
  pos, ops
end

next(w::Walk, state) = (state[2][state[1]], (state[1]+1, state[2]))
done(w::Walk, state) = state[1] == length(state[2])+1

type RevWalk
  start::Union{Op, Void}
  g::Graph
end

RevWalk(g::Graph) = RevWalk(nothing, g) # start from top

function start(w::RevWalk) # w = RevWalk(g.block.ops[2], g)
	ops = flatops(w.g)
  pos = w.start==nothing ? length(ops) : findfirst(o -> o == w.start, ops) - 1
  pos, ops
end

next(w::RevWalk, state) = (state[2][state[1]], (state[1]-1, state[2]))
done(w::RevWalk, state) = state[1] == 0

# removes unecessary elements (as specified by 'keep')
# function prune!(g, keep)
# 	# find all locs that are relevant to calculate 'keep'
# 	keep2 = intersect(keep, keys(g.block.symbols))
# 	lset = Set{Loc}([ g.block.symbols[s] for s in keep2])
#
# 	for o in RevWalk(g)
# 		if any(l -> l in o.desc, lset)
# 			union!(lset, o.asc)
# 			isa(o, FOp) && push!(lset, o.f)
# 		end
# 	end
#
# 	# filter all locs, symbols, ops unrelated to lset
# 	filter!(l -> l in lset, g.locs)
#
# 	for bl in allblocks(g)
# 		filter!((s,l) -> l in lset, bl.symbols)
# 		for ops in getops(bl)
# 			filter!(o -> any(l -> l in o.desc, lset), ops)
# 		end
# 		bl.asc, bl.desc = summarize(bl)
# 	end
#
# 	g
# end


prune!(g::Graph, keep::Set{Loc}) = prune!(g.block, keep)

function prune!(bl::AbstractBlock, keep::Set{Loc})
	del_list = Int64[]
	iop = collect(enumerate(bl.ops))
	for (i, op) in reverse(iop) # i,op = iop[9]
		if any(l -> l in op.desc, keep)
			println("keep $i")
			isa(op, AbstractBlock) && prune!(op, keep)
			union!(keep, op.asc)
		else
			println("remove $i")
			push!(del_list,i)
		end
	end
	deleteat!(bl.ops, reverse(del_list))
	bl.asc, bl.desc = summarize(bl)
end



# splits n-ary functions into binary ones
function splitnary!(g)
	for bl in allblocks(g)
		for ops in getops(bl)
			for (line, o) in enumerate(ops)
				isa(o, FOp) || continue
				o.f.val in [+, *, sum, min, max] || continue
				length(o.asc) > 2 || continue

				nloc = RLoc( (o.f.val)(o.asc[1].val, o.asc[2].val) ) # intermediate
				push!(g.locs, nloc)
				insert!(ops, line, FOp(o.f, o.asc[1:2], [nloc;]))
				o.asc = [nloc;o.asc[3:end]]
			end
		end
		bl.asc, bl.desc = summarize(bl)
	end
	g
end

function isfusable(org::Loc, cpy::Loc, w::Walk)
	# org, cpy, g = o.asc[1], o.desc[1], g
	# if org is external, checks that copy is not mutated
	if loctype(org) == :external
		any(l -> cpy in l.desc, w) && return false
	end

	# is 'org' written to and 'cpy' used afterward ?
	writ = false
	for o2 in w
		writ && cpy in o2.asc && return false
		writ = writ || org in o2.desc
	end

	# is 'cpy' written to and 'org' used afterward ?
	writ = false
	for o2 in w
		writ && org in o2.asc && return false
		writ = writ || cpy in o2.desc
	end

	true
end

# replaces occurrences of 'cpy' by 'org'
function fuse(org::Loc, cpy::Loc, g::Graph) # org, cpy, g = A.g.locs[1], A.g.locs[3], A.g
	for bl in allblocks(g)
		for ops in getops(bl)
			for o in ops
				o.asc[  o.asc  .== cpy ] = org
				o.desc[ o.desc .== cpy ] = org
			end
		end
		for (k,v) in filter((k,v) -> v == cpy, bl.symbols)
			bl.symbols[k] = org
		end
		bl.asc, bl.desc = summarize(bl)
	end
end

# removes redundant 'copy'
function fusecopies!(g)
	for bl in allblocks(g)
		for ops in getops(bl)
			del_list = Int64[]
			for (line, o) in enumerate(ops)
				# line, o, ops = 1, g.block.ops[1], g.block.ops
				# line, o, ops = 1, g.block.ops[1], g.block.ops
				isa(o, FOp) || continue
				o.f.val == copy || continue
				# isfusable(o.asc[1], o.desc[1], g, line+1) || continue
				isfusable(o.asc[1], o.desc[1], Walk(o,g)) || continue

				push!(del_list, line)
				# replace occurences of copy by original
				fuse(o.asc[1], o.desc[1], g)
			end
			deleteat!(ops, del_list)
		end
	end
end

function removerightneutral!(g)
	const conds = [(/, 1.), (./, 1.),
								 (+, 0.), (.+, 0.),
								 (-, 0.), (.-, 0.),
								 (*, 1.), (.*, 1.),
								 (^, 1.), (.^, 1.)]

  for bl in allblocks(g)
		for ops in getops(bl)
			del_list = Int64[]
			for (line, o) in enumerate(ops) # line=1 ; o = ops[1]
				isa(o, FOp) || continue
				length(o.asc) == 2 || continue
				loctype(o.asc[2]) == :constant || continue
				any(cp -> cp==(o.f.val, o.asc[2].val), conds) || continue
				isfusable(o.asc[1], o.desc[1], Walk(o,g)) || continue

				push!(del_list, line)
				# replace occurences of copy by original
				fuse(o.asc[1], o.desc[1], g)
			end
			deleteat!(ops, del_list)
		end
	end
end

function removeleftneutral!(g)
	const conds = [(+, 0.), (.+, 0.),
								 (*, 1.), (.*, 1.)]

  for bl in allblocks(g)
		for ops in getops(bl)
			del_list = Int64[]
			for (line, o) in enumerate(ops) # line=1 ; o = ops[1]
				isa(o, FOp) || continue
				length(o.asc) == 2 || continue
				loctype(o.asc[1]) == :constant || continue
				any(cp -> cp==(o.f.val, o.asc[1].val), conds) || continue
				isfusable(o.asc[2], o.desc[1], Walk(o,g)) || continue

				push!(del_list, line)
				# replace occurences of copy by original
				fuse(o.asc[2], o.desc[1], g)
			end
			deleteat!(ops, del_list)
		end
	end
end



# function simplify!(g::ExGraph, emod = Main)
#
# 	i = 1
# 	evalsort!(g)
# 	markalloc!(g)
# 	while i <= length(g.nodes)
# 		restart = false
# 		n = g.nodes[i]
#
# 		for n2 in g.nodes[i+1:end]
# 			restart = identical(n, n2, g)
# 			restart && break
# 		end
#
# 		restart = restart || evalmoduleref(n, g, emod) ||
# 			evalconstants(n, g, emod) ||
# 			rule1(n, g) ||
# 			rule2(n, g) ||
# 			rule3(n, g) ||
# 			rule4(n, g) ||
# 			rule5(n, g) ||
# 			rule6(n, g) ||
# 			rule7(n, g) ||
# 			rule8(n, g) ||
# 			rule9(n, g) ||
# 			rule10(n, g) ||
# 			rule11(n, g)
#
# 		if restart
# 			markalloc!(g)
# 			i = 1
# 		else
# 			i += 1
# 		end
# 	end
#
# 	# separate pass on subgraphs
# 	for n in filter(n->isa(n, NFor), g.nodes)
# 		simplify!(n.main[2], emod)
# 	end
#
# 	g
# end

# function fuseidentical!(g::ExGraph)
# 	ns = filter(n -> !n.alloc, g.nodes)
# 	nl = collect( zip(ns, map(n -> (n.main, vcat(n.parents, n.precedence)), ns)) )
#     sort!(nl, lt= (a,b) -> length(a[2]) < length(b[2]))
#     sort!(ns, lt= (a,b) -> b[1] in a[2] & !(a[1] in b[2]))
# 	for n in g.nodes
# 		any(n2 -> identical(n, n2, g), g.nodes[i+1:end])
# 	end
# end

## mark nodes that can't be fused because they are modified by a setindex/setfield or a for loop
# function markalloc!(g::ExGraph)
# 	for n in g.nodes
# 		if isa(n, Union{NSRef, NSDot})
# 			n.parents[1].alloc = true
#
# 		elseif isa(n, NFor)
# 			g2 = n.main[2]  # subgraph
# 			for (n2, s2) in g2.exto
# 				hassym(g2.seto, s2) || continue
# 				n2.alloc = true
# 			end
# 		else
# 			n.alloc = false
# 		end
# 	end
# end
#
# # TODO : propagate to grand-parent graph etc...
# function constequiv(n, g)
# 	isa(n, NConst)          && return n.main
# 	!isa(n, NExt)           && return nothing
#
# 	!hasnode(g.exti, n)     && return nothing
# 	sym = g.exti.kv[n]
# 	!hassym(g.exto, sym)    && return nothing
# 	 hassym(g.seti, sym)    && return nothing
#
# 	pn = getnode(g.exto, sym)
# 	!isa(pn, NConst)  && return nothing
# 	pn.main
# end
#
# ## fusion of identical nodes
# function identical(n,n2,g)
# 	!is(n.main, n2.main)    && return false
# 	n.parents != n2.parents && return false
# 	n.alloc	                && return false
# 	n2.alloc	            && return false
#
# 	fusenodes(g, n, n2)
# 	true
# end
#
# ### calculate constant nodes
# #  only if they reduce to a real (zeros(..), etc. should not be converted)
# function evalconstants(n, g, emod)
# 	!isa(n, NCall)                       && return false
# 	vals = similar(n.parents, Any)
# 	for (i,p) in enumerate(n.parents)
# 		vals[i] = constequiv(p,g)
# 		vals[i] == nothing && return false
# 	end
#
# 	# calculate value
# 	res = 0.
# 	try
# 		res = vals[1](vals[2:end]...)
# 	catch e
# 		println("error $e while evaluating $(vals[1])($(vals[2:end]...))")
# 		rethrow(e)
# 	end
#
# 	!isa(res, Real) && return false
#
# 	# create a new constant node and replace n with it
# 	nn = addnode!(g, NConst(res) )
# 	fusenodes(g, nn, n)
# 	true
# end
#
# ### calculate Module.xxx constants nodes
# function evalmoduleref(n, g, emod)
# 	!isa(n, NDot)                    && return false
# 	!isa(n.parents[1], NConst)       && return false
# 	!isa(n.parents[1].main, Module)  && return false
#
# 	# calculate value
# 	res = 0.
# 	ex  = :()
# 	try
# 		ex = Expr(:., n.parents[1].main, n.main )
# 		res = emod.eval( ex )
# 	catch e
# 		println("error $e \n while evaluating $ex")
# 		rethrow(e)
# 	end
#
# 	nn = addnode!(g, NConst( res ) )
# 	fusenodes(g, nn, n)
# 	true
# end
#
#
# ## right neutral element
# function rule1(n, g)
# 	!isa(n, NCall)             && return false
# 	(length(n.parents) != 3)   && return false # restricted to binary ops
# 	val = constequiv(n.parents[3], g)
# 	(val == nothing)           && return false
#
# 	op = n.parents[1].main
# 	if val == 0 && in(op, [+, -, .+, .-])
# 		fusenodes(g, n.parents[2], n)
# 		return true
#
# 	elseif val == 1 && in(op, [*, /, ^, .*, ./, .^])
# 		fusenodes(g, n.parents[2], n)
# 		return true
#
# 	else
# 		return false
# 	end
# end
#
# ## right zero element
# # FIXME : incorrect if left term is an array ?
# function rule2(n, g)
# 	!isa(n, NCall)             && return false
# 	(length(n.parents) != 3)   && return false # restricted to binary ops
# 	val = constequiv(n.parents[3], g)
# 	(val == nothing)           && return false
#
# 	op = n.parents[1].main
# 	if val == 0 && in(op, [*, .*])
# 		nn = addnode!(g, NConst(0.0) )
# 		fusenodes(g, nn, n)
# 		return true
#
# 	else
# 		return false
# 	end
# end
#
# ## left neutral element
# function rule3(n, g)
# 	# n.alloc	                   && return false
# 	!isa(n, NCall)             && return false
# 	(length(n.parents) != 3)   && return false # restricted to binary ops
# 	val = constequiv(n.parents[2], g)
# 	(val == nothing)           && return false
#
# 	op = n.parents[1].main
# 	if val == 0 && in(op, [+, .+])
# 		fusenodes(g, n.parents[3], n)
# 		return true
#
# 	elseif val == 1 && in(op, [*, .*])
# 		fusenodes(g, n.parents[3], n)
# 		return true
#
# 	else
# 		return false
# 	end
# end
#
# ## left zero element
# # FIXME : incorrect if right term is an array ?
# function rule4(n, g)
# 	!isa(n, NCall)             && return false
# 	(length(n.parents) != 3)   && return false # restricted to binary ops
# 	val = constequiv(n.parents[2], g)
# 	(val == nothing)           && return false
#
# 	op = n.parents[1].main
# 	if val == 0 && in(op, [*, /, ^, .*, ./, .^])
# 		nn = addnode!(g, NConst(0.0) )
# 		fusenodes(g, nn, n)
# 		return true
#
# 	else
# 		return false
# 	end
# end
#
# ## setindex on same getindex
# function rule5(n, g)
# 	!isa(n, NSRef)                              && return false
# 	!isa(n.parents[2], NRef)                    && return false
# 	n2 = n.parents[2]
# 	(n.parents[1] != n2.parents[1])             && return false
# 	# check that indexing is the same
# 	(length(n.parents)-1 != length(n2.parents))   && return false
# 	!all( n.parents[3:end] .== n2.parents[2:end]) && return false
#
# 	fusenodes(g, n.parents[1], n)
# 	true
# end
#
# ## setfield on same getfield
# function rule6(n, g)
# 	!isa(n, NSDot)                            && return false
# 	!isa(n.parents[2], NDot)                  && return false
# 	(n.main != n.parents[2].main)             && return false
# 	(n.parents[1] != n.parents[2].parents[1]) && return false
#
# 	fusenodes(g, n.parents[1], n)
# 	true
# end
#
# ## getindex on zeros()
# # FIXME : incorrect if index is a range
# function rule7(n, g)
# 	!isa(n, NRef)                               && return false
# 	p = n.parents[1]
# 	!isa(p, NCall)                              && return false
# 	p.parents[1].main != zeros                  && return false
# 	# any(x -> !isa(x, NConst), n.parents[2:end]) && return false
#
# 	nn = addnode!(g, NConst(0.0) )
# 	fusenodes(g, nn, n)
# 	true
# end
#
# ## change (-1 * x)  to  (-x)
# function rule8(n, g)
# 	!isa(n, NCall)             && return false
# 	(length(n.parents) != 3)   && return false # restricted to binary ops
#
# 	op = n.parents[1].main
# 	!in(op, [*, .*])     && return false
# 	!isa(n.parents[2], NConst) && return false
# 	(n.parents[2].main != -1)  && return false
#
# 	nm = addnode!(g, NConst(-))
# 	nn = addnode!(g, NCall(:call, [nm, n.parents[3]]) )
# 	fusenodes(g, nn, n)
# 	true
# end
#
# ## change (x * -1)  to  (-x)
# function rule9(n, g)
# 	!isa(n, NCall)             && return false
# 	(length(n.parents) != 3)   && return false # restricted to binary ops
#
# 	op = n.parents[1].main
# 	!in(op, [*, .*])           && return false
# 	!isa(n.parents[3], NConst) && return false
# 	(n.parents[3].main != -1)  && return false
#
# 	nm = addnode!(g, NConst(-))
# 	nn = addnode!(g, NCall(:call, [nm, n.parents[2]]) )
# 	fusenodes(g, nn, n)
# 	true
# end
#
# ## getindex on fill()
# # FIXME : incorrect if index is a range
# function rule10(n, g)
# 	!isa(n, NRef)                               && return false
#
# 	p = n.parents[1]
# 	!isa(p, NCall)                              && return false
# 	p.parents[1].main != fill                   && return false
# 	val = constequiv(p.parents[2], g)
# 	(val == nothing)                            && return false
#
# 	# any(x -> !isa(x, NConst), n.parents[3:end]) && return false
#
# 	nn = addnode!(g, NConst(val) )
# 	fusenodes(g, nn, n)
# 	true
# end
#
# ## getindex on ones()
# # FIXME : incorrect if index is a range
# function rule11(n, g)
# 	!isa(n, NRef)                               && return false
# 	p = n.parents[1]
# 	!isa(p, NCall)                              && return false
# 	p.parents[1].main != ones                   && return false
# 	# any(x -> !isa(x, NConst), n.parents[2:end]) && return false
#
# 	nn = addnode!(g, NConst(1.0) )
# 	fusenodes(g, nn, n)
# 	true
# end
