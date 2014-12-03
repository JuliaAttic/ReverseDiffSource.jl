#########################################################################
#
#    Graph simplification
#      Walks the tree to 
#           - fuse duplicates
#           - apply simplification rules  
#
#########################################################################

function simplify!(g::ExGraph, emod = Main)

	i = 1
	markalloc!(g)
	while i <= length(g.nodes)
		restart = false
		n = g.nodes[i]

		restart = any(n2 -> identical(n, n2, g), g.nodes[i+1:end]) ||
			evalconstants(n, g, emod) ||
			rule1(n, g) ||
			rule2(n, g) ||
			rule3(n, g) ||
			rule4(n, g) ||
			rule5(n, g) ||
			rule6(n, g) ||
			rule7(n, g) ||
			rule8(n, g) ||
			rule9(n, g) ||
			rule10(n, g)
		
		if restart
			markalloc!(g)
			i = 1
		else
			i += 1
		end
	end

	# separate pass on subgraphs
	map( n -> simplify!(n.main[2], emod), 
		filter(n->isa(n, NFor), g.nodes))

	
	g
end

## mark nodes that can't be fused because they are modified by a setindex/setfield or a for loop
function markalloc!(g::ExGraph)
	for n in g.nodes
		if isa(n, Union(NSRef, NSDot))
			n.parents[1].alloc = true

		elseif isa(n, NFor)
			g2 = n.main[2]  # subgraph
			syms = collect(values(g2.seto))
			sn = collect(keys(filter((k,v) -> v in syms, g2.exto.kv)))
			for n2 in sn
				n2.alloc = true
			end

		else
			n.alloc = false	
		end
	end
end

## fusion of identical nodes
function identical(n,n2,g)
	n.main != n2.main       && return false
	n.parents != n2.parents && return false
	n.alloc	                && return false
	n2.alloc	            && return false
	# isa(n, NConst) && isa(n.main, Real) && return false # no need for small constants

	fusenodes(g, n, n2)
	true
end

### calculate constant nodes 
#  only if they reduce to a real (zeros(..), etc. should not be converted)
# TODO : check that externals point to a constant in upper levels ?
function evalconstants(n, g, emod)
	!isa(n, NCall)                       && return false
	# any(m -> !isa(m, NConst), n.parents) && return false
	vals = similar(n.parents, Any)
	for (i,p) in enumerate(n.parents)
		vals[i] = constequiv(p,g)
		vals[i] == nothing && return false
	end
	# any(p -> constequiv(p,g) == nothing, n.parents) && return false

	# calculate value
	res = 0.
	try
		# res = apply(emod.eval(n.main), [ x.main for x in n.parents]...)
		res = apply(emod.eval(n.main), vals...)
	catch e
		println("error $e \n while evaluating $(n.main) on $([ x.main for x in n.parents]')")
		rethrow(e)
	end

	!isa(res, Real) && return false

	# create a new constant node and replace n with it
	nn = addnode!(g, NConst(res) )
	fusenodes(g, nn, n) 
	true
end


## right neutral element
function rule1(n, g)
	!isa(n, NCall)             && return false
	(length(n.parents) != 2)   && return false # restricted to binary ops
	val = constequiv(n.parents[2], g)
	(val == nothing)           && return false
	# !isa(n.parents[2], NConst) && return false

	if val == 0 && in(n.main, [:+, :-, :.+, :.-])
		fusenodes(g, n.parents[1], n)
		return true

	elseif val == 1 && in(n.main, [:*, :/, :^, :.*, :./, :.^])
		fusenodes(g, n.parents[1], n)
		return true

	else
		return false
	end
end

## right zero element
function rule2(n, g)
	!isa(n, NCall)             && return false
	(length(n.parents) != 2)   && return false # restricted to binary ops
	val = constequiv(n.parents[2], g)
	(val == nothing)           && return false
	# !isa(n.parents[2], NConst) && return false

	if val == 0 && in(n.main, [:*, :.*])
		nn = addnode!(g, NConst(0.0) )
		fusenodes(g, nn, n)
		return true

	else
		return false
	end
end

## left neutral element
function rule3(n, g)
	!isa(n, NCall)             && return false
	(length(n.parents) != 2)   && return false # restricted to binary ops
	val = constequiv(n.parents[1], g)
	(val == nothing)           && return false
	# !isa(n.parents[1], NConst) && return false

	if val == 0 && in(n.main, [:+, :.+])
		fusenodes(g, n.parents[2], n)
		return true

	elseif val == 1 && in(n.main, [:*, :.*])
		fusenodes(g, n.parents[2], n)
		return true

	else
		return false
	end
end

## left zero element
function rule4(n, g)
	!isa(n, NCall)             && return false
	(length(n.parents) != 2)   && return false # restricted to binary ops
	val = constequiv(n.parents[1], g)
	(val == nothing)           && return false
	# !isa(n.parents[1], NConst) && return false

	if val == 0 && in(n.main, [:*, :/, :^, :.*, :./, :.^])
		nn = addnode!(g, NConst(0.0) )
		fusenodes(g, nn, n) 
		return true

	else
		return false
	end
end

## setindex on same getindex
function rule5(n, g)
	!isa(n, NSRef)                              && return false
	!isa(n.parents[2], NRef)                    && return false
	n2 = n.parents[2]
	(n.parents[1] != n2.parents[1])             && return false
	# check that indexing is the same
	(length(n.parents)-1 != length(n2.parents))   && return false
	!all( n.parents[3:end] .== n2.parents[2:end]) && return false

	fusenodes(g, n.parents[1], n)
	true
end

## setfield on same getfield
function rule6(n, g)
	!isa(n, NSDot)                            && return false
	!isa(n.parents[2], NDot)                  && return false
	(n.main != n.parents[2].main)             && return false
	(n.parents[1] != n.parents[2].parents[1]) && return false

	fusenodes(g, n.parents[1], n)
	true
end

## getindex on zeros()
function rule7(n, g)
	!isa(n, NRef)                               && return false
	p = n.parents[1]
	!isa(p, NCall)                              && return false
	p.main != :zeros                            && return false
	any(x -> !isa(x, NConst), n.parents[2:end]) && return false

	nn = addnode!(g, NConst(0.0) )
	fusenodes(g, nn, n)
	true
end


## change (-1 * x)  to  (-x) 
function rule8(n, g)
	!isa(n, NCall)             && return false
	(length(n.parents) != 2)   && return false # restricted to binary ops
	!in(n.main, [:*, :.*])     && return false
	!isa(n.parents[1], NConst) && return false
	(n.parents[1].main != -1)  && return false

	nn = addnode!(g, NCall(:-, [n.parents[2]]) )
	fusenodes(g, nn, n)
	true
end

## change (x * -1)  to  (-x) 
function rule9(n, g)
	!isa(n, NCall)             && return false
	(length(n.parents) != 2)   && return false # restricted to binary ops
	!in(n.main, [:*, :.*])     && return false
	!isa(n.parents[2], NConst) && return false
	(n.parents[2].main != -1)  && return false

	nn = addnode!(g, NCall(:-, [n.parents[1]]) )
	fusenodes(g, nn, n)
	true
end


# TODO : propagate to grand-parent graph etc...
function constequiv(n, g)
	isa(n, NConst)          && return n.main
	!isa(n, NExt)           && return nothing

	sym = get(g.exti.kv, n, nothing)
	(sym == nothing)        && return nothing
	!haskey(g.exto.vk, sym) && return nothing
	haskey(g.seto.vk, sym)  && return nothing

	!isa(g.exto.vk[sym], NConst)  && return nothing
	g.exto.vk[sym].main
end

## getindex on fill()
function rule10(n, g)
	!isa(n, NRef)                               && return false
	p = n.parents[1]
	!isa(p, NCall)                              && return false
	p.main != :fill                             && return false
	val = constequiv(p.parents[1], g)
	(val == nothing)                            && return false

	any(x -> !isa(x, NConst), n.parents[2:end]) && return false

	nn = addnode!(g, NConst(val) )
	fusenodes(g, nn, n)
	true
end