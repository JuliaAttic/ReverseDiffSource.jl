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
			rule6(n, g)
		
	    i = restart ? 1 : (i + 1)
	end

	# separate pass on subgraphs
	map( n -> simplify!(n.main[2], emod), 
		filter(n->isa(n, NFor), g.nodes))
end

## fusion of identical nodes
function identical(n,n2,g)
	n.main != n2.main       && return false
	n.parents != n2.parents && return false
	isa(n2, NAlloc)         && return false

	fusenodes(g, n, n2)
	true
end

## calculate constant nodes
# TODO : check that externals point to a constant in upper levels ?
function evalconstants(n, g, emod)
	!isa(n, NCall)                       && return false
	any(m -> !isa(m, NConst), n.parents) && return false
	# keep the function form for these
	n.main in [:zeros, :ones, :vcat]     && return false 

	# calculate value
	# TODO : add error catching here
	res = invoke(emod.eval(n.main), 
        tuple([ typeof(x.main) for x in n.parents]...),
        [ x.main for x in n.parents]...)

	# create a new constant node and replace n with it
	nn = addnode!(g, NConst(res) )
	fusenodes(g, nn, n) 
	true
end


## right neutral element
function rule1(n, g)
	!isa(n, NCall)             && return false
	(length(n.parents) != 2)   && return false # restricted to binary ops
	!isa(n.parents[2], NConst) && return false

	if n.parents[2].main == 0 && in(n.main, [:+, :-, :.+, :.-])
		fusenodes(g, n.parents[1], n)
		return true

	elseif n.parents[2].main == 1 && in(n.main, [:*, :/, :^, :.*, :./, :.^])
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
	!isa(n.parents[2], NConst) && return false

	if n.parents[2].main == 0 && in(n.main, [:*, :.*])
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
	!isa(n.parents[1], NConst) && return false

	if n.parents[1].main == 0 && in(n.main, [:+, :.+])
		fusenodes(g, n.parents[2], n)
		return true

	elseif n.parents[1].main == 1 && in(n.main, [:*, :.*])
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
	!isa(n.parents[1], NConst) && return false

	if n.parents[1].main == 0 && in(n.main, [:*, :/, :^, :.*, :./, :.^])
		nn = addnode!(g, NConst(0.0) )
		fusenodes(g, nn, n) 
		return true

	else
		return false
	end
end

## setindex on same getindex
function rule5(n, g)
	!isa(n, NSRef)                            && return false
	!isa(n.parents[2], NRef)                  && return false
	(n.main != n.parents[2].main)             && return false
	(n.parents[1] != n.parents[2].parents[1]) && return false

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

## getindex on ones() or zeros()
# function rule7(n, g)
# 	!isa(n, NRef)                             && return false
# 	!isa(n.parents[2], NDot)                  && return false
# 	(n.main != n.parents[2].main)             && return false
# 	(n.parents[1] != n.parents[2].parents[1]) && return false

# 	fusenodes(g, n.parents[1], n)
# 	true
# end

 