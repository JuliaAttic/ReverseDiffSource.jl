#########################################################################
#
#    Reverse diff on graph
#
#########################################################################

###### creates reverse mode diff graph ######
function reversegraph(g::ExGraph, exitnode::ExNode, diffsym::Array{Symbol})
	g2     = ExGraph()  # graph that will contain the derivative evaluation

	# create starting nodes
	dnodes = Dict()     # map linking nodes of g to their derivative node in g2
	for n in filter(n-> !isa(n,NFor), g.nodes)
		if n == exitnode
			dnodes[n] = addnode!(g2, NConst(1.0))
		else
			dnodes[n] = createzeronode!(g2, n)
		end
	end

	# builds the graph for derivatives calculations
	reversepass!(g2, g, dnodes)

	# store in setmap the nodes containing the derivatives of diffsym
	for (k,v) in filter((k,v) -> isa(k, NExt) & in(k.main, diffsym), dnodes)
		g2.seti[v] = dprefix(k.main)
	end

    g2
end

# creates the starting points for derivatives accumulation variables
function createzeronode!(g2::ExGraph, n)
	# d_equivnode_1 is the name of function returning dnodes constructors
	#   as defined by calls to the macro @typeequiv

	if method_exists(d_equivnode_1, (typeof(n.val),) )
		rn = invoke(d_equivnode_1, (typeof(n.val),) , n.val)
    	dg, dd, de = rdict[ rn ]
    	smap = [ dd[1] => n ]  # map 'x' node to n
    	exitnode = addgraph!(dg, g2, smap)

    	return exitnode
	
	# try the array of defined types
	elseif (isa(n.val, Array) || isa(n.val, Tuple)) && method_exists(d_equivnode_1, (eltype(n.val),) )
		rn = invoke(d_equivnode_1, (eltype(n.val),) , n.val[1])
    	dg, dd, de = rdict[ rn ]
    	smap = [ dd[1] => n ]  # map 'x' node to n
    	exitnode = addgraph!(dg, g2, smap)

    	v1 = addnode!(g2, NCall(:size, [n]))
		return addnode!(g2, NCall(:fill, [exitnode, v1]))

	end

	error("[reversegraph] Unknown type $(typeof(n.val)) for node $n")
end

#  climbs the reversed evaluation tree
function reversepass!(g2::ExGraph, g::ExGraph, dnodes::Dict)
	# TODO : have a generic treatment of mutating nodes (NSRef / NSDot / NFor ...)

	rev(n::ExNode) = nothing  # do nothing

	function rev(n::NCall)
		vargs = [ x.val for x in n.parents ]
		for (index, arg) in enumerate(n.parents)
			if !isa(arg, Union(NConst, NComp))
            	fn = dfuncname(n.main, index)
            	dg, dd, de = rdict[ eval(Expr(:call, fn, vargs...)) ]

            	smap = Dict( dd, [n.parents, dnodes[n]])

            	exitnode = addgraph!(dg, g2, smap)

        		v2 = addnode!(g2, NCall(:+, [dnodes[arg], exitnode]) )
        		dnodes[arg] = v2
            end
        end
	end		 

	function rev(n::NRef)
        v2 = addnode!(g2, NRef(:getidx,  [ dnodes[n.parents[1]], n.parents[2:end] ]) )
        v3 = addnode!(g2, NCall(:+, [v2, dnodes[n]]) )
		v4 = addnode!(g2, NSRef(:setidx, [ dnodes[n.parents[1]], v3, n.parents[2:end] ]) )
		# TODO : update precedence of v4 here ? can 'dnodes[n.parents[1]' be already a parent elsewhere ?
		dnodes[n.parents[1]] = v4
	end

	function rev(n::NSRef)
		if length(n.parents) >= 3   # regular setindex
			v2 = addnode!(g2, NRef(:getidx, [ dnodes[n] , n.parents[3:end] ]) )

			# treat case where a single value is allocated to several array elements
			if length(n.parents[2].val) == 1 
				sz = mapreduce(x -> length(x.val), *, n.parents[3:end])
				if sz > 1
					v2 = addnode!(g2, NCall(:sum, [ v2]))
				end
			end

			v3 = addnode!(g2, NCall(:+, [ dnodes[n.parents[2]], v2 ]) )
			dnodes[n.parents[2]] = v3

			# shut down the influence of these indices
			zn = addnode!(g2, NConst(0.))
			v4 = addnode!(g2, NSRef(:setidx, [ dnodes[n], zn, n.parents[3:end] ]) )
			v4.precedence = filter(n2 -> dnodes[n] in n2.parents && n2 != v4, g2.nodes)
			dnodes[n.parents[1]] = v4
		else   # scalar assignment
			v3 = addnode!(g2, NCall(:+, [ dnodes[n.parents[2]], dnodes[n] ]) )
			dnodes[n.parents[2]] = v3

			# shut down the influence of the variable
			zn = addnode!(g2, NConst(0.))
			v4 = addnode!(g2, NSRef(:setidx, [ dnodes[n], zn ]) )
			v4.precedence = filter(n2 -> dnodes[n] in n2.parents && n2 != v4, g2.nodes)
			dnodes[n.parents[1]] = v4
		end
	end


	function rev(n::NDot)
        v2 = addnode!(g2, NDot( n.main, [dnodes[n.parents[1]]]) )
        v3 = addnode!(g2, NCall(:+, [v2, dnodes[n]]) )
		v4 = addnode!(g2, NSDot(n.main, [dnodes[n.parents[1]], v3]) )
		# TODO : update precedence of v4 here ? can 'dnodes[n.parents[1]' be already a parent elsewhere ?
		dnodes[n.parents[1]] = v4
	end

	function rev(n::NIn)
		isa(n.parents[1], NFor) && return nothing  # do nothing in the case of for loops

        v2 = addnode!(g2, NCall(:+, [dnodes[n], dnodes[n.parents[1]]]) )
		dnodes[n.parents[1]] = v2
	end

	function rev(n::NFor)
		fg  = copy(n.main[2])      # subgraph of for loop, copied to make new loop
		fg2 = ExGraph()            # will contain dnodes
		is  = n.main[1]            # symbol of loop index

		fdnodes = Dict()
		ndmap = Dict()

		nexti = NSMap()
		nexto = NSMap()
		# nexti = fg.exti
		# nexto = fg.exto
		# outgoing nodes generate ingoing dnodes
		for (n2, sym) in filter((n,s) -> hassym(fg.seto, s), fg.seti.kv)
			on = getnode(fg.seto, sym)
			dsym = newvar(:_dtmp)  # dprefix(sym) 
			#  derivative of var
			nn = addnode!(fg2, NExt(dsym))
			nn.val = "seti"
			nexti[nn] = dsym
			nexto[dnodes[on]] = dsym
			# println("seti : $sym / $dsym : $nn")
			fdnodes[n2] = nn
		end

		# ingoing nodes become potential outgoing dnodes
		for (n2, sym) in filter((n,s) -> hassym(fg.exto, s), fg.exti.kv)
			on = getnode(fg.exto, sym)

			## derivative accumulator
			# if haskey(nexti.vk, dsym)  # already mapped ?
			if hassym(fg.seti, sym)  # already mapped ?
				on2 = getnode(fg.seti, sym)
				nn   = fdnodes[on2]  # nexti.vk[dsym]
				dsym = nexti[nn]
				# println("exti (refused) : $sym / $dsym : $nn")
			else
				dsym = newvar(:_dtmp)  # dprefix(sym)  
				nn = addnode!(fg2, NExt(dsym))
				nn.val = "exti"
				nexti[nn] = dsym
				nexto[dnodes[on]] = dsym

				# println("exti : $sym / $dsym : $nn")
			end
			ndmap[n2] = (dsym, on)
			fdnodes[n2] = nn
		end

		fg.exti = NSMap(merge(fg.exti.kv, nexti.kv))
		fg.exto = NSMap(merge(fg.exto.kv, nexto.kv)) 

		# create regular zeronodes for the remaining fg nodes
		for n2 in filter(n-> !isa(n,NFor) & !haskey(fdnodes, n), fg.nodes)
			nn = createzeronode!(fg2, n2)
			fdnodes[n2] = nn
		end

		# builds the graph for derivatives calculations
		reversepass!(fg2, fg, fdnodes)
		append!(fg.nodes, fg2.nodes)

		# println("after reverse\n$fg")
		# variables of interest are derivatives only
		fg.seti = NSMap()
		for (ni, (sym, on)) in ndmap
			fg.seti[ fdnodes[ni] ] = sym
		end
		# println("after seti update\n$fg")

		prune!(fg) # reduce to derivatives evaluation only
		# println("after pruning\n$fg")

		# create for loop
		nr = addgraph!( :( reverse( x ) ), g2, [ :x => n.parents[1] ] ) # range in reverse order
		v2 = addnode!(g2, NFor(Any[ n.main[1], fg ]) )
		v2.parents = [nr, collect( keys( fg.exto)) ]

		# seto = dnodes of fg's ingoing variables
		fg.seto = NSMap()
		for (ns2, (sym, on)) in ndmap
			rn = addnode!(g2, NIn(sym, [v2]))  # external node, receiving loop result
			fg.seto[rn] = sym
			
			append!(v2.precedence, filter(n -> dnodes[on] in n.parents && n != v2, g2.nodes))
			dnodes[on] = rn 
		end
	end

	evalsort!(g)
	for n2 in reverse(g.nodes)
		rev(n2)
	end
end


