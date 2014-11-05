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
		# elseif isa(n, NSRef) || isa(n, NSDot)  # unique alloc for all NSRef and NSDot on same instance
		# 	@assert haskey(dnodes, n.parents[1]) "[reversegraph] nodes not ordered ?"
		# 	dnodes[n] = dnodes[n.parents[1]]
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
		#  find potential NSRef having n as a parent, so has to base derivative on it
		ins = findfirst( x -> n in x.parents && isa(x, NSRef), reverse(g.nodes) )
		# ins = findfirst( x -> n in x.parents && isa(x, NSRef), g.nodes )
		println("oooo ", n, "   n = ", ins)

		if ins != 0
			ns = reverse(g.nodes)[ins]
			# ns = g.nodes[ins]
			dnodes[n] = dnodes[ns]
			v2 = addnode!(g2, NRef(:getidx, [ dnodes[ns] , n.parents[3:end] ]) )
		else
			v2 = addnode!(g2, NRef(:getidx, [  dnodes[n] , n.parents[3:end] ]) )
		end
		
		# treat case where a single value is allocated to several array elements
		# if length(dnodes[n.parents[2]].val) == 1 
		if length(n.parents[2].val) == 1 
			sz = mapreduce(x -> length(x.val), *, n.parents[3:end])
			if sz > 1
				v2 = addnode!(g2, NCall(:sum, [ v2]))
			end
		end

		v3 = addnode!(g2, NCall(:+, [ dnodes[n.parents[2]], v2 ]) )
		dnodes[n.parents[2]] = v3
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
		for n2 in filter(n-> !isa(n,NFor), fg.nodes)
			# outgoing nodes become ingoing nodes
	 		#   both for the var and its derivative accumulator
			if haskey(fg.seti, n2)
				sym = fg.seti[n2]
				if haskey(fg.seto.vk, sym)  # this is not a local var
					on = fg.seto.vk[sym]

					# assumption : exti / onodes already exists for this sym
 					dsym = dprefix(sym)  # newvar() 

					#  derivative of var
					nn = addnode!(fg2, NExt(dsym))
					fg.exti[nn] = dsym
					fg.exto[dnodes[on]] = dsym
				else  # it is a local var
					nn = createzeronode!(fg2, n2)
				end

			# ingoing nodes become potential outgoing dnodes
			elseif haskey(fg.exti, n2)
				sym = fg.exti[n2]
				if sym != is
					on = fg.exto.vk[sym]
 					dsym = dprefix(sym)  # newvar()

					nn = addnode!(fg2, NExt(dsym))
					fg.exti[nn] = dsym
					fg.exto[dnodes[on]] = dsym

					ndmap[n2] = (dsym, on)
				end

			else
				nn = createzeronode!(fg2, n2)
			end	

			fdnodes[n2] = nn
		end

		# builds the graph for derivatives calculations
		reversepass!(fg2, fg, fdnodes)
		append!(fg.nodes, fg2.nodes)
		
		# variables of interest are derivatives only
		fg.seti = NSMap()
		for (ni, (sym, on)) in ndmap
			fg.seti[ fdnodes[ni] ] = sym
		end

		# println(fg.nodes)
		# println("ndmap = $(collect(ndmap))")
		# println("fdnodes = $(collect(fdnodes))")
		# println("ext = $(collect(fg.exti.kv))")
		# println("set = $(collect(fg.seti.kv))")
		# println("oext = $(collect(fg.exto.kv))")
		# println("oset = $(collect(fg.seto.kv))")

		prune!(fg) # reduce to derivatives evaluation only

		# create for loop
		v2 = addnode!(g2, NFor(Any[ n.main[1], fg ]) )
		v2.parents = [n.parents[1], collect( keys( fg.exto)) ]

		# seto = dnodes of fg's ingoing variables
		fg.seto = NSMap()
		for (ns2, (sym, on)) in ndmap
			rn = addnode!(g2, NIn(sym, [v2]))  # external node, receiving loop result
			fdn = fdnodes[ns2]                 # final node in loop containing derivative
			fg.seto[rn] = sym
			dnodes[on] = rn 
		end
		# TODO : update precedence of v2 here ? 
	end

	evalsort!(g)
	for n2 in reverse(g.nodes)
		rev(n2)
		# for n3 in g2.nodes
		# 	dn = collect(keys(filter( (k,v) -> v == n3, dnodes ) ))
		# 	println(" $n3,  dn = $(repr(dn))")
		# end
		# println(g2)
	end
end


