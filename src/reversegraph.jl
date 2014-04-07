#########################################################################
#
#    Reverse diff on graph
#
#########################################################################

###### creates reverse mode diff graph ######
function reversegraph(g::ExGraph, exitnode::ExNode, diffsym::Array{Symbol})
	g2 = ExGraph()
	createzeronodes!(g2, g, exitnode)
	reversepass!(g2, g)

    diffn = [ filter(n -> isa(n, NExt) & (n.main==ds), g.nodes)[1] for ds in diffsym]
    dnodes = map(n -> g2.inmap[n], diffn)
    g2, dnodes
end

# creates the starting points for derivatives accumulation variables
function createzeronodes!(g2::ExGraph, g::ExGraph, exitnode::ExNode)
	for n in filter(n-> !isa(n,NFor), g.nodes) # n = g.nodes[3]
		# exit node, which should always be a Real
		if n == exitnode
			g2.inmap[n] = add_node(g2, NConst(1.0))

		# Real
		elseif isa(n.val, Real)
			g2.inmap[n] = add_node(g2, NConst(0.0))
		
		# Array of Real
		elseif isa(n.val, Array{Float64}) | isa(n.val, Array{Int})
			v1 = add_node(g2, NCall(:size, [n]))
			g2.inmap[n] = add_node(g2, NAlloc(:zeros, [v1]))
			# TODO : alloc necessary only if diffsym ?

		# Composite type
		elseif haskey(tdict, typeof(n.val))   # known composite type
			v1 = add_node(g2, NConst( tdict[typeof(n.val)]) )
			g2.inmap[n] = add_node(g2, NAlloc(:zeros, [v1]) )
			# TODO : alloc necessary only if diffsym ?

		# Array of composite type
		elseif isa( n.val, Array) && haskey(tdict, eltype(n.val))  
			v1 = add_node(g2, NCall(:size, [n]) )
			# TODO : alloc necessary only if diffsym ?
			aa = ExNode[ add_node(g2, NAlloc(:zeros, [v1]) )
			               for i in 1:(tdict[eltype(n.val)]) ]
			g2.inmap[n] = add_node(g2, NCall(:vcat, aa) )

		else
			error("[reversegraph] Unknown variable type $(typeof(n.val))")
		end
	end
end

#  climbs the reversed evaluation tree
function reversepass!(g2::ExGraph, g::ExGraph)

	rev(n::ExNode) = nothing  # do nothing

	function rev(n::NCall)
		vargs = [ x.val for x in n.parents ]
		for (index, arg) in zip(1:length(n.parents), n.parents)
            if !isa(arg, Union(NConst, NComp))

            	fn = dfuncname(n.main, index)
            	dg, dd, de = rdict[ eval(Expr(:call, fn, vargs...)) ]

            	smap = Dict( dd, [n.parents, g2.inmap[n]])

            	nmap = add_graph!(dg, g2, smap)

        		v2 = add_node(g2, NCall(:+, [g2.inmap[arg], nmap[de]]) )
        		g2.inmap[arg] = v2

            end
        end
	end		 

	function rev(n::NRef)
        v2 = add_node(g2, NRef(n.main, [g2.inmap[n.parents[1]]]) )
        v3 = add_node(g2, NCall(:+, [v2, g2.inmap[n]]) )
		v4 = add_node(g2, NSRef(n.main, [g2.inmap[n.parents[1]], v3]) )
		g2.inmap[n.parents[1]] = v4
	end

	function rev(n::NDot)
        v2 = add_node(g2, NDot( n.main, [g2.inmap[n.parents[1]]]) )
        v3 = add_node(g2, NCall(:+, [v2, g2.inmap[n]]) )
		v4 = add_node(g2, NSDot(n.main, [g2.inmap[n.parents[1]], v3]) )
		g2.inmap[n.parents[1]] = v4
	end

	function rev(n::NFor)
		g3 = n.main[2]
		g2.inmap2 = createzeronodes(g3) 
		g4 = reversepass(g3)

		# update inmap by replacing symbol with corresponding outer node in this graph
		# dict key is the node in subgraph, and dict value is the node in parent graph
		for (inode, sym) in g2.inmap
			if sym==is   # index var should be removed
				delete!(g2.inmap, inode)
			else
				pn = explore(sym)  # look in setmap, externals or create it
				g2.inmap[inode] = pn
				push!(nf.parents, pn) # mark as parent of for loop
				# println("[subgraph inmap] inner $inode linked outer $pn")
			end
		end

		# update outmap by replacing symbol with corresponding outer node in this graph
		for (inode, sym) in g2.outmap
			if sym==is   # index var should be removed
				delete!(g2.inmap, inode)
			else
				# println("[subgraph outmap] inner $inode sets $sym")
				pn = explore(sym)  # create node if needed
				rn = add_node(g, NIn(sym, [nf]))  # exit node for this var in this graph
				g2.outmap[inode] = rn
				g.setmap[sym] = rn      # signal we're setting the var
			end
		end

		v = add_node(g2, NFor([n.main[1], [g3.nodes, g4]], n.inmap, n.outmap, Dict()) )
		# g2.inmap[n.parents[1]] = v4
	end

	evalsort!(g)
	map(rev, reverse(g.nodes))
end