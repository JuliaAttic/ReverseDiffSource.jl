#########################################################################
#
#    Reverse diff on graph
#
#########################################################################

###### creates reverse mode diff graph ######

function reversegraph(g::ExGraph, exitnode::ExNode, diffsym::Array{Symbol})
	vdict = Dict()
	vdict[exitnode] = 
	vdict = createzeronodes(g)
	g2 = reversepass(g, vdict)

    diffn = [ filter(n -> isa(n, NExt) & (n.main==ds), g.nodes)[1] for ds in diffsym]
    dnodes = map(n -> vdict[n], diffn)
    (g2.nodes, dnodes)
end

# creates the starting points for derivatives variables
function createzeronodes(g::ExGraph, vdict=Dict())
	for n in filter(n-> !isa(n,NFor), g.nodes) # n = g.nodes[3]
		# exit node, which should always be a Real
		if n == exitnode
			vdict[n] = add_node(g2, NConst(1.0))

		# Real
		elseif isa(n.val, Real)
			vdict[n] = add_node(g2, NConst(0.0))
		
		# Array of Real
		elseif isa(n.val, Array{Float64}) | isa(n.val, Array{Int})
			v1 = add_node(g2, NCall(:size, [n]))
			vdict[n] = add_node(g2, NAlloc(:zeros, [v1]))
			# TODO : alloc necessary only if diffsym ?

		# Composite type
		elseif haskey(tdict, typeof(n.val))   # known composite type
			v1 = add_node(g2, NConst( tdict[typeof(n.val)]) )
			vdict[n] = add_node(g2, NAlloc(:zeros, [v1]) )
			# TODO : alloc necessary only if diffsym ?

		# Array of composite type
		elseif isa( n.val, Array) && haskey(tdict, eltype(n.val))  
			v1 = add_node(g2, NCall(:size, [n]) )
			# TODO : alloc necessary only if diffsym ?
			aa = ExNode[ add_node(g2, NAlloc(:zeros, [v1]) )
			               for i in 1:(tdict[eltype(n.val)]) ]
			vdict[n] = add_node(g2, NCall(:vcat, aa) )

		else
			error("[reversegraph] Unknown variable type $(typeof(n.val))")
		end
	end
	vdict	
end

#  climbs the reversed evaluation tree
function reversepass(g::ExGraph, vdict::Dict{ExNode, ExNode})

	rev(n::ExNode) = ()  # do nothing

	function rev(n::NCall)
		vargs = [ x.val for x in n.parents ]
		for (index, arg) in zip(1:length(n.parents), n.parents)
            if !isa(arg, Union(NConst, NComp))

            	fn = dfuncname(n.main, index)
            	dg, dd, de = rdict[ eval(Expr(:call, fn, vargs...)) ]

            	smap = Dict( dd, [n.parents, vdict[n]])

            	nmap = add_graph!(dg, g2, smap)

        		v2 = add_node(g2, NCall(:+, [vdict[arg], nmap[de]]) )
        		vdict[arg] = v2

            end
        end
	end		 

	function rev(n, NRef)
        v2 = add_node(g2, NRef(n.main, [vdict[n.parents[1]]]) )
        v3 = add_node(g2, NCall(:+, [v2, vdict[n]]) )
		v4 = add_node(g2, NSRef(n.main, [vdict[n.parents[1]], v3]) )
		vdict[n.parents[1]] = v4
	end

	function rev(n, NDot)
        v2 = add_node(g2, NDot( n.main, [vdict[n.parents[1]]]) )
        v3 = add_node(g2, NCall(:+, [v2, vdict[n]]) )
		v4 = add_node(g2, NSDot(n.main, [vdict[n.parents[1]], v3]) )
		vdict[n.parents[1]] = v4
	end

	function rev(n::NFor)
		g3 = n.main[2]
		vdict2 = createzeronodes(g3) 
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
		# vdict[n.parents[1]] = v4

	end

	evalsort!(g)
	g2 = ExGraph()

	map(rev, reverse(g.nodes))

	g2
end