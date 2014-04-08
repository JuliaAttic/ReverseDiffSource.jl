#########################################################################
#
#    Reverse diff on graph
#
#########################################################################

###### creates reverse mode diff graph ######
function reversegraph(g::ExGraph, exitnode::ExNode, diffsym::Array{Symbol})
	g2 = ExGraph()
	createzeronodes!(g2, g, exitnode)

	# store in setmap the nodes containing the derivatives of diffsym
	for (k,v) in filter((k,v) -> isa(k, NExt) & in(k.main, diffsym), g2.inmap)
		g2.setmap[dprefix(k.main)] = v
	end

	reversepass!(g2, g)


    g2
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
		gf = n.main[2]
		is = n.main[1].args[1]

		gf2 = ExGraph()
		createzeronodes!(gf2, gf, NConst(0.))  # ça va péter
		reversepass!(gf2, gf)

		v2 = add_node(g2, NFor([ n.main[1], gf2]) )

		# update inmap by replacing symbol with corresponding outer node in this graph
		# dict key is the node in subgraph, and dict value is the node in parent graph
		for (inode, sym) in gf2.inmap
			if sym==is   # index var should be removed
				delete!(gf2.inmap, inode)
			else
				# pn = gf2.setmap[sym]  # look in setmap, externals or create it
				# gf2.inmap[inode] = pn
				# push!(v2.parents, pn) # mark as parent of for loop
				# println("[subgraph inmap] inner $inode linked outer $pn")
			end
		end

		# update outmap by replacing symbol with corresponding outer node in this graph
		for (inode, sym) in gf2.outmap
			if sym==is   # index var should be removed
				delete!(gf2.inmap, inode)
			else
				# println("[subgraph outmap] inner $inode sets $sym")
				# pn = explore(sym)  # create node if needed
				rn = add_node(g, NIn(sym, [v2]))  # exit node for this var in this graph
				gf2.outmap[inode] = rn
				g.setmap[sym] = rn      # signal we're setting the var
			end
		end
	end

	evalsort!(g)
	map(rev, reverse(g.nodes))
end