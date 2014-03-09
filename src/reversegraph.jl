#########################################################################
#
#    Reverse diff on graph
#
#########################################################################

###### creates reverse mode diff graph ######

function reversegraph(g::ExGraph, exitnode::ExNode, diffsym::Array{Symbol})

	g2 = ExGraph()
	vdict = Dict()

	# creates the starting points for derivatives variables
	for n in g.nodes # n = g.nodes[3]
		# exit node, which should always be a Real
		if n == exitnode
			vdict[n] = add_node(g2, :constant, 1.0)

		# Real
		elseif isa(n.val, Real)
			vdict[n] = add_node(g2, :constant, 0.0)
		
		# Array of Real
		elseif any( map(t->isa(n.val,t), [Array{Float64}, Array{Int}]) )
			v1 = add_node(g2, :call, :size, [n])
			vdict[n] = add_node(g2, :alloc, :zeros, [v1])  
			# TODO : alloc necessary only if diffsym ?

		# Composite type
		elseif haskey(tdict, typeof(n.val))   # composite type
			v1 = add_node(g2, :constant, tdict[typeof(n.val)])
			vdict[n] = add_node(g2, :alloc, :zeros, [v1])  
			# TODO : alloc necessary only if diffsym ?

		# Array of composite type
		elseif isa( n.val, Array) && haskey(tdict, eltype(n.val))  
			v1 = add_node(g2, :call, :size, [n])
			# TODO : alloc necessary only if diffsym ?
			aa = ExNode[ add_node(g2, :alloc, :zeros, [v1]) 
			               for i in 1:(tdict[eltype(n.val)]) ]
			vdict[n] = add_node(g2, :call, :vcat, aa)  

		else
			error("[reversegraph] Unknown variable type $(typeof(n.val))")
		end
	end

	#  now climb the reversed evaluation tree
	evalsort!(g)
	for n in reverse(g.nodes)  
		if isa(n, NCall)
			vargs = [ x.val for x in n.parents ]
			for (index, arg) in zip(1:length(n.parents), n.parents)
	            if !isa(arg, Union(NConst, NComp))

	            	fn = dfuncname(n.main, index)
	            	dg, dd, de = rdict[ eval(Expr(:call, fn, vargs...)) ]

	            	smap = Dict( dd, [n.parents, vdict[n]])

	            	nmap = add_graph!(dg, g2, smap)

            		v2 = add_node(g2, :call, :+, [vdict[arg], nmap[de]])
            		vdict[arg] = v2

	            end
	        end
	    
	    elseif isa(n, NRef)
	        v2 = add_node(g2, :ref, n.main, [vdict[n.parents[1]]])
	        v3 = add_node(g2, :call, :+, [v2, vdict[n]])
			v4 = add_node(g2, :subref, n.main, [vdict[n.parents[1]], v3])
			vdict[n.parents[1]] = v4

	    elseif isa(n, NDot)
	        v2 = add_node(g2, :., n.main, [vdict[n.parents[1]]])
	        v3 = add_node(g2, :call, :+, [v2, vdict[n]])
			v4 = add_node(g2, :subdot, n.main, [vdict[n.parents[1]], v3])
			vdict[n.parents[1]] = v4

	    end
	end

    diffn = [ filter(n -> isa(n, NExt) & (n.main==ds), g.nodes)[1] for ds in diffsym]
    dnodes = map(n -> vdict[n], diffn)
    (g2.nodes, dnodes)
end