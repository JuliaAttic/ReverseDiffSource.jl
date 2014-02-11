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
		elseif isa(n.value, Real)
			vdict[n] = add_node(g2, :constant, 0.0)
		
		# Array of Real
		elseif any( map(t->isa(n.value,t), [Array{Float64}, Array{Int}]) )
			v1 = add_node(g2, :call, :size, [n])
			vdict[n] = add_node(g2, :alloc, :zeros, [v1])  # TODO : alloc necessary only if diffsym ?

		# Composite type
		elseif haskey(tdict, typeof(n.value))   # composite type
			v1 = add_node(g2, :constant, tdict[typeof(n.value)])
			vdict[n] = add_node(g2, :alloc, :zeros, [v1])  # TODO : alloc necessary only if diffsym ?

		# Array of composite type
		elseif isa( n.value, Array) && haskey(tdict, eltype(n.value))  
			v1 = add_node(g2, :call, :size, [n])
			# TODO : alloc necessary only if diffsym ?
			aa = ExNode[ add_node(g2, :alloc, :zeros, [v1]) for i in 1:(tdict[eltype(n.value)]) ]
			vdict[n] = add_node(g2, :call, :vcat, aa)  

		else
			error("[reversegraph] Unknown variable type $(typeof(n.value))")
		end
	end

	#  now climb the reversed evaluation tree
	evalsort!(g)
	for n in reverse(g.nodes)  
		if n.nodetype == :call
			vargs = [ x.value for x in n.parents ]
			for (index, arg) in zip(1:length(n.parents), n.parents)
	            if !in(arg.nodetype, [:constant, :comp])

	            	fn = dfuncname(n.name, index)
	            	(dg, dd, de) = rdict[ eval(Expr(:call, fn, vargs...)) ]

	            	smap = Dict( dd, [n.parents, vdict[n]])
	            	delete!(smap, nothing)

	            	dres = add_graph!(dg, g2, de, smap)

            		v2 = add_node(g2, :call, :+, [vdict[arg], dres])
            		vdict[arg] = v2

	            end
	        end
	    
	    elseif n.nodetype == :ref
	        v2 = add_node(g2, :ref, n.name, [vdict[n.parents[1]]])
	        v3 = add_node(g2, :call, :+, [v2, vdict[n]])
			v4 = add_node(g2, :subref, n.name, [vdict[n.parents[1]], v3])
			vdict[n.parents[1]] = v4

	    elseif n.nodetype == :dot
	        v2 = add_node(g2, :., n.name, [vdict[n.parents[1]]])
	        v3 = add_node(g2, :call, :+, [v2, vdict[n]])
			v4 = add_node(g2, :subdot, n.name, [vdict[n.parents[1]], v3])
			vdict[n.parents[1]] = v4

	    end
	end

    diffn = [ filter(n -> (n.nodetype==:external) & (n.name==ds), g.nodes)[1] for ds in diffsym]
    dnodes = map(n -> vdict[n], diffn)
    (g2.nodes, dnodes)
end