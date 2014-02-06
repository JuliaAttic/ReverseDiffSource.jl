###### creates reverse mode diff graph ######
function reversegraph(g::ExGraph, exitnode::ExNode, diffsym::Array{Symbol})

	g2 = Proto.ExGraph()
	vdict = Dict()

	# creates the starting points for derivatives variables
	for n in g.nodes # n = g.nodes[3]
		if n == exitnode
			vdict[n] = Proto.add_node(g2, :constant, 1.0)

		elseif isa(n.value, Real)
			vdict[n] = Proto.add_node(g2, :constant, 0.0)
		
		elseif any( map(t->isa(n.value,t), [Array{Float64}, Array{Int}]) )
			v1 = Proto.add_node(g2, :call, :size, [n])
			vdict[n] = Proto.add_node(g2, :alloc, :zeros, [v1])

		elseif haskey(tdict, typeof(n.value))
			v1 = Proto.add_node(g2, :constant, tdict[typeof(n.value)])
			vdict[n] = Proto.add_node(g2, :alloc, :zeros, [v1])

		else
			error("[reversegraph] Unknown variable type $(typeof(n.value))")
		end
	end

	#  now climb the reversed evaluation tree
	Proto.evalsort!(g)
	for n in reverse(g.nodes)  # n = reverse(g)[1]
		if n.nodetype == :call
			vargs = [ x.value for x in n.parents ]
			for (index, arg) in zip(1:length(n.parents), n.parents)
	            # index = 1 ; arg = n.parents[1] 
	            if !in(arg.nodetype, [:constant, :comp])

	            	fn = symbol("d_$(n.name)_x$index")
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