#########################################################################
#
#   Graph to expression block conversion
#
#########################################################################

# g   : ExGraph to translate to code
# pgs : vector of enclosing graphs to search for external references
function tocode(g::ExGraph, pgs::Vector{ExGraph}=ExGraph[])

	translate(n::NConst) = n.main
	translate(n::NExt) = n.main
	translate(n::NCall)  = Expr(:call, n.main, 
		                       { x.val for x in n.parents}...)
	translate(n::NComp)  = Expr(:comparison, 
		                       { n.parents[1].val, n.main, n.parents[2].val }...)

	translate(n::NRef)   = Expr(:ref, n.parents[1].val, n.main...)
	translate(n::NDot)   = Expr(:(.), n.parents[1].val, n.main)
	translate(n::NIn)    = n.main.val
	translate(n::NAlloc) = Expr(:call, n.main, 
		                        { x.val for x in n.parents}...)

	function translate(n::NSRef)
		np = n.parents
    	# an assign is necessary
    	push!(out, :( $(Expr(:ref, np[1].val, n.main...)) = $(np[2].val) ) ) 
        n.parents[1].val
	end

	function translate(n::NSDot)
		np = n.parents
    	# an assign is necessary
    	push!(out, :( $(Expr(:., np[1].val, n.main)) = $(np[2].val) ) )
        n.parents[1].val
	end

	function translate(n::NFor)
		# we have to set all the externals of the 'for' subgraph
		#  that have an evaluated value in the enclosing graphs
		# FIXME : fusenodes() will not update inner-outer map
		g2 = n.main[2]
		mp = n.main[3]
		for n2 in filter(n -> isa(n, NExt), g2.nodes)
			n2.val = mp[n2].val
		end

		g2 = ExGraph(filter(n -> !isa(n, NExt), g2.nodes), Dict())
    	fb = tocode(g2, vcat(g, pgs))
    	push!(out, Expr(:for, n.main[1], fb))
        nothing
	end


	evalsort!(g)  # order is important
	out = Expr[]
	for n in g.nodes # n = g.nodes[4]
		n.val = translate(n)

	    # variable name(s) for this node
	    nvn = collect(keys( filter( (k,v) -> is(v, n), g.exitnodes) ) ) 

        # number of times n is a parent (force np> 1 if 
        #   used in "for" loop, sref, sdot)
        function usecount(ntest::ExNode, nref::ExNode)
        	np = sum(n->is(n, nref), ntest.parents)
        	(np == 0) && return 0

        	if isa(ntest, NFor)
        		return 2
        	elseif isa(ntest, Union(NSRef, NSDot)) && is(ntest.parents[1], nref)
        		return 2
        	else
        		return np
        	end
        end

        np = mapreduce(n1 -> usecount(n1, n), +, g.nodes )

		# create an assignment statement if...        
        if ( length(nvn) > 0 ) |                       # is an exit node
        	# isa(n, NAlloc) |                         # is an allocation
        	((np > 1) & isa(n, Union(NCall, NAlloc)) ) # has several children

        	if length(nvn) > 0
	        	lhs = nvn[1]
	        	for nv in nvn[2:end]
	        		lhs = :( $nv = $lhs )
	        	end
        	else
        		lhs = newvar()
        	end

        	# create assgnmt if code not redundant
	        lhs != n.val && push!(out, :( $lhs = $(n.val) ))
	        n.val = lhs
	    end

	end 

	return Expr(:block, out...)
end 
