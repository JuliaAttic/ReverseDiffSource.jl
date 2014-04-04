#########################################################################
#
#   Graph to expression block conversion
#
#########################################################################

# g   : ExGraph to translate to code
function tocode(g::ExGraph)

	valueof(n::ExNode, child::ExNode) = n.val
	valueof(  n::NFor, child::ExNode) = valueof(n.val[child], n)

	translate(n::NConst) = n.main
	translate(n::NCall)  = Expr(:call, n.main, 
		                       { valueof(x,n) for x in n.parents}...)
	translate(n::NComp)  = Expr(:comparison, 
		                       	{ valueof(n.parents[1],n), 
		                       	  n.main, 
		                       	  valueof(n.parents[2],n) }...)

	translate(n::NRef)   = Expr(:ref, valueof(n.parents[1],n), n.main...)
	translate(n::NDot)   = Expr(:(.), valueof(n.parents[1],n), n.main)
	translate(n::NIn)    = n.parents[1].val[n]
	translate(n::NAlloc) = Expr(:call, n.main, 
		                        { valueof(x,n) for x in n.parents}...)

	function translate(n::NExt)
		if haskey(g.inmap, n)
			pn = g.inmap[n]
			return isa(pn, ExNode) ? pn.val : pn
		end
		n.main
	end

	function translate(n::NSRef)
		np = n.parents
    	# an assign is necessary
    	push!(out, :( $(Expr(:ref, valueof(np[1],n), n.main...)) = $(valueof(np[2],n)) ) ) 
        valueof(np[1],n)
	end

	function translate(n::NSDot)
		np = n.parents
    	# an assign is necessary
    	push!(out, :( $(Expr(:., valueof(np[1],n), n.main)) = $(valueof(np[2],n)) ) )
        valueof(np[1],n)
	end

	function translate(n::NFor)
    	g2 = n.main[2]
    	fb = tocode(g2)
    	push!(out, Expr(:for, n.main[1], fb))
        
        valdict = Dict()
        for (inode, onode) in g2.outmap
        	println(inode, onode)
        	valdict[onode] = inode.val
        end
        valdict
	end


	evalsort!(g)  # order is important
	out = Expr[]
	for n in g.nodes # n = g.nodes[4]
		n.val = translate(n)

	    # variable name(s) for this node
	    nvn = collect(keys( filter( (k,v) -> is(v, n), g.setmap) ) ) 

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
