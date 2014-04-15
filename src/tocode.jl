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
        	valdict[onode] = inode.val
        end
        valdict
	end


	evalsort!(g)  # order is important
	out = Expr[]
	for n in g.nodes 
		n.val = translate(n)

        nvn = getnames(n, g)
		# create an assignment statement if...        
        if ( length(nvn) > 0 ) | ispivot(n,g)
        	if length(nvn) > 0
	        	lhs = nvn[1]
	        	for nv in nvn[2:end]
	        		lhs = :( $nv = $lhs )
	        	end
        	else
        		lhs = newvar()
        	end

        	# create assgnmt if code not redundant
	        if lhs != n.val
	        	push!(out, :( $lhs = $(n.val) ))
	        	n.val = lhs
	        end
	    end

	end 

	return Expr(:block, out...)
end 


#  variable names assigned to this node
function getnames(n::ExNode, g::ExGraph)
	syms = Symbol[]
	if haskey(g.link, n) # this node modifies a var in parent
		push!(syms, g.link[n].val)  # this var has necessarily been evaluated to a symbol
	else
		for (k,v) in g.setmap
			is(v,n) && push!(syms, k==nothing ? newvar() : k)
		end
	end
	syms
end

# tests if an assignment should be created for this node
ispivot(n::Union(NExt, NRef, NDot, NSRef, NSDot, NFor), g::ExGraph) = false

function ispivot(n::Union(NCall, NAlloc, NComp), g::ExGraph)
	nbref = 0
	for n2 in g.nodes
		np = sum(i -> is(i, n), n2.parents)
		(np == 0) && continue

		isa(n2, NFor) && return true    # force assignment if used in for loops
		isa(n2, Union(NSRef, NSDot)) && 
			is(n2.parents[1], n) && return true  # force if setindex/setfield applies to it

		nbref += np
		(nbref >= 2) && return true  # if used more than once
	end

	false
end

function ispivot(n::Union(NConst, NIn), g::ExGraph)
	any( i -> in(n, i.parents) && isa(i, NFor), g.nodes)
end


