#########################################################################
#
#   Graph to expression block conversion
#
#########################################################################

# g   : ExGraph to translate to code
# function tocode_old(g::ExGraph)

# 	valueof(n::ExNode, child::ExNode) = n.val
# 	valueof(n::NFor,   child::ExNode) = valueof(n.val[child], n)

# 	translate(n::NConst) = n.main
# 	translate(n::NCall)  = Expr(:call, n.main, 
# 		                       { valueof(x,n) for x in n.parents}...)
# 	translate(n::NComp)  = Expr(:comparison, 
# 		                       	{ valueof(n.parents[1],n), 
# 		                       	  n.main, 
# 		                       	  valueof(n.parents[2],n) }...)

# 	translate(n::NRef)   = Expr(:ref, valueof(n.parents[1],n), n.main...)
# 	translate(n::NDot)   = Expr(:(.), valueof(n.parents[1],n), n.main)
# 	translate(n::NIn)    = n.parents[1].val[n]
# 	translate(n::NAlloc) = Expr(:call, n.main, 
# 		                        { valueof(x,n) for x in n.parents}...)

# 	function translate(n::NExt)
# 	    haskey(g.ext_inodes, n) || return n.main

# 	    sym = g.ext_inodes[n]  # should be equal to n.main but just to be sure.. 
# 	    haskey(g.ext_onodes.vk, sym) || return n.main
# 	    return g.ext_onodes.vk[sym].val  # return node val in parent graph
# 	end

# 	function translate(n::NSRef)
# 		np = n.parents
#     	push!(out, :( $(Expr(:ref, valueof(np[1],n), n.main...)) = $(valueof(np[2],n)) ) ) 
#         valueof(np[1],n)
# 	end

# 	function translate(n::NSDot)
# 		np = n.parents
#     	push!(out, :( $(Expr(:., valueof(np[1],n), n.main)) = $(valueof(np[2],n)) ) )
#         valueof(np[1],n)
# 	end

# 	function translate(n::NFor)
#     	g2 = n.main[2]
#     	push!(out, Expr(:for, n.main[1], tocode(g2)))
        
#         valdict = Dict()
# 	    for (k, sym) in g2.set_onodes
# 	      valdict[k] = g2.set_inodes.vk[sym].val
# 	    end
#         valdict
# 	end

# 	evalsort!(g)
# 	out = Expr[]
# 	for n in g.nodes 
# 		n.val = translate(n)

#         nvn = getnames(n, g)
# 		# create an assignment statement if...        
#         if ( length(nvn) > 0 ) | ispivot(n,g)
#         	if length(nvn) > 0
# 	        	lhs = nvn[1]
# 	        	for nv in nvn[2:end]
# 	        		lhs = :( $nv = $lhs )
# 	        	end
#         	else
#         		lhs = newvar()
#         	end

#         	# create assgnmt if code not redundant
# 	        if lhs != n.val
# 	        	push!(out, :( $lhs = $(n.val) ))
# 	        	n.val = lhs
# 	        end
# 	    end

# 	end 

# 	return Expr(:block, out...)
# end 

function tocode(g::ExGraph)

	valueof(n::ExNode, child::ExNode) = n.val
	valueof(n::NFor,   child::ExNode) = valueof(n.val[child], n)

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
	    haskey(g.ext_inodes, n) || return n.main
	    sym = g.ext_inodes[n]  # should be equal to n.main but just to be sure.. 
	    haskey(g.ext_onodes.vk, sym) || return n.main
	    return g.ext_onodes.vk[sym].val  # return node val in parent graph
	end

	function translate(n::NSRef)
		np = n.parents
    	:( $(Expr(:ref, valueof(np[1],n), n.main...)) = $(valueof(np[2],n)) ) 
	end

	function translate(n::NSDot)
		np = n.parents
    	:( $(Expr(:., valueof(np[1],n), n.main)) = $(valueof(np[2],n)) )
	end

	translate(n::NFor) = Expr(:for, n.main[1], tocode(n.main[2]))

	evalsort!(g)
	out = Expr[]
	for n in g.nodes

	    # translate to Expr
		n.val = translate(n)

		stat, lhs = ispivot(n, g)

	    if stat && isa(n, Union(NSRef, NSDot))
	    	push!(out, n.val)
	    	n.val = n.parents[1].val

	    elseif stat && isa(n, NFor)
   			push!(out, n.val)

	    	g2 = n.main[2]
	        valdict = Dict()
		    for (k, sym) in g2.set_onodes
		      valdict[k] = g2.set_inodes.vk[sym].val
		    end
	        n.val = valdict

		elseif stat && (lhs != n.val)  
			nlhs = lhs == nothing ? newvar() : lhs
			push!(out, :( $nlhs = $(n.val) ))
	        n.val = nlhs

	    end

	end 

	return Expr(:block, out...)
end 

ancestors(ns::Vector, except=[]) = mapreduce(n -> ancestors(n,except), union, setdiff(ns, except))
ancestors(ns::ExNode, except=[]) = union([ns], ancestors(ns.parents, except))

#  variable names assigned to this node
function getnames(n::ExNode, g::ExGraph)
	haskey(g.set_inodes, n) || return nothing
	sym = g.set_inodes[n]
	# return parent node evaluation if it exists
	haskey(g.ext_onodes.vk, sym) || return sym==nothing ? newvar() : sym

	return g.ext_onodes.vk[sym].val
end

#### tells if an assignment should be created for this node

# always evaluate nodes that change a variable's state
ispivot(n::Union(NSRef, NSDot, NFor), g::ExGraph) = (true, nothing)

# evaluate only if names are linked
function ispivot(n::Union(NExt, NRef, NDot), g::ExGraph)
	sym = getnames(n, g)
	sym == nothing || return (true, sym)
	return (false, nothing)
end

# evaluate only if used in For loop  (TODO : not sure about this)
function ispivot(n::Union(NConst, NIn), g::ExGraph)
	sym = getnames(n, g)
	sym == nothing || return (true, sym)
	any( i -> n in i.parents && isa(i, NFor), g.nodes) && return (true, sym)
	return (false, nothing)
end

function ispivot(n::Union(NCall, NAlloc, NComp), g::ExGraph)
	sym = getnames(n, g)
	sym == nothing || return (true, sym)

	nbref = 0	
	for n2 in filter(x -> n in x.parents, g.nodes)
		np = sum(i -> i == n, n2.parents)
		(np == 0) && continue

		isa(n2, NFor) && (nbref=2 ; break)   # force assignment if used in for loops
		isa(n2, Union(NSRef, NSDot)) && 
			n2.parents[1] == n && (nbref = 2 ; break )  # force if setindex/setfield applies to it

		nbref += np
		(nbref >= 2) && break  # if used more than once
	end

	nbref > 1 && return (true, getnames(n, g))

	# another cause could be n in precedence of a node changing a variable state
	ps = filter(x -> n in x.precedence, g.nodes)
	length(ps) == 0 && return (false, nothing)
	sv = collect(keys(g.set_inodes))
	(n in ancestors(sv, ps)) && return (true, getnames(n, g))

	return (false, nothing)
end






# ispivot(n::Union(NExt, NRef, NDot, NFor), g::ExGraph) = false

# function ispivot(n::Union(NCall, NAlloc, NComp, NSRef, NSDot), g::ExGraph)
# 	nbref = 0
# 	for n2 in g.nodes
# 		np = sum(i -> i == n, n2.parents)
# 		(np == 0) && continue

# 		isa(n2, NFor) && return true    # force assignment if used in for loops
# 		isa(n2, Union(NSRef, NSDot)) && 
# 			n2.parents[1] == n && return true  # force if setindex/setfield applies to it

# 		nbref += np
# 		(nbref >= 2) && return true  # if used more than once
# 	end

# 	false
# end

# function ispivot(n::Union(NConst, NIn), g::ExGraph)
# 	any( i -> n in i.parents && isa(i, NFor), g.nodes)
# end


