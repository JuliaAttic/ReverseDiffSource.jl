#########################################################################
#
#   Graph to expression block conversion
#
#########################################################################

# g   : ExGraph to translate to code
function tocode(g::ExGraph)

	valueof(n::ExNode, child::ExNode) = n.val
	valueof(n::NFor,   child::ExNode) = valueof(n.val[child], n)

	translate(n::NConst) = n.main
	translate(n::NComp)  = Expr(:comparison, 
		                       	{ valueof(n.parents[1],n), 
		                       	  n.main, 
		                       	  valueof(n.parents[2],n) }...)

	translate(n::NRef)   = Expr(:ref, { valueof(x,n) for x in n.parents}...)
	translate(n::NDot)   = Expr(:(.), valueof(n.parents[1],n), n.main)

	function translate(n::NIn)
	    isa(n.parents[1], NFor) && return n.parents[1].val[n]
	    return n.parents[1].val
	end

	function translate(n::NCall)
	  	# special translation cases
	  	if n.main == :vcat
	  		return Expr(:vcat, { valueof(x,n) for x in n.parents}...)
	  	elseif n.main == :colon
	  		return Expr(:(:), { valueof(x,n) for x in n.parents}...)
	  	elseif n.main == :transpose
	  		return Expr(symbol("'"), valueof(n.parents[1], n) )
	  	elseif n.main == :tuple
	  		return Expr(:tuple, { valueof(x,n) for x in n.parents}... )
		end

		# default translation
		Expr(:call, n.main, { valueof(x,n) for x in n.parents}...)
	end

	function translate(n::NExt)
	    haskey(g.ext_inodes, n) || return n.main
	    sym = g.ext_inodes[n]  # should be equal to n.main but just to be sure.. 
	    haskey(g.ext_onodes.vk, sym) || return n.main
	    return g.ext_onodes.vk[sym].val  # return node val in parent graph
	end

	function translate(n::NSRef)
		np = n.parents
    	:( $(Expr(:ref, valueof(np[1],n), { valueof(x,n) for x in np[3:end]}...)) = $(valueof(np[2],n)) ) 
	end

	function translate(n::NSDot)
		np = n.parents
    	:( $(Expr(:., valueof(np[1],n), n.main)) = $(valueof(np[2],n)) )
	end

	translate(n::NFor) = Expr(:for, 
		                      Expr(:(=), n.main[1], valueof(n.parents[1],n)), 
		                      tocode(n.main[2]))

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

		# elseif n == g.nodes[end]  # if last statement, evaluate anyway
		# 	push!(out, :( $(n.val) ) )

	    end

	end 

	return Expr(:block, out...)
end 

function ancestors(ns::Vector, except=[])
    ss = setdiff(ns, except)
    isempty(ss) ? [] : mapreduce(n -> ancestors(n,except), union, ss)
end
ancestors(ns, except=[]) = union([ns], ancestors(ns.parents, except))

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

# evaluate only if used in For loop
function ispivot(n::Union(NConst, NIn), g::ExGraph)
	sym = getnames(n, g)
	sym != nothing && return (true, sym)

	any(x -> isa(x, NFor) && n in x.parents[2:end], g.nodes) && 
		return (true, sym)

	return (false, nothing)
end

function ispivot(n::Union(NCall, NComp), g::ExGraph)
	sym = getnames(n, g)

	# it has a name assigned
	sym != nothing && return (true, sym)

	# it is used in a for loop (except index range)
	any(x -> isa(x, NFor) && n in x.parents[2:end], g.nodes) && 
		return (true, sym)

	# it is used in a setfield/index or getfield/index 
	any(x -> isa(x, Union(NSRef, NSDot, NRef, NDot)) && n == x.parents[1], g.nodes) &&
		return (true, sym)

	# it is used more than once
	(sum(x -> sum([ p == n for p in x.parents ]), g.nodes) > 1) &&
		return (true, sym)

	# it is in the precedence of another node
	ps = filter(x -> n in x.precedence, g.nodes)
	if length(ps) > 0
		sv = collect(keys(g.set_inodes))
		(n in ancestors(sv, ps)) && return (true, sym)
	end

	# otherwise do not create assignment
	return (false, nothing)
end

