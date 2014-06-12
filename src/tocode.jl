#########################################################################
#
#   Graph to expression conversion
#
#########################################################################

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
	out = {}
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
			if lhs == nothing && n == g.nodes[end] # last statment without assign
				push!(out, :( $(n.val) )) 
			else
				( lhs in {nosym, nothing} ) && ( lhs = newvar() )
				# nlhs = ( lhs in {nosym, nothing} ) ? newvar() : lhs
				# println("***", lhs, "^^", nlhs)
				push!(out, :( $lhs = $(n.val) ))
			end				

	        n.val = lhs

	    end

	end 

	return Expr(:block, out...)
end 

#####################################################################
#  variable names assigned to this node
#####################################################################
const nosym = 0x7c883061f2344364  # code for no symbol associated

function getnames(n::ExNode, g::ExGraph)
	haskey(g.set_inodes, n) || return nosym
	sym = g.set_inodes[n]

	# return parent node evaluation if it exists
	# haskey(g.ext_onodes.vk, sym) || return sym==nothing ? newvar() : sym
	haskey(g.ext_onodes.vk, sym) || return sym

	return g.ext_onodes.vk[sym].val
end

#####################################################################
#  tells if an assignment should be created for this node
#####################################################################

# always evaluate nodes that change a variable's state
ispivot(n::Union(NSRef, NSDot, NFor), g::ExGraph) = (true, nothing)

# evaluate only if names are linked
function ispivot(n::Union(NExt, NRef, NDot), g::ExGraph)
	sym = getnames(n, g)
	(sym != nosym, sym)
end

# evaluate only if used in For loop
function ispivot(n::Union(NConst, NIn), g::ExGraph)
	sym = getnames(n, g)
	sym != nosym && return (true, sym)

	any(x -> isa(x, NFor) && n in x.parents[2:end], g.nodes) && 
		return (true, nosym)

	(false, nothing)
end

function ispivot(n::Union(NCall, NComp), g::ExGraph)
	sym = getnames(n, g)

	# it has a name assigned
	sym != nosym && return (true, sym)

	# it is used in a for loop (except index range)
	any(x -> isa(x, NFor) && n in x.parents[2:end], g.nodes) && 
		return (true, nosym)

	# it is used in a setfield/index or getfield/index 
	any(x -> isa(x, Union(NSRef, NSDot, NRef, NDot)) && n == x.parents[1], g.nodes) &&
		return (true, nosym)

	# it is used more than once
	(sum(x -> sum([ p == n for p in x.parents ]), g.nodes) > 1) &&
		return (true, nosym)

	# it is in the precedence of another node
	ps = filter(x -> n in x.precedence, g.nodes)
	if length(ps) > 0
		sv = collect(keys(g.set_inodes))
		(n in ancestors(sv, ps)) && return (true, nosym)
	end

	# otherwise do not create assignment
	return (false, nothing)
end

