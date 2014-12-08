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
		                       	valueof(n.parents[1],n), 
		                       	n.main, 
		                       	valueof(n.parents[2],n) )

	translate(n::NRef)   = Expr(:ref, Any[ valueof(x,n) for x in n.parents ]...)
	translate(n::NDot)   = Expr(:(.), valueof(n.parents[1],n), n.main)

	function translate(n::NIn)
	    isa(n.parents[1], NFor) && return n.parents[1].val[n]
	    return n.parents[1].val
	end

	function translate(n::NCall)
	  	# special translation cases
	  	if n.main == :vcat
	  		return Expr      (:vcat, Any[ valueof(x,n) for x in n.parents ]...)
	  	elseif n.main == :colon
	  		return Expr(       :(:), Any[ valueof(x,n) for x in n.parents ]...)
	  	elseif n.main == :transpose
	  		return Expr(symbol("'"),                 valueof(n.parents[1], n) )
	  	elseif n.main == :tuple
	  		return Expr(     :tuple, Any[ valueof(x,n) for x in n.parents ]...)
		end

		# default translation
		Expr(:call, n.main, Any[ valueof(x,n) for x in n.parents ]...)
	end

	function translate(n::NExt)
	    haskey(g.exti, n) || return n.main
	    sym = g.exti[n]  # should be equal to n.main but just to be sure.. 
	    haskey(g.exto.vk, sym) || return n.main
	    return g.exto.vk[sym].val  # return node val in parent graph
	end

	function translate(n::NSRef)
		np = n.parents
		(length(np) == 2) && return :( $(valueof(np[1],n)) = $(valueof(np[2],n)) ) 

    	:( $(Expr(:ref, valueof(np[1],n), Any[ valueof(x,n) for x in np[3:end]]...)) = $(valueof(np[2],n)) ) 
	end

	function translate(n::NSDot)
		np = n.parents
    	:( $(Expr(:., valueof(np[1],n), n.main)) = $(valueof(np[2],n)) )
	end

	translate(n::NFor) = Expr(:for, 
		                      Expr(:(=), n.main[1], valueof(n.parents[1],n)), 
		                      tocode(n.main[2]))

	evalsort!(g)
	out = Any[]
	for n in g.nodes
		n.val = translate(n)       # translate to Expr
		stat, lhs = ispivot(n, g)

	    if stat && isa(n, Union(NSRef, NSDot))
	    	push!(out, n.val)
	    	n.val = n.parents[1].val

	    elseif stat && isa(n, NFor)
   			push!(out, n.val)

	    	g2 = n.main[2]
	        valdict = Dict()
		    for (k, sym) in g2.seto
		      valdict[k] = g2.seti.vk[sym].val
		    end
	        n.val = valdict

		elseif stat && (lhs != n.val)  
			if lhs == nothing && n == g.nodes[end] # last statement without assignment
				push!(out, :( $(n.val) )) 

			else
				( lhs in [ nosym, nothing] ) && ( lhs = newvar() )
				push!(out, :( $lhs = $(n.val) ))
			end				

	        n.val = lhs

	    end

	end 

	return Expr(:block, out...)
end 

#####################################################################
#  variable name assigned to this node
#####################################################################
const nosym = 0x7c883061f2344364  # code for no symbol associated

function getname(n::ExNode, g::ExGraph)
	if haskey(g.seti, n)
		sym = g.seti[n]
		haskey(g.exto.vk, sym) || return sym
		return g.exto.vk[sym].val
	elseif haskey(g.exti, n)
		sym = g.exti[n]
		haskey(g.exto.vk, sym) || return sym
		return g.exto.vk[sym].val
	else	
		return nosym
	end

end


#####################################################################
#  tells if an assignment should be created for this node
#####################################################################

# always print nodes that change a variable's state
ispivot(n::Union(NSRef, NSDot, NFor), g::ExGraph) = (true, nothing)

# print only if names are linked
function ispivot(n::Union(NExt, NRef, NDot), g::ExGraph)
	sym = getname(n, g)
	# (sym != nosym, sym)
	sym != nosym && return (true, sym)

	# it is in the precedence of another node and is (by another path) a parent of an exitnode
	ps = filter(x -> (n in x.precedence), g.nodes) 
	if length(ps) > 0
		sv = collect(keys(g.seti))
		isancestor(n, sv, g, ps) && return (true, nosym)
	end

	# otherwise do not create assignment
	return (false, nothing)
end

# print constants that are named or modified by a for loop
function ispivot(n::NConst, g::ExGraph)
	sym = getname(n, g)
	sym != nosym && return (true, sym)

	# it is used in a setfield/index or getfield/index 
	any(x -> isa(x, Union(NSRef, NSDot, NRef, NDot)) && n == x.parents[1], g.nodes) &&
		return (true, nosym)

	for x in filter(x -> isa(x, NFor) && n in x.parents[2:end], g.nodes)
		fg = x.main[2]
		isym = fg.exto[n]
		haskey(fg.exti.vk, isym) && haskey(fg.exti.vk, isym) && return (true, nosym)
		# haskey(fg.seto.vk, isym) && return (true, nosym)
	end

	(false, nothing)
end

# print only if used in For loop
function ispivot(n::NIn, g::ExGraph)
	sym = getname(n, g)
	sym != nosym && return (true, sym)

	any(x -> isa(x, NFor) && n in x.parents[2:end], g.nodes) && 
		return (true, nosym)

	# it is in the precedence of another node and is (by another path) a parent of an exitnode
	ps = filter(x -> (n in x.precedence), g.nodes) 
	if length(ps) > 0
		sv = collect(keys(g.seti))
		isancestor(n, sv, g, ps) && return (true, nosym)
	end

	(false, nothing)
end


function ispivot(n::Union(NCall, NComp), g::ExGraph)
	sym = getname(n, g)

	# it has a name assigned
	sym != nosym && return (true, sym)

	# it is used in a for loop (except index range)
	any(x -> isa(x, NFor) && n in x.parents[2:end], g.nodes) && 
		return (true, nosym)

	# it is used in a setfield/index or getfield/index 
	any(x -> isa(x, Union(NSRef, NSDot, NRef, NDot)) && n == x.parents[1], g.nodes) &&
		return (true, nosym)

	# it is used more than once
	# (sum(x -> sum([ p == n for p in x.parents ]), g.nodes) > 1) &&
	(sum(x -> sum(n .== x.parents), g.nodes) > 1) && return (true, nosym)

	# it is in the precedence of another node and is (by another path) a parent of an exitnode
	ps = filter(x -> (n in x.precedence), g.nodes) 
	if length(ps) > 0
		sv = collect(keys(g.seti))
		isancestor(n, sv, g, ps) && return (true, nosym)
	end

	# otherwise do not create assignment
	return (false, nothing)
end

