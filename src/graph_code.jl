
##########  Parameterized type to ease AST exploration  ############
type ExH{H}
	head::Symbol
	args::Vector
	typ::Any
end
toExH(ex::Expr) = ExH{ex.head}(ex.head, ex.args, ex.typ)
toExpr(ex::ExH) = Expr(ex.head, ex.args...)

typealias ExEqual    ExH{:(=)}
typealias ExDColon   ExH{:(::)}
typealias ExPEqual   ExH{:(+=)}
typealias ExMEqual   ExH{:(-=)}
typealias ExTEqual   ExH{:(*=)}
typealias ExTrans    ExH{symbol("'")} 
typealias ExCall     ExH{:call}
typealias ExBlock	 ExH{:block}
typealias ExLine     ExH{:line}
typealias ExVcat     ExH{:vcat}
typealias ExFor      ExH{:for}
typealias ExRef      ExH{:ref}
typealias ExIf       ExH{:if}
typealias ExComp     ExH{:comparison}
typealias ExDot      ExH{:.}


	
######## maps expr to a ExNode graph ###################
function tograph(s, g::ExGraph = ExGraph(), vdict::Dict = Dict() )

	explore(ex::Expr)      = explore(toExH(ex))
	explore(ex::ExH)       = error("[tograph] unmanaged expr type $(ex.head) in ($ex)")
	explore(ex::ExLine)    = nothing     # remove line info
	explore(ex::LineNumberNode) = nothing     # remove line info


	explore(ex::ExVcat)    = explore(Expr(:call, :vcat, ex.args...) )  # translate to vcat() call, and explore
	explore(ex::ExTrans)   = explore(Expr(:call, :transpose, ex.args[1]) )  # translate to transpose() and explore

	explore(ex::ExPEqual)  = (args = ex.args ; explore( Expr(:(=), args[1], Expr(:call, :+, args[1], args[2])) ) )
	explore(ex::ExMEqual)  = (args = ex.args ; explore( Expr(:(=), args[1], Expr(:call, :-, args[1], args[2])) ) )
	explore(ex::ExTEqual)  = (args = ex.args ; explore( Expr(:(=), args[1], Expr(:call, :*, args[1], args[2])) ) )

	explore(ex::Real)      = add_node(g, :constant, ex)

	explore(ex::Any)       = error("[tograph] unmanaged type $ex")

	explore(ex::ExBlock)   = map( explore, ex.args )[end]

	explore(ex::ExRef)     = add_node(g, :ref, ex.args[2], [ explore(ex.args[1]) ])
	explore(ex::ExDot)     = add_node(g, :dot, ex.args[2], [ explore(ex.args[1]) ])

	explore(ex::ExComp)    = add_node(g, :comp, ex.args[2], 
										[ explore(ex.args[1]), explore(ex.args[3])])

	function explore(ex::Symbol)
		if haskey(vdict, ex) # var already set in expression
			return vdict[ex]
		else
			nr = filter(n -> (n.name==ex) & (n.nodetype==:external) , g.nodes)
			return length(nr)==0 ? add_node(g, :external, ex) : nr[1]
		end
	end

	function explore(ex::ExCall)
	    add_node(g, :call, ex.args[1], map(explore, ex.args[2:end]) )
	end

	function explore(ex::ExEqual) 
		lhs = ex.args[1]
		
		if isSymbol(lhs)
			vdict[lhs] = explore(ex.args[2])

		elseif isRef(lhs)
			v2 = add_node(g, :subref, lhs.args[2], 
							[ explore(lhs.args[1]),  # var whose subpart is assigned
							  explore(ex.args[2])] ) # assigned value
			vdict[lhs.args[1]] = v2

		elseif isDot(lhs)
			v2 = add_node(g, :subdot, lhs.args[2], 
							[ explore(lhs.args[1]),  # var whose subpart is assigned
							  explore(ex.args[2])] ) # assigned value
			vdict[lhs.args[1]] = v2

		else
			error("[tograph] not a symbol on LHS of assigment $(toExpr(ex))")
		end
		return nothing
	end

	exitnode = explore(s)  
	# exitnode = nothing if only variable assigments in expression
	#          = ExNode of last calc otherwise

	(g, vdict, exitnode)
end

###### builds expr from graph  ######
function tocode(g::ExGraph)

	Proto.evalsort!(g)
	out = Expr[]
	for n in g.nodes # n = g.nodes[4]
	    if n.nodetype == :constant
	        if isa(n.name, Real)
	        	n.value = n.name
	        elseif isa(n.name, Expr)
	        	n.value = n.name
	        end

	    elseif n.nodetype == :external
	        n.value = n.name 

	    elseif n.nodetype == :call
	        n.value = Expr(:call, n.name, { x.value for x in n.parents}...)

	    elseif n.nodetype == :comp
	        n.value = Expr(:comparison, { n.parents[1].value, n.name, n.parents[2].value }...)

	    elseif n.nodetype == :ref
	        n.value = Expr(:ref, n.parents[1].value, n.name)

	    elseif n.nodetype == :dot
	        n.value = Expr(:(.), n.parents[1].value, n.name)

	    elseif n.nodetype == :subref
	    	push!(out, :( $(Expr(:ref, n.parents[1].value, n.name)) = $(n.parents[2].value) ) ) # an assign is necessary
	        n.value = n.parents[1].value

	    elseif n.nodetype == :subdot
	    	push!(out, :( $(Expr(:., n.parents[1].value, n.name)) = $(n.parents[2].value) ) ) # an assign is necessary
	        n.value = n.parents[1].value

	    elseif n.nodetype == :alloc
	        n.value = Expr(:call, n.name, { x.value for x in n.parents}...)

	    end

	    # variable name(s) for this node
	    nvn = collect(keys( filter( (k,v) -> v == n, g.exitnodes) ) ) 
        # number of times n is a parent
        np = mapreduce(n1 -> count(n2->n2==n, n1.parents), +, g.nodes)

		# create an assignment statement if...        
        if ( length(nvn) > 0 ) |                 # is an exit node
        	((np > 1) & (n.nodetype == :call))   # has several children

        	if length(nvn) > 0
	        	lhs = nvn[1]
	        	for nv in nvn[2:end]
	        		lhs = :( $nv = $lhs )
	        	end
        	else
        		lhs = newvar()
        	end

	        push!(out, :( $lhs = $(n.value) ))
	        n.value = lhs

	    end

	end 

	return Expr(:block, out...)
end 
