#########################################################################
#
#   Expression to graph, graph to expression functions
#
#########################################################################


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

## variable symbol sampling functions
getSymbols(ex::Any)    = Set{Symbol}()
getSymbols(ex::Symbol) = Set{Symbol}(ex)
getSymbols(ex::Array)  = mapreduce(getSymbols, union, ex)
getSymbols(ex::Expr)   = getSymbols(toExH(ex))
getSymbols(ex::ExH)    = mapreduce(getSymbols, union, ex.args)
getSymbols(ex::ExCall) = mapreduce(getSymbols, union, ex.args[2:end])  # skip function name
getSymbols(ex::ExRef)  = setdiff(mapreduce(getSymbols, union, ex.args), Set(:(:), symbol("end")) )# ':'' and 'end' do not count
getSymbols(ex::ExDot)  = Set{Symbol}(ex.args[1])  # return variable, not fields
getSymbols(ex::ExComp) = setdiff(mapreduce(getSymbols, union, ex.args), 
	Set(:(>), :(<), :(>=), :(<=), :(.>), :(.<), :(.<=), :(.>=), :(==)) )

## variable symbol subsitution functions
substSymbols(ex::Any, smap::Dict)     = ex
substSymbols(ex::Expr, smap::Dict)    = substSymbols(toExH(ex), smap::Dict)
substSymbols(ex::Vector, smap::Dict)  = map(e -> substSymbols(e, smap), ex)
substSymbols(ex::ExH, smap::Dict)     = Expr(ex.head, map(e -> substSymbols(e, smap), ex.args)...)
substSymbols(ex::ExCall, smap::Dict)  = Expr(:call, ex.args[1], map(e -> substSymbols(e, smap), ex.args[2:end])...)
substSymbols(ex::ExDot, smap::Dict)   = (ex = toExpr(ex) ; ex.args[1] = substSymbols(ex.args[1], smap) ; ex)
substSymbols(ex::Symbol, smap::Dict)  = get(smap, ex, ex)


	
######## maps expr to a ExNode graph ###################
function tograph(s, 
	             g::ExGraph = ExGraph(), 
	             setvars::Dict = Dict(),
	             externals::Dict = Dict() )

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
		if haskey(setvars, ex) # var already set in expression
			return setvars[ex]
		elseif haskey(externals, ex) # external ref already turned into a node
			return externals[ex]
		else
			return add_node(g, :external, ex)  # create node for this external var
		end
	end

	function explore(ex::ExCall)
		if in(ex.args[1], [:zeros, :ones, :vcat])
			# add_node(g, :alloc, ex.args[1], map(explore, ex.args[2:end]) )
			add_node(g, :call, ex.args[1], map(explore, ex.args[2:end]) )  # TODO : decide what to do here
	    else
	    	add_node(g, :call, ex.args[1], map(explore, ex.args[2:end]) )
	    end
	end

	function explore(ex::ExFor)
		# explore the for block as a separate graph 
		# (with external references and setvars of enclosing graph passed as externals)
		g2, sv2, ext2, dummy = tograph(ex.args[2], ExGraph(), Dict(), merge(externals, setvars))

		# remove nodes already known by enclosing graph
		for (k,v) in merge(externals, setvars) ; delete!(ext2, k) ; end

		# update externals with really new external vars found in 'for' block
		for v in keys(ext2) ; externals[v] = ext2[v] ; end

		append!(g.nodes, g2.nodes)
		n = add_node(g, :for, ex.args[1], collect(values(sv2)) )

		# update vdict for vars set in 'for' block
		for (k,v) in sv2 ; setvars[k] = n ; end

	end

	function explore(ex::ExEqual) 
		lhs = ex.args[1]
		
		if isSymbol(lhs)
			setvars[lhs] = explore(ex.args[2])

		elseif isRef(lhs)
			v2 = add_node(g, :subref, lhs.args[2], 
							[ explore(lhs.args[1]),  # var whose subpart is assigned
							  explore(ex.args[2])] ) # assigned value
			setvars[lhs.args[1]] = v2

		elseif isDot(lhs)
			v2 = add_node(g, :subdot, lhs.args[2], 
							[ explore(lhs.args[1]),  # var whose subpart is assigned
							  explore(ex.args[2])] ) # assigned value
			setvars[lhs.args[1]] = v2

		else
			error("[tograph] not a symbol on LHS of assigment $(toExpr(ex))")
		end
		return nothing
	end

	exitnode = explore(s)  
	# exitnode = nothing if only variable assigments in expression
	#          = ExNode of last calc otherwise

	(g, setvars, externals, exitnode)
end

###### builds expr from graph  ######
function tocode(g::ExGraph)

	evalsort!(g)
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

	    elseif n.nodetype == :for
	    	fb = tocode(ExGraph(n.parents, Dict{Symbol, ExNode}()))
	    	append!( out, fb.args )
	        n.value = nothing

	    end

	    # variable name(s) for this node
	    nvn = collect(keys( filter( (k,v) -> v == n, g.exitnodes) ) ) 
        # number of times n is a parent
        np = mapreduce(n1 -> count(n2->n2==n, n1.parents), +, g.nodes)

		# create an assignment statement if...        
        if ( length(nvn) > 0 ) |                 # is an exit node
        	( n.nodetype ==:alloc ) |            # is an allocation
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
