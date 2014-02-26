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

# variable symbol sampling functions
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

## variable symbol substitution functions
substSymbols(ex::Any, smap::Dict)     = ex
substSymbols(ex::Expr, smap::Dict)    = substSymbols(toExH(ex), smap::Dict)
substSymbols(ex::Vector, smap::Dict)  = map(e -> substSymbols(e, smap), ex)
substSymbols(ex::ExH, smap::Dict)     = Expr(ex.head, map(e -> substSymbols(e, smap), ex.args)...)
substSymbols(ex::ExCall, smap::Dict)  = Expr(:call, ex.args[1], map(e -> substSymbols(e, smap), ex.args[2:end])...)
substSymbols(ex::ExDot, smap::Dict)   = (ex = toExpr(ex) ; ex.args[1] = substSymbols(ex.args[1], smap) ; ex)
substSymbols(ex::Symbol, smap::Dict)  = get(smap, ex, ex)


	
######## maps expr to a ExNode graph ###################
function tograph(s, externals::Dict = Dict() )

	explore(ex::Any)       = error("[tograph] unmanaged type $ex")
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

	explore(ex::ExBlock)   = map( explore, ex.args )[end]

	explore(ex::ExRef)     = add_node(g, :ref, ex.args[2:end], [ explore(ex.args[1]) ])
	explore(ex::ExDot)     = add_node(g, :dot, ex.args[2], [ explore(ex.args[1]) ])

	explore(ex::ExComp)    = add_node(g, :comp, ex.args[2], 
										[ explore(ex.args[1]), explore(ex.args[3])])

	function explore(ex::Symbol)
		if haskey(setvars, ex) # var already set in expression
			return setvars[ex]
		elseif haskey(externals, ex) # external ref already turned into a node
			return externals[ex]
		else # symbol neither set var nor known external
			externals[ex] = add_node(g, :external, ex)  # create external node for this var
			return externals[ex]
		end
	end

	function explore(ex::ExCall)
		if in(ex.args[1], [:zeros, :ones, :vcat])
			# add_node(g, :alloc, ex.args[1], map(explore, ex.args[2:end]) )
			add_node(g, :call, ex.args[1], map(explore, ex.args[2:end]) )  
			# TODO : decide what to do here
	    else
	    	add_node(g, :call, ex.args[1], map(explore, ex.args[2:end]) )
	    end
	end

	function explore(ex::ExEqual) 
		lhs = ex.args[1]
		
		if isSymbol(lhs)
			setvars[lhs] = explore(ex.args[2])

		elseif isRef(lhs)
			v2 = add_node(g, :subref, lhs.args[2:end], 
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

	function explore(ex::ExFor)
		is = ex.args[1].args[1]
		isa(is, Symbol) || error("[tograph] for loop not using a single variable $is ")

		# explore the for block as a separate graph 
		g2, sv2, ext2, exitnode = tograph(ex.args[2], merge(externals, setvars))

		# update externals if new symbol found inside loop
		for (k,v) in ext2
			if !in(k, keys(setvars)) && 
				!in(k, keys(externals)) && (k != is) 
				externals[k] = v
			end
		end

		#  find nodes dependant on indexing variable
		gi = ExNode[]
		for n2 in g2.nodes
			if (in(n2.nodetype, [:ref, :subref]) && in(is, n2.name)) ||
				( n2.nodetype == :external && n2.name == is) 
				push!(gi, n2)
			end
		end
		g2in = ExNode[]; g2out = ExNode[]
		for n2 in g2.nodes
			if length(intersect(ancestors(n2), gi)) > 0
				push!(g2in, n2)
			else
				push!(g2out, n2)
			end
		end

		# independant nodes can be outside of loop
		append!(g.nodes, g2out)

		# create "for" node parent list
		fp = mapreduce(n2->n2.parents, union, g2in)
		fp = setdiff(fp, g2in)

		# create "for" node, "in" nodes being stored in parent field
		nf = add_node(g, :for, 
			          (ex.args[1], ExGraph(g2in, Dict()), sv2), 
			          fp )

		# update setvars
		for (k,v) in sv2
			if in(v, g2out)
				setvars[k] = sv2[k]
			else
				setvars[k] = nf
			end
		end
	end

    g = ExGraph()
	setvars = Dict()

	exitnode = explore(s)  
	# exitnode = nothing if only variable assigments in expression
	#          = ExNode of last calc otherwise

	(g, setvars, externals, exitnode)
end

###### builds expr from graph  ######
function tocode(g::ExGraph)

	function valueof(n::ExNode)
		if n.nodetype == :for
			return ?????
		else
			return n.value
		end
	end

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
	        n.value = Expr(:ref, n.parents[1].value, n.name...)

	    elseif n.nodetype == :dot
	        n.value = Expr(:(.), n.parents[1].value, n.name)

	    elseif n.nodetype == :subref
	    	push!(out, :( $(Expr(:ref, n.parents[1].value, n.name...)) = $(n.parents[2].value) ) ) # an assign is necessary
	        n.value = n.parents[1].value

	    elseif n.nodetype == :subdot
	    	push!(out, :( $(Expr(:., n.parents[1].value, n.name)) = $(n.parents[2].value) ) ) # an assign is necessary
	        n.value = n.parents[1].value

	    elseif n.nodetype == :alloc
	        n.value = Expr(:call, n.name, { x.value for x in n.parents}...)

	    elseif n.nodetype == :for
	    	fb = tocode(n.name[2])
	    	ne = Expr(:for, n.name[1], fb)
	    	push!(out, ne)
	    	# force assignement of exitnodes set in loop
	    	for (k,v) in g.exitnodes
	    		if in(v, n.name[2].nodes) && k != v.value
	    			push!(out, :( $k = $(v.value) ))
	    		end
	    	end
	        n.value = nothing

	    end

	    # variable name(s) for this node
	    nvn = collect(keys( filter( (k,v) -> v == n, g.exitnodes) ) ) 
        # number of times n is a parent (count multiple times if "for" loop)
        np = mapreduce(n1 -> sum(n1.parents .== n) * (n1.nodetype==:for ? 2 : 1), +, g.nodes)

		# create an assignment statement if...        
        if ( length(nvn) > 0 ) |                 # is an exit node
        	# ( n.nodetype ==:alloc ) |            # is an allocation
        	((np > 1) & in(n.nodetype, [:call, :alloc]) )   # has several children

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
