#########################################################################
#
#   Expression to graph, graph to expression functions
#
#########################################################################
	
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
			add_node(g, :alloc, ex.args[1], map(explore, ex.args[2:end]) )
			# add_node(g, :call, ex.args[1], map(explore, ex.args[2:end]) )  
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
		g2, sv2, ext2, exit2 = tograph(ex.args[2], merge(externals, setvars))

		# update externals if new symbol found inside loop
		for (k,v) in ext2
			if !haskey(setvars,k) && !haskey(externals,k) && (k != is) 
				externals[k] = v
			end
		end

		#  find nodes dependant on indexing variable
		gi = ExNode[]
		for n2 in g2.nodes
			if (isa(n2, Union(NRef, NSRef)) && in(is, n2.main)) ||
				( isa(n2, NExt) && n2.main == is) ||
				( isa(n2, NAlloc) )
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

		# create "for" node
		nf = add_node(g, :for, 
			          (ex.args[1], ExGraph(g2in, Dict())), 
			          fp )


		# update setvars
		for (k,v) in sv2
			ni = sv2[k]
			if in(v, g2out)
				setvars[k] = ni
			else
				# var set repeatedly ?
				if !isa(ni, NSRef) || !in(is, ni.main)
					print("$k set repeatedly,")

					svaext = merge(setvars, externals)

					if (isa(ni, NCall) &&
					   (ni.main == :+) &&
					   (in(svaext[k], ni.parents))
					   	println("but may be it's ok")
					else
						println("there is a problem, really !")
					end

				end
				setvars[k] = add_node(g, :within, ni, [nf]) 

				!isa(ni, NSRef) && (nf.main[2].exitnodes[k] = v)

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

	function process!(n::NConst)
        if isa(n.main, Real)
        	n.value = n.main
        elseif isa(n.main, Expr)
        	n.value = n.main
        end
	end 

	process!(n::NExt)  = n.value = n.main 
	process!(n::NCall) = n.value = Expr(:call, n.main, { x.value for x in n.parents}...)
	process!(n::NComp) = n.value = Expr(:comparison, { n.parents[1].value, n.main, n.parents[2].value }...)

	evalsort!(g)
	out = Expr[]
	for n in g.nodes # n = g.nodes[4]


	    elseif n.nodetype == :ref
	        n.value = Expr(:ref, n.parents[1].value, n.main...)

	    elseif n.nodetype == :dot
	        n.value = Expr(:(.), n.parents[1].value, n.main)

	    elseif n.nodetype == :subref
	    	push!(out, :( $(Expr(:ref, n.parents[1].value, n.main...)) = $(n.parents[2].value) ) ) # an assign is necessary
	        n.value = n.parents[1].value

	    elseif n.nodetype == :subdot
	    	push!(out, :( $(Expr(:., n.parents[1].value, n.main)) = $(n.parents[2].value) ) ) # an assign is necessary
	        n.value = n.parents[1].value

	    elseif n.nodetype == :alloc
	        n.value = Expr(:call, n.main, { x.value for x in n.parents}...)

	    elseif n.nodetype == :for
	    	fb = tocode(n.main[2])
	    	push!(out, Expr(:for, n.main[1], fb))
	        n.value = nothing

	    elseif n.nodetype == :within
	    	n.value = n.main.value

	    end

	    # variable name(s) for this node
	    nvn = collect(keys( filter( (k,v) -> v == n, g.exitnodes) ) ) 

        # number of times n is a parent (force np> 1 if used in "for" loop, ref, dot)
        # np = mapreduce(n1 -> sum(n1.parents .== n) * (in(n1.nodetype, [:for, :subref, :subdot]) ? 2 : 1), +, g.nodes)
        function usecount(ntest::ExNode, nref::ExNode)
        	all(ntest.parents .!= nref) && return 0

        	if isa(ntest, NFor)
        		return 2
        	elseif isa(ntest, Union(NSRef, NSDot)) && ntest.parents[1] == nref
        		return 2
        	else
        		return sum(ntest.parents .== nref)
        	end
        end

        np = mapreduce(n1 -> usecount(n1, n), +, g.nodes )

		# create an assignment statement if...        
        if ( length(nvn) > 0 ) |                 # is an exit node
        	# ( n.nodetype ==:alloc ) |            # is an allocation
        	((np > 1) & isa(n, Union(NCall, NAlloc)) )   # has several children

        	if length(nvn) > 0
	        	lhs = nvn[1]
	        	for nv in nvn[2:end]
	        		lhs = :( $nv = $lhs )
	        	end
        	else
        		lhs = newvar()
        	end

        	# create assgnmt if code not redundant
	        lhs != n.value && push!(out, :( $lhs = $(n.value) ))
	        n.value = lhs
	    end

	end 

	return Expr(:block, out...)
end 
