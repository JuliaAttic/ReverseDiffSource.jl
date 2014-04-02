#########################################################################
#
#   Expression to graph conversion
#
#########################################################################
	
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

	explore(ex::Real)      = add_node(g, NConst(ex))

	explore(ex::ExBlock)   = map( explore, ex.args )[end]

	explore(ex::ExRef)     = add_node(g, NRef(ex.args[2:end], [ explore(ex.args[1]) ]))
	explore(ex::ExDot)     = add_node(g, NDot(ex.args[2],     [ explore(ex.args[1]) ]))

	explore(ex::ExComp)    = add_node(g, NComp(ex.args[2], [explore(ex.args[1]), explore(ex.args[3])]))

	function explore(ex::Symbol)
		if haskey(g.setmap, ex) # var already set in expression
			return g.setmap[ex]
		elseif haskey(externals, ex) # external ref already turned into a node
			return externals[ex]
		else # symbol neither set var nor known external
			externals[ex] = add_node(g, NExt(ex))  # create external node for this var
			return externals[ex]
		end
	end

	function explore(ex::ExCall)
		if in(ex.args[1], [:zeros, :ones, :vcat])
			add_node(g, NAlloc(ex.args[1], map(explore, ex.args[2:end]) ))
			# add_node(g, :call, ex.args[1], map(explore, ex.args[2:end]) )  
			# TODO : decide what to do here
	    else
	    	add_node(g, NCall( ex.args[1], map(explore, ex.args[2:end]) ))
	    end
	end

	function explore(ex::ExEqual) 
		lhs = ex.args[1]
		
		if isSymbol(lhs)
			haskey(externals, lhs) && 
				warn("$lhs is used as an external reference and then set within the expression")

			g.setmap[lhs] = explore(ex.args[2])

		elseif isRef(lhs)
			haskey(externals, lhs.args[1]) && 
				warn("$(lhs.args[1]) is used as an external reference and then set within the expression")

			v2 = add_node(g, NSRef(lhs.args[2:end], 
							       [ explore(lhs.args[1]),   # var whose subpart is assigned
							         explore(ex.args[2])] )) # assigned value
			g.setmap[lhs.args[1]] = v2

		elseif isDot(lhs)
			haskey(externals, lhs.args[1]) && 
				warn("$(lhs.args[1]) is used as an external reference and then set within the expression")

			v2 = add_node(g, NSDot(lhs.args[2], 
								   [ explore(lhs.args[1]),   # var whose subpart is assigned
							         explore(ex.args[2])] )) # assigned value
			g.setmap[lhs.args[1]] = v2

		else
			error("[tograph] not a symbol on LHS of assigment $(toExpr(ex))")
		end
		return nothing
	end

	function explore(ex::ExFor)
		is = ex.args[1].args[1]
		isa(is, Symbol) || 
			error("[tograph] for loop not using a single variable $is ")

		# explore the for block as a separate graph 
		g2 = tograph(ex.args[2])

		# create "for" node
		nf = add_node(g, NFor( [ ex.args[1], g2 ] ))

		# fill the missing pieces of inmap now that
		#  we are in the parent graph
		# k is the node in subgraph to map to a node in parent graph
		for (k,v) in g2.inmap 
			pn = explore(k.main) # look for symbol in setvars & externals
			g2.inmap[k] = pn
			push!(nf.parents, pn) # mark as parent of for loop
		end

		# fill inner nodes to outer nodes map for loop output
		for (k,v) in g2.outmap
			if haskey(g.setmap, v)
				g.setmap[v] = add_node(g, NIn((v, k), [nf]) )
			else
				warn("$(v) ignored outside of loop")
			end
		end

	end

    g = ExGraph()
	g.setmap = Dict{Union(Nothing, Symbol), ExNode}()

	exitnode = explore(s)  
	# exitnode = nothing if only variable assigments in expression
	#          = ExNode of last calc otherwise

	# setmap key is 'nothing' for unnassigned last statement
	exitnode!=nothing && (g.setmap[nothing] = exitnode) 


	# inmap can only be fully known at the parent level (if there is one)
	# we only do a partial initialization here
	g.inmap  = Dict{ExNode, Any}(collect(values(externals)), 
									fill(nothing, length(externals)) )

	# same for outmap
	g.outmap = Dict{ExNode, Any}(collect(values(g.setmap)), 
									collect(keys(g.setmap)) )  # dummy nodes


	g
end


	# function explore(ex::ExFor)
	# 	is = ex.args[1].args[1]
	# 	isa(is, Symbol) || error("[tograph] for loop not using a single variable $is ")

	# 	# explore the for block as a separate graph 
	# 	g2, sv2, ext2, exit2 = tograph(ex.args[2], merge(externals, g.setmap))

	# 	# update externals if new symbol found inside loop
	# 	for (k,v) in ext2
	# 		if !haskey(g.setmap,k) && !haskey(externals,k) && (k != is) 
	# 			externals[k] = v
	# 		end
	# 	end

	# 	#  find nodes dependant on indexing variable
	# 	gi = ExNode[]
	# 	for n2 in g2.nodes
	# 		if (isa(n2, Union(NRef, NSRef)) && in(is, n2.main)) ||
	# 			( isa(n2, NExt) && n2.main == is) ||
	# 			( isa(n2, NAlloc) )
	# 			push!(gi, n2)
	# 		end
	# 	end
	# 	g2in = ExNode[]; g2out = ExNode[]
	# 	for n2 in g2.nodes
	# 		if length(intersect(ancestors(n2), gi)) > 0
	# 			push!(g2in, n2)
	# 		else
	# 			push!(g2out, n2)
	# 		end
	# 	end

	# 	# independant nodes can be outside of loop
	# 	append!(g.nodes, g2out)

	# 	# create "for" node parent list
	# 	fp = mapreduce(n2->n2.parents, union, g2in)
	# 	fp = setdiff(fp, g2in)

	# 	# create "for" node
	# 	nf = add_node(g, NFor( ( ex.args[1], ExGraph(g2in, Dict()) ), 
	# 		          		   fp ) )


	# 	# update g.setmap
	# 	for (k,v) in sv2
	# 		ni = sv2[k]
	# 		if in(v, g2out)
	# 			g.setmap[k] = ni
	# 		else
	# 			# var set repeatedly ?
	# 			if !isa(ni, NSRef) || !in(is, ni.main)
	# 				print("$k set repeatedly,")

	# 				svaext = merge(g.setmap, externals)

	# 				if isa(ni, NCall) &&
	# 				   (ni.main == :+) &&
	# 				   in(svaext[k], ni.parents)
	# 				   	println("but may be it's ok")
	# 				else
	# 					println("there is a problem, really !")
	# 				end

	# 			end
	# 			g.setmap[k] = add_node(g, NIn(ni, [nf]) )

	# 			!isa(ni, NSRef) && (nf.main[2].exitnodes[k] = v)

	# 		end
	# 	end