#########################################################################
#
#   Expression to graph conversion
#
#########################################################################

#  top-level call	
function tograph(s)

	# start with empty list of setvars
	tograph2(s, Dict{Symbol, ExNode}())

end

#  inner levels exploration
#  s     : expression to convert
#  pvars : vars defined at enclosing levels
function tograph2(s, pvars::Dict{Symbol, ExNode})

	explore(ex::Any)       = error("[tograph] unmanaged type $ex")
	explore(ex::Expr)      = explore(toExH(ex))
	explore(ex::ExH)       = error("[tograph] unmanaged expr type $(ex.head) in ($ex)")

	explore(ex::ExLine)         = nothing     # remove line info
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
		if haskey(g.setmap, ex)        # var already set in expression
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
			# TODO : NAlloc or NCall ? decide what to do
	    else
	    	add_node(g, NCall( ex.args[1], map(explore, ex.args[2:end]) ))
	    end
	end

	function explore(ex::ExEqual) 
		lhs = ex.args[1]
		
		# haskey(externals, lhs) && 
		# 	warn("$lhs is used as an external reference and then set within the expression")
		if isSymbol(lhs)
			lhss = lhs
			rhn  = explore(ex.args[2])
		elseif isRef(lhs)
			lhss = lhs.args[1]
			rhn  = add_node(g, NSRef(lhs.args[2:end], 
							       [ explore(lhs.args[1]),   # var whose subpart is assigned
							         explore(ex.args[2])] )) # assigned value
		elseif isDot(lhs)
			lhss = lhs.args[1]
			rhn  = add_node(g, NSDot(lhs.args[2], 
								   [ explore(lhs.args[1]),   # var whose subpart is assigned
							         explore(ex.args[2])] )) # assigned value
		else
			error("[tograph] $(toExpr(ex)) not allowed on LHS of assigment")
		end

		g.setmap[lhss] = rhn

		return nothing
	end

	function explore(ex::ExFor)
		is = ex.args[1].args[1]
		isa(is, Symbol) || 
			error("[tograph] for loop not using a single variable $is ")

		# explore the for block as a separate graph 
		g2 = tograph2(ex.args[2], merge(pvars, g.setmap))
		g2.setmap = Dict()     # remove

		# create "for" node
		nf = add_node(g, NFor( [ ex.args[1], g2 ] ))

		# update inmap by replacing symbol with corresponding outer node in this graph
		# dict key is the node in subgraph, and dict value is the node in parent graph
		for (inode, sym) in g2.inmap
			if sym==is   # index var should be removed
				delete!(g2.inmap, inode)
			else
				pn = explore(sym)  # look in setmap, externals or create it
				g2.inmap[inode] = pn
				push!(nf.parents, pn) # mark as parent of for loop
				# println("[subgraph inmap] inner $inode linked outer $pn")
			end
		end

		# update outmap by replacing symbol with corresponding outer node in this graph
		for (inode, sym) in g2.outmap
			if sym==is   # index var should be removed
				delete!(g2.outmap, inode)
			else
				# println("[subgraph outmap] inner $inode sets $sym")
				pn = explore(sym)  # create node if needed
				rn = add_node(g, NIn(sym, [nf]))  # exit node for this var in this graph
				g2.outmap[inode] = rn
				g2.link[inode] = pn
				g.setmap[sym] = rn      # signal we're setting the var
			end
		end
	end

    g = ExGraph()
	externals = Dict{Symbol, ExNode}()

	exitnode = explore(s)  
	# exitnode = nothing if only variable assigments in expression
	#          = ExNode of last calc otherwise

	# setmap key is 'nothing' for unnassigned last statement
	exitnode!=nothing && (g.setmap[nothing] = exitnode) 

	# outmap is the subset of setmap for variables that exist in the parent scope
	for (sym, node) in g.setmap
		if haskey(pvars, sym)
			g.outmap[node] = sym
		end
	end

	# inmap keys = externals, value = symbol used
	for (sym, node) in externals
		g.inmap[node] = sym
	end

	g
end


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