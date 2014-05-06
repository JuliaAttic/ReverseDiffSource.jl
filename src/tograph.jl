#########################################################################
#
#   Expression to graph conversion
#
#########################################################################

tograph(s) = tograph(s, Set{Any}())

#  s     : expression to convert
#  svars : vars set since the toplevel graph (helps separate globals / locals)
function tograph(s, svars::Set{Any})

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

	explore(ex::Real)      = addnode!(g, NConst(ex))

	explore(ex::ExBlock)   = map( explore, ex.args )[end]

	explore(ex::ExRef)     = addnode!(g, NRef(ex.args[2:end], [ explore(ex.args[1]) ]))
	explore(ex::ExDot)     = addnode!(g, NDot(ex.args[2],     [ explore(ex.args[1]) ]))

	explore(ex::ExComp)    = addnode!(g, NComp(ex.args[2], [explore(ex.args[1]), explore(ex.args[3])]))

	function explore(ex::Symbol)
		# println("=========  $ex")
		# for (k,v) in g.map.kv ; println(" - ($(repr(hash(k))))  : $k  / $v") ; end

		if haskey(g.map.vk, (ex, :out_inode))       # var already set before
			return g.map.vk[(ex, :out_inode)]

		elseif haskey(g.map.vk, (ex, :in_inode))   # external ref already turned into a node
			return g.map.vk[(ex, :in_inode)]

		else # neither a var set before nor a known external
			# nn = NExt(ex)
			# push!(g.nodes, nn)
			nn = addnode!(g, NExt(ex))    # create external node for this var
			# for n2 in g.nodes ; println(" node3 ($(repr(hash(n2))))  : $n2") ; end
			# println("+++  $nn  ($(repr(hash(nn)))")
			g.map[nn] = (ex, :in_inode)
			# for (k,v) in g.map.kv ; println(" + ($(repr(hash(k))))  : $k  / $v") ; end
			return nn
		end
	end

	function explore(ex::ExCall)
		if in(ex.args[1], [:zeros, :ones, :vcat])
			addnode!(g, NCall(ex.args[1], map(explore, ex.args[2:end]) ))
	    else
	    	addnode!(g, NCall(ex.args[1], map(explore, ex.args[2:end]) ))
	    end
	end

	function explore(ex::ExEqual) 

		# for n2 in g.nodes ; println(" node ($(repr(hash(n2))))  : $n2") ; end

		lhs = ex.args[1]
		
		if isSymbol(lhs)
			lhss = lhs
			rhn  = explore(ex.args[2])

		elseif isRef(lhs)
			lhss = lhs.args[1]
			rhn  = addnode!(g, NSRef(lhs.args[2:end], 
							       [ explore(lhs.args[1]),   # var whose subpart is assigned
							         explore(ex.args[2])] )) # assigned value
		elseif isDot(lhs)
			lhss = lhs.args[1]
			rhn  = addnode!(g, NSDot(lhs.args[2], 
								   [ explore(lhs.args[1]),   # var whose subpart is assigned
							         explore(ex.args[2])] )) # assigned value
		else
			error("[tograph] $(toExpr(ex)) not allowed on LHS of assigment")
		end

		# g.setmap[lhss] = rhn
		g.map[rhn] = (lhss, :out_inode)

		# for n2 in g.nodes ; println(" node2 ($(repr(hash(n2))))  : $n2") ; end

		return nothing
	end

	function explore(ex::ExFor)
		is = ex.args[1].args[1]
		isa(is, Symbol) || 
			error("[tograph] for loop not using a single variable $is ")

		# explore the for block as a separate graph 
		oin = filter(t -> t[2] == :out_inode, collect(values(g.map.kv)))
		nsvars = Set([ v[1] for v in oin ])
		for e in svars ; add!(nsvars, e) ; end
		g2 = tograph(ex.args[2], nsvars)

		# create "for" node
		nf = addnode!(g, NFor( [ ex.args[1], g2 ] ))

		# create onodes (node in parent graph) for each :in_inode
		for (sym, typ) in values(g2.map.kv)
			if typ == :in_inode && sym != is
				pn = explore(sym)  # look in setmap, externals or create it
				g2.map[pn] = (sym, :in_onode)
				push!(nf.parents, pn) # mark as parent of for loop
				# println("[subgraph inmap] inner $inode linked outer $pn")
			end
		end

		# create onodes and 'Nin' nodes for each :out_inode
		#  will be restricted to variables that are defined in parent
		#   (others are assumed to be local to the loop)
		for (sym, typ) in values(g2.map.kv)
			if typ == :out_inode && sym in nsvars && sym != is
				# println("[subgraph outmap] inner $inode sets $sym")
				pn = explore(sym)                   # create node if needed
				rn = addnode!(g, NIn(sym, [nf]))    # exit node for this var in this graph
				g.map[rn] = (sym, :out_inode)       # signal we're setting the var
				g2.map[rn] = (sym, :out_onode)
			end
		end
	end

	#  top level graph
    g = ExGraph()

	exitnode = explore(s)  
	# exitnode = nothing if only variable assigments in expression
	#          = ExNode of last calc otherwise

	# id is 'nothing' for unnassigned last statement
	exitnode!=nothing && ( g.map[exitnode] = (nothing, :out_inode) ) 

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
	# 	nf = addnode!(g, NFor( ( ex.args[1], ExGraph(g2in, Dict()) ), 
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
	# 			g.setmap[k] = addnode!(g, NIn(ni, [nf]) )

	# 			!isa(ni, NSRef) && (nf.main[2].exitnodes[k] = v)

	# 		end
	# 	end