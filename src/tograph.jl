#########################################################################
#
#   Expression to graph conversion
#
#########################################################################

tograph(s) = tograph(s, {})

#  s     : expression to convert
#  svars : vars set since the toplevel graph (helps separate globals / locals)
function tograph(s, svars::Vector{Any})

	explore(ex::Any)       = error("[tograph] unmanaged type $ex")
	explore(ex::Expr)      = explore(toExH(ex))
	explore(ex::ExH)       = error("[tograph] unmanaged expr type $(ex.head) in ($ex)")

	explore(ex::ExLine)         = nothing     # remove line info
	explore(ex::LineNumberNode) = nothing     # remove line info

	explore(ex::ExVcat)    = explore(Expr(:call, :vcat, ex.args...) )  # translate to vcat() call, and explore
	explore(ex::ExCell1d)  = explore(Expr(:call, :(Base.cell_1d), ex.args...) )  # translate to cell_1d() call, and explore
	explore(ex::ExTrans)   = explore(Expr(:call, :transpose, ex.args[1]) )  # translate to transpose() and explore
	explore(ex::ExColon)   = explore(Expr(:call, :colon, ex.args...) )  # translate to colon() and explore
	explore(ex::ExTuple)   = explore(Expr(:call, :tuple, ex.args...) )  # translate to tuple() and explore

	explore(ex::ExPEqual)  = (args = ex.args ; explore( Expr(:(=), args[1], Expr(:call, :+, args[1], args[2])) ) )
	explore(ex::ExMEqual)  = (args = ex.args ; explore( Expr(:(=), args[1], Expr(:call, :-, args[1], args[2])) ) )
	explore(ex::ExTEqual)  = (args = ex.args ; explore( Expr(:(=), args[1], Expr(:call, :*, args[1], args[2])) ) )

	explore(ex::Real)      = addnode!(g, NConst(ex))

	explore(ex::ExBlock)   = map( explore, ex.args )[end]

	explore(ex::ExDot)     = addnode!(g, NDot(ex.args[2],     [ explore(ex.args[1]) ]))

	explore(ex::ExComp)    = addnode!(g, NComp(ex.args[2], [explore(ex.args[1]), explore(ex.args[3])]))

	# explore(ex::ExRef)     = addnode!(g, NRef(ex.args[2:end], [ explore(ex.args[1]) ]))
	explore(ex::ExRef)     = addnode!(g, NRef(:getidx, map(explore, ex.args)))

	function explore(ex::Symbol)
		ex in {:(:), symbol("end")} && return addnode!(g, NConst(ex))  # plain symbols (used in x[1,:] or y[1:end])
		haskey(g.set_inodes.vk, ex) && return g.set_inodes.vk[ex]
		haskey(g.ext_inodes.vk, ex) && return g.ext_inodes.vk[ex]

		nn = addnode!(g, NExt(ex))    # create external node for this var
		g.ext_inodes[nn] = ex
		return nn
	end

	explore(ex::ExCall) = addnode!(g, NCall(ex.args[1], map(explore, ex.args[2:end]) ))

	function explore(ex::ExEqual) 
		lhs = ex.args[1]
		
		if isSymbol(lhs)
			lhss = lhs
			rhn  = explore(ex.args[2])
			# we test if RHS has already a symbol
			# if it does, to avoid loosing it, we create an NIn node
			if haskey(g.set_inodes, rhn) 
				rhn = addnode!(g, NIn(lhss, [rhn]))
			end

		elseif isRef(lhs)
			lhss = lhs.args[1]
			vn = explore(lhss) # node whose subpart is assigned
			rhn  = addnode!(g, NSRef(:setidx, [ vn,    # var modified in pos #1
				                                explore(ex.args[2]), # value affected in pos #2
				                                map(explore, lhs.args[2:end])] ))  # indexing starting at #3
			rhn.precedence = filter(n -> vn in n.parents && n != rhn, g.nodes)

		elseif isDot(lhs)
			lhss = lhs.args[1]
			vn = explore(lhss) # node whose subpart is assigned
			rhn  = addnode!(g, NSDot(lhs.args[2], [ vn, explore(ex.args[2])] )) 
			rhn.precedence = filter(n -> vn in n.parents && n != rhn, g.nodes)

		else
			error("[tograph] $(toExpr(ex)) not allowed on LHS of assigment")
		end

		# g.map[rhn] = (lhss, :out_inode)
		g.set_inodes[rhn] = lhss

		return nothing
	end

	function explore(ex::ExFor)
		is = ex.args[1].args[1]
		isa(is, Symbol) || 
			error("[tograph] for loop using several indexes : $is ")

		# explore the index range
		nir = explore(ex.args[1].args[2])

		# explore the for block as a separate graph 
		nsvars = union(svars, collect(keys(g.set_inodes.vk)))
		g2 = tograph(ex.args[2], nsvars)

		# create "for" node
		nf = addnode!(g, NFor( { is, g2 } ))
		nf.parents = [nir]  # first parent is indexing range fo the loop

		# create onodes (node in parent graph) for each :in_inode
		for (k, sym) in g2.ext_inodes.kv
			if sym != is  # loop index should be excluded
				pn = explore(sym)  # look in setmap, externals or create it
				g2.ext_onodes[pn] = sym
				push!(nf.parents, pn) # mark as parent of for loop
			end
		end

		# create onodes and 'Nin' nodes for each :out_inode
		#  will be restricted to variables that are defined in parent
		#   (others are assumed to be local to the loop)
		for (k, sym) in g2.set_inodes.kv
			if sym in nsvars && sym != is # only for variables set in parent scope
				pn = explore(sym)                   # create node if needed
				rn = addnode!(g, NIn(sym, [nf]))    # exit node for this var in this graph
				g.set_inodes[rn] = sym              # signal we're setting the var
				g2.set_onodes[rn] = sym

				append!(nf.precedence, filter(n -> pn in n.parents && n != nf, g.nodes))
			end
		end
	end

	#  top level graph
    g = ExGraph()

	exitnode = explore(s)  
	# exitnode = nothing if only variable assigments in expression
	#          = ExNode of last calc otherwise

	# id is 'nothing' for unnassigned last statement
	exitnode!=nothing && ( g.set_inodes[exitnode] = nothing ) 

	g
end
