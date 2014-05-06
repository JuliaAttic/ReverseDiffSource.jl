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
		haskey(g.set_inodes.vk, ex) && return g.set_inodes.vk[ex]
		haskey(g.ext_inodes.vk, ex) && return g.ext_inodes.vk[ex]

		nn = addnode!(g, NExt(ex))    # create external node for this var
		g.ext_inodes[nn] = ex
		return nn
	end

	function explore(ex::ExCall)
		if in(ex.args[1], [:zeros, :ones, :vcat])
			addnode!(g, NCall(ex.args[1], map(explore, ex.args[2:end]) ))
	    else
	    	addnode!(g, NCall(ex.args[1], map(explore, ex.args[2:end]) ))
	    end
	end

	function explore(ex::ExEqual) 
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

		# g.map[rhn] = (lhss, :out_inode)
		g.set_inodes[rhn] = lhss

		return nothing
	end

	function explore(ex::ExFor)
		is = ex.args[1].args[1]
		isa(is, Symbol) || 
			error("[tograph] for loop not using a single variable : $is ")

		# explore the for block as a separate graph 
		nsvars = union(svars, collect(keys(g.set_inodes.vk)))
		g2 = tograph(ex.args[2], nsvars)

		# create "for" node
		nf = addnode!(g, NFor( [ ex.args[1], g2 ] ))

		# create onodes (node in parent graph) for each :in_inode
		for (k, sym) in g2.ext_inodes.kv
			if sym != is  # loop index should be excluded
				pn = explore(sym)  # look in setmap, externals or create it
				g2.ext_onodes[pn] = sym
				push!(nf.parents, pn) # mark as parent of for loop
			end
		end
		# for (sym, typ) in values(g2.map.kv)
		# 	if typ == :in_inode && sym != is
		# 		pn = explore(sym)  # look in setmap, externals or create it
		# 		g2.map[pn] = (sym, :in_onode)
		# 		push!(nf.parents, pn) # mark as parent of for loop
		# 		# println("[subgraph inmap] inner $inode linked outer $pn")
		# 	end
		# end

		# create onodes and 'Nin' nodes for each :out_inode
		#  will be restricted to variables that are defined in parent
		#   (others are assumed to be local to the loop)
		for (k, sym) in g2.set_inodes.kv
			if sym in nsvars && sym != is # only for variables set in parent scope
				pn = explore(sym)                   # create node if needed
				rn = addnode!(g, NIn(sym, [nf]))    # exit node for this var in this graph
				g.set_inodes[rn] = sym              # signal we're setting the var
				g2.set_onodes[rn] = sym
			end
		end
		# for (sym, typ) in values(g2.map.kv)
		# 	if typ == :out_inode && sym in nsvars && sym != is
		# 		# println("[subgraph outmap] inner $inode sets $sym")
		# 		pn = explore(sym)                   # create node if needed
		# 		rn = addnode!(g, NIn(sym, [nf]))    # exit node for this var in this graph
		# 		g.map[rn] = (sym, :out_inode)       # signal we're setting the var
		# 		g2.map[rn] = (sym, :out_onode)
		# 	end
		# end
	end

	#  top level graph
    g = ExGraph()

	exitnode = explore(s)  
	# exitnode = nothing if only variable assigments in expression
	#          = ExNode of last calc otherwise

	# id is 'nothing' for unnassigned last statement
	# exitnode!=nothing && ( g.map[exitnode] = (nothing, :out_inode) ) 
	exitnode!=nothing && ( g.set_inodes[exitnode] = nothing ) 

	g
end
