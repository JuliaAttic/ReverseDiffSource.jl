#########################################################################
#
#    Reverse diff on graph
#
#########################################################################

###### creates reverse mode diff graph ######
function reversegraph(g::ExGraph, exitnode::ExNode, diffsym::Array{Symbol})
	g2     = ExGraph()  # graph that will contain the derivative evaluation

	# create starting nodes
	dnodes = Dict()     # map linking nodes of g to their derivative node in g2
	for n in filter(n-> !isa(n,NFor), g.nodes)
		if n == exitnode
			dnodes[n] = addnode!(g2, NConst(1.0))
		else
			dnodes[n] = createzeronode!(g2, n)
		end
	end

	# builds the graph for derivatives calculations
	reversepass!(g2, g, dnodes)

	# store in setmap the nodes containing the derivatives of diffsym
	for (k,v) in filter((k,v) -> isa(k, NExt) & in(k.main, diffsym), dnodes)
		g2.set_inodes[v] = dprefix(k.main)
	end

    g2
end

# creates the starting points for derivatives accumulation variables
function createzeronode!(g2::ExGraph, n)
	# isa(n, NConst) && return nothing  # not needed for constants

	# d_equivnode_1 is the name of function returning dnodes constructors
	#   as defined by calls to the macro @typeequiv

	if method_exists(d_equivnode_1, (typeof(n.val),) )
		rn = invoke(d_equivnode_1, (typeof(n.val),) , n.val)
    	dg, dd, de = rdict[ rn ]
    	smap = { dd[1] => n }  # map 'x' node to n
    	exitnode = addgraph!(dg, g2, smap)

    	return exitnode
	
	# try the array of defined types
	elseif isa(n.val, Array) && method_exists(d_equivnode_1, (eltype(n.val),) )
		rn = invoke(d_equivnode_1, (eltype(n.val),) , n.val[1])
    	dg, dd, de = rdict[ rn ]
    	smap = { dd[1] => n }  # map 'x' node to n
    	exitnode = addgraph!(dg, g2, smap)

    	v1 = addnode!(g2, NCall(:size, [n]))
		return addnode!(g2, NCall(:fill, [exitnode, v1]))

	end

	error("[reversegraph] Unknown type $(typeof(n.val)) for node $n")
	# if isa(n.val, Real)
	# 	return addnode!(g2, NConst(0.0))
	
	# # Array of Real
	# elseif isa(n.val, Array{Float64}) | isa(n.val, Array{Int})
	# 	v1 = addnode!(g2, NCall(:size, [n]))
	# 	return addnode!(g2, NCall(:zeros, [v1]))

	# # Composite type
	# elseif haskey(tdict, typeof(n.val))   # known composite type
	# 	v1 = addnode!(g2, NConst( tdict[typeof(n.val)]) )
	# 	return addnode!(g2, NCall(:zeros, [v1]) )

	# # Array of composite type
	# elseif isa( n.val, Array) && haskey(tdict, eltype(n.val))  
	# 	v1 = addnode!(g2, NCall(:size, [n]) )
	# 	aa = ExNode[ addnode!(g2, NCall(:zeros, [v1]) )
	# 	               for i in 1:(tdict[eltype(n.val)]) ]
	# 	return addnode!(g2, NCall(:(Base.cell_1d), aa) )

	# else
	# 	isa(n, NConst) || error("[reversegraph] Unknown type $(typeof(n.val)) for node $n")
	# end
end

#  climbs the reversed evaluation tree
function reversepass!(g2::ExGraph, g::ExGraph, dnodes::Dict)

	rev(n::ExNode) = nothing  # do nothing

	function rev(n::NCall)
		vargs = [ x.val for x in n.parents ]
		for (index, arg) in enumerate(n.parents)
			if !isa(arg, Union(NConst, NComp))
            	fn = dfuncname(n.main, index)
            	dg, dd, de = rdict[ eval(Expr(:call, fn, vargs...)) ]

            	smap = Dict( dd, [n.parents, dnodes[n]])

            	exitnode = addgraph!(dg, g2, smap)

        		v2 = addnode!(g2, NCall(:+, [dnodes[arg], exitnode]) )
        		dnodes[arg] = v2
            end
        end
	end		 

	function rev(n::NRef)
        v2 = addnode!(g2, NRef("getidx",  [ dnodes[n.parents[1]], n.parents[2:end] ]) )
        v3 = addnode!(g2, NCall(:+, [v2, dnodes[n]]) )
		v4 = addnode!(g2, NSRef("setidx", [ dnodes[n.parents[1]], v3, n.parents[2:end] ]) )
		dnodes[n.parents[1]] = v4
	end

	function rev(n::NSRef)
		v2 = addnode!(g2, NRef("getidx", [ dnodes[n] , n.parents[3:end] ]) )
		v3 = addnode!(g2, NCall(:+, [ dnodes[n.parents[2]], v2 ]) )
		dnodes[n.parents[2]] = v3
	end

	function rev(n::NDot)
        v2 = addnode!(g2, NDot( n.main, [dnodes[n.parents[1]]]) )
        v3 = addnode!(g2, NCall(:+, [v2, dnodes[n]]) )
		v4 = addnode!(g2, NSDot(n.main, [dnodes[n.parents[1]], v3]) )
		dnodes[n.parents[1]] = v4
	end

	function rev(n::NFor)
		fg  = copy(n.main[2])      # subgraph of for loop, copied to make new loop
		fg2 = ExGraph()            # will contain dnodes
		# is  = n.main[1].args[1]    # symbol of loop index
		is  = n.main[1]            # symbol of loop index

		println("=== create zero nodes ===")
		fdnodes = Dict()
		ndmap = Dict()
		for n2 in filter(n-> !isa(n,NFor), fg.nodes)
			# outgoing nodes become ingoing nodes
	 		#   both for the var and its derivative accumulator
			if haskey(fg.set_inodes, n2)
				sym = fg.set_inodes[n2]
				if haskey(fg.set_onodes.vk, sym)  # this is not a local var
					on = fg.set_onodes.vk[sym]

					# assumption : ext_inodes / onodes already exists for this sym
			 		dsym = dprefix(sym)  # newvar() 

					#  derivative of var
					nn = addnode!(fg2, NExt(dsym))
					fg.ext_inodes[nn] = dsym
					fg.ext_onodes[dnodes[on]] = dsym
				else  # it is a local var
					nn = createzeronode!(fg2, n2)
				end

			# ingoing nodes become potential outgoing dnodes
			elseif haskey(fg.ext_inodes, n2)
				sym = fg.ext_inodes[n2]
				if sym != is
					on = fg.ext_onodes.vk[sym]
			 		dsym = dprefix(sym)  # newvar()

					nn = addnode!(fg2, NExt(dsym))
					fg.ext_inodes[nn] = dsym
					fg.ext_onodes[dnodes[on]] = dsym

					ndmap[n2] = (dsym, on)
				end

			else
				nn = createzeronode!(fg2, n2)
			end	

			fdnodes[n2] = nn
		end

		# builds the graph for derivatives calculations
		println("===  reverse pass ===")
		reversepass!(fg2, fg, fdnodes)
		fg.nodes = [ fg.nodes, fg2.nodes]
		
		# variables of interest are derivatives only
		fg.set_inodes = BiDict{ExNode, Any}()
		for (ni, (sym, on)) in ndmap
			fg.set_inodes[ fdnodes[ni] ] = sym
		end

	println("=== fg  1 ===")
	# println(fg.nodes)
	# println("ndmap = $(collect(ndmap))")
	# println("fdnodes = $(collect(fdnodes))")
	# println("ext = $(collect(fg.ext_inodes.kv))")
	# println("set = $(collect(fg.set_inodes.kv))")
	# println("oext = $(collect(fg.ext_onodes.kv))")
	# println("oset = $(collect(fg.set_onodes.kv))")

		prune!(fg) # reduce to derivatives evaluation only

	println("=== fg  2 ===")
	# println(fg.nodes)
	# println("ndmap = $(collect(ndmap))")
	# println("fdnodes = $(collect(fdnodes))")
	# println("ext = $(collect(fg.ext_inodes.kv))")
	# println("set = $(collect(fg.set_inodes.kv))")
	# println("oext = $(collect(fg.ext_onodes.kv))")
	# println("oset = $(collect(fg.set_onodes.kv))")
		# create for loop
		println("=== create dfor node ===")
		v2 = addnode!(g2, NFor({ n.main[1], fg}) )
		v2.parents = [n.parents[1], collect( keys( fg.ext_onodes)) ]

		# set_onodes = dnodes of fg's ingoing variables
		# for ns2 in filter((k,v) -> haskey(fdnodes,n) & haskey(fg2.set_inodes,n), foutmap)
		fg.set_onodes = BiDict{ExNode, Any}()
		for (ns2, (sym, on)) in ndmap
			rn = addnode!(g2, NIn("dout", [v2]))  # external node, receiving loop result
			fdn = fdnodes[ns2]                    # final node in loop containing derivative
			fg.set_onodes[rn] = sym
			dnodes[on] = rn 
		end

	println("=== fg  3 ===")
	# println(fg.nodes)
	# println("ndmap = $(collect(ndmap))")
	# println("fdnodes = $(collect(fdnodes))")
	# println("ext = $(collect(fg.ext_inodes.kv))")
	# println("set = $(collect(fg.set_inodes.kv))")
	# println("oext = $(collect(fg.ext_onodes.kv))")
	# println("oset = $(collect(fg.set_onodes.kv))")

	end

	evalsort!(g)
	# println(g2)
	for n2 in reverse(g.nodes)
		println("======  $n2  ======")
		rev(n2)
		# for n3 in g2.nodes
		# 	dn = collect(keys(filter( (k,v) -> v == n3, dnodes ) ))
		# 	println(" $n3,  dn = $(repr(dn))")
		# end
		# println(g2)
	end
end


