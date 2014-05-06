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
	if isa(n.val, Real)
		return addnode!(g2, NConst(0.0))
	
	# Array of Real
	elseif isa(n.val, Array{Float64}) | isa(n.val, Array{Int})
		v1 = addnode!(g2, NCall(:size, [n]))
		return addnode!(g2, NAlloc(:zeros, [v1]))

	# Composite type
	elseif haskey(tdict, typeof(n.val))   # known composite type
		v1 = addnode!(g2, NConst( tdict[typeof(n.val)]) )
		return addnode!(g2, NAlloc(:zeros, [v1]) )

	# Array of composite type
	elseif isa( n.val, Array) && haskey(tdict, eltype(n.val))  
		v1 = addnode!(g2, NCall(:size, [n]) )
		aa = ExNode[ addnode!(g2, NAlloc(:zeros, [v1]) )
		               for i in 1:(tdict[eltype(n.val)]) ]
		return addnode!(g2, NCall(:vcat, aa) )

	else
		error("[reversegraph] Unknown type for node $n")
	end
end

#  climbs the reversed evaluation tree
function reversepass!(g2::ExGraph, g::ExGraph, dnodes::Dict)

	rev(n::ExNode) = nothing  # do nothing

	function rev(n::NCall)
		vargs = [ x.val for x in n.parents ]
		for (index, arg) in zip(1:length(n.parents), n.parents)
            if !isa(arg, Union(NConst, NComp))
            	fn = dfuncname(n.main, index)
            	dg, dd, de = rdict[ eval(Expr(:call, fn, vargs...)) ]

            	smap = Dict( dd, [n.parents, dnodes[n]])

            	nmap = addgraph!(dg, g2, smap)

        		v2 = addnode!(g2, NCall(:+, [dnodes[arg], nmap[de]]) )
        		dnodes[arg] = v2
            end
        end
	end		 

	function rev(n::NRef)
        v2 = addnode!(g2, NRef(n.main, [ dnodes[n.parents[1]] ]) )
        v3 = addnode!(g2, NCall(:+, [v2, dnodes[n]]) )
		v4 = addnode!(g2, NSRef(n.main, [dnodes[n.parents[1]], v3]) )
		dnodes[n.parents[1]] = v4
	end

	function rev(n::NSRef)
		v2 = addnode!(g2, NRef(n.main, [ dnodes[n] ]) )
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
		is  = n.main[1].args[1]    # symbol of loop index

		println("=== create zero nodes ===")
		fdnodes = Dict()
		foutmap = {}  # outmap = dnodes of initial inmap
		for n2 in filter(n-> !isa(n,NFor), fg.nodes)
			# outgoing nodes become ingoing nodes
	 		#   both for the var and its derivative accumulator
			if haskey(fg.set_inodes, n2)
				sym = fg.set_inodes[n2]

		 		nsym = newvar()
				nn = addnode!(fg2, NExt(sym))
				fg.ext_inodes[nn] = nsym

				on = fg.set_onodes.vk[sym]   # is it always there ? 
				fg.ext_onodes[on] = nsym

				#  derivative of var
				nn = addnode!(fg2, NExt(dprefix(sym)))
				fg.ext_inodes[nn] = dprefix(nsym)
				fg.ext_onodes[dnodes[on]] = dprefix(nsym)

			# ingoing nodes only need derivative accumulator creation			
			elseif haskey(fg.ext_inodes, n2)
				sym = fg.ext_inodes[n2]

		 		nsym = newvar()
				nn = addnode!(fg2, NExt(dprefix(sym)))
				fg.ext_inodes[nn] = dprefix(nsym)

				on = fg.ext_onodes.vk[sym]   # is it always there ? 
				fg.ext_onodes[dnodes[on]] = dprefix(nsym)

				push!(foutmap, n2)

			else
				nn = createzeronode!(fg2, n2)
			end	

			fdnodes[n2] = nn
		end

		# builds the graph for derivatives calculations
		println("===  reverse pass ===")
		reversepass!(fg2, fg, fdnodes)
		fg.nodes = [ fg.nodes, fg2.nodes]
		# merge!(fg.inmap, fg2.inmap)

		# foutmap2 = {}
		# for ni in foutmap
		# 	push!(foutmap2, (fdnodes[ni], fg.map.inmap[ni]))
		# end

	println("=== fg  1 ===")
	println(fg.nodes)
	println("foutmap = $(repr(foutmap))")
	println("fdnodes = $(collect(fdnodes))")
	println("ext = $(collect(fg.ext_inodes.kv))")
	println("set = $(collect(fg.set_inodes.kv))")

		prune!(fg, [ fdnodes[ni] for ni in foutmap ]) # reduce to derivatives evaluation only

	println("=== fg  2 ===")
	println(fg.nodes)
	println("foutmap = $(repr(foutmap))")
	println("fdnodes = $(collect(fdnodes))")
	println("ext = $(collect(fg.ext_inodes.kv))")
	println("set = $(collect(fg.set_inodes.kv))")

		# create for loop
		println("=== create dfor node ===")
		v2 = addnode!(g2, NFor([ n.main[1], fg]) )
		v2.parents = collect( keys( fg.ext_onodes))

		# outmap = dnodes of fg's ingoing variables
		for ns2 in filter(n -> haskey(fdnodes,n) & haskey(fg.set_inodes,n), foutmap)
			# rn = addnode!(g2, NIn("dout", [v2]))  # external node, receiving loop result
			# fdn = ns2[1]                          # final node in loop containing derivative
			# fg.outmap[ fdn ] = rn                 # link those two

			# n0 = ns2[2]
			# pn = dnodes[n0] 
			# fg.link[fdn] = pn
			# dnodes[n0] = rn 

			rn = addnode!(g2, NIn("dout", [v2]))  # external node, receiving loop result
			fdn = fdnodes[ns2]                    # final node in loop containing derivative
			nsym = fg.set_inodes.kv[fdn]
			fg.map[fdn] = (nsym, :out_inode)
			fg.map[rn]  = (nsym, :out_onode)

			n0 = fg.map.vk[ (nsym, :in_onode)]
			dnodes[n0] = rn 
		end

	println("=== fg  3 ===")
	println(fg.nodes)
	println("foutmap = $(repr(foutmap))")
	println("fdnodes = $(collect(fdnodes))")
	println("ext = $(collect(fg.ext_inodes.kv))")
	println("set = $(collect(fg.set_inodes.kv))")

	end

	evalsort!(g)
	for n2 in reverse(g.nodes)
		println("=======")
		for n3 in g2.nodes
			dn = collect(keys(filter( (k,v) -> v == n3, dnodes ) ))
			println(" $n3,  dn = $(repr(dn))")
		end
		# tg = copy(g)
		# # tg2 = copy(g2)
	 #    tg.nodes = [ tg.nodes, g2.nodes]
	 #    tsetmap = Dict()
	 #   	for (k,v) in filter((k,v) -> isa(k, NExt) & in(k.main, [:x]), dnodes)
		# 	tsetmap[dprefix(k.main)] = v
		# end
  #   	tg.setmap = merge(tg.setmap, tsetmap)
  #   	println(tocode(tg))

		rev(n2)
	end
end


