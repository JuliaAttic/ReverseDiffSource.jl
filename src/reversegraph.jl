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
			dnodes[n] = add_node(g2, NConst(1.0))
		else
			dnodes[n] = createzeronode!(g2, n)
		end
	end

	# builds the graph for derivatives calculations
	reversepass!(g2, g, dnodes)

	# store in setmap the nodes containing the derivatives of diffsym
	for (k,v) in filter((k,v) -> isa(k, NExt) & in(k.main, diffsym), dnodes)
		g2.setmap[dprefix(k.main)] = v
	end

    g2
end

# creates the starting points for derivatives accumulation variables
function createzeronode!(g2::ExGraph, n)
	if isa(n.val, Real)
		return add_node(g2, NConst(0.0))
	
	# Array of Real
	elseif isa(n.val, Array{Float64}) | isa(n.val, Array{Int})
		v1 = add_node(g2, NCall(:size, [n]))
		return add_node(g2, NAlloc(:zeros, [v1]))

	# Composite type
	elseif haskey(tdict, typeof(n.val))   # known composite type
		v1 = add_node(g2, NConst( tdict[typeof(n.val)]) )
		return add_node(g2, NAlloc(:zeros, [v1]) )

	# Array of composite type
	elseif isa( n.val, Array) && haskey(tdict, eltype(n.val))  
		v1 = add_node(g2, NCall(:size, [n]) )
		aa = ExNode[ add_node(g2, NAlloc(:zeros, [v1]) )
		               for i in 1:(tdict[eltype(n.val)]) ]
		return add_node(g2, NCall(:vcat, aa) )

	else
		error("[reversegraph] Unknown variable type $(typeof(n.val))")
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

            	nmap = add_graph!(dg, g2, smap)

        		v2 = add_node(g2, NCall(:+, [dnodes[arg], nmap[de]]) )
        		dnodes[arg] = v2
            end
        end
	end		 

	function rev(n::NRef)
        v2 = add_node(g2, NRef(n.main, [dnodes[n.parents[1]]]) )
        v3 = add_node(g2, NCall(:+, [v2, dnodes[n]]) )
		v4 = add_node(g2, NSRef(n.main, [dnodes[n.parents[1]], v3]) )
		dnodes[n.parents[1]] = v4
	end

	function rev(n::NSRef)
		v2 = add_node(g2, NRef(n.main, [dnodes[n.parents[1]]]) )
		println("v2  $v2")
		v3 = add_node(g2, NCall(:+, [v2, dnodes[n]]) )
		println("v3  $v3")
		# v4 = add_node(g2, NSRef(n.main, [dnodes[n.parents[1]], v3]) )
		dnodes[n] = v3
	end

	function rev(n::NDot)
        v2 = add_node(g2, NDot( n.main, [dnodes[n.parents[1]]]) )
        v3 = add_node(g2, NCall(:+, [v2, dnodes[n]]) )
		v4 = add_node(g2, NSDot(n.main, [dnodes[n.parents[1]], v3]) )
		dnodes[n.parents[1]] = v4
	end

	function rev(n::NFor)
		gf = n.main[2]          # subgraph of for loop
		is = n.main[1].args[1]  # symbol of loop index

		gf2     = copy(gf)
		dgf2    = ExGraph()
		fdnodes = Dict()
		println("==========")
		for n2 in filter(n-> !isa(n,NFor), gf2.nodes)
			nn = isa(n2.main, Symbol) ? NExt(dprefix(n2.main)) : NExt("abc")
			add_node(dgf2, nn)  # start point of deriv accumulator
			fdnodes[n2] = nn
			if haskey(gf2.outmap, n2)
				dgf2.inmap[nn] = dnodes[gf2.outmap[n2]]
			elseif isa(n2, NExt)  # update inmap, etc..
				dgf2.inmap[nn] = dnodes[gf2.inmap[n2]]
			end	
		end
		println("==========")

		# builds the graph for derivatives calculations
		reversepass!(dgf2, gf2, fdnodes)
		println("==========")

		# create for loop
		v2 = add_node(g2, NFor([ n.main[1], dgf2]) )
		v2.parents = collect(values(dgf2.inmap))

		println("==== gf2 =====")
		println(gf2.nodes)
		println("==== dgf2 ====")
		println(dgf2.nodes)

		for n2 in dgf2.nodes
			for n3 in n2.parents
				if !in(n3, dgf2.nodes)
					println("!!! : $n2 has parents outside of dgf2")
				end
			end
		end

		# gf2.nodes = [ gf2.nodes, dgf2.nodes ]
		println("==========")
		for (k,v) in fdnodes ; println("dnodes -- $k  => $v") ; end
		for (k,v) in dgf2.inmap ; println("inmap -- $k  => $v") ; end
		for (k,v) in dgf2.outmap ; println("outmap -- $k  => $v") ; end
		println("==========")

		for (k,v) in filter((k,v) -> in(v, dgf2.nodes) & haskey(gf2.inmap, k), fdnodes)
			println("[dfor outmap] (1) $k - $v")
			rn = add_node(g2, NIn("duh", [v2]))  # exit node for this var in this graph
			dgf2.outmap[v] = rn 
			println("[dfor outmap] (2) $v - $rn")
			p0 = gf2.inmap[ k ]
			println("[dfor outmap] (3) p0 = $p0")
			pn = dnodes[ p0 ]
			println("[dfor outmap] (3) pn = $pn")
			dgf2.link[v] = pn
			println("[dfor outmap] (4)")
			dnodes[p0] = rn      # are you lost ?  me too
		end

	end

	evalsort!(g)
	map(rev, reverse(g.nodes))
end