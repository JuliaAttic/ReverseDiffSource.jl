#########################################################################
#
#    Graph manipulation functions
#
#########################################################################

######## transforms n-ary +, *, max, min, sum, etc...  into binary ops  ###### 
function splitnary!(g::ExGraph)
	for n in g.nodes
	    if isa(n, NCall) &
	        in(n.main, [:+, :*, :sum, :min, :max]) & 
	        (length(n.parents) > 2 )

	        nn = add_node(g, :call, n.main, n.parents[2:end] )
	        n.parents = [n.parents[1], nn]  
	    
	    elseif isa(n, NFor)
	    	splitnary!(n.main[2])

	    end
	end
end

####### fuses nodes n1 and n2  ########
# removes node nr and keeps node nk 
#  updates links and references in exitnodes
function fusenodes(g::ExGraph, nk::ExNode, nr::ExNode)

	# replace references to nr by nk in parents of other nodes
    for n in filter(n -> !in(n,[nr,nk]), g.nodes)  
        n.parents[ n.parents .== nr] = nk
    end

	# replace references to nr in exitnodes dictionnary
    for (k,v) in g.exitnodes
        (v == nr) && (g.exitnodes[k] = nk)
    end

    # remove node nr in g
    filter!(n -> n != nr, g.nodes)
end

####### fuse identical nodes  ###########
function dedup!(g::ExGraph)
	i = 1 
	while i < length(g.nodes) 
	    pg = g.nodes[i]
	    sig = (pg.main, pg.nodetype, pg.parents)

	    restart = false
	    for j in (i+1):(length(g.nodes))
	        pg2 = g.nodes[j]
		    sig2 = (pg2.main, pg2.nodetype, pg2.parents)
	        if (sig == sig2) & !isa(pg2, NAlloc)  # do not fuse allocations !

	            fusenodes(g, pg, pg2)

	            # now that the graph is changed, we need to start over
	            restart = true  
				break
	        end
	    end

	    i = restart ? 1 : (i + 1)
	end

	# separate pass on subgraphs
	map( n -> dedup!(n.main[2]), filter(n->isa(n, NFor), g.nodes))
end

####### evaluate operators on constants  ###########
function evalconstants!(g::ExGraph, emod = Main)
	for n in g.nodes
		if isa(n, NCall) & 
			all( map(n->isa(n, NConst), n.parents) ) &
			!in(n.main, [:zeros, :ones, :vcat])
			res = invoke(emod.eval(n.main), 
	            tuple([ typeof(x.main) for x in n.parents]...),
	            [ x.main for x in n.parents]...)

			nn = add_node(g, :constant, res)
			fusenodes(g, nn, n) # remove n
			# n.nodetype = :constant
			# n.parents = ExNode[]
			# n.main = res
	    
	    elseif isa(n, NFor)
	    	evalconstants!(n.main[2], emod)

		end
	end
end

####### simplify some expressions (+ 0., * 1., / 1.)  ###########
function simplify!(g::ExGraph)

	i = 1
	while i <= length(g.nodes)
		restart = false
		n = g.nodes[i]
		if isa(n, NCall) & (length(n.parents) == 2) # restricted to binary ops

			if isa(n.parents[1], NConst)
				sig = (n.main, n.parents[1].main)
				if in(sig, [(:+, 0), (:*, 1), (:+, 0.), (:*, 1.),
					        (:.*, 1), (:.*, 1.)]) 
					restart = true
					fusenodes(g, n.parents[2], n)

				elseif in(sig, [(:*, 0), (:*, 0.), (:/, 0), (:/, 0.),
					            (:.*, 0), (:.*, 0.), (:./, 0), (:./, 0.)]) 
					restart = true
					n.nodetype = :constant
					n.main = 0.0
					n.parents = ExNode[]
				end

			elseif isa(n.parents[2], NConst)
				sig = (n.main, n.parents[2].main)	

				if in(sig, [(:+, 0), (:-, 0), (:*, 1), (:/, 1), (:^, 1),
					        (:+, 0.), (:-, 0.), (:*, 1.), (:/, 1.), (:^, 1.),
					        (:.*, 1), (:./, 1), (:.^, 1), 
					        (:.*, 1.), (:./, 1.), (:.^, 1.)]) 
					restart = true
					fusenodes(g, n.parents[1], n)

				elseif in(sig, [(:*, 0), (:*, 0.), (:.*, 0), (:.*, 0.)])
					restart = true
					n.nodetype = :constant
					n.main = 0.0
					n.parents = ExNode[]
				end
			end

		elseif isa(n, NSRef) &&
			   isa(n.parents[2], NRef) &&
			   (n.main == n.parents[2].main) &&
			   (n.parents[1] == n.parents[2].parents[1])

			restart = true
			fusenodes(g, n.parents[1], n)

		elseif isa(n, NSDot) &&
			   isa(n.parents[2], NDot) &&
			   (n.main == n.parents[2].main) &&
			   (n.parents[1] == n.parents[2].parents[1])

			restart = true
			fusenodes(g, n.parents[1], n)

		end

	    i = restart ? 1 : (i + 1)
	end

	# separate pass on subgraphs
	map( n -> simplify!(n.main[2]), filter(n->isa(n, NFor), g.nodes))

end

####### trims the graph to necessary nodes for exitnodes to evaluate  ###########
function prune!(g::ExGraph)
	g2 = ancestors(collect(values(g.exitnodes)))
	filter!(n -> in(n, g2), g.nodes)

	# separate pass on subgraphs
	map( n -> prune!(n.main[2]), filter(n->isa(n, NFor), g.nodes))

end

####### sort graph to an evaluable order ###########
function evalsort!(g::ExGraph)
	g2 = ExNode[]

	while length(g2) < length(g.nodes)
		canary = length(g2)
		nl = setdiff(g.nodes, g2)
	    for n in nl
	        if !any( [ in(x, nl) for x in n.parents] ) # | (length(n.parents) == 0)
	            push!(g2,n)
	        end
	    end
	    (canary == length(g2)) && error("[evalsort!] probable cycle in graph")
	end

	g.nodes = g2

end

####### calculate the value of each node  ###########
function calc!(g::ExGraph; params=Dict(), emod = Main)


	function evaluate(n::Union(NAlloc, NCall))
		invoke(emod.eval(n.main), 
	           tuple([ typeof(x.val) for x in n.parents]...),
	           [ x.val for x in n.parents]...)
	end 

	evaluate(n::NExt) = get(params, n.main, emod.eval(n.main))
    # TODO : catch error if undefined
	evaluate(n::NConst) = emod.eval(n.main)
	evaluate(n::NRef)   = emod.eval( Expr(:ref, n.parents[1].val, n.main...) )
	evaluate(n::NDot)   = emod.eval( Expr(:., n.parents[1].val, n.main) )
	evaluate(n::NSRef)  = n.parents[1].val
	evaluate(n::NSDot)  = n.parents[1].val
	evaluate(n::NFor)   = (calc!(n.main[2]) ; nothing)
	evaluate(n::NIn)    = nothing

	evalsort!(g)
	for n in g.nodes
		n.val = evaluate(n)
	end

end

###### inserts graph src into dest  ######
function add_graph!(src::ExGraph, dest::ExGraph, smap::Dict)
    evalsort!(src)
    nmap = Dict()
    for n in src.nodes  #  n = src[1]  
        if isa(n, NExt) 
            nn = add_node(dest, n.nodetype, n.main, 
            				[ nmap[n2] for n2 in n.parents ])
            nmap[n] = nn
        else
            if haskey(smap, n.main)
                nmap[n] = smap[n.main]
            else
	            nn = add_node(dest, n.nodetype, n.main, [])
	            nmap[n] = nn

                warn("unmapped symbol in source graph $(n.main)")
            end
        end
    end

    nmap
end