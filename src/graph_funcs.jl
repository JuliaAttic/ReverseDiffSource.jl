#########################################################################
#
#    Graph manipulation functions
#
#########################################################################

######## transforms n-ary +, *, max, min, sum, etc...  into binary ops  ###### 
function splitnary!(g::ExGraph)
	for n in g.nodes
	    if (n.nodetype == :call) &
	        in(n.name, [:+, :*, :sum, :min, :max]) & 
	        (length(n.parents) > 2 )

	        nn = add_node(g, :call, n.name, n.parents[2:end] )
	        n.parents = [n.parents[1], nn]  
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
	    sig = (pg.name, pg.nodetype, pg.parents)

	    restart = false
	    for j in (i+1):(length(g.nodes))
	        pg2 = g.nodes[j]
		    sig2 = (pg2.name, pg2.nodetype, pg2.parents)
	        if (sig == sig2) &
	        	(pg2.nodetype != :alloc)  # do not fuse allocations !

	            fusenodes(g, pg, pg2)

	            # now that the graph is changed, we need to start over
	            restart = true  
				break
	        end
	    end

	    i = restart ? 1 : (i + 1)
	end
end

####### evaluate operators on constants  ###########
function evalconstants!(g::ExGraph, emod = Main)
	for n in g.nodes
		if (n.nodetype == :call) & 
			all( map(n->n.nodetype, n.parents) .== :constant) &
			!in(n.name, [:zeros, :ones])
			res = invoke(emod.eval(n.name), 
	            tuple([ typeof(x.name) for x in n.parents]...),
	            [ x.name for x in n.parents]...)

			n.nodetype = :constant
			n.parents = ExNode[]
			n.name = res
		end
	end
end

####### simplify some expressions (+ 0., * 1., / 1.)  ###########
function simplify!(g::ExGraph)

	i = 1
	while i <= length(g.nodes)
		restart = false
		n = g.nodes[i]
		if (n.nodetype == :call) & (length(n.parents) == 2) # restricted to binary ops

			if n.parents[1].nodetype == :constant
				sig = (n.name, n.parents[1].name)
				if in(sig, [(:+, 0), (:*, 1), (:+, 0.), (:*, 1.),
					        (:.*, 1), (:.*, 1.)]) 
					restart = true
					fusenodes(g, n.parents[2], n)

				elseif in(sig, [(:*, 0), (:*, 0.), (:/, 0), (:/, 0.),
					            (:.*, 0), (:.*, 0.), (:./, 0), (:./, 0.)]) 
					restart = true
					n.nodetype = :constant
					n.name = 0.0
					n.parents = ExNode[]
				end

			elseif n.parents[2].nodetype == :constant
				sig = (n.name, n.parents[2].name)	

				if in(sig, [(:+, 0), (:-, 0), (:*, 1), (:/, 1), (:^, 1),
					        (:+, 0.), (:-, 0.), (:*, 1.), (:/, 1.), (:^, 1.),
					        (:.*, 1), (:./, 1), (:.^, 1), 
					        (:.*, 1.), (:./, 1.), (:.^, 1.)]) 
					restart = true
					fusenodes(g, n.parents[1], n)

				elseif in(sig, [(:*, 0), (:*, 0.), (:.*, 0), (:.*, 0.)])
					restart = true
					n.nodetype = :constant
					n.name = 0.0
					n.parents = ExNode[]
				end
			end

		elseif (n.nodetype == :subref) &&
			   (n.parents[2].nodetype == :ref) &&
			   (n.name == n.parents[2].name) &&
			   (n.parents[1] == n.parents[2].parents[1])

			restart = true
			fusenodes(g, n.parents[1], n)

		elseif (n.nodetype == :subdot) &&
			   (n.parents[2].nodetype == :dot) &&
			   (n.name == n.parents[2].name) &&
			   (n.parents[1] == n.parents[2].parents[1])

			restart = true
			fusenodes(g, n.parents[1], n)

		end

	    i = restart ? 1 : (i + 1)
	end
end

####### trims the graph to necessary nodes for exitnodes to evaluate  ###########
function prune!(g::ExGraph)
	g2 = ancestors(collect(values(g.exitnodes)))
	filter!(n -> in(n, g2), g.nodes)
end

####### sort graph to an evaluable order ###########
function evalsort!(g::ExGraph)
	g2 = ExNode[]

	while length(g2) < length(g.nodes)
		canary = length(g2)
	    for n in setdiff(g.nodes, g2)
	        if all( [ in(x, g2) for x in n.parents] ) # | (length(n.parents) == 0)
	            push!(g2,n)
	        end
	    end
	    (canary == length(g2)) && error("[evalsort!] probable cycle in graph")
	end

	g.nodes = g2
end

####### calculate the value of each node  ###########
function calc!(g::ExGraph; params=nothing, emod = Main)

	evalsort!(g)
	for n in g.nodes
	    if n.nodetype==:external
	    	println(" $(typeof(params)) - $(keys(params)) - $(n.name)"  )
	    	if isa(params, Dict) && haskey(params, n.name)
	        	n.value = params[n.name]
	        else
	        	n.value = emod.eval(n.name) # TODO : catch error if undefined
	        end

	    elseif n.nodetype == :constant
	        n.value = emod.eval(n.name)

	    elseif n.nodetype == :ref
	        n.value = emod.eval( Expr(:ref, n.parents[1].value, n.name) )

	    elseif n.nodetype == :dot
	        n.value = emod.eval( Expr(:., n.parents[1].value, n.name) )

	    elseif in(n.nodetype, [:call, :alloc])
	        n.value = invoke(emod.eval(n.name), 
	            tuple([ typeof(x.value) for x in n.parents]...),
	            [ x.value for x in n.parents]...)

	    elseif in(n.nodetype, [:subref, :subdot])
	    	n.value = n.parents[1].value  

	    end
	end
end

###### inserts graph src into dest  ######
function add_graph!(src::ExGraph, dest::ExGraph, exitnode::ExNode, smap::Dict)

    evalsort!(src)
    # exitnode2
    nmap = Dict()
    for n in src.nodes  #  n = src[1]  
        if n.nodetype != :external 
            nn = add_node(dest, n.nodetype, n.name, 
            				[ nmap[n2] for n2 in n.parents ])
            nmap[n] = nn
        else
            if haskey(smap, n)
                nmap[n] = smap[ n ]
            else
                warn("unmapped symbol in source graph $(n.name)")
            end
        end
    end

    nmap[exitnode]
end
