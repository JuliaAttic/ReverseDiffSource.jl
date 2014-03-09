#########################################################################
#
#    Graph simplification
#      Walks the tree to 
#           - fuse duplicates
#           - apply  simplification rules  
#            #TODO : organize simplification rules
#
#########################################################################

function simplify!(g::ExGraph)

	i = 1
	while i <= length(g.nodes)
		restart = false
		n = g.nodes[i]

		###### duplicates ?
	    for j in (i+1):(length(g.nodes))
	        n2 = g.nodes[j]
	        if isequal(n,n2) & !isa(n2, NAlloc)  # do not fuse allocations !

	            fusenodes(g, n, n2)

	            # now that the graph is changed, we need to start over
	            restart = true  
				break
	        end
	    end

	    ###### simplifications possible ?
	    if !restart
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
						nn = add_node(g, :constant, 0.0)
						fusenodes(g, nn, n) 

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
						nn = add_node(g, :constant, 0.0)
						fusenodes(g, nn, n) 

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
		end

		
	    i = restart ? 1 : (i + 1)
	end

	# separate pass on subgraphs
	map( n -> simplify!(n.main[2]), filter(n->isa(n, NFor), g.nodes))

end
