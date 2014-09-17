#########################################################################
#
#   ExGraph type definition and related functions
#
#########################################################################
  

#####  ExGraph type definition ######
type ExGraph
  nodes::Vector{ExNode}  # nodes in this graph
  exti::BiDict{ExNode, Any}
  seti::BiDict{ExNode, Any}
  exto::BiDict{ExNode, Any}
  seto::BiDict{ExNode, Any}
end

ExGraph()                   = ExGraph( ExNode[] )
ExGraph(vn::Vector{ExNode}) = ExGraph( vn, BiDict{ExNode, Any}(), 
                                           BiDict{ExNode, Any}(), 
                                           BiDict{ExNode, Any}(), 
                                           BiDict{ExNode, Any}() )


function show(io::IO, g::ExGraph)
  # construct node number
  for (i,n) in enumerate(g.nodes)
    print(io, rpad("#$i",4))

    if haskey(g.exti, n)
      print(io, rpad("< $(g.exti[n])", 6))
    elseif haskey(g.seti, n)
      print(io, rpad("> $(g.seti[n])", 6))
    else
      print(io, rpad("", 6))
    end

    print(io, rpad("[$(subtype(n))]", 12))
    main = isa(n, NFor) ? n.main[1] : n.main
    print(io, rpad("$(repr(main)) ", 10))
    print(io, rpad("($(repr(n.val)))", 10))

    if length(n.parents) > 0
      pnn = join( map( x -> "#$x", indexin(n.parents, {g.nodes...})), ", ")
      print(io, ", parents : $pnn")
    end

    if length(n.precedence) > 0
      pnn = join( map( x -> "#$x", indexin(n.precedence, {g.nodes...})), ", ")
      print(io, ", precedence : $pnn")
    end

    println() 
  end
end

#####  ExGraph functions  #####

### looks for ancestors, optionnally excluding some nodes
function ancestors(ns::Vector, except=[])
    ss = setdiff(ns, except)
    isempty(ss) ? [] : mapreduce(n -> ancestors(n,except), union, ss)
end
ancestors(ns, except=[]) = union([ns], ancestors(ns.parents, except))

# copies a graph and its nodes, leaves onodes references intact
function copy(g::ExGraph)
  g2 = ExGraph()
  nmap = Dict()
  evalsort!(g)
  for n in g.nodes
    n2 = addnode!(g2, copy(n))
    n2.parents    = [ nmap[n] for n in n2.parents    ]
    n2.precedence = [ nmap[n] for n in n2.precedence ]
    nmap[n] = n2
  end

  # update onodes of subgraphs (for loops)
  for n in filter(x -> isa(x, NFor), g2.nodes)
    fg = n.main[2]
    no = BiDict{ExNode, Any}()
    for (k,v) in fg.exto.kv
      no[ nmap[k] ] = v
    end
    fg.exto = no

    no = BiDict{ExNode, Any}()
    for (k,v) in fg.seto.kv
      no[ nmap[k] ] = v
    end
    fg.seto = no
  end

  # copy node mapping and translate inner nodes to newly created ones
  g2.exto = BiDict{ExNode, Any}(g.exto.kv)
  g2.seto = BiDict{ExNode, Any}(g.seto.kv)
  for (k,v) in g.exti.kv
    g2.exti[ nmap[k] ] = v
  end
  for (k,v) in g.seti.kv
    g2.seti[ nmap[k] ] = v
  end

  g2
end

# add a single node
addnode!(g::ExGraph, nn::ExNode) = ( push!(g.nodes, nn) ; return g.nodes[end] )

######## transforms n-ary +, *, max, min, sum, etc...  into binary ops  ###### 
function splitnary!(g::ExGraph)
  for n in g.nodes
      if isa(n, NCall) &&
          in(n.main, [:+, :*, :sum, :min, :max, :vcat]) && 
          (length(n.parents) > 2 )

          nn = addnode!(g, NCall( n.main, n.parents[2:end] ) )
          n.parents = [n.parents[1], nn]  
      
      elseif isa(n, NFor)
        splitnary!(n.main[2])

      end
  end
  g
end

####### fuses nodes nr and nk, keeps nk ########
# removes node nr and keeps node nk 
#  updates all references to nr
function fusenodes(g::ExGraph, nk::ExNode, nr::ExNode)
  # this should not happen...
  @assert !haskey(g.exti, nr) "[fusenodes] attempt to fuse ext_inode $nr"

  # test if nr is associated to a variable
  # if true, we create an NIn on nk, and associate var to it
  if haskey(g.seti, nr)
    nn = addnode!(g, NIn(g.seti[nr], [nk]))
    g.seti[nn] = g.seti[nr]  # nn replaces nr as set_inode

    if haskey(g.seto, nr)   # change onodes too (if we are in a subgraph)
      g.seto[nn] = g.seto[nr]  # nn replaces nr as set_onode
    end  
  end

  # # same for references to nr in subgraphs
  # for n in filter(n -> isa(n, NFor) && n != nr && n != nk, g.nodes)
  #   g2 = n.main[2]

  #   # this should not happen...
  #   @assert !haskey(g2.seto, nr) "[fusenodes (for)] attempt to fuse set_onode $nr"

  #   if haskey(g2.exto, nr)
  #     nn = addnode!(g, NIn(g2.exto[nr], [nk]))
  #     g2.exto[nn] = g2.exto[nr]  # nn replaces nr as g2.exto
  #   end  
  # end

  # # replace references to nr by nk in parents of other nodes
  # for n in filter(n -> n != nr && n != nk, g.nodes)
  #   for (i, n2) in enumerate(n.parents)
  #     n2 == nr && (n.parents[i] = nk)
  #   end
  #   for (i, n2) in enumerate(n.precedence)
  #     n2 == nr && (n.precedence[i] = nk)
  #   end
  # end

  # replace references to nr by nk in parents of other nodes
  for n in filter(n -> n != nr && n != nk, g.nodes)
    if isa(n, NFor)
      g2 = n.main[2]

      # this should not happen...
      @assert !haskey(g2.seto, nr) "[fusenodes (for)] attempt to fuse set_onode $nr"

      if haskey(g2.exto, nr)
        nn = addnode!(g, NIn(g2.exto[nr], [nk]))
        g2.exto[nn] = g2.exto[nr]  # nn replaces nr as g2.exto
        for (i, n2) in enumerate(n.parents)
          n2 == nr && (n.parents[i] = nn)
        end
      end  
    end

    for (i, n2) in enumerate(n.parents)
      n2 == nr && (n.parents[i] = nk)
    end
    for (i, n2) in enumerate(n.precedence)
      n2 == nr && (n.precedence[i] = nk)
    end

  end

  # remove node nr in g
  filter!(n -> n != nr, g.nodes)
end

####### trims the graph to necessary nodes for exitnodes to evaluate  ###########
prune!(g::ExGraph) = prune!(g, collect(keys(g.seti.kv)))

function prune!(g::ExGraph, exitnodes)
  ns2 = copy(exitnodes)
  evalsort!(g)
  for n in reverse(g.nodes)
    n in ns2 || continue

    if isa(n, NFor)
      g2 = n.main[2]

      # list of g2 nodes whose outer node is in ns2
      exitnodes2 = ExNode[]
      for (k, sym) in g2.seto.kv
        k in ns2 || continue
        push!(exitnodes2, g2.seti.vk[sym])
      end

      prune!(g2, exitnodes2)

      # update parents
      n.parents = [n.parents[1], intersect(n.parents, collect(keys(g2.exto)) ) ]
    end

    ns2 = union(ns2, n.parents)
  end

  # remove unused external inodes in map and corresponding onodes (if they exist)
  for (k,v) in g.exti.kv
    k in ns2 && continue
    delete!(g.exti, k)
    haskey(g.exto.vk, v) && delete!(g.exto, g.exto.vk[v])
  end
  # reduce set inodes to what was specified in initial exitnodes parameter
  for (k,v) in g.seti.kv
    k in exitnodes && continue
    delete!(g.seti, k)
    haskey(g.seto.vk, v) && delete!(g.seto, g.seto.vk[v])
  end

  # remove precedence nodes that have disappeared
  for n in ns2
    n.precedence = intersect(n.precedence, ns2)
  end

  g.nodes = intersect(g.nodes, ns2)
  g
end

####### sort graph to an evaluable order ###########
function evalsort!(g::ExGraph)
  ns = ExNode[]
  while length(ns) < length(g.nodes)
    canary = length(ns)
    nl = setdiff(g.nodes, ns)
    for n in nl
      any(x -> x in nl, n.parents) && continue
      any(x -> x in nl, n.precedence) && continue
      push!(ns,n)
    end
    (canary == length(ns)) && error("[evalsort!] cycle in graph")
  end
  g.nodes = ns

  # separate pass on subgraphs
  map( n -> evalsort!(n.main[2]), filter(n->isa(n, NFor), g.nodes))

  g
end

####### calculate the value of each node  ###########
function calc!(g::ExGraph; params=Dict(), emod = Main)

  function myeval(thing)
    local ret   
    if haskey(params, thing)
      return params[thing]
    else
      try
        ret = emod.eval(thing)
      catch e
        println("[calc!] can't evaluate $thing")
        rethrow(e)
      end
      return ret
    end
  end

  function evaluate(n::NCall)
    local ret
    try
      # if isgeneric(n.main)
      #   ret = invoke(emod.eval(n.main), 
      #     tuple([ typeof(x.val) for x in n.parents]...),
      #     [ x.val for x in n.parents]...)

      # else
        ret = emod.eval( Expr(:call, n.main, { x.val for x in n.parents}...) )

      # end

    catch
      error("[calc!] can't evaluate $(n.main)")
    end
    return ret
  end 

  function evaluate(n::NExt)
    haskey(g.exti, n) || return myeval(n.main)

    sym = g.exti[n]  # should be equal to n.main but just to be sure.. 
    haskey(g.exto.vk, sym) || return myeval(n.main)
    return g.exto.vk[sym].val  # return node val in parent graph
  end

  evaluate(n::NConst) = n.main
  # evaluate(n::NRef)   = myeval( Expr(:ref, n.parents[1].val, 
  #                                    map(a->myeval(a), n.main)... ) )
  evaluate(n::NRef)   = myeval( Expr(:ref , { x.val for x in n.parents}...))
  evaluate(n::NDot)   = myeval( Expr(:.   , n.parents[1].val, n.main) )
  evaluate(n::NSRef)  = n.parents[1].val
  evaluate(n::NSDot)  = n.parents[1].val

  function evaluate(n::NIn)
      isa(n.parents[1], NFor) && return n.parents[1].val[n]
      n.parents[1].val
  end

  function evaluate(n::NFor)
    g2 = n.main[2]
    is = n.main[1]                          # symbol of loop index
    iter = evaluate(n.parents[1])           #  myeval(n.main[1].args[2])
    is0 = next(iter, start(iter))[2]        # first value of index
    params2 = merge(params, { is => is0 })  # set loop index to first value
    # println("params2 : $(params2)")
    calc!(g2, params=params2)
    
    valdict = Dict()
    for (k, sym) in g2.seto
      valdict[k] = g2.seti.vk[sym].val
    end

    valdict
  end

  evalsort!(g)
  for n in g.nodes
    n.val = evaluate(n)
  end

  g
end

###### inserts graph src into dest  ######
# TODO : inserted graph may update variables and necessitate a precedence update

addgraph!(src::Expr, dest::ExGraph, smap::Dict) = addgraph!(tograph(src), dest, smap)

function addgraph!(src::ExGraph, dest::ExGraph, smap::Dict)
  length(src.exto.kv)>0 && warn("[addgraph] adding graph with external onodes")
  length(src.seto.kv)>0 && warn("[addgraph] adding graph with set onodes")
  # TODO : this control should be done at the deriv_rules.jl level

  ig = copy(src) # make a copy, update references
  exitnode = ig.seti.vk[nothing] # result of added subgraph

  evalsort!(ig)

  nmap = Dict()
  for n in ig.nodes  #  n = src[1]  
    if isa(n, NExt)
      if haskey(smap, n.main)
        nmap[n] = smap[n.main]

        # should the exitnode be a NExt, it has to be updated
        if n == exitnode
          exitnode = nmap[n]
        end

      else
        error("unmapped symbol in source graph $(n.main)")
      end

    elseif isa(n, NFor) # update references, including onodes
      n.parents =    [ haskey(nmap, n2) ? nmap[n2] : n2 for n2 in n.parents    ]
      n.precedence = [ haskey(nmap, n2) ? nmap[n2] : n2 for n2 in n.precedence ]

      g2 = n.main[2]
      for (n2, sym) in g2.exto
        haskey(nmap, n2) || continue
        g2.exto[ nmap[n2]] = sym
      end

      push!(dest.nodes, n)

    else  # update references to NExt that have been remapped
      n.parents =    [ haskey(nmap, n2) ? nmap[n2] : n2 for n2 in n.parents    ]
      n.precedence = [ haskey(nmap, n2) ? nmap[n2] : n2 for n2 in n.precedence ]
      push!(dest.nodes, n)

    end
  end

  # return exitnode of subgraph
  # ig.seti.vk[nothing]
  exitnode
end
