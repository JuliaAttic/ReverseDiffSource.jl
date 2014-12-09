#########################################################################
#
#   ExGraph type definition and related functions
#
#########################################################################
  
typealias NSMap BiDict{ExNode, Any}   # ExNode - Symbol map

#####  ExGraph type definitions ######
type ExGraph
  nodes::Vector{ExNode}  # nodes in this graph
  exti::NSMap
  seti::NSMap
  exto::NSMap
  seto::NSMap
end

ExGraph()                   = ExGraph( ExNode[] )
ExGraph(vn::Vector{ExNode}) = ExGraph( vn, NSMap(), 
                                           NSMap(), 
                                           NSMap(), 
                                           NSMap() )

hasnode(m::NSMap, n::ExNode) = haskey(m.kv, n)
hassym( m::NSMap, k)         = haskey(m.vk, k)
nodes(m::NSMap)              = keys(m.kv)
syms( m::NSMap)              = keys(m.vk)

getnode(m::NSMap, k)         = m.vk[k]
getnode(m::NSMap, k::Void)   = m.vk[nothing]
getsym( m::NSMap, n::ExNode) = m.kv[n]


function show(io::IO, g::ExGraph)
  tn = fill("", length(g.nodes), 8)

  for (i,n) in enumerate(g.nodes)
    tn[i,1] = "$i"

    if hasnode(g.exti, n)
      sym = g.exti[n]
      tn[i,2] = "$sym >>"
      hassym(g.exto, sym) && (tn[i,3] = "+")
    elseif hasnode(g.seti, n)
      sym = g.seti[n]
      tn[i,2] = "$sym <<"
      hassym(g.seto, sym) && (tn[i,3] = "+")
    end

    tn[i,4] = "[$(subtype(n))]"
    main = isa(n, NFor) ? n.main[1] : n.main

    tn[i,5] = join( map( x -> "$x", indexin(n.parents,    Any[g.nodes...])), ", ")
    tn[i,6] = join( map( x -> "$x", indexin(n.precedence, Any[g.nodes...])), ", ")

    tn[i,7] = "$(repr(main))"
    tn[i,8] = "$(typeof(n.val)) $(repr(n.val)[1:min(40, end)])"
  end

  tn = vcat(["node" "symbol" "ext ?" "type" "parents" "precedence" "main" "value"], 
        tn)
  sz = maximum(map(length, tn), 1)
  tn = vcat(tn[1,:], map(s->"-"^s, sz), tn[2:end,:])
  for i in 1:size(tn,1)
    for j in 1:size(tn, 2)
      print(io, rpad(tn[i,j], sz[j]), " | ")
    end
    println(io)
  end

end


#####  ExGraph functions  #####

### checks if n is an ancestor of 'exits', optionnally excluding some nodes from the path
##  !! assumes that g has been evalsorted
function isancestor(anc::ExNode, exits::Vector{ExNode}, g::ExGraph, except::Vector{ExNode})
    gm = Set(exits)
    for n in reverse( g.nodes )
        n in gm     || continue
        n in except && continue
        anc in n.parents && return true
        union!(gm, n.parents)
    end

    false
end

isancestor(n::ExNode, exits::Vector{ExNode}, g::ExGraph) = isancestor(n, exits, g, ExNode[])


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
    no = NSMap()
    for (k,v) in fg.exto
      no[ nmap[k] ] = v
    end
    fg.exto = no

    no = NSMap()
    for (k,v) in fg.seto
      no[ nmap[k] ] = v
    end
    fg.seto = no
  end

  # copy node mapping and translate inner nodes to newly created ones
  g2.exto = NSMap(g.exto.kv)
  g2.seto = NSMap(g.seto.kv)
  for (k,v) in g.exti
    g2.exti[ nmap[k] ] = v
  end
  for (k,v) in g.seti
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
  if hasnode(g.exti, nr)
    g.exti[nk] = g.exti[nr]
  end

  # test if nr is associated to a setting node
  # if true, we create an NIn on nk, and associate var to it
  if hasnode(g.seti, nr)
    nn = addnode!(g, NIn(g.seti[nr], [nk]))
    nn.val = "fuse #1"
    g.seti[nn] = g.seti[nr]  # nn replaces nr as set_inode

    if hasnode(g.seto, nr)   # change onodes too (if we are in a subgraph)
      g.seto[nn] = g.seto[nr]  # nn replaces nr as set_onode
    end  
  end

  # test if nr is in the precedence of some node and nk is a parent of the same node
  ps = filter(x -> (nr in x.precedence) && (nk in x.parents), g.nodes)
  if length(ps) > 0
    nn = addnode!(g, NIn(nothing, [nk]))
    nn.val = "fuse #2"

    for n in g.nodes
      n.parents    = map(x -> x==nr ? nn : x, n.parents)
      n.precedence = map(x -> x==nr ? nn : x, n.precedence)
    end

  end   

  # replace references to nr by nk in parents of other nodes
  for n in filter(n -> n != nr && n != nk, g.nodes)
    if isa(n, NFor)
      g2 = n.main[2]

      # this should not happen...
      @assert !hasnode(g2.seto, nr) "[fusenodes (for)] attempt to fuse set_onode $nr"

      if hasnode(g2.exto, nr)
        if hasnode(g2.exto, nk)  # both nr and nk are used by the for loop
          symr = g2.exto[nr]
          symk = g2.exto[nk]
          fusenodes(g2, getnode(g2.exti, symk), getnode(g2.exti, symr))
        end

        g2.exto[nk] = g2.exto[nr]  # nk replaces nr in g2.exto
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
    # removed NIn nodes should be removed from For loops too
    if isa(n, NIn) && !(n in ns2) && isa(n.parents[1], NFor)
      fg = n.parents[1].main[2]
      sym = fg.seto[n]
      delete!(fg.seto, n)
      #= ni = fg.seti.vk[sym]
      delete!(fg.seti, ni) =#
    end

    n in ns2 || continue

    if isa(n, NFor)
      g2 = n.main[2]

      # list of g2 nodes whose outer node is in ns2
      exitnodes2 = ExNode[]
      for (k, sym) in g2.seto
        k in ns2 || continue
        push!(exitnodes2, getnode(g2.seti, sym))
      end
      # don't forget reentrant variables
      for (n2, sym) in g2.seti
        (n2 in exitnodes2)               && continue
        hassym(g2.exti, sym)             || continue
        on = getnode(g2.exti, sym)
        isancestor(on, exitnodes2, g2)   || continue
        push!(exitnodes2, n2)
      end
      exitnodes2 = unique(exitnodes2)
      #=println(exitnodes2)
      println("before\n", g2)
      println("after\n", g2)=#
      prune!(g2, exitnodes2)

      # update parents
      n.parents = [n.parents[1], intersect(n.parents, collect(keys(g2.exto)) ) ]
    end

    ns2 = union(ns2, n.parents)
  end

  # remove unused external inodes in map and corresponding onodes (if they exist)
  for (k,v) in g.exti
    k in ns2 && continue
    delete!(g.exti, k)
    hassym(g.exto, v) && delete!(g.exto, getnode(g.exto, v))
  end
  # reduce set inodes to what was specified in initial exitnodes parameter
  for (k,v) in g.seti
    k in exitnodes && continue
    delete!(g.seti, k)
    hassym(g.seto, v) && delete!(g.seto, getnode(g.seto, v))
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
        println("[calc!] can't evaluate $thing in \n $g \n with") ; display(params)
        rethrow(e)
      end
      return ret
    end
  end

  function evaluate(n::Union(NCall, NComp))
    local ret
    try
        ret = emod.eval( Expr(:call, n.main, Any[ x.val for x in n.parents]...) )
    catch
      error("[calc!] can't evaluate $(n.main) \n $g \n $params")
    end
    return ret
  end 

  function evaluate(n::NExt)
    hasnode(g.exti, n) || return myeval(n.main)

    sym = g.exti[n]  # should be equal to n.main but just to be sure.. 
    hassym(g.exto, sym) || return myeval(n.main)
    return getnode(g.exto, sym).val  # return node val in parent graph
  end

  evaluate(n::NConst) = n.main
  # evaluate(n::NRef)   = myeval( Expr(:ref, n.parents[1].val, 
  #                                    map(a->myeval(a), n.main)... ) )
  evaluate(n::NRef)   = myeval( Expr(:ref , Any[ x.val for x in n.parents]...))
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
    # is0 = next(iter, start(iter))[2]        # first value of index
    is0 = first(iter)                       # first value of index
    params2 = merge(params, [ is => is0 ])  # set loop index to first value
    # println("params2 : $(params2)")
    calc!(g2, params=params2)
    
    valdict = Dict()
    for (k, sym) in g2.seto
      valdict[k] = getnode(g2.seti, sym).val
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
  exitnode = getnode(ig.seti, nothing) # result of added subgraph

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
  exitnode
end
