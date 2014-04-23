#########################################################################
#
#   Expression Graph definitions
#
#########################################################################
  
#####  ExNode type  ######

type ExNode{T}
  main
  parents::Vector{Any}
  val

  ExNode()                  = new(nothing,      {}, NaN)
  ExNode(main)              = new(   main,      {}, NaN)
  ExNode(main,parents)      = new(   main, parents, NaN)
  ExNode(main,parents, val) = new(   main, parents, val)
end

copy{T}(x::ExNode{T}) = ExNode{T}(copy(x.main), copy(x.parents), copy(x.val))

typealias NConst     ExNode{:constant}  # for constant 
typealias NExt       ExNode{:external}  # external var
typealias NCall      ExNode{:call}      # function call
typealias NComp      ExNode{:comp}      # comparison operator
typealias NRef       ExNode{:ref}       # getindex
typealias NDot       ExNode{:dot}       # getfield
typealias NSRef      ExNode{:subref}    # setindex
typealias NSDot      ExNode{:subdot}    # setfield
typealias NAlloc     ExNode{:alloc}     # function call allocating memory
typealias NFor       ExNode{:for}       # for loop
typealias NIn        ExNode{:within}    # reference to var set in a loop


function show(io::IO, res::ExNode)
  pl = join( map(x->isa(x,NFor) ? "subgraph" : repr(x.main), res.parents) , " / ")
  print(io, "[$(typeof(res))] $(repr(res.main)) ($(repr(res.val)))")
  length(pl) > 0 && print(io, ", from = $pl")
end


#####  ExGraph type  ######

type ExGraph
  nodes::Vector{ExNode}  # nodes in this graph
  inmap::Dict            # map of this graph external nodes to parent graph nodes
  outmap::Dict           # map of this graph calc nodes to dependant parent graph nodes
  setmap::Dict           # map of symbol to calc nodes in this graph
  link::Dict             
end

ExGraph()                   = ExGraph( ExNode[] )
ExGraph(vn::Vector{ExNode}) = ExGraph( vn, Dict(), Dict(), Dict(), Dict() )


function copy(g::ExGraph)
  g2 = ExGraph()
  nmap = Dict()
  evalsort!(g)
  for n in g.nodes
    n2 = add_node(g2, copy(n))
    n2.parents = [ nmap[n] for n in n2.parents ]
    nmap[n] = n2
  end

  for (k,v) in g.inmap  ; g2.inmap[ nmap[k]]  = v       ; end
  for (k,v) in g.outmap ; g2.outmap[ nmap[k]] = v       ; end
  for (k,v) in g.setmap ; g2.setmap[k]        = nmap[v] ; end
  for (k,v) in g.link   ; g2.link[ nmap[k]]   = v       ; end

  g2
end

######  Graph functions  ######
add_node(g::ExGraph, nn::ExNode) = (push!(g.nodes, nn) ; nn)

if (VERSION.major, VERSION.minor) == (0,2)
  @eval ancestors(n::ExNode) = union( Set(n), ancestors(n.parents) ) # julia 0.2
else
  @eval ancestors(n::ExNode) = union( Set([n]), ancestors(n.parents) ) # julia 0.3+
end

ancestors(n::Vector) = union( map(ancestors, n)... )

