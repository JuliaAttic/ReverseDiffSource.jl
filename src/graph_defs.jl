#########################################################################
#
#   Expression Graph definitions
#
#########################################################################
  
#####  ExNode type  ######

type ExNode{T}
  main
  parents::Vector
  val

  ExNode()             = new(nothing,      {}, NaN)
  ExNode(main)         = new(   main,      {}, NaN)
  ExNode(main,parents) = new(   main, parents, NaN)
end

isequal{T}(x::ExNode{T}, y::ExNode{T}) = 
  isequal(x.main, y.main) && isequal(x.parents,y.parents)

isequal(x::ExNode, y::ExNode) = false # not equal if param type isn't  

copy{T}(x::ExNode{T}) = ExNode{T}(copy(x.main), copy(x.parents))

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
  print(io, "[$(typeof(res))] $(repr(res.main)) ($(res.val))")
  length(pl) > 0 && print(io, ", from = $pl")
end


#####  ExGraph type  ######

type ExGraph
  nodes::Vector{ExNode}  # nodes in this graph
  inmap::Dict            # map of this graph external nodes to parent graph nodes
  outmap::Dict           # map of this graph calc nodes to dependant parent graph nodes
  setmap::Dict           # map of symbol to calc nodes in this graph
end

ExGraph()                   = ExGraph( ExNode[] )
ExGraph(vn::Vector{ExNode}) = ExGraph( vn, Dict(), Dict(), Dict() )

######  Graph functions  ######
add_node(g::ExGraph, nn::ExNode) = (push!(g.nodes, nn) ; nn)

# ancestors(n::ExNode) = union( Set([n]), ancestors(n.parents) )
ancestors(n::ExNode) = union( Set(n), ancestors(n.parents) )
ancestors(n::Vector) = union( map(ancestors, n)... )

