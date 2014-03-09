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
end

ExNode{T}(main)                    = ExNode{typ}(main, ExNode[], NaN)
ExNode{T}(main, parents)           = ExNode{typ}(main, parents, NaN)
ExNode(typ::Symbol, main)          = ExNode{typ}(main, ExNode[], NaN)
ExNode(typ::Symbol, main, parents) = ExNode{typ}(main, parents,  NaN)

function isequal{T}(x::ExNode{T}, y::ExNode{T})
  isequal(x.main, y.main) && isequal(x.parents,y.parents)
  # val should not matter
end

isequal(x::ExNode, y::ExNode) = false # not equal if param type isn't  

copy{T}(x::ExNode{T}) = ExNode{T}(copy(x.main), copy(x.parents))

typealias NConst     ExNode{:constant}
typealias NExt       ExNode{:external}
typealias NCall      ExNode{:call}
typealias NComp      ExNode{:comp}
typealias NRef       ExNode{:ref}
typealias NDot       ExNode{:dot}
typealias NSRef      ExNode{:subref}
typealias NSDot      ExNode{:subdot}
typealias NExt       ExNode{:external}
typealias NAlloc     ExNode{:alloc}
typealias NFor       ExNode{:for}
typealias NIn        ExNode{:within}


function show(io::IO, res::ExNode)
  pl = join( map(x->repr(x.main), res.parents) , " / ")
  # print(io, "[$(res.nodetype)] $(repr(res.name)) ($(res.value))")
  print(io, "[$(typeof(res))] $(repr(res.main)) ($(res.val))")
  length(pl) > 0 && print(io, ", from = $pl")
end


#####  ExGraph type  ######

type ExGraph
  nodes::Vector{ExNode}
  exitnodes::Dict
end

ExGraph() = ExGraph(ExNode[], Dict{Symbol, ExNode}())

######  Graph functions  ######
function add_node(g::ExGraph, nargs...)
  v = ExNode(nargs...)
  push!(g.nodes, v)
  v
end

ancestors(n::ExNode) = union( Set(n), ancestors(n.parents) )
ancestors(n::Vector) = union( map(ancestors, n)... )

