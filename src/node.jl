#########################################################################
#
#   ExNode type definition and related functions
#
#########################################################################
  
#####  ExNode type  ######

type ExNode{T}
  main                    # main payload
  parents::Vector{Any}    # parent nodes
  precedence::Vector{Any} # nodes that should be evaluated before (but are not parents)
  val                     # value
  alloc::Bool             # Allocation ? Forbids fusions

  ExNode()                               = new(nothing,      {},   {}, NaN, false)
  ExNode(main)                           = new(   main,      {},   {}, NaN, false)
  ExNode(main,parents)                   = new(   main, parents,   {}, NaN, false)
  ExNode(main,parents, prec, val, alloc) = new(   main, parents, prec, val, alloc)
end

copy{T}(x::ExNode{T}) = ExNode{T}(copy(x.main), 
                                  copy(x.parents), 
                                  copy(x.precedence), 
                                  copy(x.val), 
                                  x.alloc)

copy(x::ExNode{:for}) = ExNode{:for}({x.main[1], copy(x.main[2])},    # make a copy of subgraph
                              copy(x.parents), 
                              copy(x.precedence), 
                              copy(x.val), 
                              x.alloc)

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


subtype{T}(n::ExNode{T}) = T

function show(io::IO, res::ExNode)
  pl = join( map(x->isa(x,NFor) ? "subgraph" : repr(x.main), res.parents) , " / ")
  print(io, "[$(subtype(res))] $(repr(res.main)) ($(repr(res.val)))")
  length(pl) > 0 && print(io, ", from = $pl")
end

