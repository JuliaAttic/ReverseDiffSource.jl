
# name for exit variable
const EXIT_SYM = :_result

type Loc{T}  # regular, constant, external
    typ::DataType
    val::Any

    Loc(x) = new(typeof(x), x)
end

# Loc{T}(x) = Loc{T}(typeof(x), x)

typealias CLoc Loc{:constant}  # constants
typealias ELoc Loc{:external}  # external
typealias RLoc Loc{:regular}   # regular

loctype{T}(l::Loc{T}) = T

abstract Block   # abstract type for 'ifblock', 'forblock', etc..

type Op
    f::Loc
    asc::Vector{Loc}  # parent Loc (function arguments)
    desc::Vector{Loc} # descendant Loc (Loc modified/created by function)
end

type Graph
    locs::Vector{Loc}
    ops::Vector{Op}
    symbols::Dict{Any, Loc}
end

Graph() = Graph(Vector{Loc}(), Vector{Op}(), Dict{Any, Loc}())

import Base.show
function show(io::IO, g::Graph)
  function printtable(t::Array{UTF8String,2})
    sz = maximum(map(length, t),1)
    for i in 1:size(t,1)
      for j in 1:size(t,2)
        l = length(t[i,j])
        print(io, " " ^ (sz[j]-l), t[i,j], " ")
      end
      println()
    end
  end

  slocs = Array(UTF8String, length(g.locs)+1, 5)
  slocs[1,:] = ["#", "type", "symbol(s)", "cat", "val" ]
  for (i,l) in enumerate(g.locs) # i,l = 1, g.locs[1]
    vs = keys(filter((k,v) -> v===l, g.symbols))
    slocs[i+1,:] = map(string, Any[i, l.typ, join(vs, ","), loctype(l), l.val])
  end
  printtable(slocs)
  println()

  sops = Array(UTF8String, length(g.ops)+1, 3)
  sops[1,:] = ["f" "parents" "children"]
  for (i,o) in enumerate(g.ops) # i,l = 1, g.ops[1]
    ps = indexin(o.asc, g.locs)
    cs = indexin(o.desc, g.locs)
    sops[i+1,:] = map(string, Any[o.f, join(ps, ","), join(cs, ",")])
  end
  printtable(sops)
end
