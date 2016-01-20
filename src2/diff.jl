################################################################################
#
#   Graph differentiation
#
################################################################################


ex = :(2*a*a)
g = tograph(ex)
splitnary!(g)
show(g)
lexit = g.locs[4]

function diff(g::Graph, lexit::Loc)

  pos = findlast(o -> lexit in o.desc, g.block.ops)
  dmap = Dict{Loc,Loc}() # Loc to dloc map
  dops = _diff(g.block.ops, pos)

end


function _diff(ops, pos) # ops = g.block.ops

  dpos = Op[]
  for i in pos:-1:1  # i = 2
    op = ops[i]
    if isa(op, FOp)
      for (index, arg) in enumerate(op.asc)
        isa(arg, CLoc) || continue  # if constant, pass

        if !isa(arg, Union{NConst, NComp})
          # haskey(drules, (op, index-1)) || error("no derivation rule for $(op) at arg #$(index-1)")
          # ddict = drules[(op, index-1)]
              ddict = getrule(op, index-1)

              targs = Tuple{ Type[ typeof(x.val) for x in n.parents[2:end]]... }

              sk = tmatch( targs, collect(keys(ddict)) )
              (sk == nothing) && error("no derivation rule for $(op) at arg #$(index-1) for signature $targs")

              # dg, dd = drules[(op, index-1)][sk]
              dg, dd = ddict[sk]
            smap = Dict( zip(dd, [n.parents[2:end]; dnodes[n]]) )

              exitnode = addgraph!(dg, g2, smap)

              vp = addnode!(g2, NConst(+))
              dnodes[arg] = addnode!(g2, NCall(:call, [vp, dnodes[arg], exitnode]) )
          end
      end

    else isa(op, AbstractBlock)
      diffblock(op)

  end

end


module DRules
function define(f::Function, sig) # f, sig = sin, Float64
  eval(Expr(:(=), Expr(:call, symbol("dr$(object_id(f))"), :(x::$sig) ),
            "$f($sig)"  ) )
  eval(Expr(:(=), Expr(:call, symbol("dr"), :(x::$sig) ), "$f($sig)"  ) )
  show(Expr(:(=), Expr(:call, symbol("dr$(object_id(f))"), :(x::$sig) ),
            "$f($sig)"  ))
end

function fetchdef(f::Function, val) # f, sig = sin, Float64
  eval(Expr(:call, symbol("dr$(object_id(f))"), val) )
end

object_id(sin)
object_id(cos)

end


DRules.fetchdef(sin, 12.)
DRules.define(cos, Float64)
DRules.fetchdef(sin, 12.)
DRules.fetchdef(cos, 12.)
DRules.define(cos, Int64)
DRules.fetchdef(cos, 12.)
DRules.fetchdef(cos, 12)

whos()
whos(DRules)

sin(3.)
sin(sin)

object_id(sin)
object_id(cos)

DRules.ff
DRules.ff == sin



sin(3)
sin(x) = 12.
isimmutable(4.)
isimmutable([0,0])
dump(:(  abcd(x::Float64) = "") )

DRules.@define(sin, 0., :abcd)
