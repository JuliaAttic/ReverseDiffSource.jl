################################################################################
#
#   Graph differentiation
#
################################################################################

function gdiff!(g::Graph, lexit::Loc, input::Loc)
    # lexit, input = g.block.symbols[EXIT_SYM], g.locs[3]
    pos  = findlast(o -> lexit in o.desc, g.block.ops)
    dmap = Dict{Loc,Loc}() # Loc to dloc map

    if lexit.typ <: Real  # scalar exit value
      # create start Loc for Derivation
      sn = CLoc(1.0)
      push!(g.locs, sn)
      dmap[lexit] = sn

      dops = _diff(g.block.ops, pos, dmap, g)
      append!(g.block.ops, dops)

      ns = Snippet(:((a,b)), [:a, :b])
      result = appendsnippet!(ns, g.block.ops, Loc[lexit, dmap[input]], g)
      g.block.symbols[EXIT_SYM] = result

    elseif lexit.typ <: AbstractArray && eltype(lexit.typ) <: Real

ex = quote
  B * B'
end
g = tograph(ex)
lexit = g.block.symbols[EXIT_SYM]
input = g.block.symbols[:B]

pos  = findlast(o -> lexit in o.desc, g.block.ops)
dmap = Dict{Loc,Loc}() # Loc to dloc map


sn   = Snippet(:(Array(Float64, length(I), length(O))), [:I, :O] )
acc  = appendsnippet!(sn, g.block.ops, Loc[input, lexit], g)

sn   = Snippet(:(zeros(exit)), [:exit;] )
acc2 = appendsnippet!(sn, g.block.ops, Loc[lexit;], g)

sn   = Snippet(:(1), Loc[] )
lidx = appendsnippet!(sn, g.block.ops, Loc[], g)

sn   = Snippet(:(fill!(dexit, 0.);dexit[idx] = 1.0;dexit), [:dexit, :idx] )
stn  = appendsnippet!(sn, g.block.ops, Loc[acc2, lidx], g)

n = length(g.block.ops)
fops = splice!(g.block.ops, n-1:n)

stn == acc2
dmap[lexit] = acc2
dops = _diff(g.block.ops, pos, dmap, g)
append!(fops, dops)

sn   = Snippet(:(acc[:,idx] = result), [:acc, :idx, :result] )
acc3 = appendsnippet!(sn, g.block.ops, Loc[acc, lidx, dmap[input]], g)

n = length(g.block.ops)
push!(fops, pop!(g.block.ops))

sn  = Snippet(:(1:length(exit)), [:exit;] )
lrg = appendsnippet!(sn, g.block.ops, Loc[lexit;], g)

fbl = ForBlock(fops, Op[], g.block.symbols, Loc[lidx, lrg], Loc[])
fbl.asc, fbl.desc = summarize(fbl)

push!(g.block.ops, fbl)

sn  = Snippet(:((a,b)), [:a, :b])
res = appendsnippet!(sn, g.block.ops, Loc[lexit, acc3], g)
g.block.symbols[EXIT_SYM] = res

show(g)
dex = tocode(g)

simplify!(g)

@eval let B = ones(4)*2. ; $dex ; end


    else
      error("[gdiff] result is neither a Real nor an Array{Real} : $(lexit.typ)")

    end
    g
end


function _diff(ops, pos, dmap, g) # ops = g.block.ops

  dops = Op[]
  for i in pos:-1:1  # i = 2
    op = ops[i]
    if isa(op, FOp)
      haskey(dmap, op.desc[1]) || continue # unused, skip
      fun  = op.f.val
      largs = collect(enumerate(op.asc))

      if ismutating(fun) # mutated arg should be processed last
        ord = mutdict[fun]
        tmp = splice!(largs, ord)
        push!(largs, tmp)
      end

      vargs  = tuple([l.val for l in op.asc]...)
      dlargs = vcat(op.asc, dmap[op.desc[1]])
      for (ord, larg) in largs # ord, larg = 1, op.asc[1]
        isa(larg, CLoc) && continue  # if constant, skip

        rul, repl = DerivRules.getrule(fun, ord, vargs)

        result = appendsnippet!(rul, dops, dlargs, g)

        if repl  # snippet replaces, is not added
          # FIXME : what if not present yet ?
          dmap[larg] = result
        elseif haskey(dmap, larg) # deriv loc existing ? => add to it
          ns = Snippet(:(a+b), [:a, :b])
          res2 = appendsnippet!(ns, dops, Loc[dmap[larg], result], g)
          dmap[larg] = res2
        else # else create new starting point
          ns = Snippet(:(copy(a)), [:a;])
          res2 = appendsnippet!(ns, dops, Loc[result;], g)
          dmap[larg] = res2
        end
      end

    elseif isa(op, AbstractBlock)
      push!(dops, blockdiff(op, dmap, g))
    end
  end

  dops
end
