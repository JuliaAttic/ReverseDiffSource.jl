################################################################################
#
#   Graph differentiation
#
################################################################################

function gdiff!(g::Graph, lexit::Loc, input::Loc)
    # lexit, input = g.block.symbols[EXIT_SYM], g.locs[3]
    pos  = findlast(o -> lexit in o.desc, g.block.ops)
    dmap = Dict{Loc,Loc}() # Loc to dloc map

    # create start Loc for Derivation
    sn = CLoc(1.0)
    push!(g.locs, sn)
    dmap[lexit] = sn

    dops = _diff(g.block.ops, pos, dmap, g)
    append!(g.block.ops, dops)

    fl = CLoc(tuple) ; push!(g.locs, fl)
    dl = RLoc((lexit.val, dmap[input].val)) ; push!(g.locs, dl)
    push!(g.block.ops, FOp(fl, [lexit, dmap[input]], [dl;]))
    g.block.symbols[EXIT_SYM] = dl

    # for (k,v) in dmap
    #     ki = findfirst(k .== g.locs)
    #     vi = findfirst(v .== g.locs)
    #     println("$ki => $vi")
    # end

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

        if !iscompiled(rul)
          fn = "$fun(" * join(map(typeof, vargs), ",") * ")"
          println("compiling '$fn' at pos $ord")
        end

        result = appendsnippet!(rul, dops, dlargs, g)

        if repl  # snippet replaces, is not added
          ### what if not present yet ?
          dmap[larg] = result
        elseif haskey(dmap, larg) # deriv loc existing ?
          ns = Snippet(:(a+b), [:a, :b])
          res2 = appendsnippet!(ns, dops, Loc[dmap[larg], result], g)
          dmap[larg] = res2
        else
          dmap[larg] = result
        end
      end

    elseif isa(op, AbstractBlock)
      println("enter")
      push!(dops, blockdiff(op, dmap, g))
      println("exit")
    end
    println(" nops $(length(dops)) locs $(length(g.locs))")
  end

  dops
end
