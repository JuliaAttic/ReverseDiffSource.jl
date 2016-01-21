################################################################################
#
#   Graph differentiation
#
################################################################################


ex = :(2*a*a)
g = tograph(ex)
splitnary!(g)
show(g)
lexit = g.block.symbols[EXIT_SYM]

pos  = findlast(o -> lexit in o.desc, g.block.ops)
dmap = Dict{Loc,Loc}() # Loc to dloc map
dops = _diff(g.block.ops, pos, dmap, g)

ex = :(2*a*a)
g = tograph(ex)
splitnary!(g)
gdiff(g, g.block.symbols[EXIT_SYM], g.block.symbols[:a])

ex = :(a+a)
g = tograph(ex)
splitnary!(g)
gdiff(g, g.block.symbols[EXIT_SYM], g.block.symbols[:a])

ex = :(sin(a))
g = tograph(ex)
splitnary!(g)
gdiff(g, g.block.symbols[EXIT_SYM], g.block.symbols[:a])

tocode(g)
show(g)

function gdiff(g::Graph, lexit::Loc, input::Loc)
  # lexit, input = g.block.symbols[EXIT_SYM], g.locs[3]
    pos  = findlast(o -> lexit in o.desc, g.block.ops)
    dmap = Dict{Loc,Loc}() # Loc to dloc map
    dops = _diff(g.block.ops, pos, dmap, g)

    append!(g.block.ops, dops)

    ds = newvar()
    g.block.symbols[ds] = dmap[input]
    keepsym = [EXIT_SYM, ds]
    simplify!(g, keepsym)

    fl = CLoc(tuple) ; push!(g.locs, fl)
    dl = RLoc((lexit.val, dmap[input].val)) ; push!(g.locs, dl)
    push!(g.block.ops, FOp(fl, [lexit, dmap[input]], [dl;]))
    g.block.symbols[EXIT_SYM] = dl

    _tocode(g.block.ops, [dl;], g.block.symbols, g)
end


function _diff(ops, pos, dmap, g) # ops = g.block.ops

    dpos = Op[]
    for i in pos:-1:1  # i = 2
        println(i)
        op = ops[i]
        if isa(op, FOp)
            fun  = op.f.val
            args = tuple([l.val for l in op.asc]...)
            for (ord, larg) in enumerate(op.asc) # ord, larg = 1, op.asc[1]
                println("ord $ord")
                isa(larg, CLoc) && continue  # if constant, pass
                println("ok ")

                rul = DerivRules.getrule(fun, ord, args)

                # if rule has not yet been compiled to a graph, then this
                # is the moment to do it with the args provided
                if !isdefined(rul, :g)
                    fn = "$fun(" * join(map(typeof, args), ",") * ")"
                    println("compile for '$fn' at pos $ord")

                    rul.g = Graph()

                    function addlocsym(val, sym, g)
                        l = RLoc(val)
                        push!(g.locs, l)
                        g.block.symbols[sym] = l
                        l
                    end

                    for (i,arg) in enumerate(args)
                        push!(rul.alocs, addlocsym(arg, rul.syms[i], rul.g))
                    end
                    push!(rul.alocs, addlocsym(op.desc[1].val, :ds, rul.g))

                    result = addtoops!(rul.ex, rul.g.block.ops, rul.g.block.symbols, rul.g)
                    if result==nothing
                        fn = "$fun(" * join(map(typeof, args), ",") * ")"
                        error("deriv rule for '$fn' at pos $ord did not yield a value")
                    end
                    rul.eloc = result
                end

                ## insert rule in graph / block
                inmap = Dict{Loc,Loc}()
                for (rl, dl) in zip(rul.alocs, vcat(op.asc, op.desc[1]))
                    rl in rul.g.locs || continue # pass if variable is unused
                    inmap[rl] = dl
                end

                print("$(length(dpos)) -> ")
                result = insertgraph!(g, dpos, rul.g, inmap, rul.eloc)
                println("$(length(dpos))")
                if haskey(dmap, larg) # deriv loc existing ?
                    fl = CLoc(+) ; push!(g.locs, fl)
                    dl = copy(result) ; push!(g.locs, dl)
                    push!(dpos, FOp(fl, [dmap[larg], result], [dl]))
                    dmap[larg] = dl
                else
                    dmap[larg] = result
                end
            end

        elseif isa(op, AbstractBlock)
            diffblock(op, ops, pos, dmap, g)
        end
    end

    dpos
end


function insertgraph!(destg::Graph, destops::Vector{Op},
                      src::Graph, inmap::Dict, srcexit::Loc)
    lmap = copy(inmap)
    for sl in src.locs
        haskey(lmap, sl) && continue
        nl = copy(sl)
        lmap[sl] = nl
        push!(destg.locs, nl)
    end

    append!(destops, remap(src.block.ops, lmap))
    lmap[srcexit]
end

function remap(ops::Vector{Op}, lmap)
    nops = Op[]
    for op in ops
        if isa(op, FOp)
            asc  = Loc[ lmap[l] for l in  op.asc  ]
            desc = Loc[ lmap[l] for l in  op.desc ]
            push!(nops, FOp(lmap[op.f], asc, desc))
        else
            push!(nops, remap(bl, lmap))
        end
    end
    nops
end

function remap(bl::Block, lmap)
    Block(remap(bl.ops, lmap),
          [ lmap[s] => l for (s,l) in bl.symbols ],
          Loc[ lmap[l] for l in  bl.asc  ],
          Loc[ lmap[l] for l in  bl.desc ] )
end

function remap(bl::ForBlock, lmap)
    ForBlock(remap(bl.ops, lmap),
             remap(bl.lops, lmap),
             [ lmap[s] => l for (s,l) in bl.symbols ],
             Loc[ lmap[l] for l in  bl.asc  ],
             Loc[ lmap[l] for l in  bl.desc ] )
end
