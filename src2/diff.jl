################################################################################
#
#   Graph differentiation
#
################################################################################

ex = :(2*a*a)
g = tograph(ex)
simplify!(g)
gdiff!(g, g.block.symbols[EXIT_SYM], g.block.symbols[:a])
simplify!(g)
tocode(g)

ex = :(a+a)
g = tograph(ex)
simplify!(g)
gdiff!(g, g.block.symbols[EXIT_SYM], g.block.symbols[:a])
simplify!(g)
tocode(g)

ex = :(sin(a))
g = tograph(ex)
simplify!(g)
gdiff!(g, g.block.symbols[EXIT_SYM], g.block.symbols[:a])
simplify!(g)
tocode(g)
show(g)





ex = quote
    X = ones(3,3) .* a
    3 ^ X[2,2]
end

g = tograph(ex)
simplify!(g)
gdiff!(g, g.block.symbols[EXIT_SYM], g.block.symbols[:a])
simplify!(g)
dex = tocode(g)

@eval let a = 1.0; $dex ; end
@eval let a = 1.00001; $dex ; end

ex = quote
    X = ones(3,3)
    X[2,2] = a
    sum(X)
end

g = tograph(ex)
simplify!(g)
gdiff!(g, g.block.symbols[EXIT_SYM], g.block.symbols[:a])
simplify!(g)
dex = tocode(g)
show(g)
g.block.ops[8]
l0 = g.block.ops[8].asc[2]
l0 in g.locs

keep=[EXIT_SYM;]) # g = A.g ; keep = [A.EXIT_SYM;]

prune!(g, [EXIT_SYM;])
show(g)

	splitnary!(g)

	fusecopies!(g)
	removerightneutral!(g)
	removeleftneutral!(g)
	prune!(g, keep)
	g




module Sandbox
    type Abcd
        a::Float64
        b::Vector{Float64}
    end
    foo(t::Abcd) = t.a + t.b[2]
end

@deriv_rule Sandbox.Abcd(a,b) a ds[1]
@deriv_rule Sandbox.Abcd(a,b) b ds[2]
@deriv_rule Sandbox.foo(t)    t Any[ ds, (a=zeros(length(t.b)) ; a[2]=ds ; a) ]

@deriv_rule getfield(A::Sandbox.Abcd, fn) A fn==:a ? Any[ds, 0.] : Any[0., ds]



foo(t)    t Any[ ds, (a=zeros(length(t.b)) ; a[2]=ds ; a) ]

E = Sandbox.Abcd(1., [2., 3.])

@compare t.a  * x        v0ref
@compare sum(t.b .* x)   v0ref
@compare sum(t.b .+ [x,x])   v0ref

ex = :(E.a  * a)
g = tograph(ex)
simplify!(g)
gdiff!(g, g.block.symbols[EXIT_SYM], g.block.symbols[:a])
simplify!(g)
tocode(g)
show(g)





@eval let a = 1.0; $dex ; end
@eval let a = 1.00001; $dex ; end


DerivRules.getrule(setindex!, 2, (zeros(3), 1., 1))
DerivRules.getrule(setindex!, 1, (zeros(3), 1., 1))
DerivRules.getrule(setindex!, 1, (zeros(3), 1., 1,1))
DerivRules.getrule(setindex!, 2, (zeros(3), 1., 1,1))
ismutating(setindex!)
ismutating(getindex)




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

    dpos = Op[]
    for i in pos:-1:1  # i = 2
        op = ops[i]
        if isa(op, FOp)
            fun  = op.f.val
            largs = collect(enumerate(op.asc))

            if ismutating(fun)
                # need to process deriv rule of mutated variable last
                ord = mutdict[fun]
                tmp = splice!(largs, ord)
                push!(largs, tmp)
            end

            args = tuple([l.val for l in op.asc]...)
            for (ord, larg) in largs # ord, larg = 1, op.asc[1]
                isa(larg, CLoc) && continue  # if constant, pass

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
                    push!(rul.alocs, addlocsym(dmap[op.desc[1]].val, :ds, rul.g))

                    result = addtoops!(rul.ex, rul.g.block.ops, rul.g.block.symbols, rul.g)
                    if result==nothing
                        fn = "$fun(" * join(map(typeof, args), ",") * ")"
                        error("deriv rule for '$fn' at pos $ord did not yield a value")
                    end
                    rul.eloc = result
                end

                ## insert rule in graph / block
                inmap = Dict{Loc,Loc}()
                for (rl, dl) in zip(rul.alocs, vcat(op.asc, dmap[op.desc[1]]))
                    rl in rul.g.locs || continue # pass if variable is unused
                    inmap[rl] = dl
                end

                result = insertgraph!(g, dpos, rul.g, inmap, rul.eloc)
                if rul.repl  # snippet replaces, is not added
                    ### what if not present yet ?
                    println("arg  !")
                    dmap[larg] = result
                elseif haskey(dmap, larg) # deriv loc existing ?
                    fl = CLoc(+) ; push!(g.locs, fl)
                    dl = copy(result) ; push!(g.locs, dl)
                    push!(dpos, FOp(fl, [dmap[larg], result], [dl]))
                    dmap[larg] = dl
                else
                    dmap[larg] = result
                end
            end

        elseif isa(op, AbstractBlock)
            blockdiff(op, ops, pos, dmap, g)
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
