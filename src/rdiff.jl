#########################################################################
#
#   rdiff differentiation function
#
#########################################################################

######### expression version   ################
# TODO : break this huge function in smaller blocks

"""
Generates an expression calculating the derivative(s) for a given expression

    rdiff( ex::Expr; kwargs... )

Arguments:

- ex: is a Julia Expression containing the code to derive
- outsym: (default = nothing) is the symbol of the variable within ``ex`` containing the expression output (the result whose derivatives are needed). This variable must evaluate to a ``Real``. If not specified, ``outsym`` defaults to ``nothing`` which signals to ``rdiff`` that the last statement is the result of interest for derivation.
- order: (default = 1) is an integer indicating the derivation order (1 for 1st order, etc.). Order 0 is allowed and will produce an expression that is a processed version of ``ex`` with some variables names rewritten and possibly some optimizations.
- init: (multiple keyword arguments) is one or several symbol / DataType pairs used to indicate for which variable a derivative is needed and how they should be interpreted. By default the generated expression will yield the derivative for each variable given unless the variable is listed in the ``ignore`` argument.
- evalmod: (default=Main) module where the expression is meant to be evaluated. External variables and functions should be evaluable in this module.
- debug: (default=false) indicates if ``rdiff`` should dump the graph of the generating expression, instead of returning the expression itself.
- allorders: (default=true) indicates whether to generate the code for all orders up to ``order`` (true) or only the last order.
- ignore: (default=[]) do not differentiate against the listed variables, useful if you are not interested in having the derivative of one of several variables in ``init``.

Usage:
```julia
julia> rdiff( :(x^3) , x=Float64)  # first order
:(begin
    (x^3,3 * x^2.0)
    end)

julia> rdiff( :(x^3) , order = 3, x=Float64)  # orders up to 3
:(begin
        (x^3,3 * x^2.0,2.0 * (x * 3),6.0)
    end)

julia> rdiff( :(sin(x)) , order=10, x=Float64)  # derivatives up to order 10
:(begin
        _tmp1 = sin(x)
        _tmp2 = cos(x)
        _tmp3 = -_tmp1
        _tmp4 = -_tmp2
        _tmp5 = -_tmp3
        (_tmp1,_tmp2,_tmp3,_tmp4,_tmp5,_tmp2,_tmp3,_tmp4,_tmp5,_tmp2,_tmp3)
    end)

julia> ex = :(p^3+y)
julia> rdiff( ex , p=Float64, y=Float64, ignore=:y) # derive for p, not for y
```
"""
function rdiff(ex;
               outsym    = nothing,
               order::Int= 1,
               evalmod   = Main,
               debug     = false,
               allorders = true,
               ignore    = Symbol[],
               params...)

    # format parameters in `params`
    paramsym  = Symbol[ e[1] for e in params]
    paramval  = [ e[2] for e in params]
    pardict   = Dict(zip(paramsym, paramval))

    # list of variables to differentiate against
    paramdiff = if isa(ignore, Symbol)
                    setdiff(paramsym, [ignore;])
                elseif isa(ignore, Union{Vector,Tuple}) && eltype(ignore)==Symbol
                    setdiff(paramsym, ignore)
                else
                    error("'ignore' arg should be a Symbol or Vector/Tuple of Symbol")
                end

    # controls
    length(paramsym) >= 1 ||
        error("There should be at least one variable specified, none found")

    length(paramdiff) >= 1 ||
        error("There should be at least one variable for differentiation, none found")

    order <= 1 ||
        length(paramdiff) == 1 ||
        error("Only one differentiation variable allowed for order >= 2")

    order <= 1 ||
        (paramval[1] <: Vector && (eltype(paramval[1]) <: Real)) ||
        (paramval[1] <: Real) ||
        error("Param should be a real or vector for order >= 2")


    g = tograph(ex, evalmod)

    hassym(g.seti, outsym) ||
        error("can't find output var $( outsym==nothing ? "" : outsym)")

    # reduce to variable of interest
    g.seti = NSMap([getnode(g.seti, outsym)], [ outsym ])

    g |> splitnary! |> prune! |> simplify!
    calc!(g, params=pardict, emod=evalmod)

    ov = getnode(g.seti, outsym).val
    ov <: Real || error("output var should be a Real, $ov found")

    voi = Any[ outsym ]

    if order == 1
        dg = reversegraph(g, getnode(g.seti, outsym), paramdiff)
        append!(g.nodes, dg.nodes)

        for p in paramdiff
            nn = getnode(dg.seti, dprefix(p))  # find the exit node of deriv of p
            ns = newvar("_dv")
            g.seti[nn] = ns
            push!(voi, ns)
        end

        g |> splitnary! |> prune! |> simplify!

    elseif order > 1 && pardict[paramdiff[1]] <: Real
        for i in 1:order
            dg = reversegraph(g, getnode(g.seti, voi[i]), paramdiff)
            append!(g.nodes, dg.nodes)
            nn = collect(nodes(dg.seti))[1]  # only a single node produced
            ns = newvar("_dv")
            g.seti[nn] = ns
            push!(voi, ns)

            g |> splitnary! |> prune! |> simplify!

            calc!(g, params=pardict, emod=evalmod)
        end

    elseif order > 1 && pardict[paramdiff[1]] <: Vector &&
                        eltype(pardict[paramdiff[1]]) <: Real
        # do first order as usual
        dg = reversegraph(g, getnode(g.seti, outsym), paramdiff)
        append!(g.nodes, dg.nodes)
        ns = newvar(:_dv)
        g.seti[ collect(nodes(dg.seti))[1] ] = ns
        push!(voi, ns)

        g |> splitnary! |> prune! |> simplify!

        # now order 2 to n
        for i in 2:order
            # launch derivation on a single value of the preceding
            #   derivation vector
            no = getnode(g.seti, voi[i])
            si = newvar(:_idx)
            ni = addnode!(g, NExt(si))
            ns = addnode!(g, NRef(:getidx, [ no, ni ]))

            calc!(g, params=Dict(zip([paramsym; si], [paramval; Int64])), emod=evalmod)
            dg = reversegraph(g, ns, paramdiff)

            #### We will now wrap dg in a loop scanning all the elements of 'no'
            # first create ext nodes to make dg a complete subgraph
            dg2 = ExNode[]
            nmap = Dict()
            for n in dg.nodes  # n = dg.nodes[2]
                for (j, np) in enumerate(n.parents)  # j,np = 1, n.parents[1]
                    if haskey(nmap, np) # already remapped
                        n.parents[j] = nmap[np]

                    elseif np == ni # it's the loop index
                        nn = NExt(si)
                        push!(dg2, nn)
                        dg.exti[nn] = si
                        n.parents[j] = nn
                        nmap[np] = nn

                    elseif np == ns # it's the selected element of the deriv vector
                        # create 'no' ref if needed
                        if !haskey(nmap, no)
                            sn = newvar()
                            nn = NExt(sn)
                            push!(dg2, nn)
                            dg.exti[nn] = sn
                            dg.exto[no] = sn
                            nmap[no] = nn
                        end

                        nn = NRef(:getidx, [ nmap[no], nmap[ni] ])
                        push!(dg2, nn)
                        nmap[ns] = nn

                    elseif !(np in dg.nodes) # it's not in dg (but in g)
                        sn = newvar()
                        nn = NExt(sn)
                        push!(dg2, nn)
                        dg.exti[nn] = sn
                        dg.exto[np] = sn
                        n.parents[j] = nn
                        nmap[np] = nn

                    end
                end

                # update onodes in for loops
                if isa(n, NFor)
                    g2 = n.main[2]
                    for (o,s) in g2.exto
                        if haskey(nmap, o)
                            g2.exto[ nmap[o] ] = s  # replace
                        end
                    end
                end
            end
            append!(dg.nodes, dg2)
            # dg |> prune! |> simplify!

            # create for loop node
            nf = addnode!(g, NFor(Any[ si, dg ] ) )

            # create param size node
            nsz = addgraph!( :( length( x ) ), g, Dict( :x => getnode(g.exti, paramdiff[1]) ) )

            # create (n-1)th derivative size node
            ndsz = addgraph!( :( sz ^ $(i-1) ), g, Dict( :sz => nsz ) )

            # create index range node
            nid = addgraph!( :( 1:dsz ),  g, Dict( :dsz => ndsz ) )
            push!(nf.parents, nid)

            # pass size node inside subgraph
            sst = newvar()
            inst = addnode!(dg, NExt(sst))
            dg.exti[inst] = sst
            dg.exto[nsz]  = sst
            push!(nf.parents, nsz)

            # create result node (alloc in parent graph)
            nsa = addgraph!( :( zeros( $( Expr(:tuple, [:sz for j in 1:i]...) ) ) ),
                            g, Dict( :sz => nsz ) )
            ssa = newvar()
            insa = addnode!(dg, NExt(ssa))
            dg.exti[insa] = ssa
            dg.exto[nsa]  = ssa
            push!(nf.parents, nsa)

            # create result node update (in subgraph)
            nres = addgraph!( :( res[ ((sidx-1)*st+1):(sidx*st) ] = dx ; res ), dg,
                              Dict(:res  => insa,
                                   :sidx => nmap[ni],
                                   :st   => inst,
                                   :dx   => collect(dg.seti)[1][1] ) )
            dg.seti = NSMap([nres], [ssa])

            # create exit node for result
            nex = addnode!(g, NIn(ssa, [nf]))
            dg.seto = NSMap([nex], [ssa])

            # update parents of for loop
            append!( nf.parents, setdiff(collect( nodes(dg.exto)), nf.parents[2:end]) )

            ns = newvar(:_dv)
            g.seti[nex] = ns
            push!(voi, ns)

            g |> splitnary! |> prune! |> simplify!
        end

    else
        error("[rdiff] inconsistent parameters")
    end

    if !allorders  # only keep the last derivative
        if order == 1 # potentially multiple diffs, issue #32
            voi = voi[2:end]
        else
            voi = [voi[end]]
        end
    end

    if length(voi) > 1  # create tuple if multiple variables
        voin = map( s -> getnode(g.seti, s), voi )
        nf = addnode!(g, NConst(tuple))
        exitnode = addnode!(g, NCall(:call, [nf, voin...]))
    else
        exitnode = getnode(g.seti, voi[1])
    end
    g.seti = NSMap( [exitnode], [nothing])  # make this the only exitnode of interest

    g |> splitnary! |> prune! |> simplify!

    resetvar()
    debug ? g : tocode(g, evalmod)
end
