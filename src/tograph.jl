#########################################################################
#
#   Expression to graph conversion
#
#########################################################################

##########  Parameterized type to ease AST exploration  ############
  type ExH{H}
    head::Symbol
    args::Vector
    typ::Any
  end
  toExH(ex::Expr) = ExH{ex.head}(ex.head, ex.args, ex.typ)
  toExpr(ex::ExH) = Expr(ex.head, ex.args...)

  typealias ExEqual    ExH{:(=)}
  typealias ExDColon   ExH{:(::)}
  typealias ExColon    ExH{:(:)}
  typealias ExPEqual   ExH{:(+=)}
  typealias ExMEqual   ExH{:(-=)}
  typealias ExTEqual   ExH{:(*=)}
  typealias ExTrans    ExH{symbol("'")} 
  typealias ExCall     ExH{:call}
  typealias ExBlock    ExH{:block}
  typealias ExLine     ExH{:line}
  typealias ExVcat     ExH{:vcat}
  typealias ExCell1d   ExH{:cell1d}
  typealias ExFor      ExH{:for}
  typealias ExRef      ExH{:ref}
  typealias ExIf       ExH{:if}
  typealias ExComp     ExH{:comparison}
  typealias ExDot      ExH{:.}
  typealias ExTuple    ExH{:tuple}
  typealias ExReturn   ExH{:return}
  typealias ExBody     ExH{:body}
  typealias ExQuote    ExH{:QuoteNode}


#  s     : expression to convert
#  svars : vars set since the toplevel graph (helps separate globals / locals)
function tograph(s, evalmod=Main, svars=Any[])

    explore(ex::Any)       = error("[tograph] unmanaged type $ex ($(typeof(ex)))")
    explore(ex::Expr)      = explore(toExH(ex))
    explore(ex::ExH)       = error("[tograph] unmanaged expr type $(ex.head) in ($ex)")

    explore(ex::ExLine)         = nothing     # remove line info
    explore(ex::LineNumberNode) = nothing     # remove line info
    explore(ex::QuoteNode)      = addnode!(g, NConst(ex.value))  # consider as constant

    explore(ex::ExReturn)  = explore(ex.args[1]) # focus on returned statement

    explore(ex::ExVcat)    = explore(Expr(:call, :vcat, ex.args...) )  # translate to vcat() call, and explore
    explore(ex::ExCell1d)  = explore(Expr(:call, :(Base.cell_1d), ex.args...) )  # translate to cell_1d() call, and explore
    explore(ex::ExTrans)   = explore(Expr(:call, :transpose, ex.args[1]) )  # translate to transpose() and explore
    explore(ex::ExColon)   = explore(Expr(:call, :colon, ex.args...) )  # translate to colon() and explore
    explore(ex::ExTuple)   = explore(Expr(:call, :tuple, ex.args...) )  # translate to tuple() and explore

    explore(ex::ExPEqual)  = (args = ex.args ; explore( Expr(:(=), args[1], Expr(:call, :+, args[1], args[2])) ) )
    explore(ex::ExMEqual)  = (args = ex.args ; explore( Expr(:(=), args[1], Expr(:call, :-, args[1], args[2])) ) )
    explore(ex::ExTEqual)  = (args = ex.args ; explore( Expr(:(=), args[1], Expr(:call, :*, args[1], args[2])) ) )

    explore(ex::Real)      = addnode!(g, NConst(ex))

    explore(ex::ExBlock)   = map( explore, ex.args )[end]
    explore(ex::ExBody)    = map( explore, ex.args )[end]

    explore(ex::ExDot)     = addnode!(g, NDot(ex.args[2],     [ explore(ex.args[1]) ]))

    explore(ex::ExComp)    = addnode!(g, NComp(ex.args[2], [explore(ex.args[1]), explore(ex.args[3])]))

    # explore(ex::ExRef)     = addnode!(g, NRef(ex.args[2:end], [ explore(ex.args[1]) ]))
    function explore(ex::ExRef)
        nv = explore(ex.args[1])
        p  = ExNode[ nv ]
        for (i,na) in enumerate(ex.args[2:end])
            if length(ex.args)==2 # single dimension
                ns = addgraph!(:( length(x) ), g, Dict(:x => nv) )
            else # several dimensions
                ns = addgraph!(:( size(x, $i) ), g, Dict(:x => nv) )
            end

            na==:(:) && (na = Expr(:(:), 1, :end) )  # replace (:) with (1:end)

            # explore the dimension expression
            nsvars = union(svars, collect(syms(g.seti)))
            ng = tograph(na, evalmod, nsvars)

            # find mappings for onodes, including :end
            vmap = Dict()
            for (k, sym) in ng.exti.kv
                vmap[sym] = sym == :end ? ns : explore(sym)
            end

            nd = addgraph!(ng, g, vmap)
            push!(p, nd)
        end
        addnode!(g, NRef(:getidx, p))
    end


    function explore(ex::Symbol)
        # ex in [:(:), symbol("end")] && return addnode!(g, NConst(ex))  # plain symbols (used in x[1,:] or y[1:end])

        hassym(g.seti, ex)       && return getnode(g.seti, ex)
        hassym(g.exti, ex)       && return getnode(g.exti, ex)

        nn = addnode!(g, NExt(ex))    # create external node for this var
        g.exti[nn] = ex
        return nn
    end

    function explore(ex::ExCall)
        sf = ex.args[1]
        if sf == :getindex  # needs a special treatment
            return addnode!(g, NRef(:getidx, map(explore, ex.args[2:end])))

        elseif sf == :getfield  # needs a special treatment
            return addnode!(g, NDot(ex.args[3], [ explore(ex.args[2]) ]))

        elseif sf == :setindex! # needs a special treatment
            isa(ex.args[2], Symbol) && error("[tograph] setindex! only allowed on variables")

            vn  = explore(ex.args[2]) # node whose subpart is assigned
            rhn = addnode!(g, NSRef(:setidx, [ vn,    # var modified in pos #1
                                               explore(ex.args[3]), # value affected in pos #2
                                               map(explore, ex.args[4:end])] ))  # indexing starting at #3
            rhn.precedence = filter(n -> vn in n.parents && n != rhn, g.nodes)
            g.seti[rhn] = ex.args[2]

            return nothing

        else #TODO : add setfield!

            # return  addnode!(g, NCall(    evalmod.eval(ex.args[1]), 
            #                           map(explore, ex.args[2:end]) ))
            return  addnode!(g, NCall(  :call, 
                                        map(explore, ex.args[1:end]) ))
        end
    end

    function explore(ex::ExEqual) 
        lhs = ex.args[1]
        
        if isSymbol(lhs)  # x = ....
            lhss = lhs

            # set before ? call explore
            if lhss in union(svars, collect(syms(g.seti)))
                vn = explore(lhss)
                rhn  = addnode!(g, NSRef(:setidx, [ vn,    # var modified in pos #1
                                                    explore(ex.args[2]) ])) # value affected in pos #2
                rhn.precedence = filter(n -> vn in n.parents && n != rhn, g.nodes)
            else # never set before ? assume it is created here
                rhn = explore(ex.args[2])

                # we test if RHS has already a symbol
                # if it does, to avoid loosing it, we create an NIn node
                if hasnode(g.seti, rhn) 
                    rhn = addnode!(g, NIn(lhss, [rhn]))
                end
            end

        elseif isRef(lhs)   # x[i] = ....
            lhss = lhs.args[1]
            vn = explore(lhss) # node whose subpart is assigned
            rhn  = addnode!(g, NSRef(:setidx, [ vn,    # var modified in pos #1
                                                explore(ex.args[2]), # value in pos #2
                                                map(explore, lhs.args[2:end])] ))  # indexing starting at #3
            rhn.precedence = filter(n -> vn in n.parents && n != rhn, g.nodes)

        elseif isDot(lhs)   # x.field = ....
            lhss = lhs.args[1]
            vn = explore(lhss) # node whose subpart is assigned
            rhn  = addnode!(g, NSDot(lhs.args[2], [ vn, explore(ex.args[2])] )) 
            rhn.precedence = filter(n -> vn in n.parents && n != rhn, g.nodes)

        else
            error("[tograph] $(toExpr(ex)) not allowed on LHS of assigment")
        end

        g.seti[rhn] = lhss

        return nothing
    end

    function explore(ex::ExFor)
        is = ex.args[1].args[1]
        isa(is, Symbol) || 
            error("[tograph] for loop using several indexes : $is ")

        # explore the index range
        nir = explore(ex.args[1].args[2])

        # explore the for block as a separate graph 
        nsvars = union(svars, collect(syms(g.seti)))
        g2 = tograph(ex.args[2], evalmod, nsvars)

        # create "for" node
        nf = addnode!(g, NFor( Any[ is, g2 ] ))
        nf.parents = [nir]  # first parent is indexing range fo the loop

        # create onodes (node in parent graph) for each exti
        for (k, sym) in g2.exti.kv
            sym==is  && continue # loop index should be excluded
            pn = explore(sym)  # look in setmap, externals or create it
            g2.exto[pn] = sym
            push!(nf.parents, pn) # mark as parent of for loop
        end

        # create onodes and 'Nin' nodes for each seti
        #  will be restricted to variables that are defined in parent
        #   (others are assumed to be local to the loop)
        for (k, sym) in g2.seti.kv
            if sym in nsvars && sym != is # only for variables set in parent scope
                pn = explore(sym)                   # create node if needed
                rn = addnode!(g, NIn(sym, [nf]))    # exit node for this var in this graph
                g.seti[rn] = sym                    # signal we're setting the var
                g2.seto[rn] = sym

                append!(nf.precedence, filter(n -> pn in n.parents && n != nf, g.nodes))

                # create corresponding exti if it's not already done
                if !hassym(g2.exto, sym)
                    g2.exto[pn] = sym
                    push!(nf.parents, pn) # mark as parent of for loop
                end
            end
        end
    end

    #  top level graph
    g = ExGraph()

    exitnode = explore(s)  
    # exitnode = nothing if only variable assigments in expression
    #          = ExNode of last calc otherwise

    # id is 'nothing' for unnassigned last statement
    exitnode!=nothing && ( g.seti[exitnode] = nothing ) 

    # Resolve external symbols that are Functions, DataTypes or Modules
    # and turn them into constants
    for en in filter(n -> isa(n, NExt) & !in(n.main, svars) , keys(g.exti))
        if isdefined(evalmod, en.main)  # is it defined
            tv = evalmod.eval(en.main)
            isa(tv, TypeConstructor) && error("[tograph] TypeConstructors not supported: $ex $(tv), use DataTypes")
            if isa(tv, DataType) || isa(tv, Module) || isa(tv, Function)
                delete!(g.exti, en)
                nc = addnode!(g, NConst( tv ))
                fusenodes(g, nc, en)
            end
        end
    end

    g
end
