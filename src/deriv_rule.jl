#########################################################################
#
#   Derivation rule and type equivalence declaration functions / macros
#
#########################################################################

rdict = Dict()

function dfuncname(nam::Union(Expr, Symbol), ind::Int)
    if isa(nam, Symbol)
        return symbol("d_$(nam)_$(ind)")
    elseif nam.head == :. 
        st = "$nam"[3:end-1]
        return symbol("d_$(st)_$(ind)")
    else
        error("[dfuncname] cannot parse expr $nam")
    end
end

function deriv_rule(func::Expr, dv::Symbol, diff::Union(Expr, Symbol, Real))
    #### list variable symbols and annotate type names with "Main." 
    argsn = Symbol[]
    sig = Any[]
    for e in func.args[2:end]
        if isa(e, Symbol)
            push!(argsn, e)
            push!(sig, e)

        elseif isa(e, Expr) && e.head== :(::)  # FIXME : will fail for complex definitions
            push!(argsn, e.args[1])
            e2 = e.args[2]
            # if isa(e2, Symbol)  # type without qualifying module
            #     ne = Expr(:., :Main, Expr(:quote, e2))
            # elseif isa(e2, Expr) && e2.head == :.  # type with module
            #     ne = e2
            if isa(e2, Symbol) || isa(e2, Expr) && e2.head == :.
                ne = Expr(:., :Main, Expr(:quote, e2))
            elseif isa(e2, Expr) && e2.head == :curly
                ne = Expr(:curly, [ Expr(:., :Main, Expr(:quote, ei)) for ei in e2.args]...)
            elseif isa(e2, Expr) && e2.head == :call && e2.args[1] == :Union
                ne = Expr(:call, :Union, [ Expr(:., :Main, Expr(:quote, ei)) for ei in e2.args[2:end]]...)
            
            else
                error("[deriv_rule] cannot parse $e")
            end

            push!(sig, Expr(:(::), e.args[1], ne))

        else
            error("[deriv_rule] cannot parse $e")
        end
    end
    # argsn = map(e-> isa(e, Symbol) ? e : e.args[1], func.args[2:end])
    push!(argsn, :ds)  # add special symbol ds

    index = find(dv .== argsn)[1] # TODO : add error message if not found

    #### make the graph
    g = tograph( diff )

    #### store graph, build proxy function
    rn = gensym("rule")
    rdict[rn] = (g, argsn, g.seti.vk[nothing])

    # diff function name
    fn = dfuncname(func.args[1], index)

    # create function returning applicable rule # for this signature
    eval( :( $(Expr(:call, fn, sig...)) = $(Expr(:quote, rn)) ) )
end

# macro version
macro deriv_rule(func::Expr, dv::Symbol, diff)
    deriv_rule(func, dv, diff)
end

#####  composite type - vector equivalence declaration  ######

macro typeequiv(typ::Union(Symbol, Expr), n::Int)
    ie = n==1 ? 0. : Expr(:vcat, zeros(n)...)
    deriv_rule(:( equivnode(x::$(typ))) , :x, ie)
end

# macro typeequiv2(typ::Union(Symbol, Expr), n::Int)
#     ie = n==1 ? 0. : Expr(:vcat, zeros(n)...)
#     cm = current_module()
#     et = cm.eval(typ)
#     te = Expr(:(.), module_name(cm) , QuoteNode(methods(et).name) )

#     se = Expr(:call, :equivnode, Expr(:(::), :x, te))
#     # println(" module $v2 ($(typeof(v2)))")
#     # local typ2 = v2.eval(typ)
#     # v1 = methods(typ2).name
#     # println(" name $v1 ($(typeof(v1))), module $v2 ($(typeof(v2)))")
#     dump(:( equivnode(x::$(te))),7)
#     deriv_rule(se , :x, ie)
# end

# function typeequiv(typ::DataType, n::Int)
#     ie = n==1 ? 0. : Expr(:vcat, zeros(n)...)
#     deriv_rule(:( equivnode(x::$(typ))) , :x, ie)
# end    