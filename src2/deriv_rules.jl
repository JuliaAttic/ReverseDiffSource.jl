################################################################################
#
#   Derivation rules definition and retrieval
#
################################################################################

# rules are stored as functions to reuse the existing multiple dispatch
# instead of recoding it.
# to keep workspaces tidy, these functions are created in an ad-hoc module
module DRules
  gg = nothing

  function define(f::Function, sig::Vector, rank, payload)
    # f, sig, rank = cos, [(:a, Real), (:b, Union{Real, Complex})], 2
    # payload = "$f $sig"
    global vp

    fn = symbol("dr_$(rank)_$(object_id(f))")

    # declare function if it does not exists yet
    isdefined(fn) || eval(Expr(:method, fn) )
    methods(eval(fn))

    # add method
    vp = payload
    fn2 = Expr(:call, fn, [ :( $vn::$typ ) for (vn, typ) in sig ]...)
    fn3 = Expr(:(=), fn2, :g)
    fn4 = Expr(:let, fn3, :(g=gg))
    eval(fn4)
    # fn2 = Expr(:call, fn, :( x::Real ) )
    eval(:( let g = gg ; $fn3 ; end ) )
    eval(fn3)
    @eval let g = gg ; dr_2_sdfgsdfg(x::Real) = g ; end
    dump( :(let g = gg ; dr_2_sdfgsdfg(x::Real) = g ; end)  )
    @eval let g = gg ; $fn2 = g ; end

    @eval $(Expr(:(=), fn2, Expr(:block, :g)))
    dump( :( dr_2_sdfgsdfg(x::Real) = g ) )
    end)
  end

dump( :( let g=gg ; f(x) = g ; end  ) )

  function fetchdef(f::Function, val) # f, sig = sin, Float64
    eval(Expr(:call, symbol("dr$(object_id(f))"), val) )
  end
end


DRules.fetchdef(sin, 12.)

DRules.define(sin, [(:a, Real);], 1, "coucou")
DRules.define(cos, [(:a, Real);], 1, "coucou")
DRules.fetchdef(sin, 12.)
DRules.fetchdef(cos, 12.)
DRules.define(cos, Int64)
DRules.fetchdef(cos, 12.)
DRules.fetchdef(cos, 12)


#### function derivation rules declaration functions/macros

:(  $(Expr(:call, :sin, :( x::Int64)))  = 3 )

[:a => Float64, :b => Union{Real, Complex}]

# macro version
macro deriv_rule(func::Expr, dv::Symbol, diff)
    # func = :(  colon(x,y) ) ; dv = :x ; diff = 0.
    emod = current_module()

    ss = Symbol[]
    ts = Type[]
    for e in func.args[2:end]
        if isa(e, Symbol)
            push!(ss, e)
            push!(ts, Any)

        elseif isa(e, Expr) && e.head== :(::)
            push!(ss, e.args[1])
            push!(ts, emod.eval(e.args[2]))

        else
            error("[deriv_rule] cannot parse $e")
        end
    end

    deriv_rule(emod.eval(func.args[1]), collect(zip(ss, ts)), dv, diff)
end

function deriv_rule(func::Union{Function, Type},
    args::Vector, dv::Symbol, diff::Union{Expr, Symbol, Real})
    emod = current_module()

    sig = Tuple{ Type[ e[2] for e in args ]... }

    ss  = Symbol[ e[1] for e in args ]

    index = findfirst(dv .== ss)
    (index == 0) && error("[deriv_rule] cannot find $dv in function arguments")

    # non generic functions are matched by their name
    fidx = isa(func, Function) && isa(func.env, Symbol) ? func.env : func

    haskey(drules, (fidx, index)) || (drules[(fidx, index)] = Dict())

    g = tograph(diff, emod)  # make the graph
    push!(ss, :ds)

    drules[(fidx, index)][sig] = (g, ss)
    nothing
end

function hasrule(f, pos)
    haskey(drules, (f, pos)) && return true
    # non generic functions are matched by their name
    isa(func, Function) && isa(f.env, Symbol) && return haskey(drules, (f.env, pos))
    false
end

function getrule(f, pos)
    if isa(f, Function) && isa(f.env, Symbol) # non generic functions are matched by their name
        haskey(drules, (f.env, pos)) && return drules[(f.env, pos)]
    else
        haskey(drules, (f, pos)) && return drules[(f, pos)]
    end
    error("no derivation rule for $(f) at arg #$(pos)")
end






g(x) = "x"
g(1)

let a = 45
  g(x::Int64) = a
end

g(1)
g(1.)

let a = 12
  h(x::Int64) = a
end

h(3)
