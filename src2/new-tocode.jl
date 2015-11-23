## temp var name generator
const TEMP_NAME = "_tmp"   # prefix of new variables

let
  vcount = Dict()
  global newvar
  function newvar(radix::Union{AbstractString, Symbol}=TEMP_NAME)
    vcount[radix] = haskey(vcount, radix) ? vcount[radix]+1 : 1
    return symbol("$(radix)$(vcount[radix])")
  end
  newvar() = newvar(TEMP_NAME)

  global resetvar
  function resetvar()
    vcount = Dict()
  end
end

function tocode(g::Graph)
  #### creates expression for names qualified by a module
  mexpr(ns) = length(ns) == 1 ? ns[1] : Expr(:., mexpr(ns[1:end-1]), QuoteNode(ns[end]) )

  function translate(o::Op)  # o = g.ops[1]
    vargs = Any[ getexpr(arg) for arg in o.asc ]

    # special translation cases
    if o.f == vcat
      return Expr(:vect, vargs...)
    elseif o.f == colon
      return Expr( :(:), vargs...)
    elseif o.f == transpose
      return Expr(symbol("'"), vargs...)
    elseif o.f == tuple
      return Expr(:tuple, vargs...)
    elseif o.f == getindex
      return Expr( :ref, vargs...)
    elseif o.f == getfield
      return Expr(   :., vargs...)
    elseif o.f == setindex!
      return Expr( :(=), Expr(:ref, vargs[1], vargs[3:end]...), vargs[2])
    elseif o.f == setfield!
      return Expr( :(=), Expr(  :., vargs[1], vargs[2]), vargs[3])
    end

    # default translation
    thing_module(op::DataType) = tuple(fullname(op.name.module)..., op.name.name)

    thing_module(op::Function) =
        tuple(fullname(Base.function_module(op, Tuple{Vararg{Any}}))...,
              op.env.name )
              # symbol(string(op)) )

    mt = try
            thing_module(o.f)
         catch e
            error("[tocode] cannot find module of $op ($(typeof(op)))")
         end

    # try to strip module names for brevity
    try
        mt2 = (:Base, mt[end])
        eval(:( $(mexpr(mt)) == $(mexpr(mt2)) )) &&  (mt = mt2)
        mt2 = (mt[end],)
        eval(:( $(mexpr(mt)) == $(mexpr(mt2)) )) &&  (mt = mt2)
    end

    Expr(:call, mexpr( mt ), Any[ getexpr(arg) for arg in o.asc ]...)
  end

  getexpr(l::Loc{:constant}) = l.val
  function getexpr(l::Loc) # l = g.locs[1]
    haskey(locex, l) && return locex[l]
    sym = 0
    for (k,v) in g.vars ; v!=l && continue ; sym = k ; break ; end
    sym = sym!= 0 ? sym : newvar()
    locex[l] = sym
    sym
  end

  function ispivot(o::Op)
    true
  end

  out = Any[]
  opex  = Dict{Op, Any}()
  locex = Dict{Loc, Any}()
  for o in g.ops #
    opex[o] = translate(o)       # translate to Expr

    # TODO : manage multiple assignment
    if o.desc[1] in o.asc   # mutating Function
      push!(out, opex[o])
    elseif ispivot(o) # assignment needed,
      sym = getexpr(o.desc[1])
      locex[o.desc[1]] = sym
      if sym == nothing # terminal calculation
        push!(out, :( $(opex[o])) )
      else
        push!(out, :( $sym = $(opex[o])) )
      end
    else
      locex[o.desc[1]] = opex[o]
    end
  end

  return Expr(:block, out...)
end
