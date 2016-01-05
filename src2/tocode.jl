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

function tocode(g::Graph, exits=[EXIT_SYM;]) #  g = A.g ; exits=[A.EXIT_SYM;]
  #### creates expression for names qualified by a module
  mexpr(ns) = length(ns) == 1 ? ns[1] : Expr(:., mexpr(ns[1:end-1]), QuoteNode(ns[end]) )

  function translate(o::Op)  # o = g.ops[1]
    # println(o.f.val)
    vargs = Any[ getexpr(arg) for arg in o.asc ]

    # special translation cases
    if o.f.val == vcat
      return Expr(:vect, vargs...)
    elseif o.f.val == colon
      return Expr( :(:), vargs...)
    elseif o.f.val == transpose
      return Expr(symbol("'"), vargs...)
    elseif o.f.val == tuple
      return Expr(:tuple, vargs...)
    elseif o.f.val == getindex
      return Expr( :ref, vargs...)
    elseif o.f.val == getfield
      return Expr(   :., vargs[1], QuoteNode(vargs[2]))
    elseif o.f.val == setindex!
      return Expr( :(=), Expr(:ref, vargs[1], vargs[3:end]...), vargs[2])
    elseif o.f.val == setfield!
      return Expr( :(=), Expr(  :., vargs[1], QuoteNode(vargs[2])), vargs[3])
    end

    # default translation
    thing_module(op::DataType) = tuple(fullname(op.name.module)..., op.name.name)

    thing_module(op::Function) =
        tuple(fullname(Base.function_module(op, Tuple{Vararg{Any}}))...,
              op.env.name )
              # symbol(string(op)) )

    mt = try
            thing_module(o.f.val)
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

    # println(Expr(:call, mexpr( mt ), Any[ getexpr(arg) for arg in o.asc ]...))
    Expr(:call, mexpr( mt ), Any[ getexpr(arg) for arg in o.asc ]...)
  end

  getexpr(l::Loc{:constant}) = l.val
  function getexpr(l::Loc) # l = g.locs[1]
    haskey(locex, l) && return locex[l]
    sym = 0
    for (k,v) in g.symbols ; v!=l && continue ; sym = k ; break ; end
    sym = sym!= 0 ? sym : newvar()
    locex[l] = sym
    sym
  end

  function ispivot(o::Op, line) # o = A.g.ops[1] ; line = 1
    # checks if desc appear several times afterward
    #  or if it is mutated
    for l in o.desc # l = o.desc[1]
      ct = 0
      for o2 in g.ops[line+1:end]
        l in o2.desc && return true
        ct += l in o2.asc
        ct > 1 && return true
      end
    end
    false
  end

  out = Any[]
  opex  = Dict{Op, Any}()
  locex = Dict{Loc, Any}()

  # check that variables to be shown are defined by graph
  vset = setdiff(exits, keys(g.symbols))
  length(vset) > 0 &&
    error("[tocode] some requested variables were not found : $vset")

  # if no op, just fetch constant/external associated
  # to each exits requested
  if length(g.ops) == 0
    for l in g.locs
      # symbols for that loc
      syms = collect(keys(filter((k,v) -> v==l && k in exits, g.symbols)))
      length(syms)==0 && continue
      syms = setdiff(syms, [EXIT_SYM;]) # result is implicit, no need for assign
      ex = foldr((x,y) -> Expr(:(=), x, y), getexpr(l), syms )
      push!(out, ex)
    end

  else # run through each op
    lexits = Loc[ g.symbols[s] for s in exits ]

    for (line, o) in enumerate(g.ops) #
      opex[o] = translate(o)       # translate to Expr

      if any(l -> l in lexits, o.desc) || ispivot(o, line) # assignment needed,
        rv = o.desc[1] # TODO : manage multiple assignment
        syms = collect(keys(filter((k,v) -> v==rv && k in exits, g.symbols)))
        length(syms)==0 && push!(syms, newvar())
        syms = setdiff(syms, [EXIT_SYM;]) # result is implicit, no need for assign
        ex = foldr((x,y) -> Expr(:(=), x, y), opex[o], syms)
        push!(out, ex)
        length(syms) > 0 && (locex[o.desc[1]] = syms[1])

      elseif o.desc[1] in o.asc   # mutating Function
        push!(out, opex[o])
      else # keep expression for later
        locex[o.desc[1]] = opex[o]
      end
    end
  end

  Expr(:block, out...)
end
