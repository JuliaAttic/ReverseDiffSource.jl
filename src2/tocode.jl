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

function tocode(g::Graph, exits=[EXIT_SYM;]) #   exits=[EXIT_SYM;]

  # check that variables to be shown are defined in root block
  vset = setdiff(exits, keys(g.block.symbols))
  length(vset) > 0 &&
    error("[tocode] some requested variables were not found : $vset")

  out   = Any[]
  locex = Dict{Loc, Any}()

  #### creates expression for names qualified by a module
  mexpr(ns) = length(ns) == 1 ? ns[1] : Expr(:., mexpr(ns[1:end-1]), QuoteNode(ns[end]) )

  function translate(o::Op)  # o = g.block.ops[1]
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

    Expr(:call, mexpr( mt ), vargs...)
  end

  ### returns the correct symbol / expression for given Loc
  getexpr(l::CLoc) = l.val

  function getexpr(l::ELoc)
    # find the symbol defined in env for this Loc
    syms = collect(keys(g.block.symbols))
    filter!(s -> g.block.symbols[s]==l, syms)
    filter!(g.isdef, syms)
    length(syms)==0 && error("[tocode] no symbol found for external $l")
    syms[1]
  end

  function getexpr(l::Loc) # l = g.locs[1]
    haskey(locex, l) && return locex[l]
    error("[tocode] no expression for Loc $l")
  end

  ### generates assignment expression for given Loc
  function genassign(l::Loc, force=false)
    # find symbols to be assigned to among exits
    syms = Set(filter(s -> g.block.symbols[s]==l, exits))
    # if force = true, EXIT_SYM is not a valid proposition
    force && setdiff!(syms, [EXIT_SYM;])

    # if none found, try to find one among other symbols
    if length(syms)==0
      ss = collect(keys(g.block.symbols))
      idx = findfirst(s -> g.block.symbols[s]==l, ss)
      idx != 0 && push!(syms, ss[idx])
    end
    # if still none found, generate one
    length(syms)==0 && push!(syms, newvar())

    # generate assignement expression for all symbols in syms, except EXIT_SYM
    setdiff!(syms, [EXIT_SYM;])
    push!(out, foldr((x,y) -> Expr(:(=), x, y), getexpr(l), collect(syms) ) )
    length(syms) > 0 && (locex[l] = pop!(syms)) # update locex (if syms not empty)
  end

  ### tells if an assignement expression should be generated
  function ispivot(o::Op, line)
    # checks if desc appear several times afterward
    #  or if it is mutated
    for l in o.desc # l = g.block.ops[1].desc[1] ; line=1
      ct = 0
      for o2 in g.block.ops[line+1:end]
        l in o2.desc && return true
        ct += l in o2.asc
        ct > 1 && return true
      end
    end
    false
  end

  # if no op, just fetch constant/external associated to an exit
  if length(g.block.ops) == 0
    lexits = unique( Loc[ g.block.symbols[s] for s in exits ] ) # Locs of interest
    map(genassign, lexits)

  # otherwise, run through each op
  else
    lexits = unique( Loc[ g.block.symbols[s] for s in exits ] )

    for (line, o) in enumerate(g.block.ops) # line=1 ; o = g.block.ops[1]
      # TO DO : manage multiple assignment

      ex = translate(o)       # translate to Expr

      if ispivot(o, line) # assignment needed (force a symbol if EXIT_SYM)
        locex[o.desc[1]] = ex
        genassign(o.desc[1], true)

      elseif any(l -> l in lexits, o.desc) # assignment needed
        locex[o.desc[1]] = ex
        genassign(o.desc[1])


      elseif o.desc[1] in o.asc   # mutating Function
        push!(out, ex)

      else # keep expression for later
        locex[o.desc[1]] = ex

      end
    end
  end

  Expr(:block, out...)
end
