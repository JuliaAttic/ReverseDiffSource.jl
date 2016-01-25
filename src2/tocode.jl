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

"""
Function `tocode(g, exits)` translates the graph g
into an expression, with assignments generated for the symbols in `exits`.
"""
function tocode(g::Graph, exits=[EXIT_SYM;]) #   exits=[EXIT_SYM;]
  # check that variables to be shown are defined in root block
  vset = setdiff(exits, keys(g.block.symbols))
  length(vset) > 0 &&
    error("[tocode] some requested variables were not found : $vset")

  lexits = unique( Loc[ g.block.symbols[s] for s in exits ] )
  _tocode(g.block.ops, lexits, g.block.symbols, g)
end

"""

Function `_tocode(ops, lexits, symbols, g, locex)` translates the vector of Op
`ops` into an expression, with assignments generated for the Locs in lexits.
`symbols` is used as suggestions for naming Locs, with the symbol `[EXIT_SYM]`
reserved for the Loc containing the exit value of the block.
"""
function _tocode(ops, lexits, symbols, g, locex=Dict{Loc, Any}()) # exits=[EXIT_SYM;]

  #### creates expression for names qualified by a module
  mexpr(ns) = length(ns) == 1 ? ns[1] : Expr(:., mexpr(ns[1:end-1]), QuoteNode(ns[end]) )

  function translate(o::Op)  # o = g.block.ops[1]
    # println(o.f.val)
    vargs = Any[ getexpr(arg) for arg in o.asc ]
    fun = o.f.val

    # special translation cases
    if fun == vcat
      return Expr(:vect, vargs...)
    elseif fun == colon
      return Expr( :(:), vargs...)
    elseif fun == transpose
      return Expr(symbol("'"), vargs...)
    elseif fun == tuple
      return Expr(:tuple, vargs...)
    elseif fun == getindex
      return Expr( :ref, vargs...)
    elseif fun == getfield
      return Expr(   :., vargs[1], QuoteNode(vargs[2]))
    elseif fun == setindex!
      return Expr( :(=), Expr(:ref, vargs[1], vargs[3:end]...), vargs[2])
    elseif fun == setfield!
      return Expr( :(=), Expr(  :., vargs[1], QuoteNode(vargs[2])), vargs[3])
    end

    # default translation
    thing_module(op::DataType) = tuple(fullname(op.name.module)..., op.name.name)

    thing_module(op::Function) =
        tuple(fullname(Base.function_module(op, Tuple{Vararg{Any}}))...,
              op.env.name )
              # symbol(string(op)) )

    mt = try
            thing_module(fun)
         catch e
            error("[tocode] cannot find module of $fun ($(typeof(fun)))")
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
  function getexpr(l::CLoc)
    haskey(locex, l) && return locex[l]
    l.val
  end

  function getexpr(l::ELoc)
    # find the symbol defined in env for this Loc
    syms = collect(keys(symbols))
    println(syms)
    filter!(s -> symbols[s]==l, syms)
    println(syms)
    filter!(g.isdef, syms)
    println(syms)
    length(syms)==0 && error("[tocode] no symbol found for external $l")
    syms[1]
  end

  function getexpr(l::RLoc) # l = g.locs[1]
    haskey(locex, l) && return locex[l]
    # warn("[getexpr] calling genassign for $l")
    # println(Expr(:block, out...))
    # return newvar()
    error("[tocode] no expression for Loc $l")
  end

  ### generates assignment expression for given Loc
  function genassign(l::Loc, force=false)
    # find assignment symbols among defined in g to be assigned to among exits
    # syms = Set(filter(s -> g.block.symbols[s]==l, exits))
    if haskey(symbols, EXIT_SYM) && !force && symbols[EXIT_SYM]==l
      syms = Symbol[]
    else
      ks  = collect(keys(symbols))
      syms = filter(s -> s!=EXIT_SYM && symbols[s]==l, ks)
      # if still none found, generate one
      length(syms)==0 && push!(syms, newvar())
    end

    # generate assignement expression for all symbols in syms
    push!(out, foldr((x,y) -> Expr(:(=), x, y), getexpr(l), collect(syms) ) )
    length(syms) > 0 && (locex[l] = pop!(syms)) # update locex (if syms not empty)
  end

  ### tells if an assignement expression should be generated
  function ispivot(o::Op, line)
    # checks if any desc of `o` appears several times afterward
    #  or if they are modified
    for l in o.desc # l = g.block.ops[1].desc[1] ; line=1
      ct = 0
      for o2 in ops[line+1:end]
        l in o2.desc && return true
        ct += l in o2.asc
        ct > 1 && return true
      end
    end
    # checks if any asc of `o` is modified later
    for l in o.asc # l = ops[1].desc[1] ; line=1
      for o2 in ops[line+1:end]
        l in o2.desc && l in o2.asc && return true
      end
    end
    #
    false
  end

  ###### start expression generation
  out   = Any[]

  # if no op, just fetch constant/external associated to an exit
  if length(ops) == 0
    map(genassign, lexits)

  else # otherwise, run through each op

    for (line, o) in enumerate(ops) # line=1 ; o = ops[1]

      if isa(o, AbstractBlock)
        exs = blockcode(o, locex, g)
        append!(out, exs)
        # map(genassign, o.desc)

      elseif length(intersect(o.desc, o.asc)) > 0   # mutating Op
        # assumptions : function modifies a single variable and return
        # value (if any) is ignored
        push!(out, translate(o))

      else # simple, non-mutating func that returns single variable
        # TODO : manage functions returning multiple values ?
        locex[o.desc[1]] = translate(o)

        if ispivot(o, line) # assignment needed (force a symbol if EXIT_SYM)
          genassign(o.desc[1], true)
        elseif o.desc[1] in lexits # assignment needed
          genassign(o.desc[1])
        end
      end

    end
  end

  Expr(:block, out...)
end
