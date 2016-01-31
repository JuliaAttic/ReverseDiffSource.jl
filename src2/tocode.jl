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
    isa(l.val, Real) ? l.val : symbol(l.val)
  end

  symbol(Float64)

  function getexpr(l::ELoc)
    # find the symbol defined in env for this Loc
    syms = collect(keys(symbols))
    filter!(s -> symbols[s]==l, syms)
    filter!(g.isdef, syms)
    if length(syms)==0
      Base.show_backtrace(STDERR, backtrace())
      error("[tocode] no symbol found for external $l (# $(indexin([l;], g.locs)[1]))")
    end
    syms[1]
  end

  function getexpr(l::RLoc) # l = g.locs[1]
    haskey(locex, l) && return locex[l]
    Base.show_backtrace(STDERR, backtrace())
    error("[tocode] no expression for Loc $l (# $(indexin([l;], g.locs)[1]))")
  end

  ### generates assignment expression for given Loc
  function genassign(l::Loc, force=false)
    sym = get(sdict, l, newvar())
    if sym == EXIT_SYM && !force
      push!(out, getexpr(l))
    else
      sym = sym==EXIT_SYM ? newvar() : sym
      push!(out, Expr(:(=), sym, getexpr(l)) )
      locex[l] = sym
    end
  end

  ###### start expression generation
  out   = Any[]

  # find exit op and symbol for each lexits remaining
  ks  = collect(keys(symbols))
  sdict = Dict() ; pdict = Dict()
  for l in lexits # l = pop!(lexits2)
    syms = filter(s -> symbols[s]==l, ks)
    syms = EXIT_SYM in syms ? [EXIT_SYM;] : syms
    length(syms)==0 && push!(syms, newvar())
    pos = findlast(o -> l in o.desc, ops)
    sdict[l]    = syms[1]
    pdict[l]    = pos
  end

  # first process lexits that are not created by any op
  for (l,p) in pdict
    p==0 && genassign(l)
  end

  # now go through the Ops
  for (line, o) in enumerate(ops) # line=1 ; o = ops[1]

    if isa(o, AbstractBlock)
      exs = blockcode(o, locex, symbols, g)
      append!(out, exs)

      for d in o.desc
        if haskey(sdict, d) && sdict[d]==EXIT_SYM && pdict[d]==line
          push!(out, locex[d])
        end
      end

    elseif length(intersect(o.desc, o.asc)) > 0   # mutating Op
      push!(out, translate(o))
      if haskey(sdict, o.desc[1]) && sdict[o.desc[1]]==EXIT_SYM && pdict[o.desc[1]]==line
        push!(out, locex[o.desc[1]])
      end

    else # simple, non-mutating func that returns single variable
      locex[o.desc[1]] = translate(o)

      if ispivot(rest(ops, line)) # assignment needed (force a symbol if EXIT_SYM)
        genassign(o.desc[1], true)
      elseif o.desc[1] in lexits # assignment needed
        genassign(o.desc[1])
      end
    end

  end


  Expr(:block, out...)
end


### tells if an assignement expression should be generated for given Op
function ispivot(nextops)
  o, _ = next(nextops, start(nextops))
  for l in o.desc
    writ = false
    used = false
    for o2 in drop(nextops,1)
      l in o2.desc && return true # result is mutated
      writ = any(a -> a in o2.desc, o.asc) # ascendants of results modified
      l in o2.asc || continue
      used && return true # result is used at least twice
      writ && return true # result is used after ascendants modification
      used = true
    end
  end
  false
end
