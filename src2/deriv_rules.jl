################################################################################
#
#   Derivation rules definition and retrieval
#
################################################################################

# rules are stored as functions to reuse the existing multiple dispatch
# instead of recoding it.
# to keep workspaces tidy, these functions are created in an ad-hoc module
module DerivRules

  rfsym(f, ord) = symbol("dr_$(ord)_$(object_id(f))")

  function define(f, sig::Vector, ord, payload)
    global vp

    fn = rfsym(f, ord)

    # declare function if it does not exists yet
    isdefined(fn) || eval( Expr(:method, fn) )

    # add method
    vp  = payload
    fn2 = Expr(:call, fn, [ :( $vn::$typ ) for (vn, typ) in sig ]...)
    @eval let g = vp ; $fn2 = g ; end
  end

  function hasrule(f, ord::Int, args::Tuple)
    fn = rfsym(f, ord)
    isdefined(DerivRules, fn) || return false
    rf = eval(fn)
    meths = methods(rf, map(typeof, args))
    length(meths) > 0
  end

  function getrule(f, ord::Int, args::Tuple)
    if !hasrule(f, ord, args)
      fn = "$f(" * join(map(typeof, args), ",") * ")"
      error("no deriv rule for '$fn' at pos $ord")
    end

    fn = rfsym(f, ord)
    eval(Expr(:call, fn, args...) )
  end
end

#### function derivation rules declaration functions/macros

# macro form
macro deriv_rule(func::Expr, dv::Symbol, diff)
  m = current_module()
  ff = m.eval(func.args[1])
  args = formatargs(func.args[2:end])
  deriv_rule(m.eval(func.args[1]), args, dv, diff)
end

# function form
function deriv_rule(func::Union{Function, Type},
                    args::Vector, dv::Symbol,
                    diff::Union{Expr, Symbol, Real};
                    repl=false)
  ss  = Symbol[ e[1] for e in args ]
  ord = findfirst(dv .== ss)
  (ord == 0) && error("[deriv_rule] cannot find $dv in function arguments")

  payload = ( Snippet(diff, vcat(ss, :ds)), repl )
  DerivRules.define(func, args, ord, payload)
  nothing
end

# macro form for mutating functions

mutdict = Dict{Function, Int}()
ismutating(f) = haskey(mutdict, f)

macro deriv_rule_mut(func::Expr, dv::Symbol, diff)
    m = current_module()
    ff = m.eval(func.args[1])
    args = formatargs(func.args[2:end])
    deriv_rule(ff, args, dv, diff, repl=true)

    ord = findfirst(dv .== Symbol[ e[1] for e in args ])
    mutdict[ff] = ord
end

macro deriv_rule_repl(func::Expr, dv::Symbol, diff)
    m = current_module()
    ff = m.eval(func.args[1])
    args = formatargs(func.args[2:end])
    deriv_rule(ff, args, dv, diff, repl=true)
end

function formatargs(fargs)
  m = current_module()
  args = Tuple{Symbol, Type}[]
  for e in fargs
      if isa(e, Symbol)
          push!(args, (e, Any))
      elseif isa(e, Expr) && e.head== :(::)
          push!(args, (e.args[1], m.eval(e.args[2])))
      else
          error("[deriv_rule] cannot parse $e")
      end
  end
  args
end
