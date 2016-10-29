################################################################################
#
#   Derivation rules definition and retrieval
#
################################################################################

# rules are stored as functions to reuse the existing multiple dispatch
# instead of trying to recode it.
# To keep workspaces tidy, these functions are created in an ad-hoc module
module DerivRules
  ruledict = Any[]
  dr() = 0  # to create initial function
end

### creates function signature for DerivRules.dr
function _tosigtuple(f, ts, ord)
  tuple([Val{Symbol(f)}, Val{ord}, ts...]...)
end

### creates rule through a new method on function DerivRules.dr
function _define(f, ts, ord, payload)
  st = _tosigtuple(f, ts, ord)

  push!(DerivRules.ruledict, payload)
  rid = length(DerivRules.ruledict)

  if length(ts) == 0
    DerivRules.dr(::st[1], ::st[2]) =  Val{rid}()
  elseif length(ts) == 1
    DerivRules.dr(::st[1], ::st[2],::st[3]) =  Val{rid}()
  elseif length(ts) == 2
    DerivRules.dr(::st[1], ::st[2],::st[3], ::st[4]) =  Val{rid}()
  elseif length(ts) == 3
    DerivRules.dr(::st[1], ::st[2],::st[3], ::st[4],::st[5]) =  Val{rid}()
  elseif length(ts) == 4
    DerivRules.dr(::st[1], ::st[2],::st[3], ::st[4],::st[5], ::st[6]) =  Val{rid}()
  else
    error("[DerivRules.define] too many arguments !")
  end
end

### fetches rule for given function / signature / differentiation var position
function getrule(f, ord::Int, args::Tuple)
  st = _tosigtuple(f, args, ord)
  if method_exists(DerivRules.dr, st)
    rts = Base.return_types(DerivRules.dr, st)
    # if single method fits the constraints,  return directly
    length(rts)==1 && return DerivRules.ruledict[ first(rts).parameters[1] ]
    # if several we will have to pick the good one
    mt = which(DerivRules.dr, st) # correct method
    mts = methods(DerivRules.dr, st) # all methods (hopefully in the same order as rts)
    mn = findfirst(mts.ms .== mt)
    mn==0 && error("no deriv rule for '$fn' at pos $ord")
    return DerivRules.ruledict[ rts[mn].parameters[1] ]
  else
    fn = "$f(" * join(args, ",") * ")"
    error("no deriv rule for '$fn' at pos $ord")
  end
end

### converts argument expressions to a vector of tuples (symbol, type)
function _formatargs(fargs)
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

# packages rule before calling DerivRules.define
function _deriv_rule(func::Union{Function, Type},
                     args::Vector, dv::Symbol,
                     diff::Union{Expr, Symbol, Real};
                     repl=false)
  ss = Symbol[ e[1] for e in args ]
  ts = Type[ e[2] for e in args ]
  ord = findfirst(dv .== ss)
  (ord == 0) && error("[deriv_rule] cannot find $dv in function arguments")

  # payload = ( Snippet(diff, vcat(ss, :ds)), repl )
  payload = ( tograph(diff, current_module()), vcat(ss, :ds) )

  _define(func, ts, ord, payload)
  nothing
end


#### function derivation rules declaration functions/macros

# macro form
"""
The `@deriv_rule(func::Expr, dv::Symbol, diff)` macro defines a derivation rule,
or more precisely the value that should be added to the derivative accumulator
in the reverse differentiation process.
- func : is the method whose rule is defined, the full signature should be
specified (for example `+(x::Int64, y::AbstractArray)` for a rule applying to
additions between and interger and an array).
- dv : is the symbol of the variable among the arguments on which the derivation
rule applies.
- diff : is the expression of the rule. The symbol `ds` is reserved to represent
the derivative accumulator of the function result.
"""
macro deriv_rule(func::Expr, dv::Symbol, diff)
  m = current_module()
  ff = m.eval(func.args[1])
  args = _formatargs(func.args[2:end])
  _deriv_rule(m.eval(func.args[1]), args, dv, diff)
end
