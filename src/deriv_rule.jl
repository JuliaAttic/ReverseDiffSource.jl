################################################################################
#
#   Derivation rules definition and retrieval
#
################################################################################

# rules are stored as functions to reuse the existing multiple dispatch
# instead of recoding it.
# to keep workspaces tidy, these functions are created in an ad-hoc module
module DerivRules
    ruledict = Any[]

    # rfsym(f, ord) = symbol("dr_$(ord)_$(object_id(f))")
    # rfsym(f, ord) = symbol("dr_$(ord)_$(symbol(f))")

    function _totuple(f, t::Tuple, ord)
        tuple([Val{symbol(f)}, Val{ord}, t...]...)
    end

    function define(f, sig::Vector, ord, payload)
        ts = tuple([ typ for (vn, typ) in sig ]...)
        st = _totuple(f, ts, ord)

        push!(ruledict, payload)
        rid = length(ruledict)

        fn2 = Expr(:call, :dr, [ :( ::$typ ) for typ in st ]...)
        @eval $fn2 = Val{$rid}()
        #
        # global vp
        #
        # fn = rfsym(f, ord)
        #
        # # declare function if it does not exists yet
        # isdefined(fn) || eval( Expr(:method, fn) )
        #
        # # add method
        # vp  = payload
        # fn2 = Expr(:call, fn, [ :( $vn::$typ ) for (vn, typ) in sig ]...)
        # @eval let g = vp ; $fn2 = g ; end
    end

    # function hasrule(f, ord::Int, args::Tuple)
    #     fn = rfsym(f, ord)
    #     isdefined(DerivRules, fn) || return false
    #     rf = eval(fn)
    #     meths = methods(rf, args)
    #     length(meths) > 0
    # end

    function getrule(f, ord::Int, args::Tuple)
        st = _totuple(f, args, ord)
        rt = Base.return_types(dr, st)
        if length(rt) == 0
            fn = "$f(" * join(args, ",") * ")"
            error("no deriv rule for '$fn' at pos $ord")
        elseif length(rt) > 1
            fn = "$f(" * join(args, ",") * ")"
            error("multiple deriv rules for '$fn' at pos $ord")
        end

        ruledict[ first(rt).parameters[1] ]
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
  ss  = Symbol[ e[1] for e in args ]
  ord = findfirst(dv .== ss)
  (ord == 0) && error("[deriv_rule] cannot find $dv in function arguments")

  # payload = ( Snippet(diff, vcat(ss, :ds)), repl )
  payload = ( tograph(diff, current_module()), vcat(ss, :ds) )

  DerivRules.define(func, args, ord, payload)
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
