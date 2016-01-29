using Base.Test

# testing vars
a, b = 2, 2.1
B = ones(2)
D = rand(2,3,4)
type Z ; x ; y ; end
C = Z(1,2)

function fullcycle(ex; env_var=Dict(), keep_var=[EXIT_SYM;])
  resetvar()
  g  = tograph(ex) |> simplify!
  c2 = tocode(g, keep_var)
  length(c2.args) == 1 ? c2.args[1] : c2
end

## removes linenumbers from expression to ease comparisons
function cleanup(ex::Expr) #  ex = a
    args = Any[]
    for a in ex.args  # a = ex.args[2]
        isa(a, LineNumberNode) && continue
        isa(a, Expr) && a.head==:line && continue
        push!(args, isa(a,Expr) ? cleanup(a) : a )
    end
    # remove useless :block
    if length(args) > 1 && ex.head == :block
      args2 = Any[]
      for a in args
        if !isa(a, Expr) || a.head != :block
          push!(args2,a)
        else
          append!(args2, a.args)
        end
      end
      args = args2
    end
    Expr(ex.head, args...)
end
