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
function cleanup(ex::Expr)
    args = Any[]
    for a in ex.args
        isa(a, LineNumberNode) && continue
        isa(a, Expr) && a.head==:line && continue
        if isa(a, Expr) && a.head==:block
          args = vcat(args, a.args)
        else
          push!(args, isa(a,Expr) ? cleanup(a) : a )
        end
    end
    Expr(ex.head, args...)
end
