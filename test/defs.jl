#################################################################
#
#    Common definitions for tests
#
#################################################################

using Base.Test

reload("ReverseDiffSource") # instead of 'using' to retest without exiting Julia
m = ReverseDiffSource

## removes linenumbers from expression to ease comparisons
function striplinenumbers(ex::Expr)
    args = Any[]
    for a in ex.args
        isa(a, LineNumberNode) && continue
        isa(a, Expr) && a.head==:line && continue
        push!(args, isa(a,Expr) ? striplinenumbers(a) : a )
    end
    Expr(ex.head, args...)
end


## first order derivation checking function

#  Error thresholds  #####
DIFF_DELTA = 1e-9
ERROR_THRESHOLD = 2e-2

good_enough(x,y) = isfinite(x) ? (abs(x-y) / max(ERROR_THRESHOLD, abs(x))) < ERROR_THRESHOLD : isequal(x,y) 
good_enough(t::Tuple) = good_enough(t[1], t[2])

#  Compares numerical gradient to automated gradient
function compare( ex::@compat(Union{Expr, Symbol}), 
                  x0::@compat(Union{Float64, Vector{Float64}, Matrix{Float64}}) )
    nx = length(x0)  

    if isa(ex, Expr)
        ex2 = m.rdiff( ex, x=x0 )
        dfunc(x0) = eval( :(let x = $x0 ; $ex2 ; end) )
    else
        dfunc = m.rdiff( eval(ex), (x0,) )
    end

    l0, grad0 = dfunc(x0)  
    if ndims(x0) == 0  # scalar
        grad1 = ( dfunc( x0 + DIFF_DELTA)[1] - l0 ) / DIFF_DELTA
    else # vector and matrices
        grad1 = zeros(size(grad0))
        for i in 1:nx  # i=1
            x1 = copy(x0)
            x1[i] += DIFF_DELTA
            grad1[i] = ( dfunc(x1)[1] - l0 ) / DIFF_DELTA
        end
    end

    if !all(good_enough, zip([grad0;], [grad1;]))
        rg0 = map(x -> round(x,5), grad0)
        rg1 = map(x -> round(x,5), grad1)
        error("\nGradient false at x=$x0, expected $rg1, got $rg0")
    end
end

macro compare(ex, x0)
    if isa(x0, Symbol) || isa(x0, Expr)
        x0 = eval(x0)
    end
    compare(ex, eval(x0))
end


## variables of different dimensions for testing
v0ref = 2.
v1ref = [2., 3, 0.1, 0, -5]
v2ref = [-1. 3 0 ; 0 5 -2] 
