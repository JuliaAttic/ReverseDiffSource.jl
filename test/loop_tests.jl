#################################################################
#
#    1st order derivation testing (loops)
#
#################################################################

begin # temp setup
    reload("ReverseDiffSource")
    m = ReverseDiffSource

    #####  Error thresholds  #####
    DIFF_DELTA = 1e-9
    ERROR_THRESHOLD = 2e-2

    good_enough(x,y) = isfinite(x) ? (abs(x-y) / max(ERROR_THRESHOLD, abs(x))) < ERROR_THRESHOLD : isequal(x,y) 
    good_enough(t::Tuple) = good_enough(t[1], t[2])

    #####  single gradient check  #####
    #  compares numerical gradient to automated gradient
    function compare( ex::Expr, x0::Union(Float64, Vector{Float64}, Matrix{Float64}) )
        # print("testing $ex with size(x) = $(size(x0))")
        nx = length(x0)  

        ex2 = m.rdiff( ex, x=x0 )
        dfunc(x0) = eval( :(let x = $x0 ; $ex2 ; end) )

        l0, (grad0,) = dfunc(x0)  
        if ndims(x0) == 0  # scalar
            grad1 = ( dfunc( x0 + DIFF_DELTA)[1] - l0 ) / DIFF_DELTA
        else # vector and matrices
            grad1 = zeros(size(grad0))
            for i in 1:nx  # i=1
                x1 = copy(x0)
                x1[i] += DIFF_DELTA
                grad1[i] = ( dfunc(x1)[1][1] - l0 ) / DIFF_DELTA
            end
        end

        if !all(good_enough, zip([grad0], [grad1]))
            rg0 = map(x -> round(x,5), grad0)
            rg1 = map(x -> round(x,5), grad1)
            println("\nGradient false for $ex at x=$x0, expected $rg1, got $rg0")
            # println( ex2 )
            error()
        else
            println(" ok")
        end
    end

    macro compare(ex::Expr, x0)
        if isa(x0, Union(Float64, Vector{Float64}, Matrix{Float64}))
            compare(ex, x0)
        elseif isa(x0, Union(Symbol, Expr))
            compare(ex, eval(x0))
        end
    end


    ## variables of different dimension for testing
    v0ref = 2.
    v1ref = [2., 3, 0.1, 0, -5]
    v2ref = [-1. 3 0 ; 0 5 -2]
end;

### vector setting
    ex = quote
        a = zeros(2)
        for i in 1:2
            a[i] = x
        end
        sum(a)
    end
    compare(ex, 1.)

    b = [1:4]
    ex = quote
        a=zeros(1+4)
        for i in 1:4
            t = 4+3+2
            a[i] += b[i]+t-x
        end
        sum(a)
    end
    compare(ex, 1.)

    ex = quote
        a=zeros(1+3)
        for i in 1:4
            t = 4+3+2
            a[i] += b[i]*x+t
        end
        sum(a)
    end
    compare(ex, 1.)

### vector accumulation
    ex = quote
        a = zeros(2)
        for i in 1:2
            a[i] += x
        end
        sum(a)
    end
    compare(ex, 1.)

### same var setting (vector)
    ex = quote
        a = zeros(2)
        for i in 1:2
            a[1] = x
        end
        sum(a)
    end
    compare(ex, 2.)  #  expected 1.0, got 2.0

    m.@deriv_rule %(x,y)      x     0
    m.@deriv_rule %(x,y)      y     0
    ex = quote
        a = zeros(2)
        for i in 1:4
            a[1 + i % 2] = x
        end
        sum(a)
    end
    compare(ex, 1.) # expected 2.0, got 4.0

### same var accumulator (vector)
    ex = quote
        a = zeros(2)
        for i in 1:2
            a[1] += x
        end
        sum(a)
    end
    compare(ex, 2.)

### same var setting (scalar)
    ex = quote
        a = 0.
        for i in 1:4
            a = 4*x
        end
        a
    end
    compare(ex, 3.)  # expected 4.0, got 16.0

    ex = quote
        a = 0.
        for i in 1:4
            a = x
        end
        a
    end
    compare(ex, 3.)  # expected 1.0, got 0.0

    ex = quote
        a = 0.
        for i in 1:length(x)
            a = x[i]
        end
        a
    end
    compare(ex, [3., 2.])  #  expected [0.0,1.0], got [1.0,1.0]
    compare(ex, [1.])      
    compare(ex, ones(10))  # faux

### same var accumulator (scalar)
    ex = quote
        a = 0.
        for i in 1:4
            a += 2+x
        end
        a
    end
    compare(ex, 2.) 

    ex = quote
        a = 0.
        for i in 1:3
            a += x^i
        end
        a
    end
    compare(ex, 2.) 

    ex = quote
        a = 0.
        for i in 1:length(x)
            a += x[i]
        end
        a
    end
    compare(ex, [3., 2.])  
    compare(ex, [1.])      
    compare(ex, ones(10)) 

    ex = quote
        a = 0.
        for i in 1:length(x)
            a += x[1]^i
        end
        a+1
    end
    compare(ex, [3., 2.])  
    compare(ex, [1.])      
    compare(ex, ones(10)) 

### nested loops
    ex = quote
        a=0
        for i in 1:10
            for j in 1:10
                a += (j < 4) * log(x) * sin(j)
            end
        end
        a
    end
    compare(ex, 0.1)