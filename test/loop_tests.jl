#################################################################
#
#    1st order derivation testing (loops)
#
#################################################################

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
    compare(ex, 2.) 

    m.@deriv_rule %(x,y)      x     0
    m.@deriv_rule %(x,y)      y     0
    ex = quote
        a = zeros(2)
        for i in 1:4
            a[1 + i % 2] = x
        end
        sum(a)
    end
    compare(ex, 1.)

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
    compare(ex, 0.1) # ERROR: syntax: invalid assignment location "1"