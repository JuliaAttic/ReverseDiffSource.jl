#################################################################
#
#    1st order derivation testing for functions
#
#################################################################

tf(x) = sum(x)
@compare tf v0ref
@compare tf v1ref
@compare tf v2ref

rosenbrock(x) = (1 - x[1])^2 + 100(x[2] - x[1]^2)^2   # function to be derived
@compare rosenbrock [1., 1.]
@compare rosenbrock [0., 3.]
@compare rosenbrock [10., -10.]

function tf(x)
    a=zeros(1+3)
    for i in 1:4
        t = 4+3+2
        a[i] += b[i]*x+t
    end
    sum(a)
end
@compare tf v0ref
@compare tf 10.

function tf(x)
    a=0
    for i in 1:10
        for j in 1:10
            a += log(x) * sin(j)
        end
    end
    a
end
@compare tf 10.

function tf(x)
    a = zeros(2)
    c = 0
    for i in 1:2
        a[i] = x
        a[i] += b[i+1]
        c = c * sin(2i)
    end
    sum(a)
end
@compare tf v0ref

function tf(x)
    a = zeros(2)
    c = 0
    for i in 1:2
        d = 3
        for j in 1:2
            a[i] = x
            a[i] += b[i+1]
            d = d * sin(2i)
        end
        c += d
    end
    z = c * sum(a)
    z - sum(a)
end
@compare tf v0ref

function tf(x)
    z = 0
    for i in 1:length(x)
        z = i * x[i]
    end
    z
end
@compare tf v1ref
@compare tf v2ref
