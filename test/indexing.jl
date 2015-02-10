#################################################################
#
#    1st order derivation testing (getindex-setindex)
#
#################################################################

##  ref  testing
@compare x[2]                v1ref
@compare sum(x[2:3])         v1ref
@compare sum(x[2:4])         v2ref

@compare sum(x[:,2])         v2ref 
@compare sum(x[1,:])         v2ref
@compare sum(x[2:end,:])     v2ref
@compare sum(x[:,2:end])     v2ref
@compare sum(x[1:end-1,:])   v2ref
@compare sum(x[v0ref:end,:]) v2ref


@compare getindex(x,2)          v1ref
@compare sum(getindex(x, 2:3))  v1ref
@compare sum(getindex(x, 2:4))  v2ref
@compare sum(getindex(x, :,2))  v2ref 
@compare sum(getindex(x, 1,:))  v2ref


@compare x[2]+x[1]           v2ref
@compare log(x[2]^2+x[1]^2)  v2ref

@compare (a = zeros(5) ; a[1]   = x[3]   ; a[3]) v1ref
@compare (a = zeros(5) ; a[1]   = x[3]   ; a[1]) v1ref
@compare (a = zeros(5) ; a[1:2] = x[3]   ; a[1]) v1ref 
@compare (a = zeros(5) ; a[1:2] = x[3:4] ; a[1]) v1ref
@compare (a = zeros(5) ; a[1:2] = x[3:4] ; a[4]) v1ref

@compare (a = zeros(5) ; setindex!(a, x[3]  , 1)   ; a[3]) v1ref
@compare (a = zeros(5) ; setindex!(a, x[3]  , 1)   ; a[1]) v1ref
@compare (a = zeros(5) ; setindex!(a, x[3]  , 1:2) ; a[1]) v1ref 
@compare (a = zeros(5) ; setindex!(a, x[3:4], 1:2) ; a[1]) v1ref
@compare (a = zeros(5) ; setindex!(a, x[3:4], 1:2) ; a[4]) v1ref


_idx2 = 1

ex = quote
    a = zeros(2,2)
    b = fill(0.0, size(x))
    b[2] = b[2] + x[2]
    b[1] = b[1] + x[1]
    a[1:2] = b
    a[_idx2]
end
compare(ex, ones(2))


ex = quote
    a = zeros(2,2)
    a[1:2] = x
    a[_idx2]
end
compare(ex, ones(2))
compare(ex, 1.)

ex = quote
    a = zeros(2,2)
    a[1:2] = x[1]
    a[_idx2]
end
compare(ex, ones(2))

ex = quote
    a = ones(5)
    b = sum(a)*x
    a[2] += x
    c = sum(a)
    b + c
end
compare(ex, 2.)

ex = quote
    a=zeros(3)
    b=zeros(3)
    b[2]=x
    a[1]=x
    sum(a)+sum(b)
end
compare(ex, 2.)

ex = quote
    a=zeros(5)
    a[2:3] = x
    a[3:4] = x
    sum(a)
end
compare(ex, -1.)


ex = quote
    a = zeros(2,2)
    b = zeros(2)

    b[2] = b[2] + x[2]
    b[1] = b[1] + x[1]
    a[1:2] = b

    b[2] = b[2] + x[2]
    b[1] = b[1] + x[1]
    a[3:4] = b

    a[_idx2]
end
_idx2 = 1
compare(ex, [1., 1.])
compare(ex, [0., 1.])
_idx2 = 3
compare(ex, [1., 1.])
compare(ex, [0., 1.])

ex = quote
    a = zeros(2,2)
    b = zeros(2)

    b[1] = b[1] + x[1]
    b[2] = b[2] + x[2]
    a[1] = b[1]
    a[2] = b[2]

    b[2] = b[2] + x[2]
    b[1] = b[1] + x[1]
    a[3] = b[1]
    a[4] = b[2]

    a[_idx2]
end
_idx2 = 1
compare(ex, [1., 1.])   #   pb : super slow
compare(ex, [0., 1.])
_idx2 = 3
compare(ex, [1., 1.])
compare(ex, [0., 1.])

ex = quote
    a = zeros(4)
    b = zeros(2)

    b += x
    a[1:2] = b
    b[1] = b[1] + x[1]
    a[3:4] = b

    a[_idx2]
end
compare(ex, [1., 1.])
compare(ex, [1., 0.])

ex = quote
    a = zeros(2,2)
    a[1] = x[1]
    a[3] = x[1]
    a[4] = x[1]
    a[1] = x[1]  
    a[_idx2]
end
compare(ex, [1., 1.])
compare(ex, ones(10))

ex = quote
    a = zeros(5,5)
    a[1:4,2] = x
    a[3,1:5] = x
    sum(a)
end
compare(ex, 1.)
