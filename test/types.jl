#################################################################
#
#    1st order derivation testing (types)
#
#################################################################

module Sandbox
    type Abcd
        a::Float64
        b::Vector{Float64}
    end
    foo(t::Abcd) = t.a + t.b[2]
end

m.@deriv_rule Sandbox.Abcd(a,b) a ds[1]
m.@deriv_rule Sandbox.Abcd(a,b) b ds[2]
m.@deriv_rule Sandbox.foo(t)    t Any[ ds, (a=zeros(length(t.b)) ; a[2]=ds ; a) ]


t = Sandbox.Abcd(1., [2., 3.])

@compare t.a  * x        v0ref
@compare sum(t.b .* x)   v0ref
@compare sum(t.b .+ [x,x])   v0ref

t = Sandbox.Abcd(1., ones(v1ref))

ex = quote
    z = Sandbox.Abcd(1., x)
    Sandbox.foo(z)
end
compare(ex, v1ref)


ex = quote
    z = Sandbox.Abcd(0., [x*x])
    z.a
end
compare(ex, v0ref)

ex = quote
    z = Sandbox.Abcd(0., [x*x])
    sum(z.b)
end
compare(ex, v0ref)

ex = quote
    z = Sandbox.Abcd(0., [0.])
    z.a = x*x
    z.a
end
compare(ex, v0ref)

ex = quote
    z = Sandbox.Abcd(1., [x, x])
    z.a = 2x
    z.b[2]
end
compare(ex, v0ref)


ex = quote
    z = Sandbox.Abcd(1., [x, x])
    z.a = 2x
    y = z.b
    y[2]
end
compare(ex, v0ref)


ex = quote
    z = Sandbox.Abcd(1., [x, x])
    z.a = x
    Sandbox.foo(z)
end
compare(ex, v0ref)


ex = quote
    z = Sandbox.Abcd(1., x)
    z.a = x[3]
    Sandbox.foo(z)
end
compare(ex, v1ref)
