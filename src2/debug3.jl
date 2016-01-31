################################################################################
#
#   Graph differentiation
#
################################################################################

ex = :(2*a*a)
g = tograph(ex)
simplify!(g)
gdiff!(g, g.block.symbols[EXIT_SYM], g.block.symbols[:a])
simplify!(g)
tocode(g)

ex = :(a+a)
g = tograph(ex)
simplify!(g)
gdiff!(g, g.block.symbols[EXIT_SYM], g.block.symbols[:a])
simplify!(g)
tocode(g)

ex = :(sin(a))
g = tograph(ex)
simplify!(g)
gdiff!(g, g.block.symbols[EXIT_SYM], g.block.symbols[:a])
simplify!(g)
tocode(g)
show(g)





ex = quote
    X = ones(3,3) .* a
    3 ^ X[2,2]
end

g = tograph(ex)
simplify!(g)
gdiff!(g, g.block.symbols[EXIT_SYM], g.block.symbols[:a])
simplify!(g)
dex = tocode(g)

@eval let a = 1.0; $dex ; end
@eval let a = 1.00001; $dex ; end

ex = quote
    X = ones(3,3)
    X[2,2] = a
    sum(X)
end

g = tograph(ex)
simplify!(g)
gdiff!(g, g.block.symbols[EXIT_SYM], g.block.symbols[:a])
simplify!(g)
dex = tocode(g)
show(g)
prune!(g, Set{Loc}([g.block.symbols[EXIT_SYM];]) )

g.block.ops[8]
l0 = g.block.ops[8].asc[2]
l0 in g.locs

keep=[EXIT_SYM;]) # g = A.g ; keep = [A.EXIT_SYM;]

prune!(g, [EXIT_SYM;])
show(g)

	splitnary!(g)

	fusecopies!(g)
	removerightneutral!(g)
	removeleftneutral!(g)
	prune!(g, keep)
	g

###################  for loops  #####################

ex = quote
    X = ones(3,3)
    for i in 1:length(X)
      X[i] += i * a
    end
    sum(X)
end

g = tograph(ex)
simplify!(g)
gdiff!(g, g.block.symbols[EXIT_SYM], g.block.symbols[:a])
simplify!(g)
dex = tocode(g)
show(g)
show(dex)
@eval let a = 1.0; $dex ; end
@eval let a = 1.00001; $dex ; end



ex = quote
    x = 0.
    for i in 1:10
      x += a^i
    end
    x
end

g = tograph(ex)
simplify!(g)
gdiff!(g, g.block.symbols[EXIT_SYM], g.block.symbols[:a])
simplify!(g)
dex = tocode(g)
show(g)

####################  if  #####################

ex = quote
  x = b
  if a > 2
    y = sin(b)
    x = a*y
  else
    z = cos(a)
    x = a^z
  end
  x
end
    g = tograph(ex)
    dex = tocode(g)


ex = quote
  x = 0.
  if a > 2
    x = a
  else
    x = a*a
  end
  x
end
    g = tograph(ex)
    dex = tocode(g)

ex = quote
  if a > 2
    x = a
  else
    y = b
  end
  y
end

g = tograph(ex)



####################  types  #####################

module Sandbox
    type Abcd
        a::Float64
        b::Vector{Float64}
    end
    foo(t::Abcd) = t.a + t.b[2]
end

@deriv_rule Sandbox.Abcd(a,b) a ds[1]
@deriv_rule Sandbox.Abcd(a,b) b ds[2]
@deriv_rule Sandbox.foo(t)    t Any[ ds, (a=zeros(length(t.b)) ; a[2]=ds ; a) ]

@deriv_rule getfield(A::Sandbox.Abcd, fn) A fn==:a ? Any[ds, 0.] : Any[0., ds]



foo(t)    t Any[ ds, (a=zeros(length(t.b)) ; a[2]=ds ; a) ]

E = Sandbox.Abcd(1., [2., 3.])

@compare t.a  * x        v0ref
@compare sum(t.b .* x)   v0ref
@compare sum(t.b .+ [x,x])   v0ref

ex = :(E.a  * a)
g = tograph(ex)
simplify!(g)
gdiff!(g, g.block.symbols[EXIT_SYM], g.block.symbols[:a])
simplify!(g)
tocode(g)
show(g)





@eval let a = 1.0; $dex ; end
@eval let a = 1.00001; $dex ; end


DerivRules.getrule(setindex!, 2, (zeros(3), 1., 1))
DerivRules.getrule(setindex!, 1, (zeros(3), 1., 1))
DerivRules.getrule(setindex!, 1, (zeros(3), 1., 1,1))
DerivRules.getrule(setindex!, 2, (zeros(3), 1., 1,1))
ismutating(setindex!)
ismutating(getindex)
