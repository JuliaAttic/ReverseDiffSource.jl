################ issue related to #24 #############
# pb 1 : ternary * not split ?
# pb 2 : no rule for Ac_mul_B (when does it appear ?)
# enhancement : look for externals in enclosing def, not in Main
# enhancement : simplify transpose(transpose(x))


whos()
Pkg.dir("ReverseDiffSource")
using ReverseDiffSource

module A
    reload("ReverseDiffSource")
    length(ReverseDiffSource.drules)
    using ReverseDiffSource
end

module A
using ReverseDiffSource
ReverseDiffSource.__init__()
import ReverseDiffSource: tocode, tograph

ex = :( tmp= x' * ones(2,2) ; tmp[1] )
g = tograph(ex)

f = g.nodes[11].main
typeof(f)
f == ones

g = g.nodes[13].main
rdiff(ex, x = zeros(2))

collect(keys(ReverseDiffSource.drules))
length(ReverseDiffSource.drules)

end

A.g
A.g == *

############## issue 25  #############################

module B ; end

module B
reload("ReverseDiffSource")
using ReverseDiffSource
function foo(x,y,z)
    return x + y + z
end
rdiff(foo,(1,1,1))
end

using ReverseDiffSource
function foo(x,y,z)
    return x + y + z
end
rdiff(foo,(1,1,1))

ReverseDiffSource.__init__()
rdiff(foo,(1,1,1))

rdiff(foo,(1,1,1), order=0, debug=true)

y,z = 1,2
rdiff(:( x + y + z), x=2)
rdiff(:( (Main.+)(x,y,z) ), x=2)

(Main.+)(1,2,3)
+

g = ReverseDiffSource.tograph(:(x+y+z))
show(g)
ReverseDiffSource.splitnary!(g)
show(g)

################ issue 26 ###############################

x = randn(3)
g = randn(3)
test = quote
    d  = sum((x-g).^2)
    d + log(d)
end
rdiff(test,x=x)

eval(test)
