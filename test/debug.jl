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

module B ; end

module B

type





end
