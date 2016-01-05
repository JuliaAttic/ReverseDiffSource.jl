# locations instead of symbols
# dict symbol -> loc
# loc have previous states (for mutating functions)
# deriv rules as real functions in a dedicated sub module

mp = joinpath(Pkg.dir("ReverseDiffSource"), "src2", "ReverseDiffSource.jl")
include(mp)


module A
  # cd(Pkg.dir("ReverseDiffSource"))
  using ReverseDiffSource
end


##############################################

module A
  reload("ReverseDiffSource")
end

module A
using ReverseDiffSource

res = rdiff(:(x[1]^3+x[2]^2) , x=[2., 3.], order=1)
@eval foo(x) = $res
foo([1.5, 2.8])
# yields (11.215,[6.75,5.6]), whereas the following


res = rdiff(:(x[1]^3+x[2]^2) , x=[2., 3.], order=1, allorders=false)
@eval foo(x) = $res
foo([1.5, 2.8])

g1 = rdiff(:(x[1]^3+x[2]^2) , x=[2., 3.], order=1, debug=true)
g2 = rdiff(:(x[1]^3+x[2]^2) , x=[2., 3.], order=1, debug=true, allorders=false)
println(g2)

abcd = zeros(3)
abcd[1] = 2.

ex = quote
  a = 0.
  for i in 1:10
    a = a + x + 456
  end
  a
end

rdiff(ex, x=3., order=0)

function foo(x,y,z)
    return x + y + z
end
rdiff(foo,(1,1,1), debug=true, order=0)

end


Pkg.test("ReverseDiffSource")

pwd()
cd(Pkg.dir("ReverseDiffSource"))
include("test/runtests.jl")


module A; end

module A
