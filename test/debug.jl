
cd(joinpath(Pkg.dir("ReverseDiffSource"), "test"))


my_tests = [
			"unit_tests.jl",
            "test_syntax.jl",
            "firstorder_tests.jl",
            "more_tests.jl"
           ]

println("Running tests:")

for my_test in my_tests
    println("  * $(my_test) *")
    include(my_test)
end

println("Finished")

######################  improved show    ################################
a = rand(20)
ex = :( (a[1] - x[1])^2 + 100(x[2] - x[1]^2)^2 )
g = ReverseDiffSource.tograph(ex)
ReverseDiffSource.calc!(g, params=[:x => [1.,1.]])

###################### issue #8   ######################################

reload("ReverseDiffSource")
m = ReverseDiffSource

ex = :( (1 - x[1])^2 + 100(x[2] - x[1]^2)^2 )
res = m.rdiff(ex, x=zeros(2), order=3)
@eval foo(x) = $res
foo([0.5, 2.])

(306.5,[-351.0,350.0],
2x2 Array{Float64,2}:
 -498.0  -200.0
 -200.0   200.0,

2x2x2 Array{Float64,3}:
[:, :, 1] =
  400.0  -400.0
 -400.0     0.0

[:, :, 2] =
 0.0  0.0
 0.0  0.0)

δ = 1e-8
1/δ * (foo([0.5+δ, 2.])[1] - foo([0.5, 2.])[1])  # - 351, ok
1/δ * (foo([0.5+δ, 2.])[2] - foo([0.5, 2.])[2])  # ok
1/δ * (foo([0.5+δ, 2.])[3] - foo([0.5, 2.])[3])  # pas ok
# 2x2 Array{Float64,2}:
#  1200.0  -400.0
#  -400.0     0.0

1/δ * (foo([0.5, 2.+δ])[1] - foo([0.5, 2.])[1])  # 350, ok
1/δ * (foo([0.5, 2.+δ])[2] - foo([0.5, 2.])[2])  # ok
1/δ * (foo([0.5, 2.+δ])[3] - foo([0.5, 2.])[3])  # pas ok
# 2x2 Array{Float64,2}:
#  -400.0  0.0
#     0.0  0.0


reload("ReverseDiffSource")
m = ReverseDiffSource

ex = :( x[1]^3 )
ex = :( x[2]^3 )
ex = :( x[1]^2 + x[2]^2 )
ex = :( x[1]^3 + x[2]^3 )
ex = :( x[1]^3 + x[1]*x[2]^2 )
ex = :( sum(x * x'))

x0 = ones(2)
typeof(x0[1])

res = m.rdiff(ex, x=x0, order=3)
@eval foo(x) = $res
foo(x0)


ex = quote
	a = 0.
	for i in 1:length(x)
		a += x[i]^i
	end
	a
end

x0 = ones(3)
res = m.rdiff(ex, x=x0, order=3)
@eval foo(x) = $res
foo(x0)

