using Base.Test

reload("ReverseDiffSource") # instead of 'using' to retest without exiting Julia
m = ReverseDiffSource

my_tests = [
			"unit_tests.jl",
            "parsing.jl",
            "syntax.jl",
            "firstorder.jl",
            "indexing.jl",
            "types.jl",
            "loops.jl",
            "more.jl"
           ]

println("Running tests:")

for my_test in my_tests
    println("  * $(my_test) *")
    include(my_test)
end

println("Finished")
