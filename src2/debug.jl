# locations instead of symbols
# dict symbol -> loc
# loc have previous states (for mutating functions)
# deriv rules as real functions in a dedicated sub module

mp = joinpath(Pkg.dir("ReverseDiffSource"), "src2", "ReverseDiffSource.jl")
include(mp)



function foo()
       a = rand(2000,2000);
       @time 2*a
       @time map(x->2x, a)
end

foo();
foo();
