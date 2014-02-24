#################################################################
#
#    Internal function testing
#
#################################################################

using Base.Test

include("../src/ReverseDiffSource.jl")
testedmod = ReverseDiffSource


@test testedmod.isSymbol(:a)            == true
@test testedmod.isSymbol(:(a[1]))       == false
@test testedmod.isSymbol(:(a.b))        == false
@test testedmod.isSymbol(:(exp(a)))     == false

@test testedmod.isRef(:a)            == false
@test testedmod.isRef(:(a[1]))       == true
@test testedmod.isRef(:(a[x]))       == true
@test testedmod.isRef(:(a.b))        == false
@test testedmod.isRef(:(a.b[end]))   == false
@test testedmod.isRef(:(a[end].b))   == false
@test testedmod.isRef(:(exp(a)))     == false

@test testedmod.isDot(:a)           == false
@test testedmod.isDot(:(a[1]))      == false
@test testedmod.isDot(:(a[x]))      == false
@test testedmod.isDot(:(a.b))       == true
@test testedmod.isDot(:(a.b[end]))  == false
@test testedmod.isDot(:(a[end].b))  == false
@test testedmod.isDot(:(exp(a)))    == false

@test testedmod.dprefix("coucou")            == :dcoucou
@test testedmod.dprefix(:tr)                 == :dtr


## expression to graph testing

function transform(ex, outsym=nothing)
    g, sv, ext, exitnode = testedmod.tograph(ex)
    if exitnode==nothing
        if outsym==nothing
            exitnode = last(collect(values(sv))) # pick at random
        else
            exitnode = sv[outsym]
        end
    end
    g.exitnodes = { :out => exitnode }

    testedmod.splitnary!(g)
    testedmod.dedup!(g)
    testedmod.evalconstants!(g)
    testedmod.simplify!(g)
    testedmod.prune!(g)

    testedmod.resetvar()
    testedmod.tocode(g)
end


@test transform(:( a = b+6 ))       == :(out = b+6;)
@test transform(:(sin(y);a=3))      == :(out = 3;)
@test transform(:(a += b+6))        == :(out = a + (b+6);)
@test transform(:(a -= b+6))        == :(out = a - (b+6);)
@test transform(:(a *= b+6))        == :(out = a * (b+6);)
@test transform(:(a = b'))          == :(out = transpose(b);)
@test transform(:(a = [1,2]))       == :(out = vcat(1,2);)

@test transform(:( a[2] ))                       == :(out = a[2];)
@test transform(:( y = a[2] ; y ))               == :(out = a[2];)
@test transform(:( y = a[2] ; y[1] ))            == :(out = a[2][1];)
@test transform(:( y[1] = a[2] ; y[1] ))         == :(y[1] = a[2]; out = y[1])
@test transform(:( y = a+1 ; y[2]+y[1] ))        == :(_tmp1 = a+1 ; out = _tmp1[2]+_tmp1[1])
@test transform(:( a[2] = x ; a[3] ))            == :(a[2] = x ; out = a[3])
@test transform(:( a[2] = x ; y=a[3] ; y ))      == :(a[2] = x ; out = a[3])
@test transform(:( b = a ; b[2] = x; 1 + b[2] )) == :(a[2] = x ; out = 1+a[2]) 
@test transform(:( b = a ; b[2] = x; 1 + b[1] )) == :(a[2] = x ; out = 1+a[1]) 
@test transform(:( a[1] + a[2] ))                == :( out = a[1] + a[2];)

@test transform(:( a.x ))                     == :(out = a.x;)
@test transform(:( y = a.x ; y ))             == :(out = a.x;)
@test transform(:( y = a.x + 1 ; y.b + y.c )) == :(_tmp1 = +(a.x,1) ; out = _tmp1.b+_tmp1.c)
@test transform(:( a.x = x ; a[3] ))          == :(a.x = x; out = a[3])
@test transform(:( a.x = x ; y = a.y ; y ))   == :(a.x = x ; out = a.y )
@test transform(:( b = a; b.x = x; 1 + b.y )) == :(a.x = x ; out = 1+a.y )
@test transform(:( a.x + a.y ))               == :( out = a.x+a.y ;)

@test transform(:( a = b.f[i]))        == :(out = b.f[i];)
@test transform(:( a = b[j].f[i]))     == :(out = b[j].f[i];)




