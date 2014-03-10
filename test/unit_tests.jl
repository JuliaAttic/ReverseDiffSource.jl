#################################################################
#
#    Internal function testing
#
#################################################################

using Base.Test

include("../src/ReverseDiffSource.jl")
tmod = ReverseDiffSource


@test tmod.isSymbol(:a)            == true
@test tmod.isSymbol(:(a[1]))       == false
@test tmod.isSymbol(:(a.b))        == false
@test tmod.isSymbol(:(exp(a)))     == false

@test tmod.isRef(:a)            == false
@test tmod.isRef(:(a[1]))       == true
@test tmod.isRef(:(a[x]))       == true
@test tmod.isRef(:(a.b))        == false
@test tmod.isRef(:(a.b[end]))   == false
@test tmod.isRef(:(a[end].b))   == false
@test tmod.isRef(:(exp(a)))     == false

@test tmod.isDot(:a)           == false
@test tmod.isDot(:(a[1]))      == false
@test tmod.isDot(:(a[x]))      == false
@test tmod.isDot(:(a.b))       == true
@test tmod.isDot(:(a.b[end]))  == false
@test tmod.isDot(:(a[end].b))  == false
@test tmod.isDot(:(exp(a)))    == false

@test tmod.dprefix("coucou")            == :dcoucou
@test tmod.dprefix(:tr)                 == :dtr


## expression to graph testing

function transform(ex, outsym=nothing)
    g, sv, ext, exitnode = tmod.tograph(ex)
    if exitnode==nothing
        if outsym==nothing
            exitnode = last(collect(values(sv))) # pick at random
        else
            exitnode = sv[outsym]
        end
    end
    g.exitnodes = { :out => exitnode }

    tmod.splitnary!(g)
    tmod.evalconstants!(g)
    tmod.simplify!(g)
    tmod.prune!(g)

    tmod.resetvar()
    tmod.tocode(g)
end

@test transform(:( a = b+6 ))       == Expr(:block, :(out = b+6) )  # syntax problem here with :(---- ;)
@test transform(:(sin(y);a=3))      == Expr(:block, :(out = 3) )
@test transform(:(a += b+6))        == Expr(:block, :(out = a + (b+6)) )
@test transform(:(a -= b+6))        == Expr(:block, :(out = a - (b+6)) )
@test transform(:(a *= b+6))        == Expr(:block, :(out = a * (b+6)) )
@test transform(:(a = b'))          == Expr(:block, :(out = transpose(b)) )
@test transform(:(a = [1,2]))       == Expr(:block, :(out = vcat(1,2)) )

@test transform(:( a[2] ))                       == Expr(:block, :(out = a[2]) )
@test transform(:( y = a[2] ; y ))               == Expr(:block, :(out = a[2]) )
@test transform(:( y = a[2] ; y[1] ))            == Expr(:block, :(out = a[2][1]) )
@test transform(:( y[1] = a[2] ; y[1] ))         == :(y[1] = a[2]; out = y[1])
@test transform(:( y = a+1 ; y[2]+y[1] ))        == :(_tmp1 = a+1 ; out = _tmp1[2]+_tmp1[1])
@test transform(:( a[2] = x ; a[3] ))            == :(a[2] = x ; out = a[3])
@test transform(:( a[2] = x ; y=a[3] ; y ))      == :(a[2] = x ; out = a[3])
@test transform(:( b = a ; b[2] = x; 1 + b[2] )) == :(a[2] = x ; out = 1+a[2]) 
@test transform(:( b = a ; b[2] = x; 1 + b[1] )) == :(a[2] = x ; out = 1+a[1]) 
@test transform(:( a[1] + a[2] ))                == Expr(:block, :( out = a[1] + a[2]) )

@test transform(:( a.x ))                     == Expr(:block, :(out = a.x) )
@test transform(:( y = a.x ; y ))             == Expr(:block, :(out = a.x) )
@test transform(:( y = a.x + 1 ; y.b + y.c )) == :(_tmp1 = +(a.x,1) ; out = _tmp1.b+_tmp1.c)
@test transform(:( a.x = x ; a[3] ))          == :(a.x = x; out = a[3])
@test transform(:( a.x = x ; y = a.y ; y ))   == :(a.x = x ; out = a.y )
@test transform(:( b = a; b.x = x; 1 + b.y )) == :(a.x = x ; out = 1+a.y )
@test transform(:( a.x + a.y ))               == Expr(:block, :( out = a.x+a.y ) )

@test transform(:( a = b.f[i]))        == Expr(:block, :(out = b.f[i]) )
@test transform(:( a = b[j].f[i]))     == Expr(:block, :(out = b[j].f[i]) )








