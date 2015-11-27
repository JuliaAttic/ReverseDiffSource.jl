# locations instead of symbols
# dict symbol -> loc
# (No) : loc have previous states (for mutating functions)
# deriv rules as real functions in a dedicated sub module
# Loc at parent level only


module A
  cd(Pkg.dir("ReverseDiffSource"))

  begin
    using Base.Test

    include("src2/ReverseDiffSource.jl")
    include("src2/tograph.jl")
    include("src2/tocode.jl")
    include("src2/simplify.jl")

    # testing vars
    a, b = 2, 2.1
    B = ones(2)
    type Z ; x ; y ; end
    C = Z(1,2)

    function fullcycle(ex; params...)
      psym  = Symbol[ e[1] for e in params]
      pvals = [ e[2] for e in params]
      m = current_module()
      function env(s::Symbol)
        i = findfirst(s .== psym)
        i != 0 && return (true, pvals[i], false, nothing)

        isdefined(m, s) || return (false, nothing, nothing, nothing)

        (true, eval(m,s), isconst(m,s), nothing)
      end

      resetvar()
      c2 = tograph(ex, env) |> simplify! |> tocode
      length(c2.args) == 1 ? c2.args[1] : c2
    end

    ## removes linenumbers from expression to ease comparisons
    function cleanup(ex::Expr)
        args = Any[]
        for a in ex.args
            isa(a, LineNumberNode) && continue
            isa(a, Expr) && a.head==:line && continue
            if isa(a, Expr) && a.head==:block
              args = vcat(args, a.args)
            else
              push!(args, isa(a,Expr) ? cleanup(a) : a )
            end
        end
        Expr(ex.head, args...)
    end
  end

end

module A

# check for errors
tograph( :(a = 1) )
tograph( :(a += 1) )
tograph( :(B = 1) )
tograph( :(B[2] = 1) )
tograph( :(A.B[2] = 1) )
tograph( :(C.x = 1) )
tograph( :(A.C.x = 1) )

# check for loc type propagation
show(tograph( :(x = a) ))  # x regular (copy)
show(tograph( :(x = B[2]) ))   # x regular (copy)
show(tograph( :(x = C.y) ))   # x regular (copy)
show(tograph( :(X = B) ))  # x external (ref)
show(tograph( :(X = B[1:2]) )) # x external => should be regular (getindex is a copy)
show(tograph( :(X = C) ))  # x external (ref)

# other checks
show(tograph( :( sin(24) ) ) )
show(tograph( :( e^x ) ) ) # x undefined
show(tograph( :( e^a ) ) )
show(tograph( :( Base.sin(24) ) ) )
show(g)

show(tograph( :( Main.A.sin(24) ) ) )
show(tograph( :( x = 2 ; sin(x) ) ) )
show(tograph( :( x = 2 ; y = x ; x+y ) ) )
show(tograph( :( X = ones(2) ; Y = X ; X+Y ) ) )
show(tograph( :( x = y = 2 ) ) )
show(tograph( :( X = Y = ones(2) ) ) )
show(tograph( :( X = ones(2) ; X[2] ) ) )
show(tograph( :( X = ones(2) ; X[2]=a ; X[1] ) ) )

show(tograph( :( C.x ) ) )
show(tograph( :( A.B[2] ) ) )
show(tograph( :( B[2] ) ) )
show(tograph( :( X=copy(B) ; X[2] = 3) ) )

############################ testing #####################################

fullcycle(:( x = 5 ; y = x+5 ; z = cos(y) ))
fullcycle(:( x = 5 ; y = a+5 ; z = cos(y) ))

fullcycle(:( x = 5 + 6 + 5))
fullcycle(:( x = a * b * B))

@test fullcycle(:( x = b+6 ))       == :(x = b+6)
@test fullcycle(:(sin(b); x=3))     == :(3)

@test fullcycle(:(x = b+6; x+=1))   == :(x = (b+6)+1 )
@test fullcycle(:(x = 1; x -= b+6)) == :(x = 1 - (b+6))
@test fullcycle(:(x = a; x *= b+6)) == :(x = a * (b+6))
@test fullcycle(:(x = b'))          == :(x = b')
@test fullcycle(:(x = [1,2]))       == :(x = [1,2])
@test fullcycle(:(x = 4:5 ))        == :(x = 4:5)

@test fullcycle(:(x = b+4+5))       == :(a = b+9)
@test fullcycle(:(x = b+0))         == :(x = b)
@test fullcycle(:(x = b*0))         == :(x = 0)
@test fullcycle(:(x = b*1))         == :(x = b)
@test fullcycle(:(x = b*(0.5+0.5))) == :(x = b)
@test fullcycle(:(x = b/1))         == :(x = b)

@test fullcycle(:(5))                          == :(5)
@test fullcycle(:(x = 2 ; y = 2 ; x:y))        == :(2:2)
@test fullcycle(:(x = 2 ; x))                  == :(2)
@test fullcycle(:(x = a ; y = 0 ))             == :(0)
@test fullcycle(:(x = a ; y = a ; x + y))      == :( a+a )
@test fullcycle(:(x = a ; y = x ; z = y ; y))  == :( a )

@test fullcycle(:( B[2] ))                      == :( B[2] )
@test fullcycle(:(y = B[2]; y ))                == :( B[2] )
@test fullcycle(:(y = B[2]; y[1] ))             == :( B[2][1] )
@test fullcycle(:(y = zeros(B); y[1] = B[2]; y[1] )) == cleanup(:(y = zeros(B); y[1] = B[2]; y[1]))
@test fullcycle(:(y = B+1 ; y[2]+y[1] ))        == :(y = B+1 ; y[2]+y[1])
@test fullcycle(:(Y=zeros(B); Y[2]=a ; a ))     == :(a)
@test fullcycle(:(Y=copy(B); Y[2]=a ; Y[1] ))   == cleanup(:(Y=copy(B); Y[2]=a ; Y[1]))

@test fullcycle(:( a[2] = x ; y=a[3] ; y ))       == :(a[2] = x ; a[3])
@test fullcycle(:( b = a ; b[2] = x; 1 + b[2] ))  == :(a[2] = x ; 1+a[2])
@test fullcycle(:( x = a ; Y = copy(B) ; Y[2] = x; 1 + Y[1] ))  == :(a[2] = x ; 1+a[1])
@test fullcycle(:( B[1] + B[2] ))                 == :(B[1] + B[2])
@test fullcycle(:( B[1:2] ))                      == :( B[1:2] )
@test fullcycle(:( X = copy(B) ; X[1:2] = X[1:2] )) == Expr(:block, :( a = x ) )
@test fullcycle(:( a = x ; a[1:2,3] = a[1:2,3] )) == Expr(:block, :( a = x ) )
@test fullcycle(:( a = x ; a[1:2,3] = a[1:2,4] ), x=ones(4,4)) == :( _tmp1 = 1:2 ; x[_tmp1,3] = x[_tmp1,4])

@test fullcycle(:( B[:] ))               == Expr(:block, :( x[1:length(x)] ) )
@test fullcycle(:( B[a+b, c:d] ))        == Expr(:block, :( x[a + b,c:d] ) )
@test fullcycle(:( B[1:4] ))             == Expr(:block, :( x[1:4] ) )
@test fullcycle(:( B[1:end] ), x=ones(5))           == Expr(:block, :( x[1:length(x)]) )
@test fullcycle(:( a[1:end, :, 10:15] )) == Expr(:block, :( a[1:size(a,1),1:size(a,2),10:15]) )

@test fullcycle(:( C.x ))                       == :( C.x )
@test fullcycle(:( y=C.x ))                     == :(y = C.x )
@test fullcycle(:( y=C.x + 1 ; C.y + C.x ))     == :(C.y + C.x)
@test fullcycle(:( X=Z(0,0); X.x=a ))           == :( a )
@test fullcycle(:( X=Z(0,0); X.x=a; y=X.y; y )) == cleanup(:(X=A.Z(0,0);X.x=a; y=X.y))
@test fullcycle(:( X=Z(1,1); X.x = a; 1+X.y ))  == cleanup(:(X=A.Z(1,1);X.x=a; 1+X.y))
@test fullcycle(:( C.x + C.y ))                 == :( C.x + C.y )

@test fullcycle(:( a = b.f[i]))        == Expr(:block, :(a = b.f[i]) )
@test fullcycle(:( a = b[j].f[i]))     == Expr(:block, :(a = b[j].f[i]) )

end # of module A
