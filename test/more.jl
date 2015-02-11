##################################################
#  More tests to improve coverage 
##################################################

# using Base.Test
# reload("ReverseDiffSource")
# m = ReverseDiffSource

#########  rdiff ###########

m.@deriv_rule .+(x::Real   , y::AbstractArray)    x     (a=0.; for x in ds ; a+=x ; end; a)

ex = m.rdiff( :( sum( x .+ [1., 2.]) ), x=1.)
ex = m.rdiff( :( sum( x .+ [1., 2.]) ), x=1., allorders=false)

######### plot  ############

ex = quote 
	a = 0.
	for i in 1:10
		a += i
	end
	a
end

m.plot( m.tograph(ex) )

sio = IOBuffer()  # to avoid printing
show(sio, m.tograph(ex) )
show(sio, m.tograph(ex).nodes[4] )

close(sio)
