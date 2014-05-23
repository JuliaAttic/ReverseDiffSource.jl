using Distributions
import Distributions.Poisson

nbt = 16
tn = [ symbol(x) for x in 'A':'A'+nbt-1]
teams = Dict( tn, 3 * rand(16) )

function meet(t1, t2)
	(rand(Poisson(teams[t1])),
		rand(Poisson(teams[t2])))
end

function meet2(t1, t2)
	s1, s2 = meet(t1, t2)
	while s1 == s2 ; s1, s2 = meet(t1, t2) ; end
	s1 > s2 ? t1 :t2
end

function scen2(poolt)
	nbp = length(poolt)
	sc = zeros(nbp, 2)
	for (i1, i2) in combinations(1:nbp,2)
		t1, t2 = poolt[i1], poolt[i2]
		g1, g2 = meet(t1, t2)

		sc[i1,1] += (g1 > g2)*3 + g1==g2
		sc[i2,1] += (g2 > g1)*3 + g1==g2
		sc[i1,2] += g1
		sc[i2,2] += g2
	end
	sc2 = [ (sc[i,1], sc[i,2]) for i in 1:nbp]

	ordre = sortperm(sc2, rev=true)
	poolt[ordre[1]], poolt[ordre[2]]
end

scen2([:A, :B, :C, :D])

function scen()
	w11, w12 = scen2(tn[1:4])
	w21, w22 = scen2(tn[5:8])
	w31, w32 = scen2(tn[9:12])
	w41, w42 = scen2(tn[13:16])

	q1 = meet2(w11, w22)
	q2 = meet2(w12, w21)
	q3 = meet2(w31, w42)
	q4 = meet2(w32, w41)

	d1 = meet2(q1, q2)
	d2 = meet2(q3, q4)

	f1 = meet2(d1, d2)
	pf1 = meet2(d1==q1 ? q2 : q1 , d2==q3 ? q4 : q3)

	f1, f1==d1 ? d2 : d1, pf1
end

nbr = 100000
res = Array(Symbol, (nbr, 3))
@time 	for r in 1:nbr
			res[r,:] = [scen()...]
		end

sc = [  sum(res[:,1] .== x)/ nbr for x in tn ] 
sum(sc) # 1.0 ok

[ tn [ teams[x] for x in tn ] sc]
