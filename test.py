from phc import *
R = PolyRing(['x','y'])
p1 = PHCPoly(R, '1 + 2*x + 3*x*y + 4*y')
p2 = PHCPoly(R, '5 + 6*x^2*y + 7*x*y^2')
S = PHCSystem(R, [p1, p2])
print S
sys, sol = S.MVstart()
print "start system"
print sys
print "start solution"
print sol
print "solution list"
print S.solution_list()
