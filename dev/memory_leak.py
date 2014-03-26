import phc

equations = ['1*X0^2*Y1^2 - 1*X1^1*Y0^1', '1*X0^46*Y1^38 - 1*Y0^4', 'X0 + Y0 - 1', 'X1 + Y1 - 1']

def run_once():
    R = phc.PolyRing(['X0', 'X1', 'Y0', 'Y1'])
    polys = [phc.PHCPoly(R, eqn) for eqn in equations]
    system = phc.PHCSystem(R, polys)
    system.solution_list()

for i in xrange(10**10):
    print i
    run_once()
