# On Linux, this example takes 30-50 seconds but uses 4.5Gb of
# memory.  With the official python interface, it takes a little
# longer but uses less than 1/10th as much memory (360Mb)

import cyphc
var_names = ['M', 'L', 'm', 'l', 'a0', 'b0', 'c0', 'f0', 'b1', 'f2', 'f3', 'b4', 'a6']
eqns_as_strings = ['a0 - 1',
                   'M*m - 1',
                   'L*l - 1',
                   '-M^3*l*a0^2 + c0^2 - b0*f0',
                   '-M^3*l*a0^2 + f0^2 - c0*b1',
                   '-M^3*a0^2 + M*L*m*b0^2 - c0*f2',
                   '-M^3*l*a0^2 + b1^2 - f0*f3',
                   '-M^4*a0^2 + M*l*f2^2 - M*b0*b4',
                   '-M^3*l*a0^2 + M*b1*b4 + f3^2',
                   'M*l*f2^2 + M^2*a0*a6 - b4^2',
                   '-M^5*l^2*a0*a6 - M^4*l*b4^2 + M*f3^2',
                   '-L*m^2*f2*f3 - L*m*b4^2 + a6^2',
                   '-l^4 + m^3']

R = cyphc.PolyRing(var_names)
polys = [cyphc.PHCPoly(R, eqn) for eqn in eqns_as_strings]
system = cyphc.PHCSystem(R, polys)
sols = system.solution_list()
print(len(sols))
