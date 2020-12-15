# cython: language_level=3, c_string_type=unicode, c_string_encoding=utf8

cdef extern from "stdlib.h":
    ctypedef unsigned long size_t
    void* malloc(size_t size)
    void free(void* mem)

cdef extern from "cell_stack.h":
    ctypedef struct cell:
        int* idx
        cell* next
    ctypedef struct CellStack:
        int size
        int count
        cell *top
        cell *cur
        
cdef extern void adainit()
cdef extern void adafinal()

cdef extern void* reset_symbols(int n)
cdef extern void* add_symbol(char* s)
cdef extern void* new_poly(int n, char* s, int* code)
cdef extern void free_poly(void* p)
cdef extern int is_null_poly(void* p)
cdef extern char* poly_to_string(void* p)
cdef extern void free_ada_string(char* s)
cdef extern int num_unknowns(void*)
cdef extern int num_terms(void*)
cdef extern void poly_coeff(void* poly, int* degrees, double* coeff)
cdef extern void get_terms(void* poly, int* degrees, double* real, double* imag)
cdef extern void call_poly(void* poly, double* reX, double* imX, double* Y)
cdef extern void* specialize_poly(void* p, double* real, double* imag, int v)
cdef extern void* mixed_volume_algorithm (int n, int m,
                                         int* indices,
                                         int* sizes,
                                         int* supports)
cdef extern void* new_solved_system(int n)
cdef extern void* get_poly(void* solved, int n)
cdef extern void  set_poly(void* solved, int n, void* p)
cdef extern int   get_num_solns (void* solved)
cdef extern void  get_solution(void *solved, int n,
                               int* mult, double* info,
                               double* real, double*imag)
cdef extern int   add_solutions(void *sys1, void* sys2, double* tolerance)
cdef extern void  do_homotopy(void* start, void* target, int allow_clustering)
cdef extern void  filter_solns(void* solved, double* tolerance)
cdef extern void  polish_solns(void* solved)

class PHCInternalAdaException(Exception):
    pass

cdef class PHCContext:

    def __cinit__(self):
        adainit()

    def __dealloc__(self):
        adafinal()

class PolyRing(tuple):
    """
    A polynomial ring over the complex numbers.  Instantiate with a
    sequence of variable names.
    
    >>> R = PolyRing(['x','y','z'])
    >>> R
    C[x, y, z]
    >>> R.without('y')
    C[x, z]
    """

    def __repr__(self):
        return 'C[%s]'%', '.join(self)

    def use(self):
        reset_symbols(len(self))
        for v in self:
            add_symbol(v)

    def without(self, var):
        new_variables = list(self)
        new_variables.remove(var)
        return PolyRing(new_variables)
    
cdef class PHCPoly:
    """
    A python constructor/inspector for a PHC standard complex
    polynomial.  Instantiate with a PolyRing and a string accepted by
    the PHC parser, involving only variables from the ring.

    >>> R = PolyRing(['x','y','z'])
    >>> p = PHCPoly(R, '(x-y)*(x+y)')
    >>> q = PHCPoly(R, '(x-z)*(x+y)')
    >>> S = PolyRing(['a','b'])
    >>> r = PHCPoly(S, '(a^2 -b)^2')
    >>> p
    x^2 - y^2;
    >>> r
    a^4 - 2*a^2*b + b^2;
    >>> q
    x^2 + x*y - x*z - y*z;
    """
    cdef void* poly
    cdef int code
    cdef initstring, ring
    errors = ['Illegal character.',
              'Illegal operator.',
              'Unknown variable.',
              'Unbalanced brackets',
              'Invalid symbol.',
              'Unknown parse error']
    
    def __init__(self, ring, initstring):
        self.ring = ring
        self.initstring = initstring+';'
        ring.use()
        self.poly = new_poly(len(ring), self.initstring, &self.code)
        if self.code:
            raise ValueError(self.errors[self.code - 1])

    def __dealloc__(self):
        free_poly(self.poly)
        
    def __repr__(self):
        cdef char* s
        self.ring.use()
        s = poly_to_string(self.poly)
        result = str(s)
        free_ada_string(s)
        return result

    def __call__(self, Z):
        cdef double* reX
        cdef double* imX
        cdef double Y[2]
        cdef int n, dim = self.num_unknowns() 
        if len(Z) != dim:
            raise ValueError('Wrong number of values.')
        reX = <double *>malloc(dim*sizeof(double))
        imX = <double *>malloc(dim*sizeof(double))
        for n in range(dim):
            W = complex(Z[n])
            reX[n], imX[n] = W.real, W.imag
        call_poly(self.poly, reX, imX, Y)
        result = complex(Y[0] + Y[1]*1j)
        free(reX)
        free(imX)
        return result
        
    cdef replace_poly(self, void *p):
        self.poly = p

    cdef void* get_pointer(self):
        return self.poly

    def num_unknowns(self):
        """
        Return the number of unknowns in this PHC Poly.
        """
        return num_unknowns(self.poly)
      
    def num_terms(self):
        """
        Return the number of monomials with non-zero coefficient.
        """
        return num_terms(self.poly)

    def get_ring(self):
        return self.ring
    
    def coeff(self, degrees):
        cdef int* degree_array
        cdef double coeff[2]
        cdef int n, dim = len(degrees)
        if dim != self.num_unknowns():
            raise ValueError('Degree tuple has wrong size')
        degree_array = <int *>malloc(dim*sizeof(int))
        
        for n in range(dim):
            degree_array[n] = degrees[n]
        poly_coeff(self.poly, degree_array, coeff)
        free(degree_array)
        return complex(coeff[0] + coeff[1]*1j)

    def terms(self):
        cdef int* degrees
        cdef double* real_parts
        cdef double* imag_parts
        cdef int n, m, p
        cdef int dim=self.num_unknowns(), terms=self.num_terms()
        degrees = <int *>malloc(dim*terms*sizeof(int))
        real_parts = <double *>malloc(terms*sizeof(double))
        imag_parts = <double *>malloc(terms*sizeof(double))
        get_terms(self.poly, degrees, real_parts, imag_parts)
        result = {}
        p=0
        for n in range(terms):
            degs = []
            for m in range(dim):
                degs.append(degrees[p])
                p += 1
            coeff = real_parts[n] + imag_parts[n]*1j
            result[tuple(degs)] = coeff
        return result

    def specialize(self, var, value):
        cdef void* new_poly
        cdef int n = 1 + self.ring.index(var)
        cdef double x_real = value.real, x_imag = value.imag
        
        new_poly = specialize_poly(self.poly, &x_real, &x_imag, n)
        new_ring = self.ring.without(var)
        result = PHCPoly(new_ring, '')
        result.replace_poly(new_poly)
        return result
                   
cdef class PHCSystem:
    """
    A system of polynomials equations, all in the same ring and having
    rhs=0.  Instantiate with a ring and a sequence of PHCPoly's.
    """
    cdef ring, polys, start_system, start_solutions, solutions
    cdef void* solved_starter
    cdef void* solved_target
    
    def __init__(self, ring, polys):
        self.ring = ring
        self.polys = tuple(polys)
        for p in polys:
            if ring != p.get_ring():
                raise ValueError("The PHCPoly's must share the System's ring.")
        self.solved_starter = NULL
        self.solved_target = NULL

    def __dealloc__(self):
        # free memory associated with solved_starter and solved_target
        pass

    def __repr__(self):
        return '\n'.join(
            ['PHCSystem(%s,\n['%str(self.ring)] +
            ['%s'%p for p in self.polys] +
            ['])']
            )

    def __getitem__(self, index):
        return self.polys[index]
    
    def __len__(self):
        return len(self.polys)

    def num_variables(self):
        return len(self.ring)
    
    cdef void* get_solved(self):
        return self.solved_target
    
    def supports(self):
        return [sorted(p.terms().keys()) for p in self]

    cdef build_starter(self):
        cdef int* supp_array
        cdef int* sizes
        cdef int* indices
        cdef int i, j, k, p, dim, count
        cdef void* starter
        dim = len(self)
        sizes = <int *> malloc( len(self)*sizeof(int) )
        indices = <int *> malloc( len(self)*sizeof(int) )
        supports = self.supports()
        count = 0
        for i in range(dim):
            indices[i] = count
            sizes[i] = len(supports[i])
            count += sizes[i]
        supp_array = <int *> malloc( count*dim*sizeof(int) )
        p = 0
        for i in range(dim):
            support = supports[i]
            for j in range(sizes[i]):
                for k in range(dim):
                    supp_array[p] = support[j][k]
                    p += 1
        self.solved_starter = mixed_volume_algorithm(dim, count, indices,
                                             sizes, supp_array)
        if self.solved_starter == NULL:
            raise PHCInternalAdaException

        poly_list = []
        for i in range(dim):
            P = PHCPoly(self.ring, '')
            PHCPoly.replace_poly(P, get_poly(self.solved_starter, 1+i) )
            poly_list.append(P)
        num_solns = get_num_solns(self.solved_starter)
        real = <double*>malloc( (1+dim)*sizeof(double) )
        imag = <double*>malloc( (1+dim)*sizeof(double) )
        self.start_solutions = self.extract_solns(self.solved_starter)
        self.start_system = PHCSystem(self.ring, poly_list)
        free(imag)
        free(real)
        free(supp_array)
        free(sizes)
        free(indices)

    cdef extract_solns(self, void* solved_system):
        cdef int num_solns, i, mult, dim=len(self)
        cdef double* real
        cdef double* imag
        cdef double info[3]
        num_solns = get_num_solns(solved_system)
        real = <double*>malloc( (1+dim)*sizeof(double) )
        imag = <double*>malloc( (1+dim)*sizeof(double) )
        solns = []
        for i in range(num_solns):
            get_solution(solved_system, i, &mult, info, real, imag)
            err, rco, res = info[0], info[1], info[2]
            t = complex(real[0], imag[0])
            point = [complex(real[j],imag[j]) for j in range(1, 1+dim)]
            solns.append(PHCSolution(t, mult, err, rco, res, point))
        free(imag)
        free(real)
        return solns
        
    def MVsolve(self, filter=True, double tolerance=1.0E-06):
        if self.solved_starter == NULL:
            self.build_starter()
        self.solved_target = new_solved_system(len(self))
        for n, P in enumerate(self):
            set_poly(self.solved_target, n+1, PHCPoly.get_pointer(P))
        do_homotopy(self.solved_starter, self.solved_target, 0)
        if filter:
            filter_solns(self.solved_target, &tolerance)
        self.solutions = self.extract_solns(self.solved_target)

    def HCsolve(self, start_system, allow_clustering=0):
        cdef void* start = PHCSystem.get_solved(start_system) 
        self.solved_target = new_solved_system(len(self))
        for n, P in enumerate(self):
            set_poly(self.solved_target, n+1, PHCPoly.get_pointer(P))
        do_homotopy(start, self.solved_target, allow_clustering)
        self.solutions = self.extract_solns(self.solved_target)

    def polish(self):
        if self.solved_target == NULL:
            raise ValueError('System has not been solved yet.')
        polish_solns(self.solved_target)
        self.solutions = self.extract_solns(self.solved_target)

    def absorb(self, other, double tolerance=1.0E-06):
        cdef void* other_sys = PHCSystem.get_solved(other) 
        result = add_solutions(self.solved_target,
                               other_sys,
                               &tolerance)
        if result:
            self.solutions = self.extract_solns(self.solved_target)
        return result
            
    def solution_list(self, filter=True, double tolerance=1.0E-06):
        if self.solved_target == NULL:
            self.MVsolve(filter=filter, tolerance=tolerance)
        return self.solutions

class ParametrizedSystem:
    """
    A polynomial system in which one of the variables is
    treated as a parameter.  Instantiate with a ring that
    includes the parameter as an indeterminate, the name of
    the parameter, and a list of polynomials over the ring.
    """
  
    def __init__(self, ring, parameter, polys):
        self.parameter = parameter
        self.ring = ring
        self.system = PHCSystem(ring, polys)
        variables = list(ring)
        variables.remove(parameter)
        self.base_ring = PolyRing(variables)
        self.fibers = []
        
    def __getitem__(self, index):
        return self.system[index]

    def __repr__(self):
        return '\n'.join(['ParametrizedSystem(%s, %s,'%
                          (repr(self.ring), self.parameter),
                          '['] +
                         ['%s'%p for p in self.system] +
                         ['])'])

    def specialize(self, param_value):
        """
        Return a PHCSytem obtained by specializing the parameter.
        """
        polys = [p.specialize(self.parameter, param_value)
                 for p in self]
        ring = self.base_ring
        return PHCSystem(ring, polys)

    def start(self, param_value, double tolerance):
        """
        Solve the specialized system from scratch, using the mixed
        volume algorithm.
        """
        system = self.specialize(param_value)
        system.MVsolve(tolerance=tolerance)
        return system 
        
    def transport(self, start, target_param, allow_collisions=False):
        """
        Starting from a solved specialized system, perform a homotopy to
        arrive at a solved sytem with a different (nearby) parameter value.
        Solutions are not allowed to collide unless the flag is set to
        true.
        """
        system = self.specialize(target_param)
        system.HCsolve(start)
        return system
        
class PHCSolution:
    def __init__(self, t=None, mult=None,
                 err=None, rco=None, res=None, point=[]):
        self.t, self.mult, self.point = t, mult, point
        self.err, self.rco, self.res = err, rco, res

    def __repr__(self):
        return (
            '\n'+
            '\n'.join(
            ['PHCSolution(t=%s, mult=%s'%(self.t, self.mult),
             'err=%s; rco=%s; res=%s'%(self.err, self.rco, self.res),
             'point='])
            + '[\n'
            + ',\n'.join([repr(z) for z in self.point])+'\n])\n' 
            )              
                          
phc_context = PHCContext()
