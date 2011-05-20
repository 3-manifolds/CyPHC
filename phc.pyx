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
cdef extern void mixed_volume_algorithm (int n, int m,
                                         int* indices,
                                         int* sizes,
                                         int* supports)
cdef extern void* poly_sys_get(void* poly_sys, int n)
    
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
            raise ValueError, self.errors[self.code - 1]

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
        cdef double *reX, *imX
        cdef double Y[2]
        cdef int n, dim = self.num_unknowns() 
        if len(Z) != dim:
            raise ValueError, 'Wrong number of values.'
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
            raise ValueError, 'Degree tuple has wrong size'
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
        print result
        result.replace_poly(new_poly)
        return result

class PHCSystem:
    """
    A system of polynomials equations, all in the same ring and having
    rhs=0.  Instantiate with a ring and a sequence of PHCPoly's.
    """
    
    def __init__(self, ring, polys):
        self.ring = ring
        self.polys = tuple(polys)
        for p in polys:
            if ring != p.get_ring():
                raise ValueError, "The PHCPoly's must share the System's ring."

    def __repr__(self):
        return '\n'.join(
            ['System over %s:'%str(self.ring)] +
            ['%6s'%('%d: %s'%(n,p)) for n, p in enumerate(self.polys)])

    def __getitem__(self, index):
        return self.polys[index]
    
    def __len__(self):
        return len(self.polys)

    def supports(self):
        return [sorted(p.terms().keys()) for p in self]

    def MVsolve(self):
        cdef int *supp_array
        cdef int *sizes
        cdef int *indices
        cdef int i, j, k, p, dim, count

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

        #for i in range(dim):
        #    print supports[i]
        #for i in range(dim*count):
        #    print supp_array[i],
        #    print

        mixed_volume_algorithm(dim, count, indices, sizes, supp_array)
        free(supp_array)
        free(sizes)
        free(indices)

phc_context = PHCContext()
