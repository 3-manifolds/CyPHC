# Configure this
#For OS X
GNATMAKE=/usr/local/ada-4.3/bin/gnatmake
GNATBIND=/usr/local/ada-4.3/bin/gnatbind
GNATFLAGS=-gnat95 -gnatv -O3 -gnatp -gnatf --GNATBIND="gnatbind -static"
#For Linux
#GNATMAKE=gnatmake
#GNATBIND=gnatbind
#GNATFLAGS=-gnat95 -gnatv -O3 -fPIC -gnatp -gnatf --GNATBIND="gnatbind -static"
#################
PHCRoot=../PHCsource
PHCLib=$(PHCRoot)/Lib
PHCAda=$(PHCRoot)/Ada
BIN=$(PHCRoot)/bin
INCLULIBS=-I$(PHCAda)/System -I$(PHCAda)/System/Unix_Timer \
 -I$(PHCAda)/Math_Lib/Numbers -I$(PHCAda)/Math_Lib/Vectors \
 -I$(PHCAda)/Math_Lib/Matrices -I$(PHCAda)/Math_Lib/Polynomials \
 -I$(PHCAda)/Math_Lib/Supports \
 -I$(PHCAda)/Homotopy -I$(PHCAda)/Newton -I$(PHCAda)/Continuation \
 -I$(PHCAda)/Root_Counts/Product -I$(PHCAda)/Root_Counts/Simpomials \
 -I$(PHCAda)/Root_Counts/Implift -I$(PHCAda)/Root_Counts/Stalift \
 -I$(PHCAda)/Root_Counts/Dynlift -I$(PHCAda)/Root_Counts/Symmetry \
 -I$(PHCAda)/Root_Counts/MixedVol -I$(PHCAda)/Root_Counts/Puiseux \
 -I$(PHCAda)/Schubert/SAGBI -I$(PHCAda)/Schubert/Pieri \
 -I$(PHCAda)/Schubert/Induction \
 -I$(PHCAda)/Components/Samplers -I$(PHCAda)/Components/Interpolators \
 -I$(PHCAda)/Components/Factorization -I$(PHCAda)/Components/Decomposition \
 -I$(PHCAda)/Components/Solver \
 -I$(PHCAda)/Differentials -I$(PHCAda)/Tasking -I$(PHCAda)/Main \
 -I$(PHCAda)/CtoPHC/Funky -I$(PHCAda)/CtoPHC/State


phc.so: b_cy2ada.c phc.pyx
	python setup.py build_ext --inplace

b_cy2ada.c: Ada_build
	cd Ada_build; $(GNATMAKE) -c $(INCLULIBS) $(GNATFLAGS) ../cy2ada
	$(GNATBIND) -n -C Ada_build/cy2ada
	-rm phc.c

Ada_build:
	mkdir Ada_build

clean :
	-rm -rf Ada_build build phc.c phc.so b_cy2ada.c