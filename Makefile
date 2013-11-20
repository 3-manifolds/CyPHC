# These variables are set in make.py, to allow the
# Makefile to be platform independent.
#For OS X
#GNATMAKE=/usr/local/ada-4.3/bin/gnatmake
#GNATBIND=/usr/local/ada-4.3/bin/gnatbind
#GNATFLAGS=-gnat95 -gnatv -O3 -gnatp -gnatf --GNATBIND="gnatbind -static"
#For Linux
#GNATMAKE=gnatmake
#GNATBIND=gnatbind
#GNATFLAGS=-gnat95 -gnatv -O3 -fPIC -gnatp -gnatf --GNATBIND="gnatbind -static"
#################
PHCRoot=../PHCsource
PHCLib=$(PHCRoot)/Lib
PHCAda=$(PHCRoot)/Ada
BIN=$(PHCRoot)/bin
INCLULIBS=-I$(PHCAda)/System \
 -I$(PHCAda)/System/Unix_Timer \
 -I$(PHCAda)/Math_Lib/Divisors \
 -I$(PHCAda)/Math_Lib/Functions \
 -I$(PHCAda)/Math_Lib/Matrices \
 -I$(PHCAda)/Math_Lib/Numbers \
 -I$(PHCAda)/Math_Lib/Polynomials \
 -I$(PHCAda)/Math_Lib/QD \
 -I$(PHCAda)/Math_Lib/Reduction\
 -I$(PHCAda)/Math_Lib/Supports\
 -I$(PHCAda)/Math_Lib/Vectors \
 -I$(PHCAda)/Root_Counts/Binomials \
 -I$(PHCAda)/Root_Counts/Dynlift \
 -I$(PHCAda)/Root_Counts/Implift \
 -I$(PHCAda)/Root_Counts/MixedVol \
 -I$(PHCAda)/Root_Counts/Product \
 -I$(PHCAda)/Root_Counts/Puiseux \
 -I$(PHCAda)/Root_Counts/Simpomials \
 -I$(PHCAda)/Root_Counts/Stalift \
 -I$(PHCAda)/Root_Counts/Symmetry \
 -I$(PHCAda)/Schubert/SAGBI \
 -I$(PHCAda)/Schubert/Pieri \
 -I$(PHCAda)/Schubert/Induction \
 -I$(PHCAda)/Components/Decomposition \
 -I$(PHCAda)/Components/Factorization \
 -I$(PHCAda)/Components/Interpolators \
 -I$(PHCAda)/Components/Samplers \
 -I$(PHCAda)/Components/Solver \
 -I$(PHCAda)/Components/Tropical \
 -I$(PHCAda)/Tasking \
 -I$(PHCAda)/Main \
 -I$(PHCAda)/CtoPHC/Funky \
 -I$(PHCAda)/CtoPHC/State \
 -I$(PHCAda)/Deformations/Continuation \
 -I$(PHCAda)/Deformations/Curves \
 -I$(PHCAda)/Deformations/End_Games \
 -I$(PHCAda)/Deformations/Homotopy \
 -I$(PHCAda)/Deformations/Newton \
 -I$(PHCAda)/Deformations/Solutions \
 -I$(PHCAda)/Deformations/Sweep \
 -I$(PHCAda)/Deformations/Trackers \

MVRoot=$(PHCRoot)/Ada/Root_Counts/MixedVol

MVSRC=$(MVRoot)/cell_stack.c $(MVRoot)/form_lp.c $(MVRoot)/index_tree_lp.c \
 $(MVRoot)/zero_index_tree.c $(MVRoot)/one_level_lp.c $(MVRoot)/mixed_volume.c \
 $(MVRoot)/relation_table.c $(MVRoot)/prepare_for_mv.c

MVOBJ=$(MVRoot)/cell_stack.o $(MVRoot)/form_lp.o $(MVRoot)/index_tree_lp.o \
 $(MVRoot)/zero_index_tree.o $(MVRoot)/one_level_lp.o $(MVRoot)/mixed_volume.o \
 $(MVRoot)/relation_table.o $(MVRoot)/prepare_for_mv.o

#phc.so: b_cy2ada.c phc.pyx mv_glue.o

phc.so: b_cy2ada.c phc.pyx
	python setup.py build_ext --inplace

b_cy2ada.c: PHCbuild
	cd PHCbuild; $(GNATMAKE) -c $(INCLULIBS) $(GNATFLAGS) ../cy2ada
	$(GNATBIND) -n -C PHCbuild/cy2ada
	-rm phc.c

PHCbuild:
	mkdir PHCbuild

mv_glue.o: PHCbuild mv_glue.c
	cd PHCbuild; gcc -c $(GCCFLAGS) -I$(MVRoot) ../mv_glue.c 

clean :
	-rm -rf PHCbuild build phc.c phc.so b_cy2ada.c

extension: b_cy2ada.c phc.pyx
	python setup.py build_ext
