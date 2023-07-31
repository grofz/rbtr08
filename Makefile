.SUFFIXES:

vpath %.f90 src
vpath %.f90 test

FC = gfortran
FFLAGS = -Og -std=f2018 -Wall -Wextra -pedantic -fimplicit-none -fcheck=all -fbacktrace
#FFLAGS = -O3 -std=f2018 -Wall -Wextra -pedantic -fimplicit-none -fbacktrace
COMPILE = $(FC) $(FFLAGS) -c
MAKEMOD = $(FC) $(FFLAGS) -fsyntax-only -c

SOURCES = kinds.f90 \
    abstract_container.f90 \
	  tree.f90 \
	  basetree.f90 \
	  rbtr.f90 \
	  tree_tests.o \
	  check.o

test.exe : $(subst .f90,.o,$(SOURCES))
	$(FC) -o $@ $+

.PHONY: clean

clean:
	-rm -f *.o *.mod *.smod

%_m.mod %.o : %.f90
	$(COMPILE) -o $*.o $<
	@touch $@

	

check.o : rbtr.o basetree.o

tree_tests.o : rbtr.o basetree.o

rbtr.o : basetree.o tree.o kinds.o

basetree.o : tree.o kinds.o


#modules = kinds_m.mod \
#	  tree_common_m.mod \
#	  basetree_m.mod \

#cc = gfortran $(FFLAGS)
#ar = ar -rcv

# path to sources and objects
#src = src
#test = test
#build = build



# object files
#objects = \
#	 $(build)/kinds.o \
#	 $(build)/tree_common.o \
#	 $(build)/basetree.o \
#	 $(build)/rbtr.o

#testobjects = \
#	      $(build)/tree_tests.o

#testmain = $(build)/check.o

#alltestobjects = $(objects) $(testobjects) $(testmain)

#	  rbtr_m.mod \
#	  tree_tests_m.mod
#


# default goal and dependencies

#all : test.x

#test.x : $(alltestobjects) Makefile
#	$(cc) -o $@ $(alltestobjects)

#$(modules) $(alltestobjects) : Makefile

#$(testmain) : $(objects)

#clean :
#	rm $(alltestobjects) $(modules)




# build rules

#$(build)/%.o : $(src)/%.f90
#	$(cc) -c $< -o $@
#$(build)/%.o : $(test)/%.f90
#	$(cc) -c $< -o $@
