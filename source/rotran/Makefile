#
# Program name:
#
program = rotran
#
# Definition of objects to be compiled:
#    objects          (used by both serial and parallel)
#    objects_serial   (only used by serial code)
#    objects_parallel (only used by parallel code)
#
objects = exittwoe.o gethti.o gethtbi.o kramers.o initc.o init.o czero.o \
          damax.o dzamax.o sr2flsb.o mtimher.o mtimhr2.o izero.o msumtim.o \
          sr2flsh.o sr2ini.o readin.o matmul1.o matmul3.o matmul7.o skelind.o
objects_serial = rotran.o comsym.o getmos.o skelop.o inittwoe.o  ppsym.o getsti.o \
                 gettw.o makind.o sort1.o srtdel.o trans1.o trans2.o trout.o trbout.o \
                 matmul1.o matmul3.o matmul7.o skelop.o cjvec.o cjnvec.o unique.o \
                 outpp0.o outpp1.o outpp2.o outpp3.o
objects_parallel = rotran.par.o comsym.par.o getmos.par.o skelop.par.o inittwoe.par.o \
                   cjmat.o clrdcf.o flshsk.o getpq.o getskel.o inttyp.o phasetr.o \
                   skelout.o symtriz.o trbout.o trfda1.o trfda2.o trout.o writwoe.o
#
# Definition of include files to be used:
#    include_files    (needed by both serial and parallel code)
#    include_parallel (needed by parallel code)
#
include_files = inpt.inc files.inc molf.inc vecs.inc sym.inc redme.inc
include_parallel = mpidef.inc
#
# A master routine needs to be defined on some platforms in order for it to compile correctly
#
master_routine = rotran.F
#
# Definition of libraries to be linked in:
# 

include ../../makefile.h

#
# If a parallel code is compiled we need another master routine
#
ifdef PARALLEL
    master_routine = rotran.par.F
endif

program_lib := $(patsubst %,$(lib_program)(%),$(objects))

$(program) : $(program_lib) general_lib
	$(FCOMPILER_DATA) $(master_routine) $(lib_program) $(lib_general) $(lib_other) -o $(bin_program) $(other_options)

general_lib :
	$(MAKE) -C ../general

include dependencies
