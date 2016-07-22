#
# Program name: 
#
program = molfdir
#
# Definition of objects to be compiled:
#    objects          (used by both serial and parallel)
#    objects_serial   (only used by serial code)
#    objects_parallel (only used by parallel code)
#
objects = basis.o grepmat.o mfd1nrm.o mfdsint.o molfdir.o rovlap.o xyztran.o \
          easyinp.o lshiftc.o mfd1out.o mfdsymf.o movlch.o setinda.o gmultb.o \
          mfd1inp.o molfdat.o ovlap.o setindn.o xovlap.o xprojop.o xrotate.o \
          xsqrt.o molecul.o roundd.o
objects_serial =
objects_parallel =
#
# Definition of include files to be used:
#    include_files    (needed by both serial and parallel code)
#    include_parallel (needed by parallel code)
#
include_files = paracom.inc
include_parallel = mpidef.inc
#
# A master routine needs to be defined on some platforms in order for it to compile correctly
#
master_routine = molfdir.F
#
# Definition of libraries to be linked in:
# 

include ../../makefile.h

program_lib := $(patsubst %,$(lib_program)(%),$(objects))

$(program) : $(program_lib) general_lib 
	$(FCOMPILER_DATA) $(master_routine) $(lib_program) $(lib_general) $(lib_other) -o $(bin_program) $(other_options)

general_lib :
	$(MAKE) -C ../general

include dependencies
