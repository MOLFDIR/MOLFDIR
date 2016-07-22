#
# Program name:
#
program = reltwel
#
# Definition of objects to be compiled:
#    objects          (used by both serial and parallel)
#    objects_serial   (only used by serial code)
#    objects_parallel (only used by parallel code)
#
objects = mfd3inp.o wrbuf.o gentab.o spdfg.o mfdata3.o fgamma.o inttyp.o fourind.o \
          hmfuns.o hlfuns.o hlmfun.o hlms.o
objects_serial = reltwel.o mfdtwoe.o 
objects_parallel = mfdtwoe.par.o reltwel.par.o sort.o   
#
# Definition of include files to be used:
#    include_files    (needed by both serial and parallel code)
#    include_parallel (needed by parallel code)
#
include_files = paracom.inc work.inc
include_parallel = mpidef.inc
#
# A master routine needs to be defined on some platforms in order for it to compile correctly
#
master_routine = reltwel.F
#
# Definition of libraries to be linked in:
# 

include ../../makefile.h

#
# If a parallel code is compiled we need another master routine
#
ifdef PARALLEL
    master_routine = reltwel.par.F
endif

program_lib := $(patsubst %,$(lib_program)(%),$(objects))

$(program) : $(program_lib) general_lib
	$(FCOMPILER_DATA) $(master_routine) $(lib_program) $(lib_general) $(lib_other) -o $(bin_program) $(other_options)

general_lib :
	$(MAKE) -C ../general

include dependencies
