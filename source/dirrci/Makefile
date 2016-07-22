#
# Program name:
#
program = dirrci
#
# Definition of objects to be compiled:
#    objects          (used by both serial and parallel)
#    objects_serial   (only used by serial code)
#    objects_parallel (only used by parallel code)
#
objects = direct.o setup.o noveri.o orbsin.o userin.o shuffle.o makegm.o twolin.o \
          vertex.o driver.o detind.o diagelm.o inprod.o diag.o selecv.o criter.o \
          densmt.o precond.o zzero.o userout.o orthn.o residue.o cimem.o trial.o \
          calids_up.o leading.o putvec.o expand.o wrivec.o reavec.o dirdat.o getvec.o \
          initvec.o makids.o getids.o natorb.o copyff.o nsrepl.o
objects_serial =
objects_parallel =
#
# Definition of include files to be used:
#    include_files    (needed by both serial and parallel code)
#    include_parallel (needed by parallel code)
#
include_files = param.inc general.inc symm.inc files.inc iterat.inc \
                memory.inc mempoint.inc
include_parallel = mpidef.inc
#
# A master routine needs to be defined on some platforms in order for it to compile correctly
#
master_routine = direct.F
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
