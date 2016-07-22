#
# Library name
#
program = general
#
# Definition of objects to be compiled:
#    objects          (used by both serial and parallel)
#    objects_serial   (only used by serial code)
#    objects_parallel (only used by parallel code)
#
objects = crdiag.o blas.o cpuused.o eispack.o locate.o runsec.o \
          chdiag.o daytime.o fmalloc.o nagfile.o utils.o
objects_serial = q2job.o
objects_parallel = q2job.par.o mpiqt.o
#
# Definition of include files to be used:
#    include_files    (needed by both serial and parallel code)
#    include_parallel (needed by parallel code)
#
include_files = 
include_parallel = mpidef.inc
#
# Definition of libraries to be linked in:
# 

include ../../makefile.h

program_lib := $(patsubst %,$(lib_program)(%),$(objects))

$(program) : $(program_lib)

include dependencies
