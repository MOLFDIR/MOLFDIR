#
# Program name: 
#
program = colib
#
# Definition of objects to be compiled:
#    objects          (used by both serial and parallel)
#    objects_serial   (only used by serial code)
#    objects_parallel (only used by parallel code)
#
objects = falloc.o fdate.o flushstdout.o fwtime.o hostnm.o runsec.o tsystem.o 
objects_serial =
objects_parallel =
#
# Definition of include files to be used:
#    include_files    (needed by both serial and parallel code)
#    include_parallel (needed by parallel code)
#
include_files = extsymbols.h
include_parallel = 
#
# Definition of libraries to be linked in:
# 

include ../../../../makefile.h

program_lib := $(patsubst %,../../$(lib_program)(%),$(objects))

$(program) : $(program_lib)

include dependencies
