#
# Program name: 
#
program = cnvrt1e
#
# Definition of objects to be compiled:
#    objects          (used by both serial and parallel)
#    objects_serial   (only used by serial code)
#    objects_parallel (only used by parallel code)
#
objects = cnvrt.o getmap.o out1e.o rdstvc.o sifr1nmc.o wrt.o xyzsoc.o
objects_serial =
objects_parallel =
#
# Definition of include files to be used:
#    include_files    (needed by both serial and parallel code)
#    include_parallel (needed by parallel code)
#
include_files = 
include_parallel = 
#
# A master routine needs to be defined on some platforms in order for it to compile correctly
#
master_routine = cnvrt.f
#
# Definition of libraries to be linked in:
# 

include ../../../makefile.h

libraries := ../../../lib/$(TARGET)/colib.a ../$(lib_general)

program_lib := $(patsubst %,../$(lib_program)(%),$(objects))

$(program) : $(program_lib) general_lib colib_lib
	$(FCOMPILER_DATA) $(master_routine) ../$(lib_program) $(libraries) $(lib_other) -o ../$(bin_program) $(other_options)

general_lib :
	$(MAKE) -C ../../general

colib_lib :
	$(MAKE) -C ../colib
