#
# Program name:
#
program = tmoone
#
# Definition of objects to be compiled:
#    objects          (used by both serial and parallel)
#    objects_serial   (only used by serial code)
#    objects_parallel (only used by parallel code)
#
objects = tmoone.o mfdata4.o mfd4inp.o mfdtabl.o mfddriv.o mfdpoef.o obktr.o tdvt.o \
          mfdensi.o mfdenss.o denss.o mfdensn.o densn.o readint.o fock.o gaunt.o \
          mfdfmcp.o trmat.o mfdprop.o mfdtrce.o mfdtran.o mherr.o addone.o scfdat.o
objects_serial = mfdfock.o mfdbreit.o
objects_parallel = mfdfock.par.o mfdbreit.par.o mfdfslv.o mfdbslv.o inttyp.o
#
# Definition of include files to be used:
#    include_files    (needed by both serial and parallel code)
#    include_parallel (needed by parallel code)
#
include_files = common.inc densfock.inc memory.inc param.inc
include_parallel = mpidef.inc
#
# A master routine needs to be defined on some platforms in order for it to compile correctly
#
master_routine = tmoone.F
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
