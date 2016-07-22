#
# Program name:
#
program = mfdscf
#
# Definition of objects to be compiled:
#    objects          (used by both serial and parallel)
#    objects_serial   (only used by serial code)
#    objects_parallel (only used by parallel code)
#
objects = scf.o mfdata4.o mfd4inp.o mfdtabl.o mfddriv.o crscf.o mfdpoef.o mfdensi.o twostrt.o mfdensn.o \
          mfdenss.o denss.o densn.o fock.o gaunt.o readint.o mfdfmcp.o trmat.o addone.o mfdiagh.o timsym.o \
          mfdseiv.o mfdprop.o mfdtrce.o mfdcrit.o mfd4out.o ohmv.o hmv.o omtr.o omtl.o obktr.o mherr.o \
          mherm.o pmmmm.o tdvt.o ortho.o concop.o selvec.o dot.o scfdat.o mfdenst.o denst.o extrap.o trc.o \
          rdens.o rsdens.o wdens.o pople.o diis.o diiser.o diisup.o diisiv.o diisuf.o comfd.o subcsq.o \
          scaledm.o wdiis.o rdiis.o bsolve.o reskb.o
objects_serial = mfdfock.o mfdbreit.o mfopen.o 
objects_parallel = mfdfock.par.o mfdbreit.par.o mfopen.par.o \
                   mfddslv.o mfdfslv.o  mfdbslv.o inttyp.o
#
# Definition of include files to be used:
#    include_files    (needed by both serial and parallel code)
#    include_parallel (needed by parallel code)
#
include_files = diis.inc general.inc memory.inc ossel.inc paracom.inc
include_parallel = mpidef.inc
#
# A master routine needs to be defined on some platforms in order for it to compile correctly
#
master_routine = scf.F
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
