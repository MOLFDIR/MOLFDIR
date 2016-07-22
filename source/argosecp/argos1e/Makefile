#
# Program name: 
#
program = argos1e
#
# Definition of objects to be compiled:
#    objects          (used by both serial and parallel)
#    objects_serial   (only used by serial code)
#    objects_parallel (only used by parallel code)
#
objects = ang1.o ang2.o aoso2.o aoso2e.o argos.o cecob.o cints.o cortab.o dfunc.o dnode.o \
          driver.o droot.o dsmit.o facab.o gcentr.o genral.o hermit.o hrecur.o hroot.o iodfac.o \
          jandk.o l2opxv.o lmnvgn.o lsints.o oneint.o pseud1.o pseud2.o pseud3.o ptwt.o qbess.o \
          qcomp.o qpasy.o rad1.o rad2.o root4.o root5.o rt123.o s0000.o seg1mn.o sf3eq1.o \
          sf3eq2.o sf3eq3.o sf3eq4.o sf3eq5.o sf3eq6.o sf3eq7.o sf3eq8.o sints.o socfpd.o \
          ssibfn.o stvcz.o stvint.o syminp.o tints.o twoint.o vints.o w1zzzz.o w2zzzz.o \
          wthead.o wtint2.o xyzint.o
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
master_routine = argos.f
#
# Definition of libraries to be linked in:
# 

include ../../../makefile.h

libraries = ../../../lib/$(TARGET)/colib.a ../$(lib_general)

program_lib := $(patsubst %,../$(lib_program)(%),$(objects))

$(program) : $(program_lib) general_lib colib_lib
	$(FCOMPILER_DATA) $(master_routine) ../$(lib_program) $(libraries) $(lib_other) -o ../$(bin_program) $(other_options)

general_lib :
	$(MAKE) -C ../../general

colib_lib :
	$(MAKE) -C ../colib
