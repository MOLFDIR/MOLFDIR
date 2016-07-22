###################################################
#                                                 #
# makefile.h definitions file for MOLFDIR program #
#                                                 #
###################################################

TARGET = $(MOLFDIR_TARGET)

ifdef MOLFDIR_PAR
   PARALLEL = yes
endif

#
# Definitions for target HP
#
ifeq ($(TARGET),HP)
  ifdef PARALLEL
     error1:
	@echo No parallel setup for $(TARGET)
	exit 1
     FCOMPILER_DATA = mpif90 +U77 +O3 -K -DHP9 -DPARALLEL -DVAR_MPI
     CCOMPILER_DATA = mpicc -Ae -DHP9 -DPARALLEL -DVAR_MPI
  else
     FCOMPILER_DATA = f90 +U77 +O3 -K -DHP9
     CCOMPILER_DATA = cc -Ae -DHP9
     ARGOSMAC = hp
  endif
endif

#
# Definitions for target IBM
#
ifeq ($(TARGET),IBM)
  ifdef PARALLEL
     FCOMPILER_DATA = mpxlf -O3 -qdpc -qstrict -WF,-DIBM,-DPARALLEL,-DVAR_MPI -qnosave
     CCOMPILER_DATA = mpcc -DIBM
     FINCLUDE_DIR = -WF,-I/usr/lpp/ppe.poe/include
     other_options = -L /usr/lpp/ppe.poe/lib/libmpi.a -bmaxdata:550000000
  else
     FCOMPILER_DATA = xlf -O3 -qstrict -qdpc -WF,-DIBM
     CCOMPILER_DATA = cc -DIBM
     ARGOSMAC = rs6000
     other_options = -bmaxdata:550000000
  endif
endif

#
# Definitions for target IBM64
#
ifeq ($(TARGET),IBM64)
  ifdef PARALLEL
     error1:
	@echo No parallel setup for $(TARGET)
	exit 1
  else
     FCOMPILER_DATA = xlf -O3 -qstrict -qdpc -q64 -WF,-DIBM
     CCOMPILER_DATA = cc -DIBM -q64
     ARGOSMAC = rs6000
  endif
endif

#
# Definitions for target LINUX
#
ifeq ($(TARGET),LINUX)
  ifdef PARALLEL
     error1:
	@echo No parallel setup for $(TARGET)
	exit 1
     FCOMPILER_DATA = mpif77 -O3 -ffast-math -fautomatic -fno-f2c -fno-globals -Wno-globals -DLINUX -DPARALLEL -DVAR_MPI
     CCOMPILER_DATA = mpicc -DLINUX -DPARALLEL -DVAR_MPI
     FINCLUDE_DIR = -I<your mpich-1.2.2 location>/include
     lib_other = -L<your mpich-1.2.2 location>/lib -lmpich -lm -ldl -lieee
  else
#     FCOMPILER_DATA = g77 -O3 -ffast-math -fautomatic -fno-f2c -fno-globals -Wno-globals -DLINUX
     FCOMPILER_DATA = gfortran -O3 -ffast-math -DLINUX
     CCOMPILER_DATA = gcc -DLINUX
#     lib_other = -ldl -lieee -lm
     lib_other = 
     ARGOSMAC = linux
  endif
endif

#
# Definitions for target SUN
#
ifeq ($(TARGET),SUN)
  ifdef PARALLEL
     error1:
	@echo No parallel setup for $(TARGET)
	exit 1
  else
     FCOMPILER_DATA = f77 -O2 -DSUN -xlic_lib=sunperf -lmvec
     CCOMPILER_DATA = cc -DSUN
     ARGOSMAC = sun
  endif
endif

#
# Definitions for target CRAYC90 (also works for CRAYJ90)
#
ifeq ($(TARGET),CRAYC90)
  ifdef PARALLEL
     error1:
	@echo No parallel setup for $(TARGET)
	exit 1
  else
     FCOMPILER_DATA = f90 -c -d p -F -O scalar3,aggress,vector3 -DCRAY -DBIT64
     CCOMPILER_DATA = cc -DCRAY -DBIT64
     lib_other = -Wl-lnag
     ARGOSMAC = xmp
  endif
endif

#
# Definitions for target CRAYT3E
#
ifeq ($(TARGET),CRAYT3E)
  ifdef PARALLEL
     FCOMPILER_DATA = f90 -dp -F -O3,unroll2,msgs,negmsgs -R nb -Wl,-Dstreams=on -DCRAY -DBIT64 -DPARALLEL -DVAR_MPI
     CCOMPILER_DATA = cc -DCRAY -DBIT64 -DPARALLEL -DVAR_MPI
     lib_other = -lnag
  else
     FCOMPILER_DATA = f90 -dp -F -O3,unroll2,msgs,negmsgs -R nb -Wl,-Dstreams=on -DCRAY -DBIT64
     CCOMPILER_DATA = cc -DCRAY -DBIT64
     lib_other = -lnag
     ARGOSMAC = T3E64
  endif
endif

#
# Definitions for target SGI32
#
# Parallel definition below compiles/runs with mpich-1.2.2. Adjust for your needs.
#
ifeq ($(TARGET),SGI32)
  ifdef PARALLEL
     error1:
	@echo No parallel setup for $(TARGET)
	exit 1
     FCOMPILER_DATA = f77 -O1 -n32 -mips4 -G 0 -woff 2290 -DSGI -static -DPARALLEL -DVAR_MPI
     CCOMPILER_DATA = cc -O1 -n32 -mips4 -G 0 -DSGI -DPARALLEL -DVAR_MPI
     FINCLUDE_DIR = -I <your mpich-1.2.2 location>/include
     lib_other = -lcomplib.sgimath -L<your mpich-1.2.2 location>/lib
     other_options = 
  else
     FCOMPILER_DATA = f77 -O1 -n32 -mips4 -G 0 -woff 2290 -DSGI -static 
     CCOMPILER_DATA = cc -O1 -n32 -mips4 -G 0 -DSGI 
     lib_other = -lcomplib.sgimath 
     ARGOSMAC = sgipower
  endif
endif

#
# Definitions for target SGI64
#
# Parallel definition below compiles/runs with mpich-1.2.2. Adjust for your needs.
#
ifeq ($(TARGET),SGI64)
  ifdef PARALLEL
     error1:
	@echo No parallel setup for $(TARGET)
	exit 1
     FCOMPILER_DATA = mpif77 -O1 -64 -mips4 -G 0 -woff 2290 -DPARALLEL -DSGI -static -DVAR_MPI
     CCOMPILER_DATA = mpicc -O1 -64 -mips4 -G 0 -DSGI -DPARALLEL
     FINCLUDE_DIR = -I<your mpich-1.2.2 location>/include
     lib_other = -lcomplib.sgimath -L<your mpich-1.2.2 location>/lib 
  else
     FCOMPILER_DATA = f77 -O1 -64 -mips4 -G 0 -woff 2290 -DSGI -static
     CCOMPILER_DATA = cc -O1 -64 -mips4 -G 0 -DSGI 
     lib_other = -lcomplib.sgimath
     ARGOSMAC = sgipower
  endif
endif

#
# Definitions for target DEC
#
ifeq ($(TARGET),DEC)
  ifdef PARALLEL
     error1:
	@echo No parallel setup for $(TARGET)
	exit 1
  else
     FCOMPILER_DATA = f77 -O5 -fast -DDEC
     CCOMPILER_DATA = cc -DDEC
     lib_other = -lnag
     ARGOSMAC = decalpha
  endif
endif

#
# Definitions for target NEW
#
# Add -DPARALLEL -DVAR_MPI  to your Fortran and C compiler options  when setting up parallel environment !!!
#
ifeq ($(TARGET),NEW)
  ifdef PARALLEL
     FCOMPILER_DATA = 
     CCOMPILER_DATA = 
     FINCLUDE_DIR =
     CINCLUDE_DIR =
     lib_other = 
     other_options = 
  else
     FCOMPILER_DATA = 
     CCOMPILER_DATA = 
     FINCLUDE_DIR =
     CINCLUDE_DIR =
     lib_other = 
     other_options = 
     ARGOSMAC = 
  endif
endif

#
# Based on requirements (PARALLEL or SERIAL) set the correct 
#  - objects 
#  - include files
#  - program binary name and location, and library name
#  - name of general library
#

ifdef PARALLEL
   objects += $(objects_parallel)
   include_files += $(include_parallel)
   lib_program = ../../lib/$(TARGET)/$(program).par.a
   bin_program = ../../bin/$(TARGET)/$(program).par.x
   lib_general = ../../lib/$(TARGET)/general.par.a
else
   objects += $(objects_serial)
   lib_program = ../../lib/$(TARGET)/$(program).a
   bin_program = ../../bin/$(TARGET)/$(program).x
   lib_general = ../../lib/$(TARGET)/general.a
endif

FCOMPILER_DATA += $(FINCLUDE_DIR)
CCOMPILER_DATA += $(CINCLUDE_DIR)

#
# Compiling definitions
#

.SUFFIXES : .f .F .c .o

.f.o: $(include_files)
	$(FCOMPILER_DATA) -c $*.f
.F.o: $(include_files)
	$(FCOMPILER_DATA) -c $*.F
.c.o :
	$(CCOMPILER_DATA) -c $*.c 
