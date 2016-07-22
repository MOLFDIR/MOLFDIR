C     =================
      BLOCK DATA SCFDAT
C     =================
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     INITIALIZE DATA
C
      INCLUDE 'paracom.inc'
      INCLUDE 'general.inc'
      INCLUDE 'diis.inc'
C
      DATA TPOEF,TDIAG,TDENS,TFMCP,TFOCK,TGAUNT,TPROP /7*0.D0/
#if defined (BIT64)
      DATA C,IFAIL/137.0359895,0/
#else
      DATA C,IFAIL/137.0359895D0,0/
#endif
      DATA ONLYONE,ENDRUN/.FALSE.,.FALSE./
      DATA PRTOD,PRTRD/.TRUE.,.TRUE./
      DATA FNAMES/'SRTTWLL','SRTTWSL','SRTTWSS','SRTBREI',
     &            'SRTLBLL','SRTLBSL','SRTLBSS','SRTBREL'/
      DATA FNAM / 'MFDOEMR','MFDVECB','MFDONEE','MFDSYMC',
     +            'SELVECF','MFDOEMI','MFDENSM','SSFOCK',
     +            'MFDVECA','SRTPQIL','SRTPQLN' /
      DATA ICYCLS,KCYCLS/0,0/
      DATA VERS0,VERS1,VERS2,T0,T1,T2,D0,D1,D2/9*' UNDEF. '/
      DATA POTNUC,ETOTAL,EPREV,EMDIF,WDIF/0.D0,2*1.D10,2*0.D0/
      DATA MFDSYMC,MFDONE ,MFDOEMR,MFDOEMI,
     &     SRTTWLL,SRTTWSL,SRTTWSS,SRTLBLL,SRTLBSL,SRTLBSS,SRTBREI,
     &     SRTBREL,MFDVECB,MFDENSM,LVECF,SSFOCK,MFDVECA,SRTPQIL
     &     / 7, 8, 9,10,40,41,42,50,51,52,43,53,11,12,13,14,15,33/
      DATA EXTRPOL/'NONE','DAMPING','DIIS',' QCSCF','POPLE'/
      DATA NREC/14/
      DATA SELECT/.FALSE./
      DATA IRECDC,IRECDP/2*0/
      DATA IPOINT/0/
      DATA DFIRST/.TRUE./
      DATA LASTRUN/.FALSE./
      DATA UDIIS/.FALSE./
      DATA INTFILE,LBLFILE/40,41,42,43,50,51,52,53/
      DATA IFIRST,ILAST,IDCYC,DIISFIL/1,0,0,16/
      DATA ACONV/1.0D10/
      DATA PDIIS/0/
      DATA VERSION/'1.31'/
C======================================================================
C     History of the code : Record of changes since 1990
C     Go to end of file to see most recent changes.
C======================================================================
C  VERSION 1.15 : CLEANUP OF SOME REDUNDANT CODE
C
C                 DYMAMIC MEMORY ALLOCATION NOW EVEN SAFER
C
C                 DIIS IMPLEMENTED
C
C                                          HIRZO MERENGA, 1992-09-10
C=======================================================================
C  VERSION 1.15 B : N3, N8, N10 AND N22 ENLARGED
C
C                   NOTE THAT : - N3 MUST BE TWICE N3 IN MOLFDIR
C                               - N8 IS NOT USED
C
C                   CHECKS THESE PARAMETERS AT INPUT
C
C                                          LUUK VISSCHER, 1992-12-17
C=======================================================================
C
C  VERSION 1.19   : Updated default speed of light   
C                   previous value 137.03602
C                   present value  137.0359895
C
C                                           LV, 1994-06-01
C========================================================================
C  VERSION 1.20   : Corrected small bug in type 13 of Gaunt
C                   Removed old options SLUPD, NSSUPD,NSSFZ and NNOSS
C                   Replaced by reading in variable MFIL
C
C                                           LV, 1994-08-04
C========================================================================
C VERSION 1.21    : TWO OPEN SHELL OPTION IMPLEMENTED
C             
C                                          BDJ, 1994-08-12
C========================================================================
C  VERSION 1.22   : Corrected memory alloc. bug in DIIS extrapol. in C1 
C                   Diagonalises unoccupied representations as well.
C
C                                           LV, 1994-09-27
C========================================================================
C  VERSION 1.22a  : Small bug corrections (2 open shell, virtuals + DIIS)
C
C                                           LV, 1994-10-04
C========================================================================
C  VERSION 1.23   : Try to make symmetry adapted coefficients real
C                   Changed TIMSYM, MFDTABL and CHDIAG
C
C                                           LV, 1994-12-21
C========================================================================
C  VERSION 1.23a  : Put save statements for MASK in MFDFOCK and MFDBREIT
C                   Corrected 2-open shell initialization error in MFDFOCK
C
C                                           LV, 1994-12-21
C========================================================================
C  VERSION 1.23b  : Small changes in output format for Gaunt interaction
C
C                                           LV, 1995-03-09
C========================================================================
C  VERSION 1.24   : Synchronized NASA and Groningen versions.
C
C                                           LV, 1995-04-07
C========================================================================
C  VERSION 1.3    : Updated readin for use under Fortran 90
C                   Memory allocation revised, now you one get what
C                   you really need
C                                          BDJ, 1998-04-01
C========================================================================
C  VERSION 1.3.1  : Runs parallel under MPI on IBM; Other machines not
C                   tested.
C                                          BDJ, 1998-04-01
C========================================================================
      END
