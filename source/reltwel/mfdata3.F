C
C     ==================
      BLOCK DATA MFDATA3
C     ==================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     INITIALIZE DATA,DEFINE PARAMETERS,STORE ALL COMMON BLOCKS USED
C     FILES ARE OPENED AND CLOSED ONLY WHEN NEEDED TO MINIMIZE LOCAL
C     FILE LIST
C
C     FILES IN USE :
C     NAME----UNIT------------PURPOSE-----------
C     INPUT    5       INPUT DATA
C     OUTPUT   6       OUTPUT DATA FORMATTED
C     SRTTWLL  40      SORTED  LARGE-LARGE COMPONENT INTEGRALS
C     SRTTWSL  41      SORTED  SMALL-LARGE COMPONENT INTEGRALS
C     SRTTWSS  42      SORTED  SMALL-SMALL COMPONENT INTEGRALS
C     SRTBREI  43      SORTED  BREIT (SL/SL)  INTEGRALS
C     SRTLBLL  50      SORTED  LARGE-LARGE COMPONENT LABELS
C     SRTLBSL  51      SORTED  SMALL-LARGE COMPONENT LABELS
C     SRTLBSS  52      SORTED  SMALL-SMALL COMPONENT LABELS
C     SRTBREL  53      SORTED  BREIT (SL/SL)  LABELS
C
      INCLUDE 'paracom.inc'
C
      DATA NX / 0,1,0,0,2,0,0,1,1,0,3,0,0,2,2,1,0,1,0,1,
     G          4,0,0,3,3,1,0,1,0,2,2,0,2,1,1,
     H          5,0,0,4,4,1,0,1,0,3,3,2,0,2,0,3,1,1,2,1,2 /
      DATA NY / 0,0,1,0,0,2,0,1,0,1,0,3,0,1,0,2,2,0,1,1,
     G          0,4,0,1,0,3,3,0,1,2,0,2,1,2,1,
     H          0,5,0,1,0,4,4,0,1,2,0,3,3,0,2,1,3,1,2,2,1 /
      DATA NZ / 0,0,0,1,0,0,2,0,1,1,0,0,3,0,1,0,1,2,2,1,
     G          0,0,4,0,1,0,1,3,3,0,2,2,1,1,2,
     H          0,0,5,0,1,0,1,4,4,0,2,0,2,3,3,1,1,3,1,2,2 /
      DATA PI,PIROOT,TWOPI,PITERM,DNORM,ENORM
     $/3.1415926535898D0,.88622692545276D0,6.2831853071796D0,
     $34.986836655250D0,32.0D0,1.0D-12,1.0D-12,1.0D-12,1.0D-12/
      DATA FACT /1.D0,1.D0,0.5D0,0.16666666666667D0,0.041666666666667D0,
     &0.0083333333333333D0,0.0013888888888889D0 /
      DATA UN/40,41,42,43,50,51,52,53/
      DATA FN / 'SRTTWLL', 'SRTTWSL', 'SRTTWSS','SRTBREI',
     &          'SRTLBLL', 'SRTLBSL', 'SRTLBSS','SRTBREL'/
      DATA VERSION/' 5.2   '/
C
C             5.0   : Major rewrite of SPDFG and related routines,
C                     this is the diskversion of direct integral code
C             5.1   : h-type functions are introduced and debugged
C             5.2   : updated readin for use with Fortran 90
      END
