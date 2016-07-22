C
C     =================
      BLOCK DATA DRELON
C     =================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'paracom.inc'
C
      DATA PI,PIROOT,TWOPI,PITERM
     +/3.1415926535898D0,.88622692545276D0,6.2831853071796D0,
     +34.986836655250D0/
C
C     DATA NLM-X
C 
      DATA (NR(I,1),I=1,N6) /
     X          0,1,0,0,2,0,0,1,1,0,3,0,0,2,2,1,0,1,0,1,
     G          4,0,0,3,3,1,0,1,0,2,2,0,2,1,1,
     H          5,0,0,4,4,1,0,1,0,3,3,2,0,2,0,3,1,1,2,1,2 /
C
C     DATA NLM-Y
C 
      DATA (NR(I,2),I=1,N6) /
     Y          0,0,1,0,0,2,0,1,0,1,0,3,0,1,0,2,2,0,1,1,
     G          0,4,0,1,0,3,3,0,1,2,0,2,1,2,1,
     H          0,5,0,1,0,4,4,0,1,2,0,3,3,0,2,1,3,1,2,2,1 /
C
C     DATA NLM-Z
C 
      DATA (NR(I,3),I=1,N6) /
     Z          0,0,0,1,0,0,2,0,1,1,0,0,3,0,1,0,1,2,2,1,
     G          0,0,4,0,1,0,1,3,3,0,2,2,1,1,2,
     H          0,0,5,0,1,0,1,4,4,0,2,0,2,3,3,1,1,3,1,2,2 /
      DATA VERSION/' 2.6  '/
C---------------------------------------------------------------------------
C     VERSION 2.14.4: H-TYPE INTEGRALS IMPLEMENTED
C     BDJ 1993-02-14
C---------------------------------------------------------------------------
C     VERSION 2.2   : CYBER COMMENTS REMOVED
C                     ADAPTED FOR IBM
C     BDJ 1994-05-27
C---------------------------------------------------------------------------
C     VERSION 2.3   : FOUND SOME SINGLE PRECISION STATEMENTS (SQRT)
C                     IN 4 BYTE MACHINE VERSIONS : CORRECTED
C     LV 1994-07-11
C---------------------------------------------------------------------------
C     VERSION 2.4   : Writes overlap integrals in scalar basis to file
C     LV 1994-10-27
C---------------------------------------------------------------------------
C     VERSION 2.5   : Bugfixes for multi-centre integrals for h-type
C                     functions. Writing format changed.
C     BDJ/LV 1996-03-27
C---------------------------------------------------------------------------
C     VERSION 2.6   : Changed readin for use with Fortran 90
C     BDJ 1998-04-01
C---------------------------------------------------------------------------
      END
