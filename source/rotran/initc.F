c
C     ================
      BLOCK DATA INITC
C     ================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE "redme.inc"
      INCLUDE "files.inc"
C
      data nmata,nmatb,mmata,mmatb/514*0/
      data zeroint/65536*.TRUE./
      DATA INTSRT, SRTTW, SRTLB, MFDSYMC, MFD1OUT,
     +     MFDVECA, INTTWO, MDCINT, MDBINT,INTSKEL,
     +     SRTPQIL,SRTPQLN
     +     /10,11,12,13,14,16,17,18,19,20,21,22,23,24,25,26,27,28/
      DATA FNAME / 'INTSRT ','SRTTWLL',
     +             'SRTTWSL','SRTTWSS',
     +             'SRTBREI','SRTLBLL',
     +             'SRTLBSL','SRTLBSS',
     +             'SRTBREL','MFDSYMC',
     +             'MFD1OUT','MFDVECA',
     +             'INTTWO ','MDCINT ',
     +             'MDBINT ','INTSKEL',
     +             'SRTPQIL','SRTPQLN'/
      DATA VERSION /'2.2      '/
C
C     1.7    Cleaned up : Ported to IBM RS6000 (LV April 8 1994)
C     1.8    Optimizations : - Replaced calculation of indices in matmuls 
C                            - Combined MSUMTIM and MSUMHER
C                            - Decreased thrsh to 1.d-16 to increase accuracy
C                            - Writes only reduced lists for either Coulomb 
C                              (MDCINT) or Gaunt (MDBINT)
C                              Format of MDCINT is changed !
C                            - Old option ADDMOI removed
C                            - Memory requirements reduced (possible because
C                              Gaunt and Coulomb are transformed in separate
C                              runs)
C     1.8.0  Temporary version : Writes Extra information for GOSCIP & DIRRCI
C     1.8.1  Changed organisation of code : common blocks as include files
C     2.0.   Major rewrite : further change of organisation 
C                            allocate memory within large block
C                            change completion of integrals
C     2.1.   File reduction of factor 4 for transformation of BREIT integrals
C     2.2.   Changed readin for use with Fortran 90 
C            Corrected some sloppy code which gave errors on IBM
      END
