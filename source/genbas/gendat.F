C     =================
      BLOCK DATA GENDAT
C     =================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     STORE ALL COMMON BLOCKS AND INITIALIZE DATA
C
      INCLUDE 'paracom.inc'
C
      DATA TOLER /1.D-10/
      DATA NTYPES,NUMCOF/16*0,12*0/
      DATA NCONTR,NPRIM /12*0,12*0/
      DATA NWCONT,NWPRIM/12*0,12*0/
      END
