      SUBROUTINE OUTPP0(ISKEL,NKR,NSKSIZ,LBRCSK,NBCK)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE "files.inc"
C
      INTEGER LBRCSK(0:NKR),NBCK(NKR)
      CHARACTER*9 FNINTSK
C
      FNINTSK(1:7) = FNAME(16)
      WRITE(FNINTSK(9:9),'(I1)') ISKEL
C
C     Initialize file on which transformed integrals are written.
C
      FNINTSK(8:8) = '1'
      OPEN (INTSKEL+ISKEL,FILE=FNINTSK,ACCESS='SEQUENTIAL',
     &      FORM='UNFORMATTED')
C
C     Initialize file on which transformed pp-interchanged integrals
C     are written. Initialize block counters
C
      FNINTSK(8:8) = '2'
C
#if defined (BIT64)
      LENREC = 8 + 8 + 8 * 2 * NSKSIZ + 16 * NSKSIZ
#else
      LENREC = 4 + 4 + 4 * 2 * NSKSIZ + 16 * NSKSIZ
#endif
C
      OPEN (INTSKEL+ISKEL+4,FILE=FNINTSK,ACCESS='DIRECT',RECL=LENREC)
C
      LBRCSK(0) = 0
C
      DO 1 I = 1, NKR
         LBRCSK(I) = 0
         NBCK(I) = 0
   1  CONTINUE
C
      NSKEL = 0
C
      RETURN
      END
