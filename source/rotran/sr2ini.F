      SUBROUTINE SR2INI(NIJBUF,NIJSIZ,LBRCIJ)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     INITIALIZES FILE AND POINTER ARRAY FOR THE FIRST HALF-TRANSFORMATION
C
      INTEGER LBRCIJ(0:*)
      INCLUDE "inpt.inc"
      INCLUDE "files.inc"
#if defined (PARALLEL)
      INCLUDE "mpidef.inc"
#else
      PARAMETER (mpirnk=0)
#endif
      character*12 file1
C
#if defined (BIT64)
      IRECL = 8 + 8 + NIJSIZ*8 + NIJSIZ*16
      IF (BREIT) IRECL =  8 + 2*8 + NIJSIZ*8 + NIJSIZ*8
#else
      IRECL = 4 + 4 + NIJSIZ*4 + NIJSIZ*16
      IF (BREIT) IRECL =  4 + 2*4 + NIJSIZ*4 + NIJSIZ*8
#endif
      NHALFTB = IRECL
C
      IF (mpirnk .EQ. 0) THEN
         WRITE (FILE1,'(A6)') FNAME(13)
      ELSEIF (mpirnk .LT. 10) THEN
         WRITE (FILE1,'(A6,A1,I1)') FNAME(13),'.',mpirnk
      ELSEIF (mpirnk .LT. 100) THEN
         WRITE (FILE1,'(A6,A1,I2)') FNAME(13),'.',mpirnk
      ELSE
         WRITE (FILE1,'(A6,A1,I3)') FNAME(13),'.',mpirnk
      ENDIF
      OPEN (INTTWO,ACCESS='DIRECT',RECL=IRECL,FILE=FILE1)
C
      LBRCIJ(0) = 0
      JI = 0
      DO IK=1,NKR
        DO JK=1,2*IK
          JI = JI + 1
          LBRCIJ(JI)=0
        ENDDO
      ENDDO
C
      RETURN
      END
