      SUBROUTINE SR2FLSB(NPQBUF,NPQSIZ,LBRCIJ,NBCK,LBCK,RBCK)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     WRITE ALL NON-EMPTY BUCKETS WITH HALF-TRANSFORMED BREIT INTEGRALS
C
      INTEGER LBRCIJ(0:*),NBCK(2,NPQBUF),LBCK(NPQSIZ,NPQBUF)
      REAL*8 RBCK(6*NPQSIZ,NPQBUF)
      INCLUDE "files.inc"
      INCLUDE "vecs.inc"
      INCLUDE "inpt.inc"
#if defined (PARALLEL)
      INCLUDE "mpidef.inc"
#else
      PARAMETER (mpirnk=0)
#endif
      CHARACTER*12 FILE1
C
#if defined (BIT64)
      IRECL = 8 + 2*8 + NPQSIZ*8 + NPQSIZ*8
#else
      IRECL = 4 + 2*4 + NPQSIZ*4 + NPQSIZ*8
#endif
C
      JI = 0
      DO 30 IBAR=1,2
      DO 20 IK=1,NKR
        DO 10 JK=IK,NKR
          JI = JI + 1
          NBL=NBCK(1,JI)
          NBI=NBCK(2,JI)
          IF (NBL.GT.0) THEN
            LREC=LBRCIJ(0)+1
            WRITE(INTTWO,REC=LREC) NBL,NBI,LBRCIJ(JI),
     +                             (LBCK(IX,JI),IX=1,NBL),
     +                             (RBCK(IX,JI),IX=1,NBI)
            LBRCIJ(0)=LREC
            LBRCIJ(JI)=LREC
            NHALFT = NHALFT + NBL
            NBCK(1,JI)=0
            NBCK(2,JI)=0
          ENDIF
 10       CONTINUE
 20     CONTINUE
 30     CONTINUE
C
      CLOSE (INTTWO,STATUS='KEEP')
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
      RETURN
      END
