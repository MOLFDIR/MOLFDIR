      SUBROUTINE SR2FLSH(NPQBUF,NPQSIZ,
     &                   LBRCIJ,NBCK,LBCK,CBCK)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     WRITE ALL NON-EMPTY BUCKETS WITH HALF-TRANSFORMED INTEGRALS TO FILE
C
      INTEGER LBRCIJ(0:*),NBCK(NPQBUF),LBCK(NPQSIZ,NPQBUF)
      COMPLEX*16 CBCK(NPQSIZ,NPQBUF)
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
      IRECL = 8 + 8 + NPQSIZ*8 + NPQSIZ*16
#else
      IRECL = 4 + 4 + NPQSIZ*4 + NPQSIZ*16
#endif
C
      JI = 0
      DO 30 IBAR=1,2
      DO 20 IK=1,NKR
        DO 10 JK=IK,NKR
          JI = JI + 1
          NB=NBCK(JI)
          NBB=NB
          IF (BREIT) NBB=NB*3
          IF (NB.GT.0) THEN
            LREC=LBRCIJ(0)+1
            WRITE(INTTWO,REC=LREC) NB,LBRCIJ(JI),
     +                             (CBCK(IX,JI),IX=1,NBB),
     +                             (LBCK(IX,JI),IX=1,NB)
            LBRCIJ(0)=LREC
            LBRCIJ(JI)=LREC
            NHALFT = NHALFT + NB
            NBCK(JI)=0
          ENDIF
 10       CONTINUE
 20     CONTINUE
 30     CONTINUE
C
        NHALFTR = LREC
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
