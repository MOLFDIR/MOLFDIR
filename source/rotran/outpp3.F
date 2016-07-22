      SUBROUTINE OUTPP3(NKR,INTSKEL1,NSKSIZ,LBRCSK,NBCK,LBCK,CBCK,NSKEL)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      CHARACTER*8 FNINTSK
      INTEGER LBRCSK(0:NKR),NBCK(NKR),LBCK(2,NSKSIZ,NKR)
      COMPLEX*16 CBCK(NSKSIZ,NKR)
C
C     Write end-of-file on INTSKEL1.
C
      WRITE (INTSKEL1) 0,0,0
C
C     Empty buffers on file INTSKEL2.
C
      INTSKEL2 = INTSKEL1 + 4
      DO 1 KKR = 1, NKR
         NB=NBCK(KKR)
         IF (NB.GT.0) THEN
            LREC=LBRCSK(0)+1
            WRITE(INTSKEL2,REC=LREC) NB,LBRCSK(KKR),
     +           ((LBCK(IX1,IX2,KKR),IX1=1,2),IX2=1,NB),
     +           (CBCK(IX,KKR),IX=1,NB)
            LBRCSK(KKR)=LREC
            LBRCSK(0)=LREC
            NSKEL = NSKEL + NB
            NB=0
         ENDIF
         NBCK(KKR)=NB
  1   CONTINUE
C
      RETURN
      END
