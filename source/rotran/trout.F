      SUBROUTINE TROUT(NSP,NKR,H,MH,IP,IQ,NPQSIZ,NPQBUF,NBCK,LBCK,
     &                 LBRCIJ,CBCK)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     IN H, THE TWO-INDEX TRANSFORMED INTEGRALS (PQ|JI) WITH PQ FIXED;
C     DISTRIBUTE INTEGRALS LARGER THEN THRESHOLD OVER BUCKETS WHICH ARE
C     LABELED BY JI; FULL BUCKETS ARE WRITTEN TO FILE
C
      INTEGER NBCK(*),LBCK(NPQSIZ,NPQBUF),LBRCIJ(0:*)
      COMPLEX*16 CBCK(NPQSIZ,NPQBUF),H(MH,*)
C
      INCLUDE "vecs.inc"
      INCLUDE "thres.inc"
      INCLUDE "files.inc"
C
#if defined (CRAY)
      ISHIFT (I, J) = SHIFT (I, J)
#else
      ISHIFT (I, J) = ISHFT (I, J)
#endif
#if defined (BIT64)
      IPQ = ISHIFT(IP,16)
      IPQ = OR(IQ,IPQ)
#else
      IPQ = ISHIFT(IP,16)
      IPQ = IOR(IQ,IPQ)
#endif
      JI = 0
      THRSH = VMX(IP)*VMX(IQ)
      IF (THRSH.LT.THRGEN) RETURN
      THRSH = THRGEN/THRSH
      DO 30 IBAR=1,2
      DO 20 IK=1,NKR
        IF (IBAR.EQ.1) I = KRA(IK,1)
        IF (IBAR.EQ.2) I = KRB(IK,1)
        DO 10 JK=IK,NKR
          J = KRB(JK,1)
          JI = JI + 1
#if defined (BIT64)
          IF (CABS (H(J,I)).LT.THRSH) GOTO 10
#else
          IF (CDABS(H(J,I)).LT.THRSH) GOTO 10
#endif
          NB=NBCK(JI)+1
          LBCK(NB,JI)=IPQ
          CBCK(NB,JI)=H(J,I)
          IF (NB.EQ.NPQSIZ) THEN
            LREC=LBRCIJ(0)+1
            WRITE(INTTWO,REC=LREC) NB,LBRCIJ(JI),
     +                             (CBCK(IX,JI),IX=1,NB),
     +                             (LBCK(IX,JI),IX=1,NB)
            LBRCIJ(0)=LREC
            LBRCIJ(JI)=LREC
            NHALFT = NHALFT + NB
            NB=0
          ENDIF
          NBCK(JI)=NB
 10       CONTINUE
 20     CONTINUE
 30     CONTINUE
      RETURN
      END
