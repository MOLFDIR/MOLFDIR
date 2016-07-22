      SUBROUTINE OUTPP2 (NKR,NSP,ITYP,INTSKEL,THRES,KRK,KRL,
     & IKR,JKR,RL,NSKSIZ,LBRCSK,NBCK,LBCK,CBCK,NSKEL)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      COMPLEX*16 RL(NSP,NSP)
      INTEGER NSKSIZ,LBRCSK(0:NKR),NBCK(NKR)
      INTEGER LBCK(2,NSKSIZ,NKR)
      INTEGER KRK(NKR),KRL(NKR)
      COMPLEX*16 CBCK(NSKSIZ,NKR)
C
C     PRESORT AND WRITE PARTICLE-PARTICLE INTERCHANGED INTEGRALS
C
C     4 SETS ARE WRITTEN : 1a) (IBAR,JBAR|KBAR,LBAR)
C                          1b) (IBAR,JBAR|LBAR,KBAR)
C                          2)  (IBAR,J   |KBAR,L   )
C                          3)  (IBAR,J   |K   ,LBAR)
C                          4a) (IBAR,J   |K   ,L   )
C                          4b) (IBAR,J   |L   ,K   )
C
C     1) CANONICAL LIST  : (K => L), (KL => IJ), (KL => JI)
C     2) CANONICAL LIST  : (K => L), (I => J), (KL => IJ)
C     3) CANONICAL LIST  : (K => L), (I => J), (KL => IJ)
C     4) CANONICAL LIST  : (K => L)
C
C     WRITE ONE OF THESE SETS WITH THE RESTRICTION DEFINED BY THE
C     TYPE ITYP
C
      IJ = (IKR-1)*NKR+JKR
C
      KKRMIN = IKR
      IF (ITYP.EQ.4) KKRMIN = 1
      DO 1 KKR = KKRMIN, NKR
         K = KRK(KKR)
         LKRMIN = 1
         IF (ITYP.LE.3.AND.IKR.EQ.KKR) LKRMIN = JKR
         DO 2 LKR = LKRMIN, KKR
            L = KRL(LKR)
#if defined (BIT64)
            IF (CABS(RL(K,L)).GT.THRES) THEN
#else
            IF (CDABS(RL(K,L)).GT.THRES) THEN
#endif
               NB=NBCK(KKR)+1
               LBCK(1,NB,KKR)=LKR
               LBCK(2,NB,KKR)=IJ
               CBCK(NB,KKR)=RL(K,L)
               IF (NB.EQ.NSKSIZ) THEN
                 LREC=LBRCSK(0)+1
                 WRITE(INTSKEL,REC=LREC) NB,LBRCSK(KKR),
     +                ((LBCK(IX1,IX2,KKR),IX1=1,2),IX2=1,NB),
     +                (CBCK(IX,KKR),IX=1,NB)
                 LBRCSK(KKR)=LREC
                 LBRCSK(0)=LREC
                 NSKEL = NSKEL + NB
                 NB=0
               ENDIF
               NBCK(KKR)=NB
            ENDIF
   2     CONTINUE
   1  CONTINUE
C
C     Write the upper triangle for the types 1 and 4
C
      JI = (JKR-1)*NKR+IKR
C
      IF (ITYP.EQ.2.OR.ITYP.EQ.3) THEN
         RETURN
      ELSEIF (ITYP.EQ.1) THEN
         IF (IKR.EQ.JKR) RETURN
         DO 3 KKR = KKRMIN, NKR
            K = KRK(KKR)
            LKRMIN = 1
            IF (IKR.EQ.KKR) LKRMIN = JKR
            DO 4 LKR = LKRMIN, KKR-1
               L = KRL(LKR)
#if defined (BIT64)
               IF (CABS(RL(L,K)).GT.THRES) THEN
#else
               IF (CDABS(RL(L,K)).GT.THRES) THEN
#endif
                  NB=NBCK(KKR)+1
                  LBCK(1,NB,KKR)=LKR
                  LBCK(2,NB,KKR)=JI
#if defined (BIT64)
                  CBCK(NB,KKR)= CONJG(RL(L,K))
#else
                  CBCK(NB,KKR)=DCONJG(RL(L,K))
#endif
                  IF (NB.EQ.NSKSIZ) THEN
                    LREC=LBRCSK(0)+1
                    WRITE(INTSKEL,REC=LREC) NB,LBRCSK(KKR),
     +                   ((LBCK(IX1,IX2,KKR),IX1=1,2),IX2=1,NB),
     +                   (CBCK(IX,KKR),IX=1,NB)
                    LBRCSK(KKR)=LREC
                    LBRCSK(0)=LREC
                    NSKEL = NSKEL + NB
                    NB=0
                  ENDIF
                  NBCK(KKR)=NB
               ENDIF
   4        CONTINUE
   3     CONTINUE
      ELSE IF (ITYP.EQ.4) THEN
         DO 5 KKR = 1, NKR
            K = KRK(KKR)
            DO 6 LKR = 1, KKR-1
               L = KRL(LKR)
#if defined (BIT64)
               IF (CABS(RL(L,K)).GT.THRES) THEN
#else
               IF (CDABS(RL(L,K)).GT.THRES) THEN
#endif
                  NB=NBCK(KKR)+1
                  LBCK(1,NB,KKR)=LKR
                  LBCK(2,NB,KKR)=JI
#if defined (BIT64)
                  CBCK(NB,KKR)= CONJG(RL(L,K))
#else
                  CBCK(NB,KKR)=DCONJG(RL(L,K))
#endif
                  IF (NB.EQ.NSKSIZ) THEN
                    LREC=LBRCSK(0)+1
                    WRITE(INTSKEL,REC=LREC) NB,LBRCSK(KKR),
     +                   ((LBCK(IX1,IX2,KKR),IX1=1,2),IX2=1,NB),
     +                   (CBCK(IX,KKR),IX=1,NB)
                    LBRCSK(KKR)=LREC
                    LBRCSK(0)=LREC
                    NSKEL = NSKEL + NB
                    NB=0
                  ENDIF
                  NBCK(KKR)=NB
               ENDIF
   6        CONTINUE
   5     CONTINUE
      ENDIF
C
      RETURN
      END
