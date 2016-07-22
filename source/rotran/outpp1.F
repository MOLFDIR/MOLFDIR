      SUBROUTINE OUTPP1 (NKR,NSP,ITYP,INTSKEL,THRES,KRK,KRL,
     & IKR,JKR,INDKL,RL,RKL,NSKEL)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      COMPLEX*16 RL(NSP,NSP),RKL(*)
      INTEGER INDKL(*),KRK(NKR),KRL(NKR)
C
C     4 SETS ARE WRITTEN : 1a) (IBAR,JBAR|KBAR,LBAR)
C                          1b) (IBAR,JBAR|LBAR,KBAR)
C                          2)  (IBAR,J   |KBAR,L   )
C                          3)  (IBAR,J   |K   ,LBAR)
C                          4a) (IBAR,J   |K   ,L   )
C                          4b) (IBAR,J   |L   ,K   )
C
C     1) CANONICAL LIST  : (I => J), (K => L), (IJ => KL)
C     2) CANONICAL LIST  : (I => J), (K => L), (IJ => KL)
C     3) CANONICAL LIST  : (I => J), (K => L), (IJ => KL)
C     4) CANONICAL LIST  : (I => J), (K => L)
C
C     WRITE ONE OF THESE SETS WITH THE RESTRICTION DETERMINED BY THE
C     LABEL ITYP
C
      NZ = 0
      KKRMAX = IKR
      IF (ITYP.EQ.4) KKRMAX = NKR
      DO 1 KKR = 1, KKRMAX
         K = KRK(KKR)
         LKRMAX = KKR
         IF (ITYP.LE.3.AND.IKR.EQ.KKR) LKRMAX = JKR
         DO 2 LKR = 1, LKRMAX
            L = KRL(LKR)
#if defined (BIT64)
            IF (CABS(RL(K,L)).GT.THRES) THEN
#else
            IF (CDABS(RL(K,L)).GT.THRES) THEN
#endif
               NZ = NZ + 1
               INDKL(NZ) = (KKR-1)*NKR+LKR
               RKL(NZ) = RL(K,L)
            ENDIF
   2     CONTINUE
   1  CONTINUE
C
C     Write the upper triangle for types 1 and 4
C
      IF ((ITYP.EQ.1.AND.IKR.NE.JKR).OR.ITYP.EQ.4) THEN
         DO 3 KKR = 1, KKRMAX
            K = KRK(KKR)
            LKRMAX = KKR
            IF (ITYP.LE.3.AND.IKR.EQ.KKR) LKRMAX = JKR
            IF (LKRMAX.EQ.KKR) LKRMAX = LKRMAX - 1
            DO 4 LKR = 1, LKRMAX
               L = KRL(LKR)
#if defined (BIT64)
               IF (CABS(RL(L,K)).GT.THRES) THEN
#else
               IF (CDABS(RL(L,K)).GT.THRES) THEN
#endif
                  NZ = NZ + 1
                  INDKL(NZ) = (LKR-1)*NKR+KKR
                  RKL(NZ) = RL(L,K)
               ENDIF
   4        CONTINUE
   3     CONTINUE
      ENDIF
C
      WRITE (INTSKEL) IKR,JKR,NZ,
     &               (INDKL(INZ),INZ=1,NZ),
     &               (RKL(INZ),INZ=1,NZ)
      NSKEL = NSKEL + NZ
C
      RETURN
      END
