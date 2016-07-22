C
      SUBROUTINE COMFD(AR,AI,BR,BI,CR,CI,L,LC)
      IMPLICIT REAL*8 (A-H, O-Z)
C
C === THIS ROUTINE COMPUTES THE COMMUTATOR BETWEEN THE COMPLEX
C === MATRICES A AND B
C
      REAL*8 AR(L,L), AI(L,L)
      REAL*8 BR(L,L), BI(L,L)
      REAL*8 CR(*), CI(*)
C
C === COMPUTE AB AND STORE IT IN C
C
      IJ = 0
      DO 100 I = 1, LC
      DO 110 J = 1, I
      IJ = IJ + 1
      CR(IJ) = 0D0
      CI(IJ) = 0D0
      DO 120 K = 1, LC
      CR(IJ) = CR(IJ) + AR(I,K) * BR(K,J)
     +                - AI(I,K) * BI(K,J)
      CI(IJ) = CI(IJ) + AI(I,K) * BR(K,J)
     +                + AR(I,K) * BI(K,J)
  120 CONTINUE
  110 CONTINUE
  100 CONTINUE
C
C === COMPUTE BA AND SUBTRACT IT FROM C
C
      IJ = 0
      DO 130 I = 1, LC
      DO 140 J = 1, I
      IJ = IJ + 1
      DO 150 K = 1, LC
      CR(IJ) = CR(IJ) - BR(I,K) * AR(K,J)
     +                + BI(I,K) * AI(K,J)
      CI(IJ) = CI(IJ) - BR(I,K) * AI(K,J)
     +                - BI(I,K) * AR(K,J)
  150 CONTINUE
  140 CONTINUE
  130 CONTINUE
C
      RETURN
      END
