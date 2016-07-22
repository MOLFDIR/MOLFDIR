C
C     =================================================
      SUBROUTINE HMV (M, N, NA, AR, AI, BR, BI, CR, CI)
C     =================================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      DIMENSION AR (NA, NA), AI (NA, NA)
      DIMENSION BR (N), BI (N), CR (M), CI (M)
      DO 30 I = 1, M
      CR (I) = 0.0D0
      CI (I) = 0.0D0
   30 CONTINUE
      DO 10 I = 1, M
      DO 20 J = 1, N
      CR (I) = CR (I) + AR (I, J) * BR (J) - AI (I, J) * BI (J)
      CI (I) = CI (I) + AR (I, J) * BI (J) + AI (I, J) * BR (J)
   20 CONTINUE
   10 CONTINUE
      RETURN
      END
