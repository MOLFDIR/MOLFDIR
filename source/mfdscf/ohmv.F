C
C     ===============================================
      SUBROUTINE OHMV (N, NA, AR, AI, BR, BI, CR, CI)
C     ===============================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      DIMENSION AR (NA, NA), AI (NA, NA)
      DIMENSION BR (N), BI (N), CR (N), CI (N)
      DO 30 I = 1, N
      CR (I) = 0.0D0
      CI (I) = 0.0D0
   30 CONTINUE
      DO 10 I = 1, N
      DO 20 J = 1, N
      CR (I) = CR (I) + AR (J, I) * BR (J) + AI (J, I) * BI (J)
      CI (I) = CI (I) + AR (J, I) * BI (J) - AI (J, I) * BR (J)
   20 CONTINUE
   10 CONTINUE
      RETURN
      END
