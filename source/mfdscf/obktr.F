C
C     ===============================================
      SUBROUTINE OBKTR (N, NU, UR, UI, D, NH, HR, HI)
C     ===============================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     CALCULTATE H = U D U+, MET D REEL EN DIAGONAAL
C
      REAL*8 UR (NU, NU), UI (NU, NU)
      REAL*8 HR (NH, NH), HI (NH, NH)
      REAL*8 D (N)
      DO 10 I = 1, N
      DO 20 J = 1, N
      SUMR = 0.0D0
      SUMI = 0.0D0
      DO 30 K = 1, N
      SUMR = SUMR + UR (I, K) * D (K) * UR (J, K)
     +            + UI (I, K) * D (K) * UI (J, K)
      SUMI = SUMI - UR (I, K) * D (K) * UI (J, K)
     +            + UI (I, K) * D (K) * UR (J, K)
   30 CONTINUE
      HR (I, J) = SUMR
      HI (I, J) = SUMI
   20 CONTINUE
   10 CONTINUE
      RETURN
      END
