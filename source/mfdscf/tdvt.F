C
C     ====================================================
      SUBROUTINE TDVT (TR, TI, NT, VR, VI, NV, RR, RI, NR,
     +                 WR, WI, NW, NL, NS)
C     ====================================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      DIMENSION TR (NT, NL), TI (NT, NL),
     +          VR (NV, NS), VI (NV, NS),
     +          RR (NR, NL), RI (NR, NL),
     +          WR (NW, NL), WI (NW, NL)
C
      DO 10 I = 1, NS
      DO 20 J = 1, NL
      WR (I, J) = 0.0D0
      WI (I, J) = 0.0D0
      DO 30 K = 1, NS
      WR (I, J) = WR (I, J) + VR (I, K) * TR (K, J)
     +                      - VI (I, K) * TI (K, J)
      WI (I, J) = WI (I, J) + VR (I, K) * TI (K, J)
     +                      + VI (I, K) * TR (K, J)
 30   CONTINUE
 20   CONTINUE
 10   CONTINUE
C
      DO 40 I = 1, NL
      DO 50 J = 1, NL
      RR (I, J) = 0.0D0
      RI (I, J) = 0.0D0
      DO 60 K = 1, NS
      RR (I, J) = RR (I, J) + TR (K, I) * WR (K, J)
     +                      + TI (K, I) * WI (K, J)
      RI (I, J) = RI (I, J) + TR (K, I) * WI (K, J)
     +                      - TI (K, I) * WR (K, J)
 60   CONTINUE
 50   CONTINUE
 40   CONTINUE
      RETURN
      END
