C
C     ======================
      FUNCTION XDOT (A, B, N)
C     ======================
C
      REAL*8 A (*), B (*)
      REAL*8 SUM, XDOT
      INTEGER I, N
C
      SUM = 0.0D0
      DO 10 I = 1, N
        SUM = SUM + A (I) * B (I)
 10   CONTINUE
      XDOT = SUM
      RETURN
      END
