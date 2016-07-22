      SUBROUTINE AZERO(A,I)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 A(I)
      DO 100 J = 1, I
      A(J) = 0.0D0
  100 CONTINUE
      RETURN
      END
