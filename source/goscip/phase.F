C
C     =====================
      FUNCTION PHASE(I, ID)
C     =====================
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INCLUDE 'param.inc'
      INCLUDE 'general.inc'
C
#if defined (CRAY)
      ISHIFT (I, J) = SHIFT (I, J)
      IAND (I, J) = AND (I, J)
#else
      ISHIFT (I, J) = ISHFT (I, J)
#endif
C
      P = 1.0D0
      DO 10 K = I + 1, NORB
        IF (IAND (ID, ISHIFT (1, K - 1)) .NE. 0) P = - P
 10     CONTINUE
      PHASE = P
      RETURN
      END
