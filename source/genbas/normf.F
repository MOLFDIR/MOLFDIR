C     =========================================
      SUBROUTINE NORMF (RNT,NPR,EXPON,C1,CNORM)
C     =========================================
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'paracom.inc'
C
      DIMENSION C1(N17),EXPON(N17)
C
      CALL OVRLAP (RNT,NPR,EXPON,C1,C1,OVLAP)
      IF (OVLAP.LT.TOLER) THEN
         CNORM = 0.0D0
      ELSE
#if defined (BIT64)
         CNORM = SQRT (OVLAP)
#else
         CNORM = DSQRT (OVLAP)
#endif
         DO 100 J = 1, NPR
         C1 (J) = C1 (J) / CNORM
  100    CONTINUE
      ENDIF
      RETURN
      END
