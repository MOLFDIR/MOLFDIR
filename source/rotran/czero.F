      SUBROUTINE CZERO (LV,VECTOR)
      IMPLICIT REAL*8 (A-H, O-Z)
      COMPLEX*16 VECTOR (LV)
      INTEGER LV
      DO 100 I = 1, LV
#if defined (BIT64)
      VECTOR (I) = (0.0,0.0)
#else
      VECTOR (I) = (0.D0,0.D0)
#endif
  100 CONTINUE
      RETURN
      END
