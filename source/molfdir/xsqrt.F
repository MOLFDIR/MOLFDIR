      COMPLEX*16 FUNCTION XSQRT (A)
C
      PARAMETER (TRES=1.D-14) 
      COMPLEX*16 A
C
#if defined (BIT64)
      A = CSQRT(A)
      IF (ABS(REAL(A))/CABS(A).LT.TRES.AND.AIMAG(A).LT.0.0)
#else
      A = ZSQRT(A)
      IF (DABS(DREAL(A))/CDABS(A).LT.TRES.AND.DIMAG(A).LT.0.D0)
#endif
     &    A = - A
C
      XSQRT = A
C
      RETURN
      END
