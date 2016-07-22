#if defined (BIT64)
      REAL*8 FUNCTION SCAMAX(N,X,INCX)
#else
      REAL*8 FUNCTION DZAMAX(N,X,INCX)
#endif
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C---------------Description--------------------------------------------
C
C     Gives maximum absolute value of vector X
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      INTEGER N,INCX
      COMPLEX*16 X(*)
C
C---------------Common Blocks--------------------------------------
C
C---------------Local variables--------------------------------------
C
      REAL*8 A,B
C
C---------------Executable code--------------------------------------
C
#if defined (BIT64)
      A = 0.0
#else
      A = 0.D0
#endif
      II = 1
      DO I = 1, N
#if defined (BIT64)
         B = CABS(X(II))
#else
         B = CDABS(X(II))
#endif
         IF (A.LT.B) A = B
         II = II + INCX
      ENDDO
C
#if defined (BIT64)
      SCAMAX = A
#else
      DZAMAX = A
#endif
C
      RETURN
      END

