      SUBROUTINE GENTAB1
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     ==============
C     GENERATE TABLE
C     ==============
C
      INCLUDE 'paracom.inc'
C
      DIMENSION X(13)
      EQUIVALENCE(X1,X)
C
      DO 10 J=1,13
#if defined (BIT64)
      X(J)=1.0/FLOAT(J+J-1)
#else
      X(J)=1.0D0/DBLE(J+J-1)
#endif
   10 CONTINUE
      RETURN
      END
