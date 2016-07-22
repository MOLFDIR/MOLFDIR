C
C     ==========================
      SUBROUTINE PRTMAT(N,AR,AI)
C     ==========================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     PRINT LOWER TRIANGLE OF COMPLEX*16 HERMITIAN MATRIX
C     N---DIMENSION OF MATRIX
C     AR--REAL PART OF TRIANGLE
C     AI--IMAGINARY PART OF TRIANGLE
C
      DIMENSION AR(N*(N+1)/2),AI(N*(N+1)/2)
      KK=0
      DO 88 II=1,N
      LL=KK+1
      KK=KK+II
      PRINT 888,(AR(JJ),AI(JJ),JJ=LL,KK)
  888 FORMAT(10('(',F3.1,',',F3.1,')'))
  88  CONTINUE
      RETURN
      END
