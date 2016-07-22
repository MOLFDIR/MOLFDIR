C
C     ===============================
      SUBROUTINE MHERM (AR, AI, N, M)
C     ===============================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     MAKE HERMITIAN MATRIX FROM UPPER TRIANGLE OF ROWWISE MATRIX OR
C          FROM LOWER TRIANGLE OF NORMAL MATRIX
C
      DIMENSION AR (N, N), AI (N, N)
      DO 10 I = 1, M
      DO 20 J = 1, I - 1
      AR (J, I) = AR (I, J)
      AI (J, I) = - AI (I, J)
   20 CONTINUE
   10 CONTINUE
      RETURN
      END
