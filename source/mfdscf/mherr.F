C
C     ===============================
      SUBROUTINE MHERR (AR, AI, N, M)
C     ===============================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     MAKE HERMITIAN MATRIX FROM LOWER TRIANGLE OF ROWWISE MATRIX OR
C          FROM UPPER TRIANGLE OF NORMAL MATRIX
C
      REAL*8 AR (N, N), AI (N, N)
      DO 10 I = 1, M
      DO 20 J = 1, I - 1
      AR (I, J) = AR (J, I)
      AI (I, J) = - AI (J, I)
   20 CONTINUE
   10 CONTINUE
      RETURN
      END
