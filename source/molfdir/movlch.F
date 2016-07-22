C
C     ===========================
      SUBROUTINE MOVLCH (A, B, N)
C     ===========================
      CHARACTER*(*) A, B
      DO 1 I = 1, N
      B (I:I) = A (I:I)
    1 CONTINUE
      RETURN
      END
