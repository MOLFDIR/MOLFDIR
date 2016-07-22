C     ================================
      SUBROUTINE MTIMHR2(H,MH,R,MR,NA)
C     ================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE "vecs.inc"
C
      INTEGER MH,NA,I,J,IS
      COMPLEX*16 H(MH,*),R(MR,*)
C
      DO 10 I=1,NA
        DO 20 J=1,NA
          ITR = ABS(ITRMO(I))
          JTR = ABS(ITRMO(J))
          IS = - SIGN(1,ITRMO(I)*ITRMO(J))
          H(J,I) = R(J,I) + R(ITR,JTR)*IS
 20       CONTINUE
 10     CONTINUE
      RETURN
      END
