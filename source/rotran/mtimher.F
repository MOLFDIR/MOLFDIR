C
C     ====================================
      SUBROUTINE MTIMHER(H,MH,R,MR,NA,ISG)
C     ====================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE "vecs.inc"
C
      INTEGER MH,NA,I,J,ISG
      COMPLEX*16 H(MH,*),R(MR,*)
C
      DO 10 I=1,NA
        DO 20 J=1,NA
          ITR = ABS(ITRMO(I))
          JTR = ABS(ITRMO(J))
          IS = SIGN(1,ITRMO(I)*ITRMO(J)) * ISG
          H(J,I) = R(J,I) + R(ITR,JTR)*IS
#if defined (BIT64)
     &           + CONJG(R(I,J)) + CONJG(R(JTR,ITR))*IS
#else
     &           + DCONJG(R(I,J)) + DCONJG(R(JTR,ITR))*IS
#endif
 20       CONTINUE
 10     CONTINUE
      RETURN
      END
