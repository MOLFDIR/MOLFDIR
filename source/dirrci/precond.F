C
      SUBROUTINE PRECOND (DGEL,VECTR,VECTI)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     ==================================
C     PRECONDITIONING OF RESIDUAL VECTOR
C     ==================================
C
      INCLUDE 'param.inc'
      INCLUDE 'general.inc'
      INCLUDE 'iterat.inc'
C
      DIMENSION VECTR(NDET,*),VECTI(NDET,*)
      DIMENSION DGEL(NDET)
C
      DO 200 IVEC = 1, NVEC - MVEC
      JROOT = IVEC
      E = EIGEN(NSEL(JROOT))
C
      DO 100 I = 1, NDET
      DE = DGEL(I) - E
#if defined (BIT64)
      IF (ABS(DE).LT.1.E-8) DE = SIGN(1.E-8,DE)
#else
      IF (DABS(DE).LT.1.D-8) DE = DSIGN(1.D-8,DE)
#endif
      VECTR(I,IVEC) = VECTR(I,IVEC) / DE
      IF (.NOT.REALAR)
     &   VECTI(I,IVEC) = VECTI(I,IVEC) / DE
  100 CONTINUE
  200 CONTINUE
      RETURN
      END
