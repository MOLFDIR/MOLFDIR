C
      SUBROUTINE INPROD (VECTR,VECTI,SIGMAR,SIGMAI)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     =================================================================
C     COMPUTES INNER PRODUCT OF NEW SIGMA VECTORS WITH PREVIOUS VECTORS
C     =================================================================
C
      INCLUDE 'param.inc'
      INCLUDE 'general.inc'
      INCLUDE 'iterat.inc'
C
      DIMENSION VECTR(NDET,*),VECTI(NDET,*)
      DIMENSION SIGMAR(NDET,*),SIGMAI(NDET,*)
C
      KVEC = NROOTS + 1
C
C     Real or Complex arithmetic
C
      IF (REALAR) THEN
         DO 201 JVEC = 1, NVEC
         CALL GETVEC (JVEC,VECTR(1,KVEC),VECTI(1,KVEC))
         DO 101 IVEC = MAX0(1,JVEC-MVEC), NVEC - MVEC
         LVEC = IVEC + MVEC
C
#if defined (BIT64)
         CMTR(LVEC,JVEC) = SDOT (NDET,SIGMAR(1,IVEC),1,VECTR(1,KVEC),1)
#else
         CMTR(LVEC,JVEC) = DDOT (NDET,SIGMAR(1,IVEC),1,VECTR(1,KVEC),1)
#endif
  101    CONTINUE
  201    CONTINUE
      ELSE
         DO 200 JVEC = 1, NVEC
         CALL GETVEC (JVEC,VECTR(1,KVEC),VECTI(1,KVEC))
         DO 100 IVEC = MAX0(1,JVEC-MVEC), NVEC - MVEC
         LVEC = IVEC + MVEC
C
#if defined (BIT64)
         CMTR(LVEC,JVEC) = SDOT (NDET,SIGMAR(1,IVEC),1,VECTR(1,KVEC),1)
     &                   + SDOT (NDET,SIGMAI(1,IVEC),1,VECTI(1,KVEC),1)
         CMTI(LVEC,JVEC) = SDOT (NDET,SIGMAR(1,IVEC),1,VECTI(1,KVEC),1)
     &                   - SDOT (NDET,SIGMAI(1,IVEC),1,VECTR(1,KVEC),1)
#else
         CMTR(LVEC,JVEC) = DDOT (NDET,SIGMAR(1,IVEC),1,VECTR(1,KVEC),1)
     &                   + DDOT (NDET,SIGMAI(1,IVEC),1,VECTI(1,KVEC),1)
         CMTI(LVEC,JVEC) = DDOT (NDET,SIGMAR(1,IVEC),1,VECTI(1,KVEC),1)
     &                   - DDOT (NDET,SIGMAI(1,IVEC),1,VECTR(1,KVEC),1)
#endif
  100    CONTINUE
  200    CONTINUE
      ENDIF
C
      RETURN
      END
