C
      SUBROUTINE ORTHN (VECTR,VECTI)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     =========================================================
C     SCHMIDT ORTHONORMALIZATION OF THE NEW EXPANSION VECTORS
C     ON THE PREVIOUS ONES (2 TIMES TO AVOID NUMERICAL PROBLEMS)
C     ==========================================================
C
      INCLUDE 'param.inc'
      INCLUDE 'general.inc'
C
      DIMENSION VECTR(NDET,*),VECTI(NDET,*)
C
      KVEC = NROOTS + 1
      DO 1000 IVEC = 1, NVEC - MVEC
C
#if defined (BIT64)
      RR = SDOT (NDET,VECTR(1,IVEC),1,VECTR(1,IVEC),1)
#else
      RR = DDOT (NDET,VECTR(1,IVEC),1,VECTR(1,IVEC),1)
#endif
      IF (.NOT.REALAR) RR = RR +
#if defined (BIT64)
     &     SDOT (NDET,VECTI(1,IVEC),1,VECTI(1,IVEC),1)
      RR = 1.D0 / SQRT (RR)
      CALL SSCAL (NDET,RR,VECTR(1,IVEC),1)
#else
     &     DDOT (NDET,VECTI(1,IVEC),1,VECTI(1,IVEC),1)
      RR = 1.D0 / DSQRT (RR)
      CALL DSCAL (NDET,RR,VECTR(1,IVEC),1)
#endif
      IF (.NOT.REALAR) 
#if defined (BIT64)
     &   CALL SSCAL (NDET,RR,VECTI(1,IVEC),1)
#else
     &   CALL DSCAL (NDET,RR,VECTI(1,IVEC),1)
#endif
C
      DO 200 IONR = 1, 2
      DO 100 JVEC = 1, MVEC + IVEC - 1
      CALL GETVEC (JVEC,VECTR(1,KVEC),VECTI(1,KVEC))
#if defined (BIT64)
      RR = SDOT (NDET,VECTR(1,KVEC),1,VECTR(1,IVEC),1)
#else
      RR = DDOT (NDET,VECTR(1,KVEC),1,VECTR(1,IVEC),1)
#endif
      IF (.NOT.REALAR) THEN
#if defined (BIT64)
         RI = SDOT (NDET,VECTR(1,KVEC),1,VECTI(1,IVEC),1)
         RI = RI - SDOT (NDET,VECTI(1,KVEC),1,VECTR(1,IVEC),1)
         RR = RR + SDOT (NDET,VECTI(1,KVEC),1,VECTI(1,IVEC),1)
#else
         RI = DDOT (NDET,VECTR(1,KVEC),1,VECTI(1,IVEC),1)
         RI = RI - DDOT (NDET,VECTI(1,KVEC),1,VECTR(1,IVEC),1)
         RR = RR + DDOT (NDET,VECTI(1,KVEC),1,VECTI(1,IVEC),1)
#endif
      ENDIF
#if defined (BIT64)
      CALL SAXPY (NDET,-RR,VECTR(1,KVEC),1,VECTR(1,IVEC),1)
#else
      CALL DAXPY (NDET,-RR,VECTR(1,KVEC),1,VECTR(1,IVEC),1)
#endif
      IF (.NOT.REALAR) THEN
#if defined (BIT64)
         CALL SAXPY (NDET,-RI,VECTR(1,KVEC),1,VECTI(1,IVEC),1)
         CALL SAXPY (NDET,-RR,VECTI(1,KVEC),1,VECTI(1,IVEC),1)
         CALL SAXPY (NDET, RI,VECTI(1,KVEC),1,VECTR(1,IVEC),1)
#else
         CALL DAXPY (NDET,-RI,VECTR(1,KVEC),1,VECTI(1,IVEC),1)
         CALL DAXPY (NDET,-RR,VECTI(1,KVEC),1,VECTI(1,IVEC),1)
         CALL DAXPY (NDET, RI,VECTI(1,KVEC),1,VECTR(1,IVEC),1)
#endif
      ENDIF
  100 CONTINUE
#if defined (BIT64)
      RR = SDOT (NDET,VECTR(1,IVEC),1,VECTR(1,IVEC),1)
#else
      RR = DDOT (NDET,VECTR(1,IVEC),1,VECTR(1,IVEC),1)
#endif
      IF (.NOT.REALAR) RR = RR +
#if defined (BIT64)
     &     SDOT (NDET,VECTI(1,IVEC),1,VECTI(1,IVEC),1)
      RR = 1.D0 / SQRT (RR)
      CALL SSCAL (NDET,RR,VECTR(1,IVEC),1)
#else
     &     DDOT (NDET,VECTI(1,IVEC),1,VECTI(1,IVEC),1)
      RR = 1.D0 / DSQRT (RR)
      CALL DSCAL (NDET,RR,VECTR(1,IVEC),1)
#endif
      IF (.NOT.REALAR) 
#if defined (BIT64)
     &   CALL SSCAL (NDET,RR,VECTI(1,IVEC),1)
#else
     &   CALL DSCAL (NDET,RR,VECTI(1,IVEC),1)
#endif
  200 CONTINUE
      IF (RR.GT.1.D6) THEN
         WRITE (*,*) ' NORM OF NEW VECTOR < 1.D-12 '
         WRITE (*,*)
         WRITE (*,*) '  -- SILLY END OF PROGRAM -- '
         STOP
      ENDIF
      CALL PUTVEC (MVEC+IVEC,VECTR(1,IVEC),VECTI(1,IVEC))
 1000 CONTINUE
C
      RETURN
      END
