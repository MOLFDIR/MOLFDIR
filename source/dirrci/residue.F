C
      SUBROUTINE RESIDUE (VECTR,VECTI,SIGMAR,SIGMAI)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'param.inc'
      INCLUDE 'general.inc'
      INCLUDE 'iterat.inc'
C
      DIMENSION VECTR(NDET,*),VECTI(NDET,*)
      DIMENSION SIGMAR(NDET,*),SIGMAI(NDET,*)
C
      DO IROOT = 1, NROOTS
      IVEC = IROOT
      CALL ZZERO (NDET,VECTR(1,IVEC))
      IF (.NOT.REALAR) CALL ZZERO (NDET,VECTI(1,IVEC))
      ENDDO
C
C     Calculate residue : R = HC - EC = Sig.B - E.D.B
C     Start with EC part.
      KVEC = NROOTS + 1
      DO 20 N = 1, NVEC
      CALL GETVEC (N,VECTR(1,KVEC),VECTI(1,KVEC))
      DO 10 IROOT = 1, NROOTS
      IVEC = IROOT
      ER = - EIGEN(NSEL(IROOT)) * EVECR(N,NSEL(IROOT))
#if defined (BIT64)
      CALL SAXPY (NDET,ER,VECTR(1,KVEC),1,VECTR(1,IVEC),1)
#else
      CALL DAXPY (NDET,ER,VECTR(1,KVEC),1,VECTR(1,IVEC),1)
#endif
      IF (.NOT.REALAR) THEN
         EI = - EIGEN(NSEL(IROOT)) * EVECI(N,NSEL(IROOT))
#if defined (BIT64)
         CALL SAXPY (NDET,EI,VECTR(1,KVEC),1,VECTI(1,IVEC),1)
         CALL SAXPY (NDET,ER,VECTI(1,KVEC),1,VECTI(1,IVEC),1)
#else
         CALL DAXPY (NDET,EI,VECTR(1,KVEC),1,VECTI(1,IVEC),1)
         CALL DAXPY (NDET,ER,VECTI(1,KVEC),1,VECTI(1,IVEC),1)
#endif
         EI = - EI
#if defined (BIT64)
         CALL SAXPY (NDET,EI,VECTI(1,KVEC),1,VECTR(1,IVEC),1)
#else
         CALL DAXPY (NDET,EI,VECTI(1,KVEC),1,VECTR(1,IVEC),1)
#endif
      ENDIF
   10 CONTINUE
   20 CONTINUE
C     Calculate HC part.
      DO 40 N = 1, NVEC
      CALL GETSIG (N,VECTR(1,KVEC),VECTI(1,KVEC))
      DO 30 IROOT = 1, NROOTS
      IVEC = IROOT
      ER = EVECR(N,NSEL(IROOT))
#if defined (BIT64)
      CALL SAXPY (NDET,ER,VECTR(1,KVEC),1,VECTR(1,IVEC),1)
#else
      CALL DAXPY (NDET,ER,VECTR(1,KVEC),1,VECTR(1,IVEC),1)
#endif
      IF (.NOT.REALAR) THEN
         EI = EVECI(N,NSEL(IROOT))
#if defined (BIT64)
         CALL SAXPY (NDET,ER,VECTI(1,KVEC),1,VECTI(1,IVEC),1)
         CALL SAXPY (NDET,EI,VECTR(1,KVEC),1,VECTI(1,IVEC),1)
#else
         CALL DAXPY (NDET,ER,VECTI(1,KVEC),1,VECTI(1,IVEC),1)
         CALL DAXPY (NDET,EI,VECTR(1,KVEC),1,VECTI(1,IVEC),1)
#endif
         EI = - EI
#if defined (BIT64)
         CALL SAXPY (NDET,EI,VECTI(1,KVEC),1,VECTR(1,IVEC),1)
#else
         CALL DAXPY (NDET,EI,VECTI(1,KVEC),1,VECTR(1,IVEC),1)
#endif
      ENDIF
   30 CONTINUE
   40 CONTINUE
C
C     Calculate norm of the residual vector
C
      DO 50 IROOT = 1, NROOTS
      IVEC = IROOT
#if defined (BIT64)
      RES = SDOT (NDET,VECTR(1,IVEC),1,VECTR(1,IVEC),1)
#else
      RES = DDOT (NDET,VECTR(1,IVEC),1,VECTR(1,IVEC),1)
#endif
      IF (.NOT.REALAR) RES = RES +
#if defined (BIT64)
     &      SDOT (NDET,VECTI(1,IVEC),1,VECTI(1,IVEC),1)
#else
     &      DDOT (NDET,VECTI(1,IVEC),1,VECTI(1,IVEC),1)
#endif
      RESIDU(IROOT) = RES
   50 CONTINUE
C
C     Calculate norm of reference part of the residual vector
C
      DO 60 IROOT = 1, NROOTS
      IVEC = IROOT
#if defined (BIT64)
      RES = SDOT (NREFDET,VECTR(1,IVEC),1,VECTR(1,IVEC),1)
#else
      RES = DDOT (NREFDET,VECTR(1,IVEC),1,VECTR(1,IVEC),1)
#endif
      IF (.NOT.REALAR) RES = RES +
#if defined (BIT64)
     &      SDOT (NREFDET,VECTI(1,IVEC),1,VECTI(1,IVEC),1)
#else
     &      DDOT (NREFDET,VECTI(1,IVEC),1,VECTI(1,IVEC),1)
#endif
      REFRDU(IROOT) = RES
   60 CONTINUE
C
      RETURN
      END
