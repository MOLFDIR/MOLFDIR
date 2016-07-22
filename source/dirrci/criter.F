C
      SUBROUTINE CRITER (ENDRUN)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     =================================================
C     CHECKS IF ONE OF ENDRUN CONDITIONS IS FULFILLED :
C     1) CONVERGENCE REACHED
C     2) MAXITER REACHED
C     3) MAXIMUM CPU TIME USED
C     =================================================
C
      INCLUDE 'param.inc'
      INCLUDE 'general.inc'
      INCLUDE 'symm.inc'
      INCLUDE 'iterat.inc'
C
      LOGICAL ENDRUN
      DIMENSION MSEL(N8)
C
      open (83,file='Intermediate_results',form='FORMATTED')
      ENDRUN = .FALSE.
      CALL CPUUSED (CPUTOT)
      DO 10 IROOT = 1, NROOTS
      MSEL(IROOT) = NSEL(IROOT)
      CONV(IROOT) = EIGEN(NSEL(IROOT)) + ECORE - EIGEN2(IROOT)
      EIGEN2(IROOT) = EIGEN(NSEL(IROOT)) + ECORE
C
C  Determine whether the Davidson correction can be calculated
C
      IF (ITER.EQ.1) THEN
         EIGEN1(IROOT) = EIGEN2(IROOT)
C
C  The start vector should be within the reference space
C
         IF (1.D0-REFVEC(IROOT).LT.1.D-12) THEN
             DCORR(IROOT) = .TRUE.
         ELSE
             DCORR(IROOT) = .FALSE.
         ENDIF
C
C  Do not calculate correction in non SDCI cases and silly cases
C
         IF (MAXE3.NE.2.OR.NELEC.EQ.2) DCORR(IROOT) = .FALSE.
C
C  The reference space must be pre-diagonalized
C
         IF (REFRDU(IROOT).GT.1.D-12) DCORR(IROOT) = .FALSE.
      ENDIF
C
      ECORR = EIGEN2(IROOT) - EIGEN1(IROOT)
      WRITE (6,1000) ITER,IROOT,NSEL(IROOT),
     &               EIGEN2(IROOT),ECORR,CONV(IROOT),RESIDU(IROOT)
      write (83,1000) ITER,IROOT,NSEL(IROOT),
     &               EIGEN2(IROOT),ECORR,CONV(IROOT),RESIDU(IROOT)
C
#if defined (CRAY) || defined (LINUX) || defined (SUN)
      CALL FLUSH (6)
#endif
#if defined (IBM) 
      CALL FLUSH_(6)
#endif
C
      IF (ABS(CONV(IROOT)).LT.CONVERE.AND.ITER.NE.1) THEN
         MSEL(IROOT) = 0
      ENDIF

      IF (ABS(RESIDU(IROOT)).LT.CONVERR) THEN
         MSEL(IROOT) = 0
      ENDIF
   10 CONTINUE
C
      JROOT = 0
      DO 20 IROOT = 1, NROOTS
         IF (MSEL(IROOT).GT.0) THEN
            JROOT = JROOT + 1
          ENDIF
   20     CONTINUE
C
C     CHECK CRITERIA FOR ENDRUN :
C
      IF (JROOT.EQ.0) THEN
         WRITE (6,1010) 'Converged'
         ENDRUN = .TRUE.
      ENDIF
C
      IF (CPUTOT.GE.CPUMAX) THEN
         WRITE (6,1010) 'CPU time exceeded'
         ENDRUN = .TRUE.
      ENDIF
C
      IF (ITER.EQ.MAXITER) THEN
         WRITE (6,1010) 'Maxiter reached'
         ENDRUN = .TRUE.
      ENDIF
C
      IF (ENDRUN) THEN
         DO IROOT = 1, NROOTS
	  IF (DCORR(IROOT)) THEN
            ECORR = EIGEN2(IROOT)-EIGEN1(IROOT)
	    DAVCOR = (1.D0-REFVEC(IROOT))*ECORR
            WRITE (6,1020) IROOT,EIGEN2(IROOT),ECORR,DAVCOR,
     &                     EIGEN2(IROOT)+DAVCOR,RESIDU(IROOT)
	  ELSE
	    WRITE (6,1021) IROOT,EIGEN2(IROOT),EIGEN2(IROOT)-
     &                     EIGEN1(IROOT),(1.D0-REFVEC(IROOT)),
     &                     RESIDU(IROOT)
	  ENDIF
         ENDDO
         WRITE (6,1030) CPUTOT
         RETURN
      ENDIF
C
C     CONTINUE OPTIMIZATION OF NON-CONVERGED ROOTS :
C
      JROOT = 0
      DO 30 IROOT = 1, NROOTS
         IF (MSEL(IROOT).GT.0) THEN
            JROOT = JROOT + 1
             NSEL(JROOT) = MSEL(IROOT)
          ENDIF
   30     CONTINUE
      MVEC = NVEC
      NVEC = NVEC + JROOT
      close (83,status='KEEP')
C
      RETURN
C
 1000 FORMAT (3(1X,I4),1X,G22.12,1X,G18.12,2(1X,G10.4))
 1010 FORMAT (//' End of iterations : ',A25)
 1020 FORMAT (/' --- Root',I4,' ---'/
     &' (MR)CI-SD Energy :    ',F30.12/
     &' Correlation energy :  ',F30.12/
     &' Davidson correction : ',F30.12/
     &' (MR)CI-SD+Q energy:   ',F30.12/
     &' Residue :             ',F30.12)
 1021 FORMAT (/' --- Root',I4,' ---'/
     &' CI Total Energy :       ',F30.12/
     &' Relative to reference : ',F30.12/
     &' 1 - Reference weight :  ',F30.12/
     &' Residue :               ',F30.12)
 1030 FORMAT (/' Total CPU time :     ',F10.3)
C
      END
