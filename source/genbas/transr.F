C     ===================================
      SUBROUTINE TRANSR (NCO,CCR,CCI)
C     ===================================

      IMPLICIT REAL*8 (A-H, O-Z)
C
C     TRANSFORM TO REAL CONTRACTION COEFFICIENTS AND CHECK
C     THE PHASE OF THE COEFFICIENTS (MUST BE CONSTANT) IN THE MO
C
      INCLUDE 'paracom.inc'
C
      DIMENSION CCR(N17),CCI(N17)
C
      SINA = 0.0D0
      COSA = 0.0D0
      CNORMMX = 0.0D0
      DO 10 J = 1, NCO
#if defined (BIT64)
      CNORM = SQRT ( CCR(J)**2+CCI(J)**2)
#else
      CNORM = DSQRT( CCR(J)**2+CCI(J)**2)
#endif
      IF (CNORM.GT.CNORMMX) THEN
        CNORMMX = CNORM
        JNORMMX = J
      ENDIF
 10   CONTINUE
      SINA = CCI(JNORMMX) / CNORMMX
      COSA = CCR(JNORMMX) / CNORMMX
      DO 15 J = 1, NCO
#if defined (BIT64)
      CNORM = SQRT( CCR(J)**2+CCI(J)**2)
#else
      CNORM = DSQRT( CCR(J)**2+CCI(J)**2)
#endif
      CCR (J) = CCR(J) * COSA + CCI(J) * SINA
#if defined (BIT64)
      IF ( ABS(ABS(CCR(J))-CNORM) .GT. TOLER )  THEN
#else
      IF ( DABS(DABS(CCR(J))-CNORM) .GT. TOLER )  THEN
#endif
          PRINT*,'J,CCR(J),CNORM : ',J,CCR(J),CNORM
c         CALL Q2JOB  (6,'PHASES DIFFERENT IN TRANSR',0,0)
      ENDIF
#if defined (BIT64)
      CCR (J) = CNORM * SIGN(1.0,CCR(J))
#else
      CCR (J) = CNORM * DSIGN(1.0D0,CCR(J))
#endif
   15 CONTINUE
      RETURN
      END
