C
      FUNCTION FMCH (M,X,Y)
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     ===================================================
C     THIS FUNCTION EVALUATES THE INTEGRAL FROM 0 TO 1 OF
C                (U**(2*M)) * EXPF(-X*(U**2))
C     ===================================================
C
      INCLUDE 'paracom.inc'
C
#if defined (BIT64)
      A=REAL(M)
#else
      A=DBLE(M)
#endif
      IF (X.LE.10.0D0) THEN
          A=A+0.5D0
          TERM=1.0D0/A
          PTLSUM=TERM
          DO 1 I=2,50
          A=A+1.0D0
          TERM=TERM*X/A
          PTLSUM=PTLSUM+TERM
          IF (TERM/PTLSUM.LT.0.00000001) GOTO 2
    1     CONTINUE
          WRITE (*,10) M,X
          STOP 'NO CONVERGENCE FOR FMCH'
    2     FMCH=0.5D0*PTLSUM*Y
      ELSE
          B=A+0.5D0
          A=A-0.5D0
          XD=1.0D0/X
#if defined (BIT64)
          APPROX=PIROOT*(SQRT(XD)*XD**M)
#else
          APPROX=PIROOT*(DSQRT(XD)*XD**M)
#endif
          DO 3 I=1,M
          B=B-1.0D0
    3     APPROX=APPROX*B
          FIMULT=0.5D0*Y*XD
          PTLSUM=0.0D0
          IF (FIMULT.EQ.0.0) GOTO 5
          FIPROP=FIMULT/APPROX
          TERM=1.0D0
          PTLSUM=TERM
          NOTRMS=INT(X)
          NOTRMS=NOTRMS+M
          DO 4 I=2,NOTRMS
          TERM=TERM*A*XD
          PTLSUM=PTLSUM+TERM
#if defined (BIT64)
          IF (ABS (TERM*FIPROP/PTLSUM).LE.0.00000001)  GOTO 5
#else
          IF (DABS (TERM*FIPROP/PTLSUM).LE.0.00000001D0)  GOTO 5
#endif
    4     A=A-1.0D0
          WRITE(*,10) M,X
          STOP'NO CONVERGENCE FOR FMCH'
    5     FMCH=APPROX-FIMULT*PTLSUM
      ENDIF
      RETURN
   10 FORMAT (' NO CONVERGENCE FOR FMCH', I6, E16.9)
      END
