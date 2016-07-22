C
      SUBROUTINE GENTAB
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     GENERATES TABLES NECESSARY TO EVALUATE THE INCOMPLETE GAMMA FUNCTION
C
      INCLUDE 'paracom.inc'
C
      DIMENSION TERM(0:120),PTLSUM(0:120),T0(0:120),Y(0:120)
      LOGICAL CONV
C
      DO 10 L = 0, JMAX
      XU(L) = DBLE (2 * L + 1)
      XD(L) = 1.0D0 / XU (L)
   10 CONTINUE
C
      A = DBLE (JMAX) + 0.5D0
      B = 1 / A
      DO 20 N = 0, 120
      TERM(N) = B
      PTLSUM(N) = B
      T0(N) = DBLE(N) * 0.1D0
   20 CONTINUE
      NC = 0
   30 DO 50 I = 1, 25
      A = A + 1.0D0
      B = 1 / A
      DO 40 N = 0, 120
      TERM(N) = TERM(N) * T0(N) * B
      PTLSUM(N) = PTLSUM(N) + TERM(N)
   40 CONTINUE
   50 CONTINUE
C     -----------------
C     CHECK CONVERGENCE
C     -----------------
      CONV = .TRUE.
      DO 60 N = 0, 120
      CONVER = TERM(N) / PTLSUM(N)
      IF (CONVER.GT.ACC) CONV = .FALSE.
   60 CONTINUE
      IF (.NOT.CONV) THEN
         NC = NC + 1
         IF (NC.GE.4) CALL Q2JOB (6,'NO CONVERGENCE IN GENTAB',' ',0)
         GOTO 30
      ENDIF
      DO 70 N = 0, 120
#if defined (BIT64)
      Y(N) = EXP (-T0(N))
#else
      Y(N) = DEXP (-T0(N))
#endif
      STR(N,JMAX) = 0.5D0 * PTLSUM(N) * Y(N)
      T0(N) = 2.0D0 * T0(N)
   70 CONTINUE
C     ------------------
C     DOWNWARD RECURSION
C     ------------------
      DO 90 J = 1, JMAX
      DO 80 N = 0, 120
      STR(N,JMAX-J) = (T0(N) * STR(N,JMAX-J+1) + Y(N)) * XD(JMAX-J)
   80 CONTINUE
   90 CONTINUE
      RETURN
      END
