      SUBROUTINE FGAMMA(MS,KLPRIM,R,Z,TT,TS,Y,
     &                  IND,ITNEAR,DELTA,FS1,FS2,F)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C---------------Description--------------------------------------------
C
C     Evaluates incomplete gamma function.
C     Necessary information in R,Z
C     Values stored in array F
C     Scratch arrays TT,TS,Y,IND,ITNEAR,DELTA,FS1,FS2
C     Based on old RELTWEL routine
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      INTEGER KLPRIM
      REAL*8 R(KLPRIM,3),F(KLPRIM,0:MS),Z(KLPRIM),
     &       TT(KLPRIM),TS(KLPRIM),Y(KLPRIM),DELTA(KLPRIM),
     &       FS1(KLPRIM),FS2(KLPRIM)
      INTEGER IND(KLPRIM),ITNEAR(KLPRIM)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE 'paracom.inc'  
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
C
      DO 10 KL = 1, KLPRIM
      TT(KL) = R(KL,1) * R(KL,1) + R(KL,2) * R(KL,2) +
     &         R(KL,3) * R(KL,3)
      TT(KL) = TT(KL) * Z(KL) * 0.25D0
#if defined (BIT64)
      Y(KL) = EXP(-TT(KL))
#else
      Y(KL) = DEXP(-TT(KL))
#endif
   10 CONTINUE
C     -----------------------
C     TABLE LOOKUP FOR T < 12
C     -----------------------
      N = 0
      DO 20 KL = 1, KLPRIM
      IF (TT(KL).LT.12.D0) THEN
         N = N + 1
         TS(N) = TT(KL)
         IND(N) = KL
      ENDIF
   20 CONTINUE
      DO 30 IJKLS = 1, N
      ITNEAR(IJKLS) = NINT (10.D0 * TS(IJKLS))
      TNEAR = (DBLE (ITNEAR(IJKLS))) * 0.1D0
      DELTA(IJKLS) = TNEAR - TS(IJKLS)
      FS1(IJKLS) = STR(ITNEAR(IJKLS),MS+6) * FACT (6)
   30 CONTINUE
      DO 50 K = 1, 6
      J = MS + 6 - K
      DO 40 IJKLS = 1, N
      FS2(IJKLS) = STR(ITNEAR(IJKLS),J) * FACT (6-K)
      FS1(IJKLS) = FS2(IJKLS) + FS1(IJKLS) * DELTA(IJKLS)
   40 CONTINUE
   50 CONTINUE
C$DIR NO_RECURRENCE
CDIR$ IVDEP
      DO 60 IJKLS = 1, N
      F(IND(IJKLS),MS) = FS1(IJKLS)
   60 CONTINUE
C     ------------------
C     DOWNWARD RECURSION
C     ------------------
      DO 68 IJKLS = 1, N
      TS(IJKLS) = 2.D0 * TS(IJKLS)
   68 CONTINUE
      DO 70 J = 1, MS
C$DIR NO_RECURRENCE
CDIR$ IVDEP
      DO 69 IJKLS = 1, N
      IJKL = IND(IJKLS)
      F(IJKL,MS-J) = (TS(IJKLS) * F(IJKL,MS-J+1) + Y(IJKL)) * XD(MS-J)
   69 CONTINUE
   70 CONTINUE
      IF (N.EQ.KLPRIM) RETURN
C     --------------------------
C     4 TERM FIT FOR 12 < T < 15
C     --------------------------
      N = 0
      DO 80 KL = 1, KLPRIM
      IF (TT(KL).GE.12.D0.AND.TT(KL).LT.15.D0) THEN
         N = N + 1
         TS(N) = TT(KL)
         IND(N) = KL
      ENDIF
   80 CONTINUE
      M1 = N
      DO 90 IJKLS = 1, M1
      DELTA(IJKLS) = 1.D0 / TS(IJKLS)
      FS1(IJKLS) = 0.321180909D0 - 0.3811559346D0 * DELTA(IJKLS)
      FS1(IJKLS) = - 0.2473631686D0  + FS1(IJKLS) * DELTA(IJKLS)
      FS1(IJKLS) = 0.4999489092D0  + FS1(IJKLS) * DELTA(IJKLS)
   90 CONTINUE
      IF (N.EQ.KLPRIM) GOTO 199
C     --------------------------
C     3 TERM FIT FOR 15 < T < 18
C     --------------------------
      DO 110 KL = 1, KLPRIM
      IF (TT(KL).GE.15.D0.AND.TT(KL).LT.18.D0) THEN
         N = N + 1
         TS(N) = TT(KL)
         IND(N) = KL
      ENDIF
  110 CONTINUE
      M2 = N
      DO 120 IJKLS = M1+1, M2
      DELTA(IJKLS) = 1.D0 / TS(IJKLS)
      FS1(IJKLS) = - 0.24249438D0 + 0.24642845D0 * DELTA(IJKLS)
      FS1(IJKLS) = 0.4998436875D0 + FS1(IJKLS) * DELTA(IJKLS)
  120 CONTINUE
      IF (N.EQ.KLPRIM) GOTO 199
C     --------------------------
C     2 TERM FIT FOR 18 < T < 24
C     --------------------------
      DO 140 KL = 1, KLPRIM
      IF (TT(KL).GE.18.D0.AND.TT(KL).LT.24.D0) THEN
         N = N + 1
         TS(N) = TT(KL)
         IND(N) = KL
      ENDIF
  140 CONTINUE
      M3 = N
      DO 150 IJKLS = M2+1, M3
      DELTA(IJKLS) = 1.D0 / TS(IJKLS)
      FS1(IJKLS) = 0.499093162D0 + 0.2152832D0 * DELTA(IJKLS)
  150 CONTINUE
      IF (N.EQ.KLPRIM) GOTO 199
C     ----------------------
C     1 TERM ONLY FOR T > 24
C     ----------------------
      DO 170 KL = 1, KLPRIM
      IF (TT(KL).GE.24.D0) THEN
         N = N + 1
         TS(N) = TT(KL)
         IND(N) = KL
      ENDIF
  170 CONTINUE
      M4 = N
      DO 180 IJKLS = M3+1, M4
      DELTA(IJKLS) = 1.D0 / TS(IJKLS)
      FS1(IJKLS) = 0.490D0
  180 CONTINUE
C$DIR NO_RECURRENCE
CDIR$ IVDEP
  199 DO 200 IJKLS = 1, N
      F(IND(IJKLS),0) = Y(IND(IJKLS)) * FS1(IJKLS) * DELTA(IJKLS)
#if defined (BIT64)
      TS(IJKLS) = SQRT(DELTA(IJKLS))
#else
      TS(IJKLS) = DSQRT(DELTA(IJKLS))
#endif
      F(IND(IJKLS),0) = PIROOT * TS(IJKLS) - F(IND(IJKLS),0)
  200 CONTINUE
C     ----------------
C     UPWARD RECURSION
C     ----------------
      DO 208 IJKLS = 1, N
      DELTA(IJKLS) = 0.5D0 * DELTA (IJKLS)
  208 CONTINUE
      DO 210 J = 1, MS
C$DIR NO_RECURRENCE
CDIR$ IVDEP
      DO 209 IJKLS = 1, N
      IJKL = IND(IJKLS)
      F(IJKL,J) = DELTA(IJKLS) * (XU(J-1)*F(IJKL,J-1) - Y(IJKL))
  209 CONTINUE
  210 CONTINUE
      RETURN
      END
