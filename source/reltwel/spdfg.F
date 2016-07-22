      SUBROUTINE SPDFG (ICNT,JCNT,KCNT,LCNT,IPRIM,JPRIM,KPRIM,LPRIM,
     &                  MS,NI,NJ,NK,NL,DOIJ,DOGAM,
     &                  ALPHAI,ALPHAJ,ALPHAK,ALPHAL,
     &                  IJPRIM,KLPRIM,IJKLPR,WORK,MWORK,NWORK,
     &                  WORK2,NWORK2,
     &                  DISTAB,DISTCD,GAB,GCD,HL,HM,
     &                  T1,T2,P,Q,R,Z,WX,F,C,RAWINT)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'paracom.inc'
C
      REAL*8 P(IJPRIM,3),Q(KLPRIM,3),HL(IJPRIM,0:12),HM(KLPRIM,0:12),
     &       GAB(IJPRIM),GCD(KLPRIM),T1(IJPRIM),T2(KLPRIM),
     &       DISTAB(IJPRIM),DISTCD(KLPRIM),R(KLPRIM,IJPRIM,3),
     &       F(KLPRIM,IJPRIM,0:MS),C(KLPRIM,0:MS+2),Z(KLPRIM,IJPRIM),
     &       WX(KLPRIM),WORK(MWORK,NWORK),WORK2(IJKLPR,NWORK2),
     &       ALPHAI(IPRIM),ALPHAJ(JPRIM),
     &       ALPHAK(KPRIM),ALPHAL(LPRIM),RAWINT(IJKLPR)
C
      DIMENSION NI(3),NJ(3),NK(3),NL(3),AR(3),BR(3),CR(3),DR(3)
      DIMENSION AB(3),CD(3),LSUM(4),MSUM(4),LMSUM(4)
      LOGICAL DOGAM,DOIJ,ABXYZ,CDXYZ,ABCDX
C
      SAVE ABXYZ,LSUM
C
      IF (DOIJ) THEN
         ABXYZ = (ICNT.EQ.JCNT)
         AR(1) = XCORD(ICNT)
         AR(2) = YCORD(ICNT)
         AR(3) = ZCORD(ICNT)
         BR(1) = XCORD(JCNT)
         BR(2) = YCORD(JCNT)
         BR(3) = ZCORD(JCNT)
         AB(1) = AR(1) - BR(1)
         AB(2) = AR(2) - BR(2)
         AB(3) = AR(3) - BR(3)
         DSAB = AB(1)**2 + AB(2)**2 + AB(3)**2
         CALL HLFUNS (ABXYZ,ALPHAI,ALPHAJ,IPRIM,JPRIM,IJPRIM,AR,BR,
     &    NI,NJ,LSUM,P,T1,DSAB,DISTAB,GAB,HL,
     &    WORK(1,1),WORK(1,2),WORK(1,3),WORK(1,4),WORK(1,5),
     &    WORK(1,6),WORK(1,7),WORK(1,8),WORK(1,9),WORK(1,10),
     &    WORK(1,11),WORK(1,12),WORK(1,13),WORK(1,14),WORK(1,15),
     &    WORK(1,16),WORK(1,17),WORK(1,18),WORK(1,19))
         DOIJ = .FALSE.
      ENDIF
      CDXYZ = (KCNT.EQ.LCNT)
      CR(1) = XCORD(KCNT)
      CR(2) = YCORD(KCNT)
      CR(3) = ZCORD(KCNT)
      DR(1) = XCORD(LCNT)
      DR(2) = YCORD(LCNT)
      DR(3) = ZCORD(LCNT)
      CD(1) = CR(1) - DR(1)
      CD(2) = CR(2) - DR(2)
      CD(3) = CR(3) - DR(3)
      DSCD = CD(1)**2 + CD(2)**2 + CD(3)**2
      ABCDX = (ABXYZ.AND.CDXYZ.AND.(ICNT.EQ.KCNT) )
      CALL HLFUNS (CDXYZ,ALPHAK,ALPHAL,KPRIM,LPRIM,KLPRIM,CR,DR,
     & NK,NL,MSUM,Q,T2,DSCD,DISTCD,GCD,HM,
     & WORK(1,1),WORK(1,2),WORK(1,3),WORK(1,4),WORK(1,5),
     & WORK(1,6),WORK(1,7),WORK(1,8),WORK(1,9),WORK(1,10),
     & WORK(1,11),WORK(1,12),WORK(1,13),WORK(1,14),WORK(1,15),
     & WORK(1,16),WORK(1,17),WORK(1,18),WORK(1,19))
      CALL HMFUNS (KLPRIM,HM,MSUM)
C
C     -----------------------------
C     START N^4 PART OF CALCULATION
C     -----------------------------
C
      DO KT = 1, 3
      LMSUM(1) = 0
      NIJ = NI(KT) + NJ(KT)
      NKL = NK(KT) + NL(KT)
      NIJKL = NIJ + NKL
      IF (ABCDX) THEN
         LMSUM(KT+1) = LMSUM(KT) + NIJKL/2 + 1
      ELSE
         LMSUM(KT+1) = LMSUM(KT) + NIJKL + 1
      ENDIF
      ENDDO
      IF (LMSUM(4)-3.GT.MS) THEN
      CALL Q2JOB(6,'ERROR IN SPDFG:MS VALUE INCONSISTENT',' ',0)
      ENDIF
C
C     ------------------------------------------------
C     CALCULATE INCOMPLETE GAMMA FUNCTION IF NECESSARY
C     ------------------------------------------------
C
      IF (DOGAM) THEN
C
      IF (.NOT.ABCDX) THEN
         DO IJ = 1, IJPRIM
         DO KL = 1, KLPRIM
            R(KL,IJ,1) = P(IJ,1) - Q (KL,1)
            R(KL,IJ,2) = P(IJ,2) - Q (KL,2)
            R(KL,IJ,3) = P(IJ,3) - Q (KL,3)
         ENDDO
         ENDDO
      ENDIF
C
      DO IJ = 1, IJPRIM
      DO KL = 1, KLPRIM
         Z (KL,IJ) = 1.D0 / (GAB(IJ) + GCD(KL))
      ENDDO
      ENDDO
C
      IF (.NOT.ABCDX) 
     & CALL FGAMMA(MS,IJKLPR,R,Z,
     & WORK2(1,1),WORK2(1,2),WORK2(1,3),WORK2(1,4),WORK2(1,5),
     & WORK2(1,6),WORK2(1,7),WORK2(1,8),F)
C
      DO IJ = 1, IJPRIM
C
      IF (ABCDX) THEN
         DO 50 KL = 1, KLPRIM
         T4 = T1 (IJ) + T2 (KL)
#if defined (BIT64)
         WX (KL) = PITERM / (T1(IJ)*T2(KL)*SQRT(T4))
#else
         WX (KL) = PITERM / (T1(IJ)*T2(KL)*DSQRT(T4))
#endif
   50    CONTINUE
      ELSE
         DO 51 KL = 1, KLPRIM
         T4 = T1 (IJ) + T2 (KL)
#if defined (BIT64)
         WX (KL) = PITERM*EXP(-DISTAB(IJ)-DISTCD(KL))/
     &               (T1(IJ)*T2(KL)*SQRT(T4))
#else
         WX (KL) = PITERM*DEXP(-DISTAB(IJ)-DISTCD(KL))/
     &               (T1(IJ)*T2(KL)*DSQRT(T4))
#endif
   51    CONTINUE
      ENDIF
C
      IF (ABCDX) THEN
         DO 70 KL = 1, KLPRIM
         F(KL,IJ,0) = WX(KL)
   70    CONTINUE
         DO 90 M = 1, MS
         DO 80 KL = 1, KLPRIM
         WX(KL) = WX(KL) * Z(KL,IJ)
         F(KL,IJ,M) = WX(KL) * XD(M)
   80    CONTINUE
   90    CONTINUE
      ELSE
         DO 91 M = 0, MS
         DO 81 KL = 1, KLPRIM
         F(KL,IJ,M) = WX(KL) * F(KL,IJ,M)
         WX(KL) = WX(KL) * Z(KL,IJ)
   81    CONTINUE
   91    CONTINUE
      ENDIF
C
      ENDDO ! (DO IJ)
      ENDIF ! (DOGAM)
C
C     -----------------------
C     CALCULATE THE INTEGRALS
C     -----------------------
C
      DO 110 IJKL = 1, IJKLPR
         RAWINT(IJKL) = 0.D0
  110 CONTINUE
C
      DO 200 IJ = 1, IJPRIM
      DO 20 KT = 1, 3
      LSKT = LSUM(KT)
      MSKT = MSUM(KT)
      LMSKT = LMSUM(KT)
      NIJ = NI(KT) + NJ(KT)
      NKL = NK(KT) + NL(KT)
      NIJKL = NIJ + NKL
      CALL HLMFUN
     &(KT,NIJKL,NIJ,NKL,LMSKT,LSKT,MSKT,IJKLPR,IJ,
     & IJPRIM,KLPRIM,ABCDX,HL,HM,R,C,
     & WORK(1,1),WORK(1,2),WORK(1,3),WORK(1,4),WORK(1,5),
     & WORK(1,6),WORK(1,7),WORK(1,8),WORK(1,9),WORK(1,10),
     & WORK(1,11),WORK(1,12),WORK(1,13),WORK(1,14),WORK(1,15),
     & WORK(1,16),WORK(1,17),WORK(1,18),WORK(1,19),WORK(1,20),
     & WORK(1,21))
   20 CONTINUE
C
      MX = LMSUM(2) - 1
      MY = LMSUM(3) - 1
      MZ = LMSUM(4) - 1
      DO 150 LX = 0, MX
      DO 140 LY = MX + 1, MY
      DO 130 LZ = MY + 1, MZ
      N = LX + LY + LZ - MX - MY - 2
      DO 120 KL = 1, KLPRIM
      IJKL = (IJ-1) * KLPRIM + KL
      RAWINT(IJKL) = RAWINT(IJKL) + C(KL,LX) * C(KL,LY)
     &               * C(KL,LZ) * F(KL,IJ,N)
  120 CONTINUE
  130 CONTINUE
  140 CONTINUE
  150 CONTINUE
  200 CONTINUE
C
      RETURN
      END
