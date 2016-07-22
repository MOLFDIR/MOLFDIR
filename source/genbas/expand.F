C     ===========================
      SUBROUTINE EXPAND (IOM,ITP)
C     ===========================
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'paracom.inc'
C
      PARAMETER (PI=3.1415926535898D0)
      DIMENSION INDP (N17)
C     ------------------------------------------------
C     FIND ALL PRIMITIVES (VECTORS, NEW, KIN. BALANCE)
C     ------------------------------------------------
      IF (NCONTR(IOM,ITP).GT.0)  THEN
         KMS = IMSCOF (IOM,ITP)
         KMF = IMFCOF (IOM,ITP)
         JS = INDBAS (KMS)
         JF = INDBAS (KMS+1)
         JPRIM = JF - JS
      ELSE
         JPRIM = 0
      ENDIF
      KPRIM = NWPRIM (IOM,ITP)
      LPRIM = 0
      MPRIM = 0
      IF (IOM.EQ.2)  THEN
         IF (ITP.GE.1) LPRIM = NPRIM (1,ITP-1)
         IF (ITP.LE.3) MPRIM = NPRIM (1,ITP+1)
      ENDIF
C     DETERMINE UNIQUE SET
      N = 0
      DO 100 J = 1, JPRIM + KPRIM + LPRIM + MPRIM
      IF (J.LE.JPRIM) THEN
         A = ALPHA (JS + J)
      ELSEIF (J.LE.JPRIM+KPRIM) THEN
             A = EN (J-JPRIM,ITP,IOM)
      ELSEIF (J.LE.JPRIM+KPRIM+LPRIM) THEN
             A = EG (J-JPRIM-KPRIM,ITP-1,1)
      ELSEIF (J.LE.JPRIM+KPRIM+LPRIM+MPRIM) THEN
             A = EG (J-JPRIM-KPRIM-LPRIM,ITP+1,1)
      ENDIF
      DO 10  K = 1, N
      B = EG (K,ITP,IOM)
#if defined (BIT64)
      DIFFER =  ABS (2.E0 * (A - B) / (A + B))
#else
      DIFFER = DABS (2.D0 * (A - B) / (A + B))
#endif
      IF (DIFFER.LT.TOLER) THEN
         INDP (J) = K
         GOTO 100
      ENDIF
   10 CONTINUE
      N = N +1
      EG (N,ITP,IOM) = A
      INDP (J) = N
  100 CONTINUE
      DO 102 I = 1, N16
      DO 101 J = 1, N
      CG (J,I,ITP,IOM) = 0.0D0
  101 CONTINUE
  102 CONTINUE
C     -----------------------------------------
C     EXPAND CONTRACTED FUNCTIONS IN UNIQUE SET
C     1) ATOMIC VECTOR PARTS :
C     -----------------------------------------
      NCO = 0
      DO 200 I = 1, NCONTR (IOM,ITP)
      NCO = NCO + 1
      DO 190 J = 1, JPRIM
      K = INDP (J)
      A = EG (K,ITP,IOM)
      XNORM = (PI**0.75D0)/((2.0D0*A)**(0.75D0+DBLE(ITP)/2.0D0))
      DO 180 KM = 0, KMF - KMS
      CO = COEFF (JS + KM * JPRIM + J)
      CO = CO * XNORM
      CG (K,NCO,ITP,IOM) = CG (K,NCO,ITP,IOM) + CV (KM+1,I,ITP,IOM) * CO
  180 CONTINUE
  190 CONTINUE
  200 CONTINUE
C     --------------------
C     2) EXTRA FUNCTIONS :
C     --------------------
      DO 500 I = 1, NWCONT (IOM,ITP)
      NCO = NCO + 1
      INDMO (NCO,ITP,IOM) = INDNW (I,IOM,ITP)
      DO 490 J = 1, KPRIM
      K = INDP (J + JPRIM)
      CG (K,NCO,ITP,IOM) = CN (J,I,ITP,IOM)
  490 CONTINUE
  500 CONTINUE
C     --------------------
C     3) KINETIC BALANCE :
C     --------------------
      IF (IOM.EQ.1) GOTO 401
      JTP = ITP + 1
      IF (JTP.GT.4) GOTO 301
      DO 300 I = 1, NCONTR (1,JTP)
      IND = INDMO (I,JTP,1)
      IF (MOS(IND,5).EQ.1.OR.MOS(IND,5).EQ.0) THEN
         NCO = NCO + 1
         INDMO (NCO,ITP,2) = - ( (JTP+1)*(N16+1) + I)
         DO 290 J = 1, MPRIM
         K = INDP (J + JPRIM + KPRIM + LPRIM)
         A = EG (K,ITP,IOM)
#if defined (BIT64)
         CO = CG (J,I,JTP,1) *  SQRT (A)
#else
         CO = CG (J,I,JTP,1) * DSQRT (A)
#endif
         CG (K,NCO,ITP,IOM) = CO
  290    CONTINUE
      ENDIF
  300 CONTINUE
  301 CONTINUE
      JTP = ITP - 1
      IF (JTP.LT.0) GOTO 401
      DO 400 I = 1, NCONTR (1,JTP)
      IND = INDMO (I,JTP,1)
      IF (MOS(IND,5).GE.1) THEN
         NCO = NCO + 1
         INDMO (NCO,ITP,2) = - ( (JTP+1)*(N16+1) + I)
         DO 390 J = 1, LPRIM
         K = INDP (J + JPRIM + KPRIM)
         A = EG (K,ITP,IOM)
#if defined (BIT64)
         CO = CG (J,I,JTP,1) *  SQRT (A)
#else
         CO = CG (J,I,JTP,1) * DSQRT (A)
#endif
         CG (K,NCO,ITP,IOM) = CO
  390    CONTINUE
      ENDIF
  400 CONTINUE
  401 CONTINUE
      NCONTR (IOM,ITP) = NCO
      NPRIM (IOM,ITP) = N
      RETURN
      END
