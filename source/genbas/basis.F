C     ==========================
      SUBROUTINE BASIS (IOM,ITP)
C     ==========================
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'paracom.inc'
C
      LOGICAL EMPTY (N16)
C
      NCON = NCONTR (IOM,ITP)
      NCO = 0
      NPR = NPRIM (IOM,ITP)
      RNT = DBLE (ITP)
C     ------------------------------
C     COPY FUNCTIONS INTO WORK ARRAY
C     ------------------------------
      DO 20 I = 1, NCON
      DO 10 J = 1, NPR
      CR (J,I) = CG (J,I,ITP,IOM)
   10 CONTINUE
   20 CONTINUE
      DO 30 J = 1, NPR
      ER (J) = EG (J,ITP,IOM)
   30 CONTINUE
      DO 40 I = 1, NCON
      IND = INDMO (I,ITP,IOM)
      JOM = IOM
      IF (IND.LT.0) THEN
         IND = - IND
         JTP = IND / (N16 + 1) - 1
         J = MOD (IND,(N16+1))
         IND = INDMO (J,JTP,1)
         JOM = 3
      ENDIF
      TRHLD (I) = TRSH (IND,JOM)
   40 CONTINUE
C     -----------------------
C     NORMALIZE THE FUNCTIONS
C     -----------------------
      DO 100 I = 1, NCON
      CALL NORMF (RNT,NPR,ER,CR(1,I),CNORM)
      EMPTY (I) = (CNORM.LT.TRHLD(I))
  100 CONTINUE
C     -----------------
C     ORTHOGONALISATION
C     -----------------
      DO 200 I = 1, NCON
      IF (EMPTY(I)) GOTO 200
      CCNORM = 0.D0
      DO 120 J = 1, NCO
      CALL OVRLAP (RNT,NPR,ER,CR(1,I),CR(1,J),OVLAP)
      DNORM (J) = OVLAP
      CCNORM = CCNORM + OVLAP**2
  120 CONTINUE
      CCNORM = 1.D0 - CCNORM
      IF (CCNORM.LE.-TOLER) CALL Q2JOB (6,'ORTHOGONALISATION',' ',0)
      IF (CCNORM.LE.TRHLD(I)) GOTO 200
      DO 130 J = 1, NCO
      OVLAP = DNORM (J)
      DO 129 K = 1, NPR
      CR (K,I) = CR (K,I) - CR (K,J) * OVLAP
  129 CONTINUE
  130 CONTINUE
      CALL NORMF (RNT,NPR,ER,CR(1,I),CNORM)
C     ----------------------------------------------------
C     EXTRA ORTHOGONALISATION TO DECREASE NUMERICAL ERRORS
C     ----------------------------------------------------
      DO 140 J = 1, NCO
      CALL OVRLAP (RNT,NPR,ER,CR(1,I),CR(1,J),OVLAP)
      DNORM (J) = OVLAP
  140 CONTINUE
      DO 150 J = 1, NCO
      OVLAP = DNORM (J)
      DO 149 K = 1, NPR
      CR (K,I) = CR (K,I) - CR (K,J) * OVLAP
  149 CONTINUE
  150 CONTINUE
      CALL NORMF (RNT,NPR,ER,CR(1,I),CNORM)
C     ----------------------------------------
C     PUT THE NEW ORTHOGONAL FUNCTION IN PLACE
C     ----------------------------------------
      NCO = NCO + 1
      VPART (NCO,ITP,IOM) = CCNORM
      INDMO (NCO,ITP,IOM) = INDMO (I,ITP,IOM)
      DO 160 J = 1, NPR
      CR (J,NCO) = CR (J,I)
  160 CONTINUE
  200 CONTINUE
C     -------------------------------------
C     COPY FUNCTIONS BACK INTO COMMON ARRAY
C     -------------------------------------
      NCONTR (IOM,ITP) = NCO
      DO 220 I = 1, NCO
      DO 210 J = 1, NPR
      CG (J,I,ITP,IOM) = CR (J,I)
  210 CONTINUE
  220 CONTINUE
      RETURN
      END
