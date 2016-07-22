C
C     ===================================================
      SUBROUTINE CALPART (ICOUNT,X,Y,Z,DENSL,DENSS,DENST)
C     ===================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INCLUDE "param.inc"
      INCLUDE "comblock.inc"     
C
      DIMENSION X(*),Y(*),Z(*)
      DIMENSION DENSL(*),DENSS(*),DENST(*)
      DIMENSION XC (NPOINTS),YC(NPOINTS),ZC (NPOINTS)
      DIMENSION AR2 (NPOINTS),GAUS (NPOINTS)
      DIMENSION F (NPOINTS, N22)
C
C     ----------------------------
C     LOOP OVER ALL BASISFUNCTIONS
C     ----------------------------
C
      IF (TWOC) THEN
      NEQ = NQ1
      ELSE
      NEQ = NEQUIV
      ENDIF
C
      IBAS = 0
C
      DO 10 IM = 1, NEQ
      ICOM = KCOMB (IM)
      ICTA = INDCOM (ICOM) + 1
      ICTB = INDCOM (ICOM + 1)
      DO 20 I = ICTA, ICTB
      IBAS = IBAS + 1
      DO 25 IP = 1, ICOUNT
      F (IP, IBAS) = 0.0D0
 25   CONTINUE
      ICGF = INDBAS (IM) + 1
      ICGL = INDBAS (IM + 1)
      ICNT = ICENT (I)
      ITYP = ITYPE (I)
      XCENT = COAT (ICNT, 1)
      YCENT = COAT (ICNT, 2)
      ZCENT = COAT (ICNT, 3)
      NX = NL (ITYP)
      NY = NM (ITYP)
      NZ = NN (ITYP)
      DO 30 J = ICGF, ICGL
      ALPH = -1.0D0 * ALPHA (J)
      COEF = COEFF (J)
C
C     -----------------------------------------------
C     CALCULATE VALUE OF BASISFUNCTION FOR EACH POINT
C     -----------------------------------------------
C
      DO 40 IP = 1, ICOUNT
      XC (IP) = X (IP) - XCENT
      YC (IP) = Y (IP) - YCENT
      ZC (IP) = Z (IP) - ZCENT
      AR2 (IP) = ALPH * (XC (IP) * XC (IP) + YC (IP) * YC (IP) +
     +                   ZC (IP) * ZC (IP))
      IF (AR2 (IP) .LT. -200) THEN
        GAUS (IP) = 0.0D0
      ELSE
        GAUS (IP) = EXP (AR2 (IP))
      ENDIF
      IF (NX .EQ. 0) THEN
        XC (IP) = 1.0D0
      ELSE
        XC (IP) = XC (IP) ** NX
      ENDIF
      IF (NY .EQ. 0) THEN
        YC (IP) = 1.0D0
      ELSE
        YC (IP) = YC (IP) ** NY
      ENDIF
      IF (NZ .EQ. 0) THEN
        ZC (IP) = 1.0D0
      ELSE
        ZC (IP) = ZC (IP) ** NZ
      ENDIF
      F (IP, IBAS) = F (IP, IBAS) +
     +               XC (IP) * YC (IP) * ZC (IP) * GAUS (IP) * COEF
 40   CONTINUE
 30   CONTINUE
 20   CONTINUE
 10   CONTINUE
C
C     -----------------
C     CALCULATE DENSITY
C     -----------------
C
      DO 45 IP = 1, ICOUNT
      DENSL (IP) = 0.0D0
      DENSS (IP) = 0.0D0
 45   CONTINUE
C
      DO 50 I = 1, NBTOT1
      DO 60 J = 1, I
      DO 70 IP = 1, ICOUNT
      DENSL (IP) = DENSL (IP) +
     +             F (IP, I) * DR (I*(I-1)/2 + J) * F (IP, J)
 70   CONTINUE
 60   CONTINUE
      DO 65 J = I + 1, NBTOT1
      DO 75 IP = 1, ICOUNT
      DENSL (IP) = DENSL (IP) +
     +             F (IP, I) * DR (J*(J-1)/2 + I) * F (IP, J)
 75   CONTINUE
 65   CONTINUE
 50   CONTINUE
      IF (.NOT. TWOC) THEN
      DO 51 I = NBTOT1 + 1, NBTOT
      DO 61 J = NBTOT1 + 1, I
      DO 71 IP = 1, ICOUNT
      DENSS (IP) = DENSS (IP) +
     +             F (IP, I) * DR (I*(I-1)/2 + J) * F (IP, J)
 71   CONTINUE
 61   CONTINUE
      DO 66 J = I + 1, NBTOT
      DO 76 IP = 1, ICOUNT
      DENSS (IP) = DENSS (IP) +
     +             F (IP, I) * DR (J*(J-1)/2 + I) * F (IP, J)
 76   CONTINUE
 66   CONTINUE
 51   CONTINUE
      ENDIF
C
      DO 79 IP = 1, ICOUNT
      DENST(IP) = DENSL(IP) + DENSS(IP)
   79 CONTINUE
C
      RETURN
      END
