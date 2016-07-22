C
C     ================================================
      SUBROUTINE ADDONE (FOCKR, FOCKI, NBL, NBS, IREC,
     +                   ER, EI, FR, FI, GR, GI)
C     ================================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     SUM ONE AND TWO-ELECTRON CONTRIBUTIONS TO THE FOCK-MATRIX
C
      INCLUDE 'param.inc'
      INCLUDE 'common.inc'
C
      REAL*8 ER (*), EI (*), GR (*), GI (*)
      REAL*8 FOCKR (*), FOCKI (*)
      REAL*8 FR (NS, NL), FI (NS, NL)
C
      IF (NBL .EQ. 0) CALL Q2JOB(6, 'ADDONE','NBL=0',0)
      IF ((.NOT. TWOC) .AND. (NBS .EQ. 0))
     +   CALL Q2JOB(6,'ADDONE','NBS=0 AND NOT TWOC',0)
C
      NTOT = NBL + NBS
      NBLLH = NBL * (NBL + 1) / 2
      NBSSH = NBS * (NBS + 1) / 2
C
      IF (.NOT. TWOC) THEN
      READ (MFDOEMR, REC = IREC + 4) (ER (I), I = 1, NBLLH)
      READ (MFDOEMI, REC = IREC + 4) (EI (I), I = 1, NBLLH)
      K = 1 - NTOT
      L = 1
      DO 110 I = 1, NBL
      K = K + NTOT
      DO 111 J = 1, I
      FOCKR (K + J - 1) = FOCKR (K + J - 1) + ER (L + J - 1)
      FOCKI (K + J - 1) = FOCKI (K + J - 1) + EI (L + J - 1)
 111  CONTINUE
      L = L + I
  110 CONTINUE
      READ (MFDOEMR, REC = IREC + 6)
     +     (( FR (I, J), I = 1, NBS), J = 1, NBL)
      READ (MFDOEMI, REC = IREC + 6)
     +     (( FI (I, J), I = 1, NBS), J = 1, NBL)
      DO 120 I = 1, NBS
      K = K + NTOT
      DO 121 J = 1, NBL
      FOCKR (K + J - 1) = FOCKR (K + J - 1) + FR (I, J)
      FOCKI (K + J - 1) = FOCKI (K + J - 1) + FI (I, J)
 121  CONTINUE
  120 CONTINUE
C
C     -----------------------------------------------
C     NOTE : INSTEAD OF VSS, VSS - 2C^2SSS  IS READ !
C     -----------------------------------------------
C
      READ (MFDOEMR, REC = IREC + 7) (GR (I), I = 1, NBSSH)
      READ (MFDOEMI, REC = IREC + 7) (GI (I), I = 1, NBSSH)
      K = NBL * (NTOT + 1) - NTOT + 1
      L = 1
      DO 130 I = 1, NBS
      K = K + NTOT
      DO 131 J = 1, I
      FOCKR (K + J - 1) = FOCKR (K + J - 1) + GR (L + J - 1)
      FOCKI (K + J - 1) = FOCKI (K + J - 1) + GI (L + J - 1)
 131  CONTINUE
      L = L + I
  130 CONTINUE
      ELSE
      READ (MFDOEMR, REC = IREC + 7) (ER (I), I = 1, NBLLH)
      READ (MFDOEMI, REC = IREC + 7) (EI (I), I = 1, NBLLH)
      K = 1 - NTOT
      L = 1
      DO 200 I = 1, NBL
      K = K + NTOT
      DO 201 J = 1, NBL
      FOCKR (K + J - 1) = FOCKR (K + J - 1) + ER (L + J - 1)
      FOCKI (K + J - 1) = FOCKI (K + J - 1) + EI (L + J - 1)
 201  CONTINUE
      L = L + I
  200 CONTINUE
      ENDIF
      CALL MHERR (FOCKR (1), FOCKI (1), NTOT, NTOT)
      RETURN
      END
