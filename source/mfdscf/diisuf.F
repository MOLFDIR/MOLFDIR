C
      SUBROUTINE DIISUF(DCR,DCI,DOR,DOI,DOCR,DOCI,FCR,FCI,
     +                  FOR,FOI,FOCR,FOCI)
      IMPLICIT REAL*8 (A-H, O-Z)
C
C === THIS ROUTINE COMPUTES THE NEW FOCK MATRIX:
C === F(NEW) = SUM( CI * FI )
C
      INCLUDE 'paracom.inc'
      INCLUDE 'general.inc'
      INCLUDE 'diis.inc'
C
      REAL*8 DCR(*), DCI(*), DOR(*), DOI(*)
      REAL*8 FCR(*), FCI(*), FOR(*), FOI(*)
      REAL*8 DOCR(*), DOCI(*), FOCR(*), FOCI(*)
C
      INTEGER NIDSRP(17)
C
      NIDSRP (1) = 0
      DO 10 IRP = 1 ,NSYMRP
      NIDSRP( IRP + 1) = NIDSRP( IRP ) + NBSIM(IRP) * NBSIM(IRP)
   10 CONTINUE
C
      ICYC = IDCYC + 1
      DO 50 IRP = 1, NSYMRP
      ISRP = NIDSRP(IRP) + 1
      NB = NBSIM(IRP)
      NBS = NB * NB
C     DO NOT ZERO OUT UNOCCUPIED REPRESENTATIONS : THEY ARE NOT EXTRAPOLATED
      IF (NCL(IRP).NE.0) THEN
         DO I = ISRP, ISRP + NBS - 1
         FCR(I) = 0.0D0
         FCI(I) = 0.0D0
         ENDDO
      ENDIF
         IF (NOP(IRP,1).NE.0) THEN
         DO I = ISRP, ISRP + NBS - 1
         FOR(I) = 0.0D0
         FOI(I) = 0.0D0
      ENDDO
      ENDIF
         IF (NOP(IRP,2).NE.0) THEN
         DO I = ISRP, ISRP + NBS - 1
         FOCR(I) = 0.0D0
         FOCI(I) = 0.0D0
      ENDDO
      ENDIF
   50 CONTINUE
      DO 100 I = ILAST - IFIRST + 1, 1, -1
      ICYC = ICYC - 1
      IF (ICYC .LE. 0) ICYC = N30
      C2R = CVEC(I)
      DO 110 IRP = 1, NSYMRP
      NB = NBSIM(IRP)
      NBS = NB * NB
      ISRP = NIDSRP(IRP) + 1
C === READ IN AN OLD FOCK MATRIX FOR IRREP IRP
      IF (NCL(IRP) .NE. 0) THEN
      CALL RDIIS(ICYC,DCR(ISRP),DCI(ISRP),NB,IRP,1)
C === WE NOW ADD THIS FOCK MATRIX TO THE ARRAYS FC AND FO WITH
C === THE APROPRIATE COEFFICIENTS
      DO 200 J = ISRP, ISRP + NBS - 1
      FCR(J) = FCR(J) + C2R * DCR(J)
      FCI(J) = FCI(J) + C2R * DCI(J)
  200 CONTINUE
      ENDIF
      IF (NOP(IRP,1) .NE. 0) THEN
      CALL RDIIS(ICYC,DOR(ISRP),DOI(ISRP),NB,IRP,2)
      DO 210 J = ISRP, ISRP + NBS - 1
      FOR(J) = FOR(J) + C2R * DOR(J)
      FOI(J) = FOI(J) + C2R * DOI(J)
  210 CONTINUE
      ENDIF
      IF (NOP(IRP,2) .NE. 0) THEN
      CALL RDIIS(ICYC,DOCR(ISRP),DOCI(ISRP),NB,IRP,7)
      DO 230 J = ISRP, ISRP + NBS - 1
      FOCR(J) = FOCR(J) + C2R * DOCR(J)
      FOCI(J) = FOCI(J) + C2R * DOCI(J)
  230 CONTINUE
      ENDIF
  110 CONTINUE
  100 CONTINUE
C
      RETURN
      END
