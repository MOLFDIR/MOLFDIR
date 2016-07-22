C
C     ========================================
      SUBROUTINE DENSS (IRP, MOF, MOL, D, OSH,NROPEN)
C     ========================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     FORM DENSITYMATRIX FOR SYM REP IRP, USING MO'S MOF TO MOL
C          FROM THAT SYM REP
C     FOR CALCULATING EXPECTATION VALUES AND COUPLING OPERATORS
C
C
      INCLUDE 'paracom.inc'
      INCLUDE 'general.inc'
C
      DIMENSION VMOR(N22*2), VMOI(N22*2)
      COMPLEX*16 D (*) , VMO ( N22*2 )
      EQUIVALENCE ( VMO , VMOR ) , ( VMO(N22+1) , VMOI )
      EQUIVALENCE (KMO,NV),(NOCT,NOCC)
      EQUIVALENCE (TITLE,FORMAT)
      CHARACTER*80 TITLE
      LOGICAL OSH
C
      DIMENSION SLLR (NNL, NNL)
      DIMENSION SLLI (NNL, NNL)
      DIMENSION WORKR (1, NNL), WORKI (1, NNL)
      IF (NNL .LT. NL) CALL Q2JOB(3,'P.DENSS','NNL',NL)
C
      LENREC=8 * MAX0(4*(NL+NS)+4,80)
      OPEN(MFDVECB,FILE=FNAM(2),ACCESS='DIRECT',FORM='UNFORMATTED',
     +     RECL=LENREC)
      LENREC=8 * (2*MSS+1)
C
      MVBREC=2
      DO 19 I = 1, IRP - 1
        MVBREC = MVBREC + NCL (I) + NOP (I,1) + NOP (I,2)
 19   CONTINUE
      MVBREC = MVBREC + MOF - 1
C
      LDREC=NSYMRP*(NREC+6)+1
C
      IREC = (IRP - 1) * NREC
      NBL = NBSIM1 (IRP)
      NBLLH = NBL * (NBL + 1) / 2
      NB = NBSIM (IRP)
C
      NEL = NB * (NB + 1) / 2
      DO 210 L = 1, NEL
        D (L) = (0.0D0, 0.0D0)
  210 CONTINUE
      MVBRCO = MVBREC
C
      IF (TWOC) THEN
      READ (MFDOEMR, REC = IREC + 2)
     +     ((SLLR (I, J), J = 1, I), I = 1, NBL)
      READ (MFDOEMI, REC = IREC + 2)
     +     ((SLLI (I, J), J = 1, I), I = 1, NBL)
      DO 610 I = 1, NBL
        DO 620 J = 1, I
          SLLR (J, I) = SLLR (I, J)
          SLLI (J, I) = - SLLI (I, J)
 620    CONTINUE
 610  CONTINUE
      ENDIF
C
C     -------------------
      DO 250 N = MOF, MOL
C     -------------------
C
      MVBREC=MVBREC+1
C
C     ---------------
      IF (TWOC)  THEN
C     ---------------
C
      READ (MFDVECB,REC=MVBREC) IDUM,IDUM,LV,DUM,
     +     (VMOR (L), VMOI (L), L = 1, LV)
C
      CALL TDVT (VMOR, VMOI, NBL, SLLR, SLLI, NNL,
     +           XR, XI, 1, WORKR, WORKI, 1, 1, NBL)
#if defined (BIT64)
      IF (ABS (XI) .GT. 1.E-13) CALL Q2JOB(6,'XI <> 0 IN DENS2',' ',1)
      XR = 1. / SQRT (XR)
#else
      IF (DABS (XI) .GT. 1.D-13) CALL Q2JOB(6,'XI <> 0 IN DENS2',' ',1)
      XR = 1.0D0 / DSQRT (XR)
#endif
C
      DO 630 I = 1, NBL
        VMOR (I) = VMOR (I) * XR
        VMOI (I) = VMOI (I) * XR
 630  CONTINUE
C
      WRITE (MFDVECB, REC = MVBREC)
     +   IDUM, IDUM, LV, DUM, (VMOR (L), VMOI (L), L = 1, LV)
C
      ENDIF
C
      READ (MFDVECB,REC=MVBREC)
     +   IDUM,IDUM,LV,DUM,(VMO(L),L=1,LV)
      L=0
      OCC = 1.0D0
      IF (OSH) OCC = OCOPN (IRP,NROPEN)
      DO 240 I=1,NB
        DO 230 J=1,I
          L=L+1
#if defined (BIT64)
          D(L)=D(L)+VMO(I)*CONJG(VMO(J))*OCC
#else
          D(L)=D(L)+VMO(I)*DCONJG(VMO(J))*OCC
#endif
  230   CONTINUE
  240 CONTINUE
C
  250 CONTINUE
      CLOSE (MFDVECB)
      RETURN
      END
