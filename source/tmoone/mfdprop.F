C
C     =============================================================
      SUBROUTINE MFDPROP(AR,AI,BR,BI,CR,CI,DR,DI,ER,EI,FR,FI,IPROP)
C     =============================================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     CALCULATE PROPERTIES
C
C     IPROP=1: CALCULATE TOTAL ENERGY CORRESPONDING TO LAST DENSITY;
C              MFDENSN, MFDENSS, MFDFOCK AND MFDFMCP SHOULD BE CALLED
C              BEFORE MFDPROP
C
C     IPROP=2: CALCULATE ESTIMATE OF TOTAL ENERGY;
C              MFDIAGH AND MFDENSS SHOULD BE CALLED BEFORE MFDPROP
C
C     IPROP=3: AS IPROP=1, BUT NOW THE EXPECTATION VALUES ARE GIVEN
C              FOR EACH ORBITALAS WELL; SHOULD ALSO BE CALLED BEFORE MFDIAGH
C
      INCLUDE 'param.inc'
      INCLUDE 'common.inc'
      INCLUDE 'densfock.inc'
C
      REAL*8 QCR (N21*2), QCI (N21*2)
      REAL*8 QOR (N21*2), QOI (N21*2)
      EQUIVALENCE (QCR, DAAR), (QCI, DAAI)
      EQUIVALENCE (QOR, DOAAR), (QOI, DOAAI)
C
      REAL*8 AR(NLLH),AI(NLLH),BR(NS,NL),BI(NS,NL),CR(NSSH),CI(NSSH)
      REAL*8 DR(NLLH),DI(NLLH),ER(NS,NL),EI(NS,NL),FR(NSSH),FI(NSSH)
      REAL*8 EIE(3)
      REAL*8 V (2), SDP (2), EPSS (2), BM (2), QC (2), QO (2), SLL (2)
      INTEGER NIDSRP (17)
C
      COMPLEX*16 D (4 * N21)
      LOGICAL TOTAL
C
C     ---------------
C     INITIALIZE JUNK
C     ---------------
C
      OPEN (MFDENSM, FILE = 'MFDENSM', FORM = 'UNFORMATTED')
      REWIND (MFDENSM)
C
      NIDSRP (1) = 0
      DO 15 IRP = 1, NSYMRP
        NIDSRP (IRP + 1) = NIDSRP (IRP) + NBSIM (IRP) * NBSIM (IRP)
 15   CONTINUE
C
      DO 10 I = 1, 2
        V (I) = 0.0D0
        BM (I) = 0.0D0
        SDP (I) = 0.0D0
        EPSS (I) = 0.0D0
        QC (I) = 0.0D0
        QO (I) = 0.0D0
        SLL (I) = 0.0D0
        ATTEN (I) = 0.0D0
        RKINEN (I) = 0.0D0
        ELREPC (I) = 0.0D0
        ELREPO (I) = 0.0D0
        ONELEN (I) = 0.0D0
        BMCON (I) = 0.0D0
        VIRIAL (I) = 0.0D0
 10   CONTINUE
C
      IF (IPROP .EQ. 3) THEN
        DO 11 I = 1, NSYMRP
          ATTENR (I) = 0.0D0
          ONELENR (I) = 0.0D0
          BMCONR (I) = 0.0D0
          RKINENR (I) = 0.0D0
          REPCR (I) = 0.0D0
          REPOR (I) = 0.0D0
 11     CONTINUE
      ENDIF
C
      MOLAST = 0
      IF (IPROP .EQ. 2) EPREV = ETOTAL
C
C     ----------------------
      DO 100 IRP = 1, NSYMRP
C     ----------------------
C
      ISRP = NIDSRP (IRP) + 1
C
C     --------------------
      DO 110 ISHELL = 1, 2
C     --------------------
C
      IF ((ISHELL .EQ. 1) .AND. (NCL (IRP) .EQ. 0)) GOTO 110
      IF ((ISHELL .EQ. 2) .AND. (NOP (IRP) .EQ. 0)) GOTO 110
C
      IF (ISHELL .EQ. 1) THEN
        MOMAX = NCL (IRP)
        MOMIN = 1
      ELSE
        MOMIN = NCL (IRP) + 1
        MOMAX = NOP (IRP) + MOMIN - 1
      ENDIF
C
      TOTAL = .TRUE.
      MO = MOMIN
C
C     -----------------------------------------------
C     CONTINUE WITH NEXT MO WITHIN THIS (IRP, ISHELL)
 600  CONTINUE
C     -----------------------------------------------
C
      NBASL = NBSIM1 (IRP)
      NBASS = NBSIM2 (IRP)
      NBAS = NBSIM (IRP)
      NBLLH = NBASL * (NBASL + 1) / 2
      NBSSH = NBASS * (NBASS + 1) / 2
      IREC = (IRP - 1) * NREC
C
C     -------------------------------
C     READ OR CALCULATE DENSITYMATRIX
C     -------------------------------
C
      IF (TOTAL) READ (MFDENSM)
     +  (DR (L), DI (L), L = 1, NBLLH),
     +
     +  (
     +     (ER (I, J), EI (I, J), J = 1, NBASL),
     +     (FR (I*(I-1)/2+L),FI(I*(I-1)/2+L), L = 1, I),
     +
     +   I = 1, NBASS)
C
      IF ((IPROP .EQ. 3) .AND. (.NOT. TOTAL)) THEN
        IF (ISHELL .EQ. 1) THEN
          CALL DENSS (IRP, MO, MO, D, .FALSE.)
        ELSE
          CALL DENSS (IRP, MO, MO, D, .TRUE.)
        ENDIF
C
        DO 810 I = 1, NBLLH
#if defined (BIT64)
          DR (I) = REAL (D (I))
          DI (I) = AIMAG (D (I))
#else
          DR (I) = DBLE (D (I))
          DI (I) = DIMAG (D (I))
#endif
 810    CONTINUE
C
        K = NBLLH
        DO 820 I = 1, NBASS
          DO 830 J = 1, NBASL
            K = K + 1
#if defined (BIT64)
            ER (I, J) = REAL (D (K))
            EI (I, J) = AIMAG (D (K))
#else
            ER (I, J) = DBLE (D (K))
            EI (I, J) = DIMAG (D (K))
#endif
 830      CONTINUE
        K = K + I
 820    CONTINUE
C
        K = 0
        DO 840 I = NBASL + 1, NBAS
          DO 850 J = NBASL + 1, I
            K = K + 1
#if defined (BIT64)
            FR (K) = REAL (D (I*(I-1)/2+J))
            FI (K) = AIMAG (D (I*(I-1)/2+J))
#else
            FR (K) = DBLE (D (I*(I-1)/2+J))
            FI (K) = DIMAG (D (I*(I-1)/2+J))
#endif
 850      CONTINUE
 840    CONTINUE
      ENDIF
C
C     -----------
C     READ V (LL)
C     -----------
C
      READ (MFDOEMR, REC = IREC + 4)
     +     (AR (L), L = 1, NBLLH)
      READ (MFDOEMI, REC = IREC + 4)
     +     (AI (L), L = 1, NBLLH)
C
      IF (.NOT. TWOC) THEN
C
C     ------------
C     READ C * @.P
C     ------------
C
      READ (MFDOEMR, REC = IREC + 6)
     +     ((BR (I, J), I = 1, NBASS), J = 1, NBASL)
      READ (MFDOEMI, REC = IREC + 6)
     +     ((BI (I, J), I = 1, NBASS), J = 1, NBASL)
C
C     -----------
C     READ V (SS)
C     -----------
C
      READ (MFDOEMR, REC = IREC + 5)
     +     (CR (L), L = 1, NBSSH)
      READ (MFDOEMI, REC = IREC + 5)
     +     (CI (L), L = 1, NBSSH)
C
        ICALC = 7
      ELSE
        ICALC = 1
      ENDIF
C
C     --------------------------------------------
C     CALCULATE EXP.VAL. OF V AND @.P
C     --------------------------------------------
C
      EIE (2) = 0.0D0
      EIE (3) = 0.0D0
      CALL MFDTRCE (NBASL, NBASS, AR, AI, BR, BI, CR, CI,
     +              ICALC, EIE, DR, DI, ER, EI, FR, FI)
C
      IF (TOTAL) THEN
        V (ISHELL) = V (ISHELL) + EIE (1) + EIE (3)
        SDP (ISHELL) = SDP (ISHELL) + EIE (2)
      ENDIF
C
      IF ((IPROP .EQ. 3) .AND. (.NOT. TOTAL)) THEN
        ATTENO (MO+MOLAST) = EIE (1) + EIE (3)
        RKINENO (MO+MOLAST) = EIE (2)
        RKINENR (IRP) = RKINENR (IRP) + RKINENO (MO+MOLAST)
        ATTENR (IRP) = ATTENR (IRP) + ATTENO (MO+MOLAST)
      ENDIF
C
C     ====================================
C     IF TWOC, CALCULATE EXP.VAL. OF T(LL)
C     ====================================
C
      IF (TWOC) THEN
        READ (MFDOEMR, REC = IREC + 10)
     +       (AR (L), L = 1, NBLLH)
        READ (MFDOEMI, REC = IREC + 10)
     +       (AI (L), L = 1, NBLLH)
C
        CALL MFDTRCE (NBASL, NBASS, AR, AI, BR, BI, CR, CI,
     +                1, EIE, DR, DI, ER, EI, FR, FI)
C
        IF (TOTAL) THEN
          SDP (ISHELL) = SDP (ISHELL) + EIE (1)
        ENDIF
C
        IF ((IPROP .EQ. 3) .AND. (.NOT. TOTAL)) THEN
          RKINENO (MO + MOLAST) = EIE (1)
          RKINENR (IRP) = RKINENR (IRP) + EIE (1)
        ENDIF
      ENDIF
C
C     ----------------------
C     READ S (LL) AND S (SS)
C     ----------------------
C
      READ (MFDOEMR, REC = IREC + 2)
     +     (AR (L), L = 1, NBLLH)
      READ (MFDOEMI, REC = IREC + 2)
     +     (AI (L), L = 1, NBLLH)
C
      READ (MFDOEMR, REC = IREC + 3)
     +     (CR (L), L = 1, NBSSH)
      READ (MFDOEMI, REC = IREC + 3)
     +     (CI (L), L = 1, NBSSH)
C
C     ---------------------------------------
C     CALCULATE EXP.VAL. OF S (LL) AND S (SS)
C     ---------------------------------------
C
      EIE (3) = 0.0D0
      CALL MFDTRCE (NBASL, NBASS, AR, AI, BR, BI, CR, CI,
     +              5, EIE, DR, DI, ER, EI, FR, FI)
C
      IF (TOTAL) THEN
        SLL (ISHELL) = SLL (ISHELL) + EIE (1)
        BM (ISHELL) = BM (ISHELL) + EIE (3)
      ENDIF
C
      IF ((IPROP .EQ. 3) .AND. (.NOT. TOTAL)) THEN
        BMCONO (MO+MOLAST) = - TWOCSQ * EIE (3)
        BMCONR (IRP) = BMCONR (IRP) + BMCONO (MO+MOLAST)
        ONELENO (MO+MOLAST) = ATTENO (MO+MOLAST) + RKINENO (MO+MOLAST)
     +                        + BMCONO (MO+MOLAST)
        ONELENR (IRP) = ONELENR (IRP) + ONELENO (MO+MOLAST)
      ENDIF
C
C     ------------------------------
C     MOVE QCR AND QCI TO A, B AND C
C     ------------------------------
C
      IF ((IPROP .NE. 2) .AND. (.NOT. ONLYONE)) THEN
C
        L = 0
        DO 20 I = 1, NBASL
          DO 20 J = 1, I
            L = L + 1
            AR (L) = QCR ((I-1)*NBAS + J + ISRP - 1)
            AI (L) = QCI ((I-1)*NBAS + J + ISRP - 1)
 20     CONTINUE
        DO 30 I = NBASL + 1, NBAS
          DO 30 J = 1, NBASL
            BR (I-NBASL, J) = QCR ((I-1)*NBAS + J + ISRP - 1)
            BI (I-NBASL, J) = QCI ((I-1)*NBAS + J + ISRP - 1)
 30     CONTINUE
        L = 0
        DO 40 I = NBASL + 1, NBAS
          DO 40 J = NBASL + 1, I
            L = L + 1
            CR (L) = QCR ((I-1)*NBAS + J + ISRP - 1)
            CI (L) = QCI ((I-1)*NBAS + J + ISRP - 1)
 40     CONTINUE
C
C       --------------------
C       CALC EXP. VAL. OF QC
C       --------------------
C
        EIE (2) = 0.0D0
        EIE (3) = 0.0D0
        IF (TWOC) THEN
          ICALC = 1
        ELSE
          ICALC = 7
        ENDIF
C
        CALL MFDTRCE (NBASL, NBASS, AR, AI, BR, BI, CR, CI,
     +                7, EIE, DR, DI , ER, EI, FR, FI)
C
        IF (TOTAL)
     +    QC (ISHELL) = QC (ISHELL) + EIE (1) + EIE (2) + EIE (3)
C
        IF ((IPROP .EQ. 3) .AND. (.NOT. TOTAL)) THEN
          IF (ISHELL .EQ. 1) THEN
            REPCO (MO+MOLAST) = 0.5D0 * (EIE (1) + EIE (2) + EIE (3))
          ELSE
            REPCO (MO+MOLAST) = EIE (1) + EIE (2) + EIE (3)
          ENDIF
          REPCR (IRP) = REPCR (IRP) + REPCO (MO+MOLAST)
        ENDIF
C
C       --------------------------------------------
C       IF OPENSHELLS MOVE QOR AND QOI TO A, B AND C
C       --------------------------------------------
C
        IF (OPENSH) THEN
        L = 0
          L = 0
          DO 25 I = 1, NBASL
            DO 25 J = 1, I
              L = L + 1
              AR (L) = QOR ((I-1)*NBAS + J + ISRP - 1)
              AI (L) = QOI ((I-1)*NBAS + J + ISRP - 1)
 25       CONTINUE
          DO 35 I = NBASL + 1, NBAS
            DO 35 J = 1, NBASL
              BR (I-NBASL, J) = QOR ((I-1)*NBAS + J + ISRP - 1)
              BI (I-NBASL, J) = QOI ((I-1)*NBAS + J + ISRP - 1)
 35       CONTINUE
          L = 0
          DO 45 I = NBASL + 1, NBAS
            DO 45 J = NBASL + 1, I
              L = L + 1
              CR (L) = QOR ((I-1)*NBAS + J + ISRP - 1)
              CI (L) = QOI ((I-1)*NBAS + J + ISRP - 1)
 45       CONTINUE
C
C         --------------------
C         CALC EXP. VAL. OF QO
C         --------------------
C
          EIE (2) = 0.0D0
          EIE (3) = 0.0D0
          IF (TWOC) THEN
            ICALC = 1
          ELSE
            ICALC = 7
          ENDIF
C
          CALL MFDTRCE (NBASL, NBASS, AR, AI, BR, BI, CR, CI,
     +                  7, EIE, DR, DI , ER, EI, FR, FI)
C
          IF (TOTAL)
     +      QO (ISHELL) = QO (ISHELL) + EIE (1) + EIE (2) + EIE (3)
C
          IF ((IPROP .EQ. 3) .AND. (.NOT. TOTAL)) THEN
            IF (ISHELL .EQ. 1) THEN
              REPOO (MO+MOLAST) = EIE (1) + EIE (2) + EIE (3)
            ELSE
              REPOO (MO+MOLAST) = 0.5D0 * COPCOF *
     +                            (EIE (1) + EIE (2) + EIE (3))
            ENDIF
            REPOR (IRP) = REPOR (IRP) + REPOO (MO+MOLAST)
          ENDIF
C
        ENDIF
C
      ENDIF
C
C.....IF IPROP = 3 CONTINUE LOOP OVER THE MO'S
C     FOR EACH ISHELL, CALCULATE ONCE THE TOTAL RESULTS
      IF (IPROP .EQ. 3) THEN
        IF (TOTAL) THEN
          TOTAL = .FALSE.
          GOTO 600
        ELSE
          MO = MO + 1
          IF (MO .LE. MOMAX) GOTO 600
        ENDIF
      ENDIF
C
C.....NEXT SHELL (ISHELL)
 110  CONTINUE
C
C     --------------------
C     SUM ORBITAL ENERGIES
C     --------------------
C
      IF (IPROP .EQ. 2) THEN
        DO 120 I = 1, NCL (IRP)
          EPSS (1) = EPSS (1) + ALLEW (IRP, I)
 120    CONTINUE
        DO 130 I = NCL (IRP) + 1, NCL (IRP) + NOP (IRP)
          EPSS (2) = EPSS (2) + ALLEW (IRP, I) * OCOPN (IRP)
 130    CONTINUE
      ENDIF
C
      MOLAST = MOLAST + MO - 1
C
C.....NEXT SYM REP (IRP)
 100  CONTINUE
C
C     ---------------
C     COMBINE RESULTS
C     ---------------
C
      SKINEN = 0.0D0
C
      IF (OPENSH) THEN
        ISHMAX = 2
      ELSE
        ISHMAX = 1
      ENDIF
C
      DO 200 ISHELL = 1, ISHMAX
C
        ATTEN (ISHELL) = V (ISHELL)
        BMCON (ISHELL) = - TWOCSQ * BM (ISHELL)
        RKINEN (ISHELL) = SDP (ISHELL)
        ONELEN (ISHELL) = V (ISHELL) + SDP (ISHELL) - TWOCSQ*BM (ISHELL)
C
      SKINEN = SKINEN + RKINEN (ISHELL) + BMCON (ISHELL)
C
 200  CONTINUE
C
      IF (IPROP .NE. 2) THEN
        ELREPC (1) = 0.5D0 * QC (1)
        ELREPO (1) = QO (1)
        ELREPC (2) = QC (2)
        ELREPO (2) = 0.5D0 * COPCOF * QO (2)
      ELSE
        ELREPC (1) = 0.0D0
        ELREPO (1) = 0.0D0
        ELREPC (2) = 0.0D0
        ELREPO (2) = 0.0D0
      ENDIF
C
      IF (IPROP .EQ. 2 ) THEN
        ETOTAL = 0.5D0 * (ONELEN (1) + ONELEN (2) +
     +                    EPSS (1) + EPSS (2))
     +                 + POTNUC
      ELSE
        IF (ABS (QO (1) - QC (2)) .GT. 1.0D-9)
     +     PRINT*, ' QO (1) <> QC (2); DIF = ',
     +               ABS (QO (1) - QC (2))
        POTEN = ATTEN (1) + ATTEN (2) + POTNUC +
     +          ELREPC (1) + ELREPO (2) + ELREPC (2)
        IF ((RKINEN (1) + RKINEN (2)).NE.0.0)
     +  VIRIAL (1) = 2.0D0 * POTEN / (RKINEN (1) + RKINEN (2))
        VIRIAL (2) = POTEN + RKINEN (1) + RKINEN (2)
        ETOTAL = SKINEN + POTEN
      ENDIF
C
      CLOSE (MFDENSM)
      IF (IPROP .EQ. 3) CLOSE (MFDVECB)
      RETURN
      END
