C
C     =========================================================
      SUBROUTINE MFDIAGH (SR, SI, BUFR, BUFI, DC, DO, DOC, E,
     +                    VR, VI, SLR, SLI, FCR, FCI, FOR, FOI,
     +                    FOCR,FOCI,QCR,QCI,QOR,QOI,SCRR,SCRI,SR1,
     +                    QOCR,QOCI,SCCR,SCCI)
C     =========================================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'paracom.inc'
      INCLUDE 'general.inc'
      INCLUDE 'memory.inc'
      INCLUDE 'diis.inc'
C
      REAL*8 SR (MS, MS), SI (MS, MS)
      REAL*8 SLR (MS, MS), SLI (MS, MS)
      REAL*8 QCR (*), QCI (*)
      REAL*8 QOR (*), QOI (*)
      REAL*8 QOCR(*),QOCI(*)
      REAL*8 BUFR (MSS), BUFI (MSS)
      INTEGER NIDSRP (17)
      REAL*8 VR (NBM), VI (NBM)
      REAL*8 DC (NBM), DO (NBM), DOC(NBM), E (NBM)
      REAL*8 FCR (*), FCI (*)
      REAL*8 FOR (*), FOI (*)
      REAL*8 FOCR(*),FOCI(*)
      REAL*8 SCRR(*), SCRI(*)
      REAL*8 SCCR(*), SCCI(*)
      REAL*8 AALPHA
C
      LENREC=8 * (2*MSS+1)
      LENRC = LENREC
      LENREC=8 * MAX0(4*(NL+NS)+4,80)
      OPEN(MFDVECB,FILE=FNAM(2),ACCESS='DIRECT',
     $     RECL=LENREC)
C
      OPEN (MFDENSM, FILE='MFDENSM', FORM='UNFORMATTED',
     +      ACCESS='DIRECT',RECL=16 * LREC)
C
      MVAREC=2*NOCT+2
      MVBREC = 2
C
      NIDSRP (1) = 0
      DO 10 IRP = 1 ,NSYMRP
      NIDSRP( IRP + 1) = NIDSRP( IRP ) + NBSIM(IRP) * NBSIM(IRP)
   10 CONTINUE
C
C     ----------------------------------------------------------
C     CLEAR TWO-ELECTRON PART OF FOCK-MATRIX FOR FIRST ITERATION
C     ----------------------------------------------------------
C
      IF (FIRST) THEN
        DO 15 I = 1, 2 * LENGTH
        QCR (I) = 0.0D0
        QCI (I) = 0.0D0
 15     CONTINUE
        IF (OPENSH) THEN
          DO 16 I = 1, 2 * LENGTH
            QOR (I) = 0.0D0
            QOI (I) = 0.0D0
 16       CONTINUE
        ENDIF
        IF (OPENSH.AND.NOPEN.GT.1) THEN
          DO 17 I = 1, 2 * LENGTH
    	    QOCR(I) = 0.0D0
	    QOCI(I) = 0.0D0
 17       CONTINUE
        ENDIF
      ENDIF
C
C     -------------------
      DO 750 IRP=1,NSYMRP
C     -------------------
C
      NBASL = NBSIM1 (IRP)
      NBASS = NBSIM2 (IRP)
      NBAS = NBSIM (IRP)
      NLR=NBLR(IRP)
      NSR=NBSR(IRP)
      IF (TWOC) NSR=0
      NTR=NLR+NSR
      IREC = (IRP - 1) * NREC
      ISRP = NIDSRP (IRP) + 1
C
      CALL MHERR (QCR(ISRP),QCI(ISRP),NBAS,NBAS)
      IF (OPENSH) CALL MHERR (QOR(ISRP),QOI(ISRP),NBAS,NBAS)
      IF (OPENSH.AND.NOPEN.GT.1) 
     &            CALL MHERR (QOCR(ISRP),QOCI(ISRP),NBAS,NBAS)
C
C
C     -------------------------------------------------
C     IF OPENSHELL CONSTRUCT F(OPEN) = ALPHA * L(CLOSE)
C     FIRST OPEN SHELL !!!!!!!
C     -------------------------------------------------
C
      IF (NOP (IRP,1) .NE. 0) THEN
C
      AALPHA = (1.0D0 - COPCOF(1)) / (1.0D0 - OCOPN (IRP,1))
      IF (FIRST .OR. (NCL (IRP) .EQ. 0)) THEN
        DO 155 I = 1, NBAS * NBAS
          FOR (ISRP+I-1) = 0.0D0
 155      FOI (ISRP+I-1) = 0.0D0
      ELSE
          CALL CONCOP (FOR(ISRP), FOI(ISRP), QOR(ISRP), QOI(ISRP), IREC,
     +                          NBASL, NBASS, SLR, SLI, SR, SI, BUFR,
     +                          BUFI, AALPHA,IRP,1)
      ENDIF
C
C     ----------------------------------------------------------
C     CONSTRUCT F(OPEN) = F(OPEN) + Q(CLOSE) + COPCOF * Q(OPEN1)
C                                 + Q(OPEN2)
C     ----------------------------------------------------------
C
      IF (NOPEN.GT.1) THEN
        DO 35 I = 1, NBAS * NBAS
          FOR (ISRP+I-1) = FOR (ISRP+I-1) + QCR (ISRP + I - 1)
     +                      + COPCOF(1) * QOR (ISRP + I - 1)
     +                      + QOCR (ISRP + I - 1)
          FOI (ISRP-1+I) = FOI (ISRP-1+I) + QCI (ISRP + I - 1)
     +                      + COPCOF(1) * QOI (ISRP + I - 1)
     +                      + QOCI (ISRP + I - 1)
 35     CONTINUE
      ELSE
        DO 36 I = 1, NBAS * NBAS
          FOR (ISRP+I-1) = FOR (ISRP+I-1) + QCR (ISRP + I - 1)
     +                      + COPCOF(1) * QOR (ISRP + I - 1)
          FOI (ISRP-1+I) = FOI (ISRP-1+I) + QCI (ISRP + I - 1)
     +                      + COPCOF(1) * QOI (ISRP + I - 1)
 36     CONTINUE
      ENDIF
C
C     ---------------------------------
C     ADD ONE-ELECTRON PART TO F(OPEN)
C     ---------------------------------
C
      CALL ADDONE (FOR(ISRP), FOI(ISRP), NBASL, NBASS, IREC,
     +             SLR, SLI, BUFR, BUFI, SR, SI)

C
C     -----------------------------------------
C     TRANSFORM FOCK MATRIX TO ORTHOGONAL BASIS
C     -----------------------------------------
C
      CALL ORTHO (FOR(ISRP), FOI(ISRP), NBASL, NBASS,
     +            IREC, SR, SI, BUFR, BUFI,NLR,NSR,1)
C     END OF OPENSHELL PART 1
      ENDIF
C
C     -------------------------------------------------
C     IF OPENSHELL CONSTRUCT F(OPEN) = ALPHA * L(CLOSE)
C     SECOND OPEN SHELL !!!!!!!
C     -------------------------------------------------
C
      IF (NOP (IRP,2) .NE. 0) THEN
C
      AALPHA = (1.0D0 - COPCOF(2)) / (1.0D0 - OCOPN (IRP,2))
      IF (FIRST .OR. (NCL (IRP) .EQ. 0)) THEN
        DO 156 I = 1, NBAS * NBAS
          FOCR (ISRP+I-1) = 0.0D0
 156      FOCI (ISRP+I-1) = 0.0D0
      ELSE
          CALL CONCOP (FOCR(ISRP),FOCI(ISRP),QOCR(ISRP),QOCI(ISRP),IREC,
     +                          NBASL, NBASS, SLR, SLI, SR, SI, BUFR,
     +                          BUFI, AALPHA,IRP,1)
      ENDIF
c
C     ----------------------------------------------------------
C     CONSTRUCT F(OPEN) = F(OPEN) + Q(CLOSE) + COPCOF * Q(OPEN2)
C                                 + Q(OPEN1)
C     ----------------------------------------------------------
C
      DO 37 I = 1, NBAS * NBAS
        FOCR (ISRP+I-1) = FOCR (ISRP+I-1) + QCR (ISRP + I - 1)
     +                    + COPCOF(2) * QOCR (ISRP + I - 1)
     +                    + QOR (ISRP + I - 1)
        FOCI (ISRP-1+I) = FOCI (ISRP-1+I) + QCI (ISRP + I - 1)
     +                    + COPCOF(2) * QOCI (ISRP + I - 1)
     +                    + QOI (ISRP + I - 1)
 37   CONTINUE
C
C     ---------------------------------
C     ADD ONE-ELECTRON PART TO F(OPEN)
C     ---------------------------------
C
      CALL ADDONE (FOCR(ISRP), FOCI(ISRP), NBASL, NBASS, IREC,
     +             SLR, SLI, BUFR, BUFI, SR, SI)

C
C     -----------------------------------------
C     TRANSFORM FOCK MATRIX TO ORTHOGONAL BASIS
C     -----------------------------------------
C
      CALL ORTHO (FOCR(ISRP), FOCI(ISRP), NBASL, NBASS,
     +            IREC, SR, SI, BUFR, BUFI,NLR,NSR,1)
C     END OF OPENSHELL PART 2
      ENDIF
C
C     -----------------------------------------------------
C     IF CLOSED SHELL CONSTRUCT F(CLOSE) = ALPHA * L(OPEN1)
C     AlWAYS CONSTRUCT CLOSED SHELL MATRIX : EVEN IF REP IS
C     UNOCCUPIED
C     -----------------------------------------------------
C
      IF (FIRST .OR. (NOP (IRP,1) .EQ. 0)) THEN
        DO 20 I = 1, NBAS * NBAS
          FCR (ISRP-1+I) = 0.0D0
          FCI (ISRP-1+I) = 0.0D0
 20     CONTINUE
      ELSE
        AALPHA = (1.0D0 - COPCOF(1)) / (1.0D0 - OCOPN (IRP,1))
        CALL CONCOP (FCR(ISRP), FCI(ISRP), QOR(ISRP), QOI(ISRP),
     +               IREC, NBASL, NBASS,
     +               SLR, SLI, SR, SI, BUFR, BUFI, AALPHA,IRP,2)
      ENDIF
C
C     -----------------------------------------------------
C     IF CLOSED SHELL CONSTRUCT F(CLOSE) = ALPHA * L(OPEN2)
C     -----------------------------------------------------
C
      IF (.NOT.(FIRST .OR. (NOP (IRP,2) .EQ. 0))) THEN
        AALPHA = (1.0D0 - COPCOF(2)) / (1.0D0 - OCOPN (IRP,2))
        CALL CONCOP (FCR(ISRP), FCI(ISRP), QOCR(ISRP), QOCI(ISRP),
     +               IREC, NBASL, NBASS,
     +               SLR, SLI, SR, SI, BUFR, BUFI, AALPHA,IRP,3)
      ENDIF
C
C     ----------------------------------------------------------------
C     CONSTRUCT F(CLOSE) = F(CLOSE) + Q(CLOSE) +? Q(OPEN1) +? Q(OPEN2)
C     ----------------------------------------------------------------
C
      DO 30 I = 1, NBAS * NBAS
        FCR (ISRP-1+I) = FCR (ISRP-1+I) + QCR(ISRP+I-1)
        FCI (ISRP-1+I) = FCI (ISRP-1+I) + QCI(ISRP+I-1)
 30   CONTINUE
C
      IF (OPENSH) THEN
        DO 31 I = 1, NBAS * NBAS
          FCR (ISRP-1+I) = FCR (ISRP-1+I) + QOR(ISRP+I-1)
          FCI (ISRP-1+I) = FCI (ISRP-1+I) + QOI(ISRP+I-1)
 31     CONTINUE
      ENDIF
      IF (OPENSH.AND.NOPEN.GT.1) THEN
        DO 32 I = 1, NBAS * NBAS
          FCR (ISRP-1+I) = FCR (ISRP-1+I) + QOCR(ISRP+I-1)
          FCI (ISRP-1+I) = FCI (ISRP-1+I) + QOCI(ISRP+I-1)
 32     CONTINUE
      ENDIF
C
C     ---------------------------------
C     ADD ONE-ELECTRON PART TO F(CLOSE)
C     ---------------------------------
C
      IF (MSS .LT. NS * NL) CALL Q2JOB(6,
     +   'MSS < NS*NL BEFORE MFDIAGH_ADDONE',' ',1)
      CALL ADDONE (FCR(ISRP), FCI(ISRP), NBASL, NBASS, IREC,
     +             SLR, SLI, BUFR, BUFI, SR, SI)
C
C     -----------------------------------------
C     TRANSFORM FOCK MATRIX TO ORTHOGONAL BASIS
C     -----------------------------------------
C
      CALL ORTHO (FCR(ISRP), FCI(ISRP), NBASL, NBASS,
     +            IREC, SR, SI, BUFR, BUFI,NLR,NSR,1)
C
C     END OF CLOSED SHELL PART
C
  750 CONTINUE
C
C === WE NOW HAVE THE ENTIRE FOCK MATRIX IN FC AND FO
C === PERFORM THE DIIS EXTRAPOLATION IF NECESSARY
C
      IF (UDIIS) THEN
      CALL DIIS(FCR,FCI,FOR,FOI,FOCR,FOCI,QCR,QCI,QOR,QOI,QOCR,QOCI,
     +          SR,SR1,SCRR,SCRI,SCCR,SCCI)
      ENDIF
C
C === WE SET UP A NEW LOOP OVER NSYMRP IN WHICH WE DIAGONALIZE THE 
C === FOCK MATRIX AND SELECT THE EIGENVECTORS
C
      DO 800 IRP = 1, NSYMRP
C
C     -----------------------------------------------------
C     Do only one of a symmetry and/or Kramers' related set.
C     -----------------------------------------------------
C
      IF (IABS(IKRIRP(IRP)).NE.IRP) THEN
         MVBREC = MVBREC + NOC(IRP)
         MVAREC = MVAREC + NBLR(IRP) + NBSR(IRP)
         GOTO 800
      ENDIF
C
      NBASL = NBSIM1 (IRP)
      NBASS = NBSIM2 (IRP)
      NBAS = NBSIM (IRP)
      NLR=NBLR(IRP)
      NSR=NBSR(IRP)
      IF (TWOC) NSR=0
      NTR=NLR+NSR
      IREC = (IRP - 1) * NREC
      ISRP = NIDSRP (IRP) + 1
      IF (OPENSH .AND. NOP(IRP,1) .NE. 0) THEN
C
C     ------------------------------------------------------------
C     ADD -2.0*C*C TO THE DIAGONAL ELEMENTS OF THE SMALL COMPONENT
C     SECTION OF THE DIRAC-MATRIX IN THE ORTHOGONAL BASIS
C     ------------------------------------------------------------
C
      IF (.NOT. TWOC) THEN
        DO 45 I = NLR * (NBAS + 1) + 1,
     +            NTR * NBAS - NBAS + NTR,
     +            NBAS + 1
        FOR (ISRP-1+I) = FOR (ISRP-1+I) - TWOCSQ
   45   CONTINUE
      ENDIF
C
C     ------------------------------------------
C     DIAGONALIZE THE OPEN FOCK MATRIX 'F' FOCK1
C     ------------------------------------------
C
      CALL CRDIAG(NBAS,NTR,FOR(ISRP),FOI(ISRP),DO,BUFR,BUFI,E,E,VR,
     +            IFAIL)
C
      IGAP = 0
      DO 900 I=1,NSR
        IF (DO(I).GT.-TWOCSQ) IGAP = IGAP + 1
 900    CONTINUE
        IF (IGAP.GT.0) WRITE(6,1000) '1st Open',IRP,IGAP,DO(NSR)+TWOCSQ
      ENDIF
C
      IF (OPENSH .AND. NOP(IRP,2) .NE. 0) THEN
C
C     ------------------------------------------------------------
C     ADD -2.0*C*C TO THE DIAGONAL ELEMENTS OF THE SMALL COMPONENT
C     SECTION OF THE DIRAC-MATRIX IN THE ORTHOGONAL BASIS
C     ------------------------------------------------------------
C
      IF (.NOT. TWOC) THEN
        DO 46 I = NLR * (NBAS + 1) + 1,
     +            NTR * NBAS - NBAS + NTR,
     +            NBAS + 1
        FOCR (ISRP-1+I) = FOCR (ISRP-1+I) - TWOCSQ
   46   CONTINUE
      ENDIF
C
C     ------------------------------------------
C     DIAGONALIZE THE OPEN FOCK MATRIX 'F' FOCK2
C     ------------------------------------------
C
      CALL CRDIAG(NBAS,NTR,FOCR(ISRP),FOCI(ISRP),DOC,BUFR,BUFI,
     +            E,E,VR,IFAIL)
C
      IGAP = 0
      DO 901 I=1,NSR
        IF (DOC(I).GT.-TWOCSQ) IGAP = IGAP + 1
 901    CONTINUE
        IF (IGAP.GT.0) WRITE(6,1000) '2nd Open',IRP,IGAP,DOC(NSR)+TWOCSQ
      ENDIF
C
C     ------------------------------------------------------------
C     ADD -2.0*C*C TO THE DIAGONAL ELEMENTS OF THE SMALL COMPONENT
C     SECTION OF THE DIRAC-MATRIX IN THE ORTHOGONAL BASIS
C     ------------------------------------------------------------
C
      IF (.NOT. TWOC) THEN
        DO 40 I = NLR * (NBAS + 1) + 1,
     +            NTR * NBAS - NBAS + NTR,
     +            NBAS + 1
        FCR (ISRP-1+I) = FCR (ISRP-1+I) - TWOCSQ
   40   CONTINUE
      ENDIF
C
C     ---------------------------------------
C     DIAGONALIZE THE CLOSED FOCK MATRIX 'F'
C     ---------------------------------------
C
      CALL CRDIAG(NBAS,NTR,FCR(ISRP),FCI(ISRP),DC,BUFR,BUFI,E,E,VR,
     +            IFAIL)
C
      IGAP = 0
      DO 910 I=1,NSR
        IF (DC(I).GT.-TWOCSQ) IGAP = IGAP + 1
 910    CONTINUE
        IF (IGAP.GT.0) WRITE(6,1000) 'Closed',IRP,IGAP,DC(NSR)+TWOCSQ
C
C     -------------------------------------------
C     SELECT OPEN SHELL VECTORS AND PRINT RESULTS
C     VIRTUALS FROM CLOSED SHELL SET
C     -------------------------------------------
C
C
      CALL SELVEC (IRP,DC(NSR+1),DO(NSR+1),DOC(NSR+1),NBAS,
     +             FCR (ISRP - 1 + (1 + NSR * NBAS)),
     +             FCI (ISRP - 1 + (1 + NSR * NBAS)),
     +             FOR (ISRP - 1 + (1 + NSR * NBAS)),
     +             FOI (ISRP - 1 + (1 + NSR * NBAS)),
     +             FOCR(ISRP - 1 + (1 + NSR * NBAS)),
     +             FOCI(ISRP - 1 + (1 + NSR * NBAS)))
C
c     IF (IAVIRP(IRP).EQ.IRP) THEN
      IF (PRTVC) THEN
        WRITE (*, 1020) IRP,INDSRP(IRP),REPN(IRP)(1:LCHAIN*5-1)
        I = NSR * NBAS
        DO 50 MO = 1, NOC (IRP)
          WRITE (*, 1030) DC (MO + NSR), (FCR(ISRP - 1 + (I + K)),
     +                    FCI(ISRP - 1 + (I + K)), K = 1, NTR)
          I = I + NBAS
 50     CONTINUE
      ENDIF
c     ENDIF
C
      DO 60 IEV = 1, NOC (IRP)
        ALLEW (IRP, IEV) = DC (NSR + IEV)
 60   CONTINUE
C
c     IF (IAVIRP(IRP).EQ.IRP) THEN
      IF (PRTEW.OR.LASTRUN)
     +   WRITE(*,1050) IRP,INDSRP(IRP),REPN(IRP)(1:LCHAIN*5-1),
     +                 (DC(I),I=1,NTR)
c     ENDIF
C
C     ---------------------------------------------------------
C     SELECT EIGENVECTORS, TEST FOR CONVERGENCY AND EXTRAPOLATE
C     ---------------------------------------------------------
C
      CALL MFDSEIV (IRP, NTR, NBAS, FCR(ISRP - 1 + (1 + NSR * NBAS)),
     +              FCI(ISRP - 1 + (1 + NSR * NBAS)),
     +              DC (NSR + 1))
C
C     -----------------------------------------------------------
C     TRANSFORM THE EIGENVECTORS BACK TO THE NON-ORTHOGONAL BASIS
C     -----------------------------------------------------------
C
      READ (MFDOEMR, REC = IREC + 8)
     +     ((SLR (I,J), I=1,NBASL), J=1,NLR)
      READ (MFDOEMI, REC = IREC + 8)
     +     ((SLI (I,J), I=1,NBASL), J=1,NLR)
C
      IF (.NOT. TWOC) THEN
        READ (MFDOEMR, REC = IREC + 9)
     +     ((SR (I,J), I=1,NBASS), J=1,NSR)
        READ (MFDOEMI, REC = IREC + 9)
     +     ((SI (I,J), I=1,NBASS), J=1,NSR)
      ENDIF
C
      NOV = INDNEV (IRP)
C
      DO 410 NO = 1, NOC (IRP)
      J = NEIV (NOV + NO) + NSR - 1
      CALL HMV (NBASL,NLR,MS,SLR,SLI,FCR(ISRP - 1 + (1 + J * NBAS)),
     +           FCI(ISRP - 1 + (1 + J * NBAS)), VR, VI)
      IF (.NOT. TWOC)
     +CALL HMV(NBASS,NSR,MS,SR,SI,FCR(ISRP - 1 + (1 + J * NBAS + NLR)),
     +           FCI(ISRP - 1 + (1 + J * NBAS + NLR)), VR (NBASL + 1),
     +           VI (NBASL + 1))
      MVBREC=MVBREC+1
      WRITE(MFDVECB,REC=MVBREC) INDSRP(IRP),IRP,NBAS,DC(J+1),
     +                    (VR(I),VI(I),I=1,NBAS)
  410 CONTINUE
C
      DO 420 I = 0, NTR - 1
      CALL HMV(NBASL,NLR,MS,SLR,SLI,FCR(ISRP - 1 + (1 + I * NBAS)),
     +           FCI(ISRP - 1 + (1 + I * NBAS)), VR, VI)
      IF (.NOT. TWOC)
     +CALL HMV(NBASS,NSR,MS,SR,SI,FCR(ISRP - 1 + (1 + I * NBAS + NLR)),
     +           FCI(ISRP - 1 + (1 + I * NBAS + NLR)),
     +           VR (NBASL + 1), VI (NBASL + 1))
      MVAREC = MVAREC + 1
      WRITE (MFDVECB,REC = MVAREC) INDSRP (IRP), IRP, NBAS, DC (I + 1),
     +                         (VR (J), VI (J), J = 1, NBAS)
 420  CONTINUE
C
  800 CONTINUE
C
      CLOSE (MFDENSM)
C
C     -------------------------------------------------------
C     FORCE DEGENERATE REPS TO BE EQUAL (PERFECT TYM REV SYM)
C     -------------------------------------------------------
C
      CALL TIMSYM(FCR,FCI)
      CLOSE (MFDVECB)
      RETURN
 1000 FORMAT(1X,'Warning: ',A10,' Shell Rep ',I2,' has',I4,
     &       ' EV above 2mc2: highest is -2mcc +',E8.1)
 1020 FORMAT(//'EIGENVALUES AND EIGENVECTORS REPRESENTATION',I3,
     &' (',I2,') (',A,')'/)
 1030 FORMAT(F15.7,(T16,' (',F9.7,',',F9.7,')',:,' (',F9.7,',',F9.7,')',
     $                :,' (',F9.7,',',F9.7,')',:,' (',F9.7,',',F9.7,')',
     $                :,' (',F9.7,',',F9.7,')'))
 1050 FORMAT(//'EIGENVALUES REPRESENTATION',I3,' (',I2,') (',A,')'
     &//6(F16.9,2X))
      END
