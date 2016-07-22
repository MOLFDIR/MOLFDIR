C
C     =========================================================
      SUBROUTINE MFDTRAN (SR, SI, BUFR, BUFI, DC, DO, E,
     +                    VR, VI, SLR, SLI, FCR, FCI, FOR, FOI)
C     =========================================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'param.inc'
      INCLUDE 'common.inc'
      INCLUDE 'densfock.inc'
C
      REAL*8 SR (MS, MS), SI (MS, MS)
      REAL*8 SLR (MS, MS), SLI (MS, MS)
      REAL*8 QCR (N21*2), QCI (N21*2)
      REAL*8 QOR (N21*2), QOI (N21*2)
      REAL*8 BUFR (MSS), BUFI (MSS)
      EQUIVALENCE (QCR, DAAR), (QCI, DAAI)
      EQUIVALENCE (QOR, DOAAR), (QOI, DOAAI)
      REAL*8 NIDSRP (17)
      REAL*8 VR (NBM), VI (NBM)
      REAL*8 DC (NBM), DO (NBM), E (NBM)
      REAL*8 FCR (NBM*NBM), FCI (NBM*NBM)
      REAL*8 FOR (NBM*NBM), FOI (NBM*NBM)
C
      CHARACTER*80 CARD
      CHARACTER*4 REPNI
C
      LOGICAL BREIT1
C
      DIMENSION IRPMO(N28),IRPAMO(N28)
      REAL*8 ORB(NX1),ORBMO(N28)
      REAL*8 ONER(N28,N28),ONEI(N28,N28)
      REAL*8 AMOR(N22*2,NX1),AMOI(N22*2,NX1)
      COMPLEX*16 VMO (N22 * 2)
C
      IF (ONLYBRT) THEN
         OPEN (75,FILE='MRCONEE',FORM='UNFORMATTED')
         READ (75) NMO1,BREIT1,ETOTAL
         IF (NMO1.NE.NMO) CALL Q2JOB(2,'NMO ON MRCONEE',' ',0)
         IF (BREIT1)
     &      CALL Q2JOB(2,'BREIT INTERACTION ALREADY INCLUDED',' ',0)
         READ (75)
         READ (75)
         READ (75)
         READ (75) (IRPMO(IMO),IRPAMO(IMO),ORBMO(IMO),IMO=1,NMO1)
         READ (75) ((ONER(I,J),ONEI(I,J),I=1,NMO),J=1,NMO)
         ETOTAL = ETOTAL + ELREPC(1)
         CLOSE (75)
      ELSE
         DO 5 I = 1, NMO
         DO 5 J = 1, NMO
         ONER(I,J) = 0.0D0
         ONEI(I,J) = 0.0D0
    5    CONTINUE
      ENDIF
C
C     -----------------------------------
C     READ FORMATTED VECTORS FROM MFDVECA
C     -----------------------------------
C
      OPEN(MFDVECA,FILE=FNAM(8),FORM='FORMATTED')
      REWIND MFDVECA
C
      READ (MFDVECA,1000) CARD
      READ (MFDVECA,1000) CARD
      READ (MFDVECA,1000) CARD
      READ (MFDVECA,1000) CARD
      READ (MFDVECA,1000) CARD
      READ (CARD, 1001) FORMAT
      IF (FORMAT.EQ.' ') FORMAT='(6F22.16)'
      READ (MFDVECA,1000) CARD
      READ (MFDVECA,1000) CARD
      READ (MFDVECA,1000) CARD
C  
      NIDSRP (1) = 0
      DO 10 IRP = 1 ,NSYMRP
      NIDSRP( IRP + 1) = NIDSRP( IRP ) + NBSIM(IRP) * NBSIM(IRP)
   10 CONTINUE
      LMO = 0
C
C     -------------------
      DO 750 IRP=1,NSYMRP
C     -------------------
C
C     ---------------------------------------------------------------
C     ONLY THE FIRST MEMBER OF A REPRESENTATION IS HANDLED EXPLICITLY
C     ---------------------------------------------------------------
C
      IF (IAVIRP(IRP).NE.IRP) GOTO 199
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
C
C     --------------------------------------
C     ALWAYS CLOSED SHELL CASE : L(OPEN) = 0
C                                Q(OPEN) = 0
C     --------------------------------------
C
C     ----------------------------------------------------------
C     IF CLOSED SHELL CONSTRUCT F(CLOSE)
C     F(CLOSE) = ALPHA * L(OPEN) + Q(CLOSE) + Q(OPEN) = Q(CLOSE)
C     ----------------------------------------------------------
C
      DO 20 I = 1, NBAS * NBAS
        FCR (I) = 0.0D0
        FCI (I) = 0.0D0
 20   CONTINUE
C
      DO 30 I = 1, NBAS * NBAS
        FCR (I) = FCR (I) + QCR(ISRP+I-1)
        FCI (I) = FCI (I) + QCI(ISRP+I-1)
 30   CONTINUE
C
C
C     ---------------------------------
C     ADD ONE-ELECTRON PART TO F(CLOSE)
C     ---------------------------------
C
      IF (MSS .LT. NS * NL) 
     +   CALL Q2JOB(6, 'MFDTRAN MSS < NS*NL',' ',0)
      IF (.NOT.ONLYBRT)  CALL ADDONE (FCR, FCI, NBASL, NBASS, IREC,
     +             SLR, SLI, BUFR, BUFI, SR, SI)
C
C     ---------------------------------------------
C     READ ACTIVE ORBITALS FROM THIS REPRESENTATION
C     ---------------------------------------------
C
      NBAS = NBSIM (IRP)
      READ (MFDVECA, 1000) CARD
      NSKIP = NCOR(IRP)
      DO 110 MO=1,NSKIP
        READ (MFDVECA, 2220) DUM
 2220   FORMAT (10X, G20.10)
        READ (MFDVECA, FORMAT)
     +       (VMO (I), I = 1, NBAS)
  110   CONTINUE
        DO 120 MO = 1, NACT(IRP)
          READ (MFDVECA, 2220) ORB(MO)
          READ (MFDVECA, FORMAT)
     +         (AMOR(I,MO), AMOI(I,MO), I = 1, NBAS)
  120   CONTINUE
C
        DO 130 MO = 1, NSKP(IRP)
          READ (MFDVECA, 2220) DUM
          READ (MFDVECA, FORMAT)
     +         (VMO (I), I = 1, NBAS)
  130   CONTINUE
C
  199 CONTINUE
C
C     ---------------------------------------------
C     TRANSFORM FOCK MATRIX TO ACTIVE ORBITAL BASIS
C     ---------------------------------------------
C
      IF (IPRNT.GE.1) WRITE(6,1010) REPN(IRP)
      IF (LCHAIN.EQ.1) REPNI = REPN(IRP)(1:4)
      IF (LCHAIN.EQ.2) REPNI = REPN(IRP)(6:9)
      IF (LCHAIN.EQ.3) REPNI = REPN(IRP)(11:14)
      DO 210 IRPA = 1, NSYMRPA
      IF (REPNI.EQ.REPNA(IRPA)) GOTO 211
  210 CONTINUE
      CALL Q2JOB (2,'GROUP CHAIN ERROR',' ',0)
  211 CONTINUE
      DO 250 IMO = 1, NACT(IRP)
      KIMO = IMORAS (LMO+IMO)
C
C     --------------------------------------------------------------
C     CHECK IF THE TRANSFORMATION IS THE SAME AS IN THE PREVIOUS RUN
C     --------------------------------------------------------------
C
      IF (ONLYBRT.AND.IRPMO(KIMO).NE.IRP)
     &    CALL Q2JOB(2,'IRPMO ON MRCONEE',' ',0)
      IF (ONLYBRT.AND.IRPAMO(KIMO).NE.IRPA)
     &    CALL Q2JOB(2,'IRPAMO ON MRCONEE',' ',0)
      IF (ONLYBRT.AND.ORBMO(KIMO).NE.ORB(IMO))
     &    CALL Q2JOB(2,'ORBMO ON MRCONEE',' ',0)
      IRPMO(KIMO) = IRP
      IRPAMO(KIMO) = IRPA
      ORBMO(KIMO) = ORB(IMO)
      DO 240 JMO = 1, NACT(IRP)
      KJMO = IMORAS (LMO+JMO)
      K = 0
      DO 230 I = 1, NBAS
      DO 220 J = 1, NBAS
      K = K + 1
      ONER(KIMO,KJMO) = ONER(KIMO,KJMO) + AMOR(I,IMO)*FCR(K)*AMOR(J,JMO)
     &                                  + AMOI(I,IMO)*FCR(K)*AMOI(J,JMO)
     &                                  - AMOR(I,IMO)*FCI(K)*AMOI(J,JMO)
     &                                  + AMOI(I,IMO)*FCI(K)*AMOR(J,JMO)
      ONEI(KIMO,KJMO) = ONEI(KIMO,KJMO) + AMOR(I,IMO)*FCI(K)*AMOR(J,JMO)
     &                                  + AMOR(I,IMO)*FCR(K)*AMOI(J,JMO)
     &                                  - AMOI(I,IMO)*FCR(K)*AMOR(J,JMO)
     &                                  + AMOI(I,IMO)*FCI(K)*AMOI(J,JMO)
  220 CONTINUE
  230 CONTINUE
      IF (IPRNT.GE.1) WRITE (6,1011) IMO,JMO,KIMO,KJMO,
     &  ONER(KIMO,KJMO),ONEI(KIMO,KJMO)
  240 CONTINUE
  250 CONTINUE
C
      LMO = LMO + NACT(IRP)
  750 CONTINUE
C
      CLOSE (MFDVECA)
      BREIT1=.FALSE.
C     DIRAC writes the number of active electrons to file, this is
C     not so easy determined in MOLFDIR so initialize to zero.
      NELEC = 0
      IF (BREIT.OR.ONLYBRT) BREIT1=.TRUE.
      OPEN (75,FILE='MRCONEE',FORM='UNFORMATTED')
      REWIND (75)
      WRITE (75) NMO,BREIT1,ETOTAL
      WRITE (75) NSYMRP,(REPN(IRP),IRP=1,NSYMRP)
     &           ,(NELEC,IRP=1,NSYMRP)
      WRITE (75) NSYMRPA,(REPNA(IRPA),IRPA=1,NSYMRPA*2)
      WRITE (75) ((MULTB(I,J),I=1,2*NSYMRPA),J=1,2*NSYMRPA)
      WRITE (75) (IRPMO(IMO),IRPAMO(IMO),ORBMO(IMO),IMO=1,NMO)
      WRITE (75) ((ONER(IMO,JMO),ONEI(IMO,JMO),JMO=1,NMO),IMO=1,NMO)
      CLOSE (75)
C
 1000 FORMAT(A)
 1001 FORMAT(20A)
 1002 FORMAT(20X,F4.1)
 1010 FORMAT (/' EFFECTIVE 1-EL MATRIX ELEMENTS REPRESENTATION: ',A14)
 1011 FORMAT (4I4,2F20.10)
C
      RETURN
      END
