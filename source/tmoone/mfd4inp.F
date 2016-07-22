C
C     ==================
      SUBROUTINE MFD4INP
C     ==================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     READ INPUT DATA
C
      INCLUDE 'param.inc'
      INCLUDE 'common.inc'
#if defined (PARALLEL)
      INCLUDE 'mpidef.inc'
#else
      PARAMETER (mpirnk=0)
#endif
C
      DIMENSION NEQ(2),INDNRP(16), NOP2(16)
      INTEGER IUNITS(6)
      CHARACTER*8 TIMEX
      CHARACTER*80 CARD, FILE1*12
      INTEGER TOTAL
      LOGICAL DEFRAS
      EQUIVALENCE  (IUNITS(1),SRTTWLL)
      EQUIVALENCE (NEQ,NQ1),(INDVEC,INDNRP)
      EQUIVALENCE (CARD,FORMAT)
C
      NAMELIST/TMOONE/IPRNT,BREIT,ONLYBRT,MFIL,CLIGHT
      NAMELIST/OCCUP/TOTAL,NOPEN
      NAMELIST/EDIT/NMO,DEFRAS
      NAMELIST/GENERAL/TWOC
C
C     -------------------------------------------------------
C     READ TABLES AND ONE-ELECTRON INTEGRALS AS CALCULATED BY
C     PROGRAM "RELONEL"
C     -------------------------------------------------------
C
      IF (mpirnk .EQ. 0) THEN
         WRITE (FILE1,'(A7)') FNAM(3)
      ELSEIF (mpirnk .LT. 10) THEN
         WRITE (FILE1,'(A7,A1,I1)') FNAM(3),'.',mpirnk
      ELSEIF (mpirnk .LT. 100) THEN
         WRITE (FILE1,'(A7,A1,I2)') FNAM(3),'.',mpirnk
      ELSE
         WRITE (FILE1,'(A7,A1,I3)') FNAM(3),'.',mpirnk
      ENDIF
      OPEN(MFDONE,STATUS='OLD',FORM='UNFORMATTED',
     +            FILE=FILE1,ERR=20000)
      REWIND MFDONE
      READ (MFDONE) T0,D0,VERS0,TEXT,T1,D1,VERS1
      READ (MFDONE) NQ1,NEQUIV,IDUM,NEQCOM,IDUM,NCOMB,NSYMOP,IDUM,
     $              NSYMF,
     $              NCOEF,ISYMRP,IDUM,IDUM,IDUM,IDUM,IDUM,POTNUC
      READ (MFDONE)
      READ (MFDONE)
      READ (MFDONE) (KCOMB(I),IDUM,INDSYM(I),I=1,NEQUIV)
      READ (MFDONE)
      READ (MFDONE) (IDUM,I=1,NCOMB*(NSYMOP+2)),(INDCOM(I),I=1,NEQCOM)
      READ (MFDONE) (INDORB(I),INDB(I),KSYMRP(I),LSYMRP(I),KSYMF(I),
     $              I=1,NSYMF)
      READ (MFDONE)
      READ (MFDONE) (NBSYM(I),NBSYM1(I),NBSYM2(I),I=1,ISYMRP)
      READ (MFDONE) LCHAIN,(REPN(I),I=1,ISYMRP)
      INDCOM(NEQCOM+1)=NCOMB
      INDORB(NSYMF+1)=NCOEF
      INDSYM(NEQUIV+1)=NSYMF
      if (mpirnk.eq.0) WRITE(*,1020) VERSION,TEXT
C
      SCFTXT='THIS IS TMOONE CALLING'
      PRTOD=.FALSE.
      PRTRD=.FALSE.
      PRTDM=.FALSE.
      PRTEW=.FALSE.
      PRTVC=.FALSE.
      XORTHO=.TRUE.
      NCYCLS=IDEF2
      CONVCRT=DEF2
      CLIGHT=0.0D0
      ONLYONE=.FALSE.
      ONLYBRT=.FALSE.
      SELECT=.FALSE.
      TWOC=.FALSE.
      MFIL=3
      BREIT=.FALSE.
      SFRAC=0.0D0
      DEGEN=1.0D-8
      TIMING=.FALSE.
      IPRNT=0
C
      OPEN (5, FILE='MOLFDIR.INP', FORM='FORMATTED')
C
      CALL LOCATE('GENERAL',7,ITEST)
      IF (ITEST.GT.0) READ(5,GENERAL)
      IF (TWOC) MFIL=1
      CALL LOCATE('TMOONE',6,ITEST)
      IF (ITEST.GT.0) READ(5,TMOONE)
      IF (BREIT) ONLYBRT=.FALSE.
      IF (ONLYBRT) BREIT=.FALSE.
      IF (CLIGHT .GT. C) THEN
      C = CLIGHT
      ONOC1 = 0.0D0
      ONOC2 = 0.0D0
      ELSE IF (CLIGHT .NE. 0.0D0) THEN
      C = CLIGHT
      ONOC1 = 1.0D0 / C
      ONOC2 = 1.0D0 / (C * C)
      ELSE
      ONOC1 = 1.0D0 / C
      ONOC2 = 1.0D0 / (C * C)
      ENDIF
C
      TWOCSQ=2.0D0*C*C
C
      PRINT=PRTOD.OR.PRTRD.OR.PRTDM.OR.PRTEW.OR.PRTVC
      IPOL=1
      CALL DAYTIME(DATUM,TIMEX)
      if (mpirnk.eq.0) WRITE (*,1030) SCFTXT,D0,T0,VERS0,
     $                      D1,T1,VERS1,
     $                      D2,T2,VERS2,
     $                      DATUM,TIMEX
      if (mpirnk.eq.0) WRITE (*,1040) BREIT,ONLYBRT,IPRNT
      FORMAT='(6F22.16)'
      if (mpirnk.eq.0) WRITE(*,1005)
      NSYMRP = 0
      NOCCT = 0
      OPENSH = .FALSE.
C
      NMO=0
      DEFRAS=.FALSE.
      CALL LOCATE('EDIT',4,ITEST)
      IF (ITEST.GT.0) THEN
         READ(5,EDIT)
      ELSE
         GOTO 10000
      ENDIF
      IF (NMO .GT. N28) CALL Q2JOB
     +   (3, 'MFD4INP', 'N28', NMO)
      READ (5,1090,END=10020) (NCOR(IRP),IRP=1,ISYMRP)
      READ (5,1090,END=10020) (NACT(IRP),IRP=1,ISYMRP)
      IF (DEFRAS) THEN
         PRINT*,'USER DEF. OF RAS SPACES NOT YET PROGRAMMED'
         CALL Q2JOB(6,'MFD4INP','RAS SPACES',0)
      ELSE
C
         TOTAL = 0
         NOPEN = 0
         CALL LOCATE('OCCUP',5,ITEST)
         IF (ITEST.GT.0) READ(5,OCCUP,END=10000)
         READ (5,1090,END=10020) (NCL(IRP),IRP=1,ISYMRP)
         DO IRP = 1, ISYMRP
            NOP(IRP)=0
         ENDDO
         DO IOPEN = 1, NOPEN
            READ (5,1090,END=10020) (NOP2(IRP),IRP=1,ISYMRP)
            READ (5,*) DUM
            DO IRP = 1, ISYMRP
               NOP(IRP)=NOP(IRP)+NOP2(IRP)
            ENDDO
         ENDDO
C
         KMO = 0
         LMO = 0
C
C        ---------------------------------------------
C        RAS1 SPACE : NOT FROZEN CLOSED SHELL ORBITALS
C        ---------------------------------------------
C
         DO 5 IRP = 1, ISYMRP
         DO 4 IMO = 1, NCL(IRP) - NCOR(IRP)
         KMO = KMO + 1
         IMORAS(LMO+IMO) = KMO
    4    CONTINUE
         LMO = LMO + NACT(IRP)
    5    CONTINUE
C
C        ---------------------------------
C        RAS2 SPACE : OPEN SHELLS ORBITALS
C        ---------------------------------
C
         LMO = 0
         DO 7 IRP = 1, ISYMRP
         DO 6 IMO = NCL(IRP)-NCOR(IRP)+1, NCL(IRP)-NCOR(IRP)+NOP(IRP)
         KMO = KMO + 1
         IMORAS(LMO+IMO) = KMO
    6    CONTINUE
         LMO = LMO + NACT(IRP)
    7    CONTINUE
C
C        --------------------------------
C        RAS3 SPACE : UNOCCUPIED ORBITALS
C        --------------------------------
C
         LMO = 0
         DO 9 IRP = 1, ISYMRP
         DO 8 IMO = NCL(IRP)-NCOR(IRP)+NOP(IRP)+1, NACT(IRP)
         KMO = KMO + 1
         IMORAS(LMO+IMO) = KMO
    8    CONTINUE
         LMO = LMO + NACT(IRP)
    9    CONTINUE
         IF (NMO.NE.KMO) CALL Q2JOB(2,
     +                        'TOTAL NUMBER OF ACT. ORB.',' ',0)
      ENDIF
C
      TOTAL = TOTAL - NMO
      DO 10 IRP = 1, ISYMRP
      IF (NACT(IRP).GT.NX1)  CALL Q2JOB (3,
     +                            'MFD4INP', 'NX1', NACT(IRP))
      NSKP(IRP) = NBSYM1(IRP) - NCOR(IRP) - NACT(IRP)
      NCL(IRP) = NCOR(IRP)
      NOP(IRP) = 0
      OCOPN(IRP) = 0.0
      NOCC = NCL (IRP) + NOP (IRP)
      INDNRP (IRP)=0
      IF ((NBSYM1 (IRP) .GT. 0) .AND.
     +   (((NBSYM2 (IRP) .GT. 0) .AND. (.NOT. TWOC)) .OR.
     +   (TWOC))) THEN
          IF (NOCC.GT.NBSYM1(IRP)) CALL Q2JOB
     +    (2,'TOO MANY OCCUPIED ORBITAL REPR. '//REPN(IRP),' ',0)
          NOC (IRP) = NOCC
          NOCCT = NOCCT + NOCC
          NSYMRP = NSYMRP + 1
          INDSRP (NSYMRP) = IRP
          INDNRP (IRP) = NSYMRP
      ELSE
          NCL (IRP)=0
          NOP (IRP)=0
          OCOPN (IRP)=0.0D0
      ENDIF
   10 CONTINUE
C
      COPCOF = 0.0D0
C
      IF (OPENSH) THEN
C
      OPTOT = 0.0D0
      NOPTOT = 0
      NCLTOT=0
C
      DO 20 IRP = 1, ISYMRP
        NCLTOT=NCLTOT+NCL(IRP)
        OPTOT = OPTOT + OCOPN (IRP) * DBLE(NOP  (IRP))
        NOPTOT = NOPTOT + NOP (IRP)
 20   CONTINUE
#if defined (BIT64)
      NCLTOT=NCLTOT+ANINT(OPTOT)
#else
      NCLTOT=NCLTOT+DNINT(OPTOT)
#endif
C
      IF ((TOTAL.NE.0).AND.(NCLTOT.NE.TOTAL)) THEN
           PRINT*,'INCONSITENT TOTAL NUMBER OF ELECTRONS'
           CALL Q2JOB(2, 'MFD4INP','NEL',TOTAL)
      ENDIF
C
#if defined (BIT64)
      OPTOT = ANINT (OPTOT) / REAL (NOPTOT)
#else
      OPTOT = DNINT (OPTOT) / (DBLE (NOPTOT))
#endif
C
      DO 30 IRP = 1, ISYMRP
        IF (NOP (IRP) .GT. 0) OCOPN (IRP) = OPTOT
 30   CONTINUE
C
      IOPTOT = NINT (OPTOT * NOPTOT)
      COPTEL = DBLE(NOPTOT) * (DBLE(IOPTOT) - 1.0D0)
      COPNOEM = (DBLE(NOPTOT) - 1.0D0) * DBLE(IOPTOT)
      COPCOF = COPTEL / COPNOEM
C
      ENDIF
C
      IF (NOCCT .GT. N28) CALL Q2JOB
     +   (3, 'MFD4INP', 'N28', NOCCT)
      if (mpirnk.eq.0) then
      WRITE(*,1010) ' ORBITAL CONFIGURATION TABLE '
      WRITE(*,1005)
      WRITE(*,1050) 'MAIN GROUP IDENTIFIER',(REPN(I)( 1: 4),I=1,ISYMRP)
      IF (LCHAIN.GT.1)
     $WRITE(*,1050) 'SUBGROUP   IDENTIFIER',(REPN(I)( 6: 9),I=1,ISYMRP)
      IF (LCHAIN.GT.2)
     $WRITE(*,1050) '2ND SUBGRP IDENTIFIER',(REPN(I)(11:14),I=1,ISYMRP)
      WRITE(*,1060) 'LARGE COMPONENT BASIS',  (NBSYM1 (I),I=1,ISYMRP)
      WRITE(*,1060) 'SMALL COMPONENT',        (NBSYM2 (I),I=1,ISYMRP)
      WRITE(*,1060) 'NUMBER OF CORE ORBITALS',(NCOR   (I),I=1,ISYMRP)
      WRITE(*,1060) 'NUMBER OF ACT. ORBITALS',(NACT   (I),I=1,ISYMRP)
      endif
      IF (NSYMRP.EQ.0 .AND. .NOT.ONLYONE) CALL Q2JOB
     $(2,'NO OCCUPIED ORBITALS',' ',0)
C
      CLOSE(5)
C
      RETURN
 1005 FORMAT(//)
 1010 FORMAT(A)
 1020 FORMAT('1R E L A T I V I S T I C    T R A N S F O R M A T I O N',
     $       '    P R O G R A M'/33X,'V E R S I O N  ',A////1X,A)
 1030 FORMAT(1X,A//' INITIAL RUN ',      T20,2A12,' V : ',A10/
     $             ' INTEGRAL RUN 1 EL.',T20,2A12,' V : ',A10/
     $             ' INTEGRAL RUN 2 EL.',T20,2A12,' V : ',A10/
     $             ' TODAY IS',          T20,2A12)
 1040 FORMAT(///' TRANSFORMATION PARAMETERS:'//' BREIT',T15,L1
     $       /' ONLYBRT',T15,L1/' IPRNT',T12,I4)
 1050 FORMAT(1X,A,T27,16(1X,A4))
 1060 FORMAT(1X,A,T25,16I5)
 1090 FORMAT (16I4)
10000 CALL Q2JOB(1,'OCCUPATION',' ',0)
10020 CALL Q2JOB(1,'--NUMBER OF CLOSED/OPEN SHELLS, OCCUP.',' ',0)
20000 CALL Q2JOB(2,'FILE MFDONE (UN14) CANNOT BE ACCESSED',' ',0)
      END
