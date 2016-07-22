C
C     ==================
      SUBROUTINE MFDTABL
C     ==================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     SET UP TABLES
C
      INCLUDE 'paracom.inc'
      INCLUDE 'general.inc'
      INCLUDE 'memory.inc'
#if defined (PARALLEL)
      INCLUDE 'mpidef.inc'
#else
      PARAMETER (mpirnk=0)
#endif
C
      DIMENSION INDNRP(16),LTOT2(2)
      EQUIVALENCE(INDVEC,INDNRP)
      CHARACTER*2 CH, FILE1*12
C
C     --------------------------------------------------------------
C     RESEQUENCE THE REPRESENTATIONS TO KEEP ONLY THE OCCUPIED ONE'S
C     --------------------------------------------------------------
C
      DO 2 IRP=1,NSYMRP
        KRP=INDSRP(IRP)
        NCL(IRP)=NCL(KRP)
        NOC(IRP)=NOC(KRP)
	DO 1 I=1,NOPEN
	   NOP(IRP,I)=NOP(KRP,I)
	   OCOPN(IRP,I)=OCOPN(KRP,I)
    1   CONTINUE
        REPN(IRP)=REPN(KRP)
    2 CONTINUE
      IND = 0
      NSKIP = 0
      LRP = 1
      DO 100 IRP = 1, NSYMRP
        KRP = INDSRP (IRP)
        DO 110 ISKIP = 1, KRP - LRP
          NSKIP = NSKIP + NBSYM (ISKIP + LRP - 1)
 110    CONTINUE
        DO 120 I = 1, NBSYM (KRP)
          IND = IND + 1
          KSYMF (IND) = KSYMF (IND + NSKIP)
 120    CONTINUE
        LRP = KRP + 1
 100  CONTINUE
      DO 3 IRP=1,NSYMRP
        KRP=INDSRP(IRP)
        NBSYM(IRP)=NBSYM(KRP)
        NBSYM1(IRP)=NBSYM1(KRP)
        NBSYM2(IRP)=NBSYM2(KRP)
    3 CONTINUE
C
C     ---------------------------------------------------
C     RESEQUENCE ARRAY KSYMRP. THE ENTRY 0 MEANS THAT THE
C     REPRESENTATION IS TO BE DISCARDED
C     ---------------------------------------------------
C
      DO 5 I=1,NSYMF
        KSI=KSYMRP(I)
        KSYMRP(I)=INDNRP(KSI)
    5 CONTINUE
C
C     ---------------------------
C     FIND LARGEST REPRESENTATION
C     ---------------------------
C
      IG=1
      IH=1
      DO 10 IRP=1,NSYMRP
        IF (NBSYM1(IRP).GT.NBSYM1(IG)) IG=IRP
        IF (NBSYM2(IRP).GT.NBSYM2(IH)) IH=IRP
   10 CONTINUE
      NL=NBSYM1(IG)
      NS=NBSYM2(IH)
      IF (TWOC) NS = 0
      NBM=NL+NS
      MS=MAX0(NL,NS)
      NLL=NL*NL
      NLLH=(NLL+NL)/2
      NSS=NS*NS
      NSSH=(NSS+NS)/2
      NLS =NL*NS
      MLS=NL*MS
      MSS=MS*MS
C
C     ------------------------------------------------
C     SET NBSIM FOR USE WITH TWO - COMPONENT FORMALISM
C     ------------------------------------------------
C
      NBTOT1 = 0
      DO 60 IRP = 1, NSYMRP
        NBTOT1 = NBTOT1 + NBSYM1 (IRP)
        IF (TWOC) THEN
          NBSIM2 (IRP) = 0
          NBSIM1 (IRP) = NBSYM1 (IRP)
          NBSIM  (IRP) = NBSYM1 (IRP)
        ELSE
          NBSIM2 (IRP) = NBSYM2 (IRP)
          NBSIM1 (IRP) = NBSYM1 (IRP)
          NBSIM  (IRP) = NBSYM (IRP)
        ENDIF
 60   CONTINUE
      NBTOT1 = NBTOT1 / 2
C
C     ----------------------------------------------------------
C     SET UP POINTERS FOR SUBROUTINE CALLS
C     ----------------------------------------------------------
C
      IP1 = 1
      IP2 = IP1 + MS * MS
      IP3 = IP2 + MS * MS
      IP4 = IP3 + NL * NL
      IP5 = IP4 + NL * NL
      IP6 = IP5 + MLS
      IP7 = IP6 + MLS
      IP8 = IP7 + MSS + MS
      IP9 = IP8 + MSS + MS
      IP10 = IP9 + NLLH
      IP11 = IP10 + NLS
      IS1 = 1
      ID1 = 1
      ID2 = ID1 + MSS
      ID3 = ID2 + MSS
      ID4 = ID3 + NBM*NBM
      ID5 = ID4 + NBM*NBM
      ID6 = ID5 + NBM
      ID7 = ID6 + NBM
      ID8 = ID7 + NBM
      ID9 = ID8 + NBM
      ID10 = ID9 + MSS
      ID11 = ID10 + MSS
      ID12 = ID11 + NBM
      ID13 = ID12 + NBM*NBM
      ID14 = ID13 + NBM*NBM
      ID15 = ID14 + NBM*NBM
      IPR1 = 1
      IPR2 = IPR1 + NLLH
      IPR3 = IPR2 + NLLH
      IPR4 = IPR3 + NS * NL
      IPR5 = IPR4 + NS * NL
      IPR6 = IPR5 + NSSH
C
C     ------------------------------------------------------
C     SET UP INDEX ARRAY'S FOR VECTORS AND OCCUPIED ORBITALS
C     ------------------------------------------------------
C
      INDVEC(1)=0
      INDNEV(1)=0
      NV=0
      NO=0
      DO 30 IRP=1,NSYMRP
      IF (NBSYM1(IRP).EQ.0 .OR.
     +   ((NBSYM2(IRP).EQ.0) .AND. (.NOT. TWOC))) GOTO 28
      IF (.NOT. TWOC) THEN
      NV=NV+NBSYM(IRP)
      ELSE
      NV = NV + NBSYM1 (IRP)
      ENDIF
      NO=NO+NOC(IRP)
   28 INDVEC(IRP+1)=NV
      INDNEV(IRP+1)=NO
   30 CONTINUE
      NOCT=NO
C
C     ---------------------------------------
C     FIND NUMBER OF BASISFUNCTIONS PER GROUP
C     ---------------------------------------
C
      NBTOT=0
      IF (TWOC) THEN
      IMNEQ = NQ1
      ELSE
      IMNEQ = NEQUIV
      ENDIF
      DO 40 IM=1,IMNEQ
      IT=KCOMB(IM)
      IND=INDCOM(IT)
      KND=INDCOM(IT+1)
      NBAS=KND-IND
      NEM(IM)=NBTOT
      NBTOT=NBTOT + NBAS
      NEQBAS(IM)=NBAS
   40 CONTINUE
      NEM(IMNEQ+1)=NBTOT
      NBTOT1 = NBTOT
C
C     -----------------------------------------------------------
C     FIND LARGEST NUMBER OF BASISFUNCTIONS AND SYMMETRYFUNCTIONS
C     IN ANY GROUP
C     -----------------------------------------------------------
C
      NBMAX=0
      NSMAX=0
      DO 50 IM=1,IMNEQ
      NBAS=NEQBAS(IM)
      IF (NBAS.GT.NBMAX) NBMAX=NBAS
      IND=INDSYM(IM)
      KND=INDSYM(IM+1)
      NBS=KND-IND
      IF (NBS.GT.NSMAX)  NSMAX=NBS
   50 CONTINUE
      IF (NSMAX.GT.N3) CALL Q2JOB(3,'MFDTABL','N3',NSMAX)
      NCTOT=4*NBMAX*NSMAX
C
C     ------------------------------------------
C     SET UP POINTERS TO FIRST SUBREPRESENTATION
C     ------------------------------------------
C
      ID=1
      KRP=0
      IERR = 0
      DO 200 IRP=1,NSYMRP
        IF (ID.GT.1) THEN
          ID=ID-1
          IAVIRP(IRP)=ITHIRP
          IF (NCL(IRP).NE.NCL(ITHIRP)) THEN
             WRITE (6,1200) 'closed',IRP,REPN(IRP),ITHIRP,REPN(ITHIRP) 
             IERR = IERR + 1
          ENDIF
          IF (NOP(IRP,1).NE.NOP(ITHIRP,1)) THEN
             WRITE (6,1200) 'first open',
     &                      IRP,REPN(IRP),ITHIRP,REPN(ITHIRP) 
             IERR = IERR + 1
          ENDIF
          IF (NOP(IRP,2).NE.NOP(ITHIRP,2)) THEN
             WRITE (6,1200) 'second open',
     &                      IRP,REPN(IRP),ITHIRP,REPN(ITHIRP) 
             IERR = IERR + 1
          ENDIF
        ELSE
          CH=REPN(IRP)(1:2)
          ID=1
          IF (CH(1:1).EQ.'E'.OR.CH.EQ.'''F'.OR.CH.EQ.'"F') ID=2
          IF (CH(1:1).EQ.'F') ID=4
          KRP=KRP+1
          ITHIRP=IRP
#if defined (BIT64)
          AVIRP(KRP)=1.0D0/REAL(ID)
#else
          AVIRP(KRP)=1.0D0/DBLE(ID)
#endif
          IAVIRP(IRP)=ITHIRP
          IIRPAV(KRP,1)=IRP
          IF (KRP.GT.1) IIRPAV(KRP-1,2)=IRP-1
        ENDIF
 200    CONTINUE
      IIRPAV(KRP,2)=NSYMRP
      NAVIRP=KRP
C
C     --------------------------------------------------------
C     SET UP POINTERS BETWEEN KRAMERS' RELATED REPRESENTATIONS
C     --------------------------------------------------------
C
      DO IRP = 1, NSYMRP
         CH=REPN(IRP)(1:2)
         IF (CH(1:1).EQ.'A') THEN
            IKRIRP(IRP) = - IRP
            IF (MOD(NCL(IRP),2).NE.0) THEN
               WRITE (6,1201) 'closed',IRP,REPN(IRP)
               IERR = IERR + 1
            ENDIF
            IF (MOD(NOP(IRP,1),2).NE.0) THEN
               WRITE (6,1201) 'first open',IRP,REPN(IRP)
               IERR = IERR + 1
            ENDIF
            IF (MOD(NOP(IRP,2),2).NE.0) THEN
               WRITE (6,1201) 'second open',IRP,REPN(IRP)
               IERR = IERR + 1
            ENDIF
         ELSEIF (CH.EQ.'"E'.OR.CH.EQ.'"F') THEN
            IKIRP = ABS(IKRIRP(IRP-1))
            IKRIRP(IRP) = - IKIRP
            IF (NCL(IRP).NE.NCL(IKIRP)) THEN
               WRITE (6,1200) 'closed',
     &         IRP,REPN(IRP),IKIRP,REPN(IKIRP) 
               IERR = IERR + 1
            ENDIF
            IF (NOP(IRP,1).NE.NOP(IKIRP,1)) THEN
               WRITE (6,1200) 'first open',
     &                        IRP,REPN(IRP),IKIRP,REPN(IKIRP) 
               IERR = IERR + 1
            ENDIF
            IF (NOP(IRP,2).NE.NOP(IKIRP,2)) THEN
               WRITE (6,1200) 'second open',
     &                        IRP,REPN(IRP),IKIRP,REPN(IKIRP) 
               IERR = IERR + 1
            ENDIF
         ELSE
            IKRIRP(IRP) = IAVIRP(IRP)
         ENDIF
      ENDDO
C 
      IF (IERR.NE.0) CALL Q2JOB(6,
     &   'Check your Occup Card for inconsistencies',' ',1)
C
C     -------------------------------------
C     READ SYMMETRY TRANSFORMATION MATRICES
C     -------------------------------------
C
      IF (mpirnk .EQ. 0) THEN
         WRITE (FILE1,'(A7)') FNAM(4)
      ELSEIF (mpirnk .LT. 10) THEN
         WRITE (FILE1,'(A7,A1,I1)') FNAM(4),'.',mpirnk
      ELSEIF (mpirnk .LT. 100) THEN
         WRITE (FILE1,'(A7,A1,I2)') FNAM(4),'.',mpirnk
      ELSE
         WRITE (FILE1,'(A7,A1,I3)') FNAM(4),'.',mpirnk
      ENDIF
      OPEN(MFDSYMC,FILE=FILE1,FORM='UNFORMATTED',STATUS='OLD',
     +             ERR=10010)
      REWIND(MFDSYMC)
      K=0
      DO 300 IM=1,IMNEQ
        IMSET(IM)=K
        NBS=INDSYM(IM+1)-INDSYM(IM)
        NBAS=NEQBAS(IM)
        IF (K+NBAS*NBS.GT.N55) CALL Q2JOB(3,'MFDTABL','N55',K+NBAS*NBS)
        DO 310 J=1,NBS
          READ (MFDSYMC,END=10000) (TRANAR(K+I),TRANAI(K+I),
     +                              TRANBR(K+I),TRANBI(K+I),I=1,NBAS)
          K=K+NBAS
 310      CONTINUE
 300    CONTINUE
      CLOSE(MFDSYMC)
C
C === COMPUTE THE THINGS WE NEED TO MANAGE THE DENSITY FILE MFDENSM
C
      NBHTC = 0
      NBHTO = 0
      NBHTO2 = 0
      LREC = 0
      NDCREC = 0
      NDOREC = 0
      NDTREC = 0
      DO 400 IRP = 1, NSYMRP
      IAUX = NBSIM(IRP)*(NBSIM(IRP)+1)/2
      IF (IAUX .GT. LREC) LREC = IAUX
      INIRP(IRP) = 0
      INIRP(16 + IRP) = 0
      INIRP(32 + IRP) = 0
      IF (NCL(IRP) .NE. 0) THEN
      NBHTC = NBHTC + IAUX
      NDCREC = NDCREC + 1
      INIRP(IRP) = NDCREC
      ENDIF
      IF (NOP(IRP,1) .NE. 0)THEN
      NBHTO = NBHTO + IAUX
      NDOREC = NDOREC + 1
      INIRP(16 + IRP) = NDOREC
      ENDIF
      IF (NOP(IRP,2) .NE. 0)THEN
      NBHTO2 = NBHTO2 + IAUX
      NDTREC = NDTREC + 1
      INIRP(32 + IRP) = NDTREC
      ENDIF
  400 CONTINUE
C
C === COMPUTE HOW MUCH SPACE WE NEED
C
      LTOT1 = 0
      LTOT2(1) = 0
      LTOT2(2) = 0
      DO 500 IRP = 1, NSYMRP
      IF (NCL(IRP).NE.0) LTOT1=LTOT1+NBSIM(IRP)*(NBSIM(IRP)+1)/2
      IF (NOP(IRP,1).NE.0) LTOT2(1)=LTOT2(1)+NBSIM(IRP)*
     $                                 (NBSIM(IRP)+1)/2
      IF (NOP(IRP,2).NE.0) LTOT2(2)=LTOT2(2)+NBSIM(IRP)*
     $                                 (NBSIM(IRP)+1)/2
  500 CONTINUE
      LTOT3 = (IP11 + NSSH) / 4 + 1
      LTOT4 = (ID15 + NBM * NBM) / 4 + 1
      LTOT5 = (IPR6 + NSSH) / 4 + 1
      LTOT6 = NBTOT * (NBTOT + 1) / 2
      LTOT7 = 0
      DO 510 IRP = 1, NSYMRP
      LTOT7 = LTOT7 + NBSIM(IRP) * NBSIM(IRP)
  510 CONTINUE
      LTOT7 = LTOT7 / 2 + 1
      LENGTH = MAX(LTOT1,LTOT2(1),LTOT2(2),LTOT5,LTOT6,LTOT7,MSS)
      LENGTH1 = MAX(LTOT1,LTOT2(1),LTOT2(2),LTOT3,LTOT4,LTOT5,
     +              LTOT6,LTOT7)
      IS2 = 4 * LENGTH + 1
C
      RETURN
 1200 FORMAT ('Number of ',A,' shells in repr.',i3,' (',A,')',
     &' should be equal to repr.',i3,'(',A,')')
 1201 FORMAT ('Number of ',A,' shells in quaternionic repr.',
     &i3,'(',A,') should be even')
 1202 FORMAT (//I6,' inconsistencies found in the input OCCUP card')
10000 CALL Q2JOB(1,'MORE TRANSFORMATION COEFFICIENTS ON MFDSYMC',' ',0)
10010 CALL Q2JOB(1,'UNABLE TO OPEN MFDSYMC IN MFDTABL',' ',0)
      END
