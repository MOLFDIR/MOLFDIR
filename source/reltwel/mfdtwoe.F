C
C     ==================
      SUBROUTINE MFDTWOE
C     ==================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'work.inc'
      INCLUDE 'paracom.inc'
C
      COMMON/BUFR/GINTEG(N2,14)
#if defined (BIT64) 
      COMMON/BUFI/LABEL(N2,14)
#else
      COMMON/BUFI/LABEL(N2,14),LABEL1(N2,14)
#endif
C
#if defined (IBM) 
      INTEGER*8 NUMBLL,NUMBSS,NUMBR,NUMBL,NUMBS,NUMBSL,NUMBZ,NUMBE
      INTEGER AND,OR,SHIFT
#else
      INTEGER*4 NUMBLL,NUMBSS,NUMBR,NUMBL,NUMBS,NUMBSL,NUMBZ,NUMBE
#endif
      INTEGER NI(3),NK(3),NL(3),NN(3)
      DIMENSION IDEM(8),NTYPE(14)
      LOGICAL DOGAM,DOIK
      LOGICAL IEQK,IEQL,IEQN,KEQL,KEQN,LEQN,YDEM(8),SPECIAL
      INTEGER UNITI,UNITL
C
#if defined (CRAY)
      ISHIFT (I, J) = SHIFT (I, J)
#else
      ISHIFT (I, J) = ISHFT (I, J)
#endif
      MAXTOP = 0
      NUMBL = NEM(NEQ1) + INDCOM(KCOMB(NEQ1)+1)
     &        - INDCOM(KCOMB(NEQ1))
      NUMBS = NEM(NEQUIV) + INDCOM(KCOMB(NEQUIV)+1)
     &        - INDCOM(KCOMB(NEQUIV)) - NUMBL
      NUMBR = NUMBS*NUMBL
      NUMBL = NUMBL*(NUMBL+1)/2
      NUMBS = NUMBS*(NUMBS+1)/2
      NUMBLL= NUMBL*(NUMBL+1)/2
      NUMBSL= NUMBS*NUMBL
      NUMBSS= NUMBS*(NUMBS+1)/2
      NUMBR = NUMBR*(NUMBR+1)/2
      PRINT 850,NUMBLL,NUMBSL,NUMBSS,NUMBR
  850 FORMAT(//' CALCULATING TWO ELECTRON INTEGRALS ',//,11X,
     $       '(LL/LL)',7X,'(SS/LL)',8X,'(SS/SS)',7X,'(SL/SL)',
     $       /,1X,'TOTAL',1X,I11,2X,I12,2X,I13,2X,I12/)
  860 FORMAT(/,1X,A7,1X,'INTEGRALS COMPLETED',4X,
     $      'NON-ZERO(SYMM.)  ABOVE THRESHOLD    CPU TIME'/
     $      9X,'CALCULATED THIS RUN   ',I11,6X,I11,6X,F10.4)
      secsp = 0.d0
      sec4i = 0.d0
      secdi = 0.d0
      CALL CPUUSED(SEC1)
      DO 1330 ID = ICR, ICMX
      DO 1 ITT = 1, 14
    1 NTYPE (ITT) = 0
      UNITI = UN (ID)
      UNITL = UN (ID+4)
C     --------------------------------------
C     FILES ARE ALREADY OPEN WHEN RESTARTING
C     --------------------------------------
      IF (IR+KR+LR+NR.EQ.4) THEN
         OPEN(UNITI,FILE=FN(ID),FORM='UNFORMATTED')
         OPEN(UNITL,FILE=FN(ID+4),FORM='UNFORMATTED')
      ENDIF
      IF (ID.EQ.1) THEN
          NEQX=NEQ1
          NEQY=NEQ1
          IKM=1
          LNM=1
      ELSEIF(ID.EQ.2) THEN
          NEQX=NEQUIV
          NEQY=NEQ1
          IKM=NEQ1+1
          LNM=1
          IF (ICR.NE.2) THEN
             CALL CPUUSED(SEC2)
             EXECS=SEC2-SEC1
             PRINT 860,'(LL/LL)',NUMBZ,NUMBE,EXECS
             SEC1=SEC2
          ENDIF
      ELSEIF(ID.EQ.3) THEN
          NEQX=NEQUIV
          NEQY=NEQUIV
          IKM=NEQ1+1
          LNM=NEQ1+1
          IF (ICR.NE.3) THEN
             CALL CPUUSED(SEC2)
             EXECS=SEC2-SEC1
             PRINT 860,'(SS/LL)',NUMBZ,NUMBE,EXECS
             SEC1=SEC2
          ENDIF
      ELSEIF(ID.EQ.4) THEN
          NEQX=NEQUIV
          NEQY=NEQ1
          IKM=NEQ1+1
          LNM=1
          IF (ICR.NE.4) THEN
             CALL CPUUSED(SEC2)
             EXECS=SEC2-SEC1
             PRINT 860,'(SS/SS)',NUMBZ,NUMBE,EXECS
             SEC1=SEC2
          ENDIF
      ENDIF
      NUMBZ = 0
      NUMBE = 0
      NSCF = 0
      ICNTOLD = 0
      KCNTOLD = 0
      LCNTOLD = 0
      NCNTOLD = 0
      ISOLD = 0
      KSOLD = 0
      LSOLD = 0
      NSOLD = 0
C     --------------------------------
C     DETERMINE STARTPOINTS OF LOOPS :
C     --------------------------------
      ICTI=KCOMB(IKM)
      KCTI=INDCOM(ICTI)
      MCTIK=KCTI+1
      ICTL=KCOMB(LNM)
      KCTL=INDCOM(ICTL)
      MCTLN=KCTL+1
      MCTI=MAX0(MCTIK,IR)
      MCTK=MAX0(MCTIK,KR)
      MCTL=MAX0(MCTLN,LR)
      MCTN=MAX0(MCTLN,NR)
      IF (ID.EQ.4) THEN
          MCTK = MAX0(MCTLN,KR)
          MCTL = MAX0(MCTIK,LR)
      ENDIF
      IR=1
      KR=1
      LR=1
      NR=1
C     -------------------------------
C     START LOOPS OVER FUNCTION TYPES
C     -------------------------------
      ICTI=KCOMB(NEQX)
      NCTI=INDCOM(ICTI+1)
      ICTL=KCOMB(NEQY)
      DO 1030 I=MCTI,NCTI
      IM=KCBINV(I,1)
      ICONTR=KCBINV(I,2)-IM+1
      IS=INDBAS(IM)+1
      IF=INDBAS(IM+1)
      IPRIM = IF - IS + 1
      ICPRI = NCORE(IM,1)
      ICCON = NCORE(IM,2)
      ICNT=ICENT(I)
      ITYP = ITYPE(I)
      NI(1) = NX(ITYP)
      NI(2) = NY(ITYP)
      NI(3) = NZ(ITYP)
      NCTK=I
      IF (ID.EQ.4) NCTK = INDCOM(ICTL+1 )
      DO 1020 K=MCTK,NCTK
      KM=KCBINV(K,1)
      KCONTR=KCBINV(K,2)-KM+1
      KS=INDBAS(KM)+1
      KF=INDBAS(KM+1)
      KPRIM = KF - KS + 1
      KCPRI = NCORE(KM,1)
      KCCON = NCORE(KM,2)
      KCNT=ICENT(K)
      KTYP = ITYPE(K)
      NK(1) = NX(KTYP)
      NK(2) = NY(KTYP)
      NK(3) = NZ(KTYP)
      IF ((ID.EQ.2.OR.ID.EQ.3).AND.
     &    (KCNT.NE.ICNT.AND.ONECEN)) GOTO 1020
      IEQK=I.EQ.K
      NCTL=MIN0( INDCOM(ICTL+1) ,I )
      IF (ID.EQ.4) NCTL = I
      DOIK = .TRUE.
      DO 1010 L=MCTL,NCTL
      LM=KCBINV(L,1)
      LCONTR=KCBINV(L,2)-LM+1
      LS=INDBAS(LM)+1
      LF=INDBAS(LM+1)
      LPRIM = LF - LS + 1
      LCPRI = NCORE(LM,1)
      LCCON = NCORE(LM,2)
      LCNT=ICENT(L)
      LTYP = ITYPE(L)
      NL(1) = NX(LTYP)
      NL(2) = NY(LTYP)
      NL(3) = NZ(LTYP)
      IF ((ID.EQ.3.OR.ID.EQ.4).AND.
     &    (LCNT.NE.ICNT.AND.ONECEN)) GOTO 1010
      IEQL=I.EQ.L
      KEQL=K.EQ.L
      NCTN=L
      IF (ID.EQ.4) NCTN = NCTK
      IF (IEQL) NCTN=K
      DO 1000 N=MCTN,NCTN
      NM=KCBINV(N,1)
      NCONTR=KCBINV(N,2)-NM+1
      NS=INDBAS(NM)+1
      NF=INDBAS(NM+1)
      NPRIM = NF - NS + 1
      NCPRI = NCORE(NM,1)
      NCCON = NCORE(NM,2)
      NCNT=ICENT(N)
      NTYP = ITYPE(N)
      NN(1) = NX(NTYP)
      NN(2) = NY(NTYP)
      NN(3) = NZ(NTYP)
C     ----------------------------------------------------------------
C     Calculate sum of l values : in one center case we need only half
C     of the incomplete gamma function values
C     ----------------------------------------------------------------
      MS = 0
      DO KT = 1, 3
      MSKT = NI(KT) + NK(KT) + NL(KT) + NN(KT)
      IF (ICNT.EQ.KCNT.AND.LCNT.EQ.NCNT.AND.ICNT.EQ.LCNT) MSKT = MSKT/2
      MS = MS + MSKT
      ENDDO
      IF ((ID.EQ.3).AND.
     &    (NCNT.NE.ICNT.AND.ONECEN)) GOTO 1000
      IEQN=I.EQ.N
      KEQN=K.EQ.N
      LEQN=L.EQ.N
C     ----------------------------------------------------
C     DETERMINE SYMMETRY EQUIVALENCES AND CHECK FOR ZERO'S
C     ----------------------------------------------------
      DO 20 IX=1,8
   20 IDEM(IX)=0
      SPECIAL=.FALSE.
      IMM=I*(I-1)/2+K
      LMM=L*(L-1)/2+N
      INDEX=(IMM)*(IMM-1)/2+LMM
      DO 980 M=1,NSYMOP
      IT=ICOMOP(I,M)
      KT=ICOMOP(K,M)
      LT=ICOMOP(L,M)
      NT=ICOMOP(N,M)
      SN=1.0
      IF (IT.GT.0) GOTO 880
      IT=-IT
      SN=-SN
  880 IF (KT.GT.0) GOTO 890
      KT=-KT
      SN=-SN
  890 IF (LT.GT.0) GOTO 900
      LT=-LT
      SN=-SN
  900 IF (NT.GT.0) GOTO 910
      NT=-NT
      SN=-SN
  910 IF (I-IT) 1030,920,950
  920 IF (K-KT) 1020,930,950
  930 IF (L-LT) 1010,940,950
  940 IF (N-NT) 1000,955,955
  950 I1=MAX0(IT,KT)
      I2=MIN0(IT,KT)
      I1=I1*(I1-1)/2+I2
      L1=MAX0(LT,NT)
      L2=MIN0(LT,NT)
      L1=L1*(L1-1)/2+L2
      II1=MAX0(I1,L1)
      LL1=MIN0(I1,L1)
      INN=II1*(II1-1)/2+LL1
      GOTO 958
  955 INN=INDEX+NT-N
  958 CONTINUE
      IF (INN.GT.INDEX) GOTO 1000
  970 IF (INN.EQ.INDEX) THEN
      IF (IT.EQ.I.AND.KT.EQ.K.AND.LT.EQ.L.AND.NT.EQ.N) THEN
         IF (SN.LT.0) GOTO 1000
         IDEM(1)=IDEM(1)+1
      ENDIF
      IF (IT.EQ.I.AND.KT.EQ.K.AND.LT.EQ.N.AND.NT.EQ.L) IDEM(2)=IDEM(2)+1
      IF (IT.EQ.K.AND.KT.EQ.I.AND.LT.EQ.L.AND.NT.EQ.N) IDEM(3)=IDEM(3)+1
      IF (IT.EQ.K.AND.KT.EQ.I.AND.LT.EQ.N.AND.NT.EQ.L) IDEM(4)=IDEM(4)+1
      IF (IT.EQ.L.AND.KT.EQ.N.AND.LT.EQ.I.AND.NT.EQ.K) IDEM(5)=IDEM(5)+1
      IF (IT.EQ.L.AND.KT.EQ.N.AND.LT.EQ.K.AND.NT.EQ.I) IDEM(6)=IDEM(6)+1
      IF (IT.EQ.N.AND.KT.EQ.L.AND.LT.EQ.I.AND.NT.EQ.K) IDEM(7)=IDEM(7)+1
      IF (IT.EQ.N.AND.KT.EQ.L.AND.LT.EQ.K.AND.NT.EQ.I) IDEM(8)=IDEM(8)+1
      ENDIF
  980 CONTINUE
C     -----------------------------------------------
C     TAKE CARE OF CASES WHERE IDEM WAS COUNTED TWICE
C     -----------------------------------------------
      DO  981 IX=1,8
  981 YDEM(IX)=IDEM(IX).GT.0
      IF (I EQ K) THEN
         IF (L EQ N) THEN
             IDEM(2)=0
             IDEM(3)=0
             IDEM(4)=0
             IDEM(6)=0
             IDEM(7)=0
             IDEM(8)=0
             IF (I EQ L) THEN
                 IDEM(5)=0
             ENDIF
         ELSE
             IDEM(3)=0
             IDEM(4)=0
             IDEM(6)=0
             IDEM(7)=0
         ENDIF
      ELSE
         IF (L EQ N) THEN
             IDEM(2)=0
             IDEM(4)=0
             IDEM(6)=0
             IDEM(7)=0
         ELSEIF (IEQL.AND.KEQN) THEN
             IDEM(5)=0
             IDEM(6)=0
             IDEM(7)=0
             SPECIAL=.TRUE.
         ELSEIF (IEQN.AND.KEQL) THEN
             SPECIAL=.TRUE.
             IDEM(6)=0
             IDEM(7)=0
             IDEM(8)=0
         ENDIF
      ENDIF
C     Setup primitive calculation:
C     Number of primitives included is determined by ICORE and NCORE
C     ICORE = 0 : All integrals calculated
C     ICORE = 1 : Discard (SS|SS) integrals with one or more diffuse exponents
C     ICORE = 2 : Discard (SL|SL) and (SS|LL) ints with one or more diffuse 
      IF (ID.EQ.1.OR.((ID.EQ.2.OR.ID.EQ.4).AND.ICORE.LE.1).OR.
     &    ID.EQ.3.AND.ICORE.EQ.0) THEN
          ICPRIM = IPRIM
          KCPRIM = KPRIM
          LCPRIM = LPRIM
          NCPRIM = NPRIM
          ICCONTR = ICONTR
          KCCONTR = KCONTR
          LCCONTR = LCONTR
          NCCONTR = NCONTR
      ELSE
          ICPRIM = ICPRI
          KCPRIM = KCPRI
          LCPRIM = LCPRI
          NCPRIM = NCPRI
          ICCONTR = ICCON
          KCCONTR = KCCON
          LCCONTR = LCCON
          NCCONTR = NCCON
      ENDIF
      IKPRIM = ICPRIM*KCPRIM
      LNPRIM = LCPRIM*NCPRIM
      IKLNPR = IKPRIM*LNPRIM
      IKLNCON = ICCONTR*KCCONTR*LCCONTR*NCCONTR
      call cpuused(sec1a)
      IF (.NOT.COUNT.AND.IKLNPR.GT.0.AND.IKLNCON.GT.0) THEN
C     -------------------------------------------------
C     COMPUTE THE WHOLE BLOCK OF THIS TYPE OF INTEGRALS
C     SKIP COMPUTATION OF FGAMMA IF POSSIBLE
C     -------------------------------------------------
C
      IF ((ICNTOLD.EQ.ICNT).AND.(KCNTOLD.EQ.KCNT).AND.
     &    (LCNTOLD.EQ.LCNT).AND.(NCNTOLD.EQ.NCNT).AND.
     &    (ISOLD.EQ.IS).AND.(KSOLD.EQ.KS).AND.
     &    (LSOLD.EQ.LS).AND.(NSOLD.EQ.NS).AND.
     &    (MSOLD.GE.MS)) THEN
         DOGAM = .FALSE.
         INDONE = INDONE + 1
      ELSE
         DOGAM = .TRUE.
         ICNTOLD = ICNT
         KCNTOLD = KCNT
         LCNTOLD = LCNT
         NCNTOLD = NCNT
         ISOLD = IS
         KSOLD = KS
         LSOLD = LS
         NSOLD = NS
         MSOLD = MS
         IDONE = IDONE + 1
      ENDIF
C
C     Allocate memory
C
      ITOP = 1
C
C     The 2-index ij quantities (I didn't invent this stupid ikln convention)
      IPOINDA = ITOP                        ! DISTAB
      IPOINGA = IPOINDA + 1    * IKPRIM     ! GAB
      IPOINHL = IPOINGA + 1    * IKPRIM     ! HL
      IPOINT1 = IPOINHL + 13   * IKPRIM     ! T1
      IPOINP  = IPOINT1 + 1    * IKPRIM     ! P
      ITOP    = IPOINP  + 3    * IKPRIM    
C     The 4-index quantities
      IPOINR = ITOP
      IPOINF = IPOINR + 3         * IKLNPR
      ITOP   = IPOINF + (MSOLD+1) * IKLNPR
C     Up to here needs to be saved between calls
C     The 2-index kl quantities
      NW1 = MAX0(IKPRIM,LNPRIM)
      IPOINDC = ITOP                         ! DISTCD
      IPOINGC = IPOINDC + 1      * LNPRIM    ! GCD
      IPOINHM = IPOINGC + 1      * LNPRIM    ! HM
      IPOINT2 = IPOINHM + 13     * LNPRIM    ! T2
      IPOINQ  = IPOINT2 + 1      * LNPRIM    ! Q
      IPOINWX = IPOINQ  + 3      * LNPRIM    ! WX
      IPOINC  = IPOINWX + 1      * LNPRIM    ! C         
      IPOINW1 = IPOINC  + (MS+3) * LNPRIM    ! WORK ARRAY
      ITOP    = IPOINW1 + 21     * NW1
C     The 4-index scratch arrays
      IPOINZ  = ITOP                          ! Z
      IPOINW2 = IPOINZ   + 1      * IKLNPR    ! WORK ARRAY IN FGAMMA
      ITOP    = IPOINW2  + 8      * IKLNPR
C     We can store the final integrals in the scratch arrays
      IPOINRI = IPOINW2           ! RAWINT
      IPOINRU = IPOINRI  + IKLNPR ! RUWINT
C     Check memory
      IF (ITOP.GT.NWORK) THEN
         PRINT*, "ITOP, NWORK in MFDTWOE",ITOP,NWORK
         STOP "NOT ENOUGH MEMORY"
      ENDIF
      MAXTOP = MAX0(MAXTOP,ITOP)
C
      CALL SPDFG (ICNT,KCNT,LCNT,NCNT,ICPRIM,KCPRIM,LCPRIM,NCPRIM,
     &            MS,NI,NK,NL,NN,DOIK,DOGAM,
     &            ALPHA(IS),ALPHA(KS),ALPHA(LS),ALPHA(NS),
     &            IKPRIM,LNPRIM,IKLNPR,
     &            WORK(IPOINW1),NW1,21,WORK(IPOINW2),8,
     &            WORK(IPOINDA),WORK(IPOINDC),
     &            WORK(IPOINGA),WORK(IPOINGC),
     &            WORK(IPOINHL),WORK(IPOINHM),
     &            WORK(IPOINT1),WORK(IPOINT2),
     &            WORK(IPOINP),WORK(IPOINQ),
     &            WORK(IPOINR),WORK(IPOINZ),
     &            WORK(IPOINWX),WORK(IPOINF),
     &            WORK(IPOINC),WORK(IPOINRI))
C
C     ----------------------------------------------
C     4-INDEX TRANSFORMATION TO CONTRACTED INTEGRALS
C     ----------------------------------------------
      call cpuused (sec2a)
      secsp = secsp + sec2a - sec1a
      sec1a = sec2a
      CALL FOURIND (ICPRIM,KCPRIM,LCPRIM,NCPRIM,
     &              ICCONTR,KCCONTR,LCCONTR,NCCONTR,
     &              COEFF(IS),COEFF(KS),COEFF(LS),COEFF(NS),
     &              IPRIM,KPRIM,LPRIM,NPRIM,
     &              WORK(IPOINRU),WORK(IPOINRI))
      call cpuused (sec2a)
      sec4i = sec4i + sec2a - sec1a
      sec1a = sec2a
      ENDIF
C     --------------------------------------
C     DISTRIBUTE THE NON-REDUNDANT INTEGRALS
C     --------------------------------------
      DO 2030 IM=KCBINV(I,1),KCBINV(I,1)+ICCONTR-1
      IMM = NEM(IM) + I - INDCOM(KCOMB(IM))
      IMIND=IM-KCBINV(I,1)
      IMIND=IMIND*KCCONTR*LCCONTR*NCCONTR+IPOINRI
      KMMAX=KCBINV(K,1)+KCCONTR-1
      IDEMSI=IDEM(1)
      IF (YDEM(3).OR.YDEM(4)) KMMAX=IM
      DO 2020 KM=KCBINV(K,1),KMMAX
      KMM = NEM(KM) + K - INDCOM(KCOMB(KM))
      IMMM=MAX0(IMM,KMM)
      KMMM=MIN0(IMM,KMM)
      KMIND=KM-KCBINV(K,1)
      KMIND=KMIND*LCCONTR*NCCONTR+IMIND
      IDEMSK=IDEMSI
      IF (IM.EQ.KM) IDEMSK=IDEMSK+IDEM(3)
      LMMAX=KCBINV(L,1)+LCCONTR-1
      IF (YDEM(5).OR.YDEM(6)) LMMAX=IM
      DO 2010 LM=KCBINV(L,1),LMMAX
      LMM = NEM(LM) + L - INDCOM(KCOMB(LM))
      LMIND=LM-KCBINV(L,1)
      LMIND=LMIND*NCCONTR+KMIND
      NMMAX=KCBINV(N,1)+NCCONTR-1
      IF (YDEM(2).OR.(YDEM(4).AND.IM.EQ.KM)) NMMAX=LM
      IF ((YDEM(5).AND.IM.EQ.LM).OR.(YDEM(6).AND.IM.EQ.LM.AND.
     &    IM.GE.KM)) NMMAX=MIN0(NMMAX,KM)
      IF (YDEM(6).AND.IM.EQ.LM.AND.IM.LT.KM) NMMAX=MIN0(NMMAX,KM-1)
      IF ((YDEM(7).AND.((KM.GT.LM).OR.(KM.EQ.LM.AND.IM.LE.KM))).OR.
     &    (YDEM(8).AND.KM.GE.LM)) NMMAX=MIN0(NMMAX,IM)
      IF ((KM.LT.LM.AND.(YDEM(7).OR.YDEM(8))).OR.(YDEM(7)
     &   .AND.(KM.EQ.LM.AND.IM.GT.KM))) NMMAX=MIN0(NMMAX,IM-1)
      DO 2000 NM=KCBINV(N,1),NMMAX
      NMM = NEM(NM) + N - INDCOM(KCOMB(NM))
      LMMM=MAX0(LMM,NMM)
      NMMM=MIN0(LMM,NMM)
      IF (IMMM.GT.LMMM.OR.(IMMM.EQ.LMMM.AND.KMMM.GE.NMMM)) THEN
         IMMMM = IMMM
         KMMMM = KMMM
         LMMMM = LMMM
         NMMMM = NMMM
      ELSE
         IMMMM = LMMM
         KMMMM = NMMM
         LMMMM = IMMM
         NMMMM = KMMM
      ENDIF
      NMIND=NM-KCBINV(N,1)+LMIND
      IDEMSN=IDEMSK
      IF (.NOT.(IM.EQ.KM.AND.LM.EQ.NM.AND.IM.EQ.LM)) THEN
         IF (LM.EQ.NM) IDEMSN=IDEMSN+IDEM(2)
         IF (IM.EQ.KM.AND.LM.EQ.NM) IDEMSN=IDEMSN+IDEM(4)
         IF (IM.EQ.LM.AND.KM.EQ.NM) IDEMSN=IDEMSN+IDEM(5)
         IF (IM.EQ.NM.AND.KM.EQ.LM) IDEMSN=IDEMSN+IDEM(8)
      ELSE
         IDEMSN=IDEMSN+IDEM(2)+IDEM(4)+IDEM(5)+IDEM(6)+IDEM(7)+IDEM(8)
         IF (SPECIAL) IDEMSN=IDEMSN-IDEM(4)
      ENDIF
      IF (.NOT.COUNT) THEN
         VALINT=WORK(NMIND)
      ELSE
         VALINT=0.D0
      ENDIF
      NUMBZ=NUMBZ+1
#if defined (BIT64)
          IF (ABS(VALINT).GT.ENORM(ID)) THEN
#else
          IF (DABS(VALINT).GT.ENORM(ID)) THEN
#endif
             NUMBE =NUMBE + 1
             NSCF = NSCF + 1
             ITT = INTTYP(IMMMM,KMMMM,LMMMM,NMMMM)
             NTT = NTYPE (ITT) + 1
             NTYPE (ITT) = NTT
#if defined (BIT64) 
             GINTEG(NTT,ITT)=VALINT/REAL(IDEMSN)
             LABEL(NTT,ITT) = ISHIFT (IMMMM,48)
             LABEL(NTT,ITT) = OR (ISHIFT(KMMMM,32),LABEL(NTT,ITT))
             LABEL(NTT,ITT) = OR (ISHIFT(LMMMM,16),LABEL(NTT,ITT))
             LABEL(NTT,ITT) = OR (NMMMM,LABEL(NTT,ITT))
#else
             GINTEG(NTT,ITT)=VALINT/DBLE(IDEMSN)
             LABEL (NTT,ITT) = ISHIFT (IMMMM,16)
             LABEL (NTT,ITT) = IOR (KMMMM,LABEL(NTT,ITT))
             LABEL1(NTT,ITT) = ISHIFT (LMMMM,16)
             LABEL1(NTT,ITT) = IOR (NMMMM,LABEL1(NTT,ITT))
#endif
             IF (NTT.EQ.N2) THEN
               CALL WRBUF (UNITI,UNITL,ITT,NTYPE)
             ENDIF
          ENDIF
 2000 CONTINUE
 2010 CONTINUE
 2020 CONTINUE
 2030 CONTINUE
      call cpuused(sec2a)
      secdi = secdi + sec2a - sec1a
      sec1a = sec2a
      IF (NSCF.GT.N2*100) THEN
         CALL WRBUF (UNITI,UNITL,0,NTYPE)
         NSCF = 0
      ENDIF
 1000 CONTINUE
      MCTN=MAX0(MCTLN,NR)
 1010 CONTINUE
      MCTL=MAX0(MCTLN,LR)
      IF (ID.EQ.4) MCTL = MAX0(MCTIK,LR)
 1020 CONTINUE
      MCTK=MAX0(MCTIK,KR)
      IF (ID.EQ.4) MCTK = MAX0(MCTLN,KR)
 1030 CONTINUE
      CALL WRBUF (UNITI,UNITL,15,NTYPE)
      CLOSE(UNITI,STATUS='KEEP')
      CLOSE(UNITL,STATUS='KEEP')
 1330 CONTINUE
      CALL CPUUSED(SEC2)
      EXECS=SEC2-SEC1
      IF (ICMX.EQ.1) THEN
          PRINT 860,'(LL/LL)',NUMBZ,NUMBE,EXECS
      ELSEIF (ICMX.EQ.2) THEN
          PRINT 860,'(SS/LL)',NUMBZ,NUMBE,EXECS
      ELSEIF (ICMX.EQ.3) THEN
          PRINT 860,'(SS/SS)',NUMBZ,NUMBE,EXECS
      ELSEIF (ICMX.EQ.4) THEN
          PRINT 860,'(SL/SL)',NUMBZ,NUMBE,EXECS
      ENDIF
      PRINT 1340
 1340 FORMAT(//' TWO ELECTRON INTEGRALS COMPLETED')
      print*
      print*
      print1350, "Time in spdfg",secsp," seconds"
      print1350, "Time in 4-ind",sec4i," seconds"
      print1350, "Time in distr",secdi," seconds"
 1350 format (a,f10.3,a)
      print*
      print*, "Maximum memory used :",maxtop," 8-Byte words"
      RETURN
      END
