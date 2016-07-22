C
C     ==================
      SUBROUTINE MFD1OUT
C     ==================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'paracom.inc'
#if defined (PARALLEL)
      INCLUDE 'mpidef.inc'
#else
      PARAMETER (mpirnk = 0)
#endif
C
      COMMON/GROUPR/PROJOP(96,16),UU(2,2,48)
      COMPLEX*16 UU
C
      DIMENSION IREP(10)
      DIMENSION SYMORA(N8*2),SYMORB(N8*2)
      DIMENSION COMP(2)
      CHARACTER*10 STREEP*27,CN*2,DATUM,TIJD*8
      CHARACTER*80 BASFIL, TEXT, RPN*4
      CHARACTER*15 TIME1*8,DATE1*10,FORMATC,FORMATE,COMP*5
      CHARACTER*15 FILE(0:3)
      CHARACTER*1 CHX,PLUSMIN(3)
      CHARACTER STRING*17,MFDAREP*12
#if defined (IBM)
      CHARACTER*32 MACHINE
#endif
      LOGICAL STR1,STR2
      EQUIVALENCE (CSYMORA,SYMORA),(CSYMORB,SYMORB)
      DATA STREEP/' --------------------------'/
      DATA PLUSMIN /'-', ' ', '+'/
      DATA FILE/'NONE','OUTPCHK','INPTCHK','INPTCHK,OUTPCHK'/
      DATA FORMATC /'(4F20.15)'/
      DATA COMP /'LARGE','SMALL'/
      CHX(I)=PLUSMIN(I+2)
      IUX(M)=M-(M/25)*24
#if defined (BIT64)
      DIST(DA,DX,DY,DZ)=DA*SQRT(DX*DX+DY*DY+DZ*DZ)
#else
      DIST(DA,DX,DY,DZ)=DA*DSQRT(DX*DX+DY*DY+DZ*DZ)
#endif
C
C     In case of AREP is .true.
C
      IF (AREP) THEN
         IF (mpirnk .EQ. 0) THEN
            WRITE (MFDAREP,'(A7)') 'MFDAREP'
         ELSEIF (mpirnk .LT. 10) THEN
            WRITE (MFDAREP,'(A7,A1,I1)') 'MFDAREP','.',mpirnk
         ELSEIF (mpirnk .LT. 100) THEN
            WRITE (MFDAREP,'(A7,A1,I2)') 'MFDAREP','.',mpirnk
         ELSE
            WRITE (MFDAREP,'(A7,A1,I3)') 'MFDAREP','.',mpirnk
         ENDIF
         OPEN(11,FILE=MFDAREP,FORM='UNFORMATTED')
         WRITE(11) FRLC(1)
      ENDIF
C
C     -- WRITE OUTPUT DATA FOR PRINTOUT --
C
      NEQUIV = NEQ(2)
      NGAUS  = NGA(2)
      NCOMB  = NCO(2)
      NQV1   = NEC(1)
      NQV2   = NEC(2)
      CALL DAYTIME(DATUM,TIJD)
#if defined (IBM)
      call hostnm_(MACHINE)
#endif
      if (mpirnk.eq.0) then
      WRITE (*,2000) ' ',NAME
      WRITE (*,2020)
      WRITE (*,1900) ' TIME       : ', TIJD   
      WRITE (*,1900) ' DATE       : ', DATUM
#if defined (IBM)
      WRITE (*,2000) ' HOST       :  ',MACHINE
#endif
      WRITE (*,2002) ' PRINT OPTN :  ',PRNT
      WRITE (*,2023) ' AREP       :  ',AREP
      WRITE (*,2001) ' POINTGROUP :  ',(GROUPN(I),I=1,IGRID(1))
      IF (FRLC (1))
     +  WRITE (*,2000)
     +  ' NON-Y(L,M) COMBINATIONS IN LARGE COMPONENT WILL BE REMOVED'
      IF (FRLC (2)) THEN
         IF (FRLC(3)) THEN
             WRITE (*, 2000)
     + ' ALL NON-Y(L,M) COMBINATIONS IN SMALL COMPONENT WILL BE REMOVED'
         ELSE
             WRITE (*, 2000)
     +  ' ONLY THE S COMBINATION FROM THE SMALL COMPONENT CARTESIAN G-',
     +  'FUNCTIONS WILL BE REMOVED'
         ENDIF
      ENDIF
C
C     -------------------------------
C     PRINT GROUPMULTIPLICATION TABLE
C     -------------------------------
C
      IF (PRNT.GT.1) THEN
          WRITE(*,2003) ' '
          NOP=MIN0(16,NSYMOP)
          DO 2 IOP=1,NSYMOP
          WRITE (*, 2004) IOP, (CHX (OPSIGN (IUX (OPERAT1 (IOP)),
     $                          IUX (OPERAT1 (KOP)))),
     $                          OPNAME (OPTABEL (OPERAT1 (IOP),
     $                          OPERAT1 (KOP))), KOP = 1, NOP)
    2     CONTINUE
          IF (NOP.EQ.NSYMOP) GOTO 5
          NOP=MIN0(32,NSYMOP)
          WRITE(*,2003) ' (CONTINUED) '
          DO 3 IOP=1,NSYMOP
          WRITE (*, 2004) IOP, (CHX (OPSIGN (IUX (OPERAT1 (IOP)),
     $                          IUX (OPERAT1 (KOP)))),
     $                          OPNAME (OPTABEL (OPERAT1 (IOP),
     $                          OPERAT1 (KOP))), KOP = 17, NOP)
    3     CONTINUE
          IF (NOP.EQ.NSYMOP) GOTO 5
          NOP=NSYMOP
          WRITE(*,2003) ' (CONTINUED) '
          DO 4 IOP=1,NSYMOP
          WRITE (*, 2004) IOP, (CHX (OPSIGN (IUX (OPERAT1 (IOP)),
     $                          IUX (OPERAT1 (KOP)))),
     $                          OPNAME (OPTABEL (OPERAT1 (IOP),
     $                          OPERAT1 (KOP))), KOP = 33, NOP)
    4     CONTINUE
    5     CONTINUE
      ENDIF
      IF (PRNT.GT.1) THEN
          WRITE(*,2020)
          WRITE(*,2000) ' UNITARY ROTATION MATRICES '
          WRITE(*,2020)
          DO 7 K=1,NSYMOP,4
          WRITE(*,2013) (OPERN(I),I=K,MIN0(K+3,NSYMOP))
          DO 6 I=1,2
          WRITE(*,2014) ((UU(I,J,IOP),J=1,2),IOP=K,MIN0(K+3,NSYMOP))
    6     CONTINUE
    7     CONTINUE
      ENDIF
      NOP=MIN0(24,NSYMOP)
      IF (PRNT.LT.-1) GOTO 36
      WRITE (6,2005) ' ',  (OPERN(I),I=1,NOP)
      DO 10 I=1,NNUCL
   10 WRITE (6,2006) I,CENTRN(I),(NUCOP(I,IOP),IOP=1,NOP)
      IF (NOP.LT.NSYMOP) THEN
          WRITE (6,2005) ' ( CONTINUED ) ' , (OPERN(I),I=NOP+1,NSYMOP)
          DO 15 I=1,NNUCL
          WRITE (6,2006) I,CENTRN(I),(NUCOP(I,IOP),IOP=NOP+1,NSYMOP)
   15     CONTINUE
      ENDIF
      IF (PRNT.LE.-1) GOTO 36
      WRITE(6,2008) ' ', (OPERN(I),I=1,NOP)
      DO 20 I=MINTYP,MAXTYP
      WRITE (6,2009) I,FNAME(I),(XMAT(I,IOP),IOP=1,NOP)
   20 CONTINUE
      IF (NOP.LT.NSYMOP) THEN
          WRITE (6,2008) ' ( CONTINUED ) ', (OPERN(I),I=NOP+1,NSYMOP)
          DO 25 I=MINTYP,MAXTYP
          WRITE (6,2009) I,FNAME(I),(XMAT(I,IOP),IOP=NOP+1,NSYMOP)
   25     CONTINUE
      ENDIF
      IF (PRNT.LE.0) GOTO 36
      WRITE (6,2011) ' ', (OPERN(I),I=1,NOP)
      DO 30 I=1,NCOMB
      WRITE (6,2012) I,(ICOMOP(I,IOP),IOP=1,NOP)
   30 CONTINUE
      IF (NOP.LT.NSYMOP) THEN
          WRITE (6,2011) ' ( CONTINUED ) ',(OPERN(I),I=NOP+1,NSYMOP)
          DO 35 I=1,NCOMB
          WRITE (6,2012) I,(ICOMOP(I,IOP),IOP=NOP+1,NSYMOP)
   35     CONTINUE
      ENDIF
   36 CONTINUE
      WRITE (*,2010) ' (IN ATOMIC UNITS (1 AU = ',BOHRRAD,' ANG) )'
     $,(I,CENTRN(I),XCORD(I),YCORD(I),ZCORD(I),CHARGE(I),RNUC(I)
     $,I=1,NNUCL)
      IF (AUANG.EQ.'ANG')
     $WRITE (*,2010) ' (IN ANGSTROMS (1 ANG = ',1.0/BOHRRAD,' AU) )'
     $,(I,CENTRN(I),XCORD(I)*BOHRRAD,YCORD(I)*BOHRRAD,ZCORD(I)*BOHRRAD
     $,CHARGE(I),RNUC(I),I=1,NNUCL)
      IF (NNUCL.LT.3) GOTO 38
      IF (AUANG.EQ.'ANG') THEN
          AA=BOHRRAD
          STRING='(IN ANGSTROMS)'
      ELSE
          AA=1.0
          STRING='(IN ATOMIC UNITS)'
      ENDIF
      WRITE(*,2016) STRING,(I,CENTRN(I),I=1,NNUCL)
      DO 37 J=1,NNUCL
      WRITE (*,2017) J,CENTRN(J),(DIST(AA,XCORD(I)-XCORD(J),
     $               YCORD(I)-YCORD(J),ZCORD(I)-ZCORD(J)),I=1,NNUCL)
   37 CONTINUE
   38 CONTINUE
      PRINT*
      IF (IDEFNUC.EQ.1) THEN
         PRINT*,'NUCLEAR EXPONENT(S) CONSTRUCTED WITH MODEL ',
     $         'OF VISSER ET AL.'
      ELSE
         PRINT*,'NUCLEAR EXPONENT(S) CONSTRUCTED WITH MODEL ',
     $         'OF GRASP'
      ENDIF
      WRITE (*,2018) POTNUC
      WRITE (*,2020)
      endif
      IF (PRNT.LE.-2) GOTO 50
      if (mpirnk.eq.0) then
      WRITE (*,2000) ' GROUPS OF SYMMETRY RELATED FUNCTIONS'
      WRITE (*,2020)
      WRITE (*,2030) ' LARGE COMPONENT',' SMALL COMPONENT'
      WRITE (*,2015)
      endif
C     NEXT SECTION PUTS THE TWO SETS OF SYMMETRY RELATED FUNCTIONS
C     FOR LARGE AND SMALL COMPONENT NEXT TO EACH OTHER
      IM=0
      KM=NQV1
      ICT=INDCOM(IM+1)+1
      KCT=INDCOM(KM+1)+1
      KND=0
      MND=KND
   40 IF (ICT.GT.KND) THEN
          IM=IM+1
          STR1=.TRUE.
          IF (IM.GT.NQV1) ICT=0
          IF (ICT.NE.0) KND=INDCOM(IM+1)
      ENDIF
      IF (KCT.GT.MND) THEN
          KM=KM+1
          STR2=.TRUE.
          IF (KM.GT.NQV2) KCT=0
          IF (KCT.NE.0) MND=INDCOM(KM+1)
      ENDIF
      IF (ICT.EQ.0) THEN
          IF(KCT.EQ.0) THEN
             if (mpirnk.eq.0) then
             IF (     STR1.AND.     STR2) WRITE (*,2030) STREEP,STREEP
             IF (     STR1.AND..NOT.STR2) WRITE (*,2030) STREEP,' '
             IF (.NOT.STR1.AND.     STR2) WRITE (*,2030) ' ',STREEP
             endif
             GOTO 50
          ELSE
             if (mpirnk.eq.0) then
             IF (     STR1.AND.     STR2) WRITE (*,2030) STREEP,STREEP
             IF (.NOT.STR1.AND.     STR2) WRITE (*,2030) ' ',STREEP
             IF (     STR1.AND..NOT.STR2) WRITE (*,2040) STREEP,
     $       KM,KCT,ICENT(KCT),CENTRN(ICENT(KCT)),FNAME(ITYPE(KCT))
             IF (.NOT.STR1.AND..NOT.STR2) WRITE (*,2040) ' ',
     $       KM,KCT,ICENT(KCT),CENTRN(ICENT(KCT)),FNAME(ITYPE(KCT))
             endif
          ENDIF
      ELSE
          IF (KCT.EQ.0) THEN
             if (mpirnk.eq.0) then
             IF (     STR1.AND.     STR2) WRITE (*,2030) STREEP,STREEP
             IF (     STR1.AND..NOT.STR2) WRITE (*,2030) STREEP,' '
             IF (.NOT.STR1.AND..NOT.STR2) WRITE (*,2050)
     $       IM,ICT,ICENT(ICT),CENTRN(ICENT(ICT)),FNAME(ITYPE(ICT))
             endif
             IF (.NOT.STR1.AND..NOT.STR2.AND.AREP) WRITE(11)
     $       IM,ICT,ICENT(ICT),ITYPE(ICT)
             IF ((.NOT.STR1.AND.STR2).and.(mpirnk.eq.0)) 
     $       WRITE(*,2050) IM,ICT,
     $       ICENT(ICT),CENTRN(ICENT(ICT)),FNAME(ITYPE(ICT)),' '//STREEP
          ELSE
             if (mpirnk.eq.0) then
             IF (.NOT.STR1.AND.     STR2) WRITE(*,2050) IM,ICT,
     $       ICENT(ICT),CENTRN(ICENT(ICT)),FNAME(ITYPE(ICT)),' '//STREEP
             IF (.NOT.STR1.AND..NOT.STR2) WRITE(*,2050)
     $       IM,ICT,ICENT(ICT),CENTRN(ICENT(ICT)),FNAME(ITYPE(ICT)),' ',
     $       KM,KCT,ICENT(KCT),CENTRN(ICENT(KCT)),FNAME(ITYPE(KCT))
             IF (     STR1.AND..NOT.STR2) WRITE (*,2040) STREEP,
     $       KM,KCT,ICENT(KCT),CENTRN(ICENT(KCT)),FNAME(ITYPE(KCT))
             IF (     STR1.AND.     STR2) WRITE (*,2030) STREEP,STREEP
             endif
          ENDIF
      ENDIF
      IF (ICT.NE.0 .AND. .NOT.STR1) ICT=ICT+1
      IF (KCT.NE.0 .AND. .NOT.STR2) KCT=KCT+1
      IF (STR1) STR1=.FALSE.
      IF (STR2) STR2=.FALSE.
      GOTO 40
   50 CONTINUE
      if (mpirnk.eq.0) then
      NRP=0
      RPN='    '
      DO 51 I=1,NSUBRP
      IF (REPN(IREPS(I))(1:4).EQ.RPN) GOTO 51
      RPN=REPN(IREPS(I))(1:4)
      NRP=NRP+1
      IREP(NRP)=IREPS(I)
   51 CONTINUE
      WRITE(*,2051) (REPN(IREP(I))(1:4),I=1,NRP)
      IF (PRNT.GE.-1) WRITE(*,2052) GROUPN,(I,REPN(IREPS(I)),I=1,NSUBRP)
      IF (PRNT.LE.0) GOTO 61
      WRITE (*,2055) (OPERN(IFASOP(I)),I=1,2),(GROUPN(I),I=1,IGRID(1))
      WRITE(*,2020)
      DO 60 IM=1,NEQSYMF
      IND=INDSOR(IM)+1
      KND=INDSOR(IM+1)
C     ---------------------------------------------------
C     RENORMALIZE SYMMETRY COEFFICIENTS FOR NICE PRINTOUT
C     ---------------------------------------------------
      CMN = 1.E10
      CMX = 0.0
      DO 56 I=IND,KND
#if defined (BIT64)
      CA = ABS(REAL(CSYMORA(I)))
      CB = ABS(REAL(CSYMORB(I)))
#else
      CA = DABS(DBLE(CSYMORA(I)))
      CB = DABS(DBLE(CSYMORB(I)))
#endif
      IF (CA.GT.CMX) CMX = CA
      IF (CB.GT.CMX) CMX = CB
      IF (CA.LT.CMN.AND.CA.GT.1.E-10) CMN = CA
      IF (CB.LT.CMN.AND.CB.GT.1.E-10) CMN = CB
#if defined (BIT64)
      CA = ABS(AIMAG(CSYMORA(I)))
      CB = ABS(AIMAG(CSYMORB(I)))
#else
      CA = DABS(DIMAG(CSYMORA(I)))
      CB = DABS(DIMAG(CSYMORB(I)))
#endif
      IF (CA.GT.CMX) CMX = CA
      IF (CB.GT.CMX) CMX = CB
      IF (CA.LT.CMN.AND.CA.GT.1.E-10) CMN = CA
      IF (CB.LT.CMN.AND.CB.GT.1.E-10) CMN = CB
   56 CONTINUE
      CMX = 8.D0/CMX
      IF (CMN*CMX.GT.1.D0) CMX = 1.D0/CMN
      DO 58 I=IND,KND
      CSYMORA(I)=CSYMORA(I)*CMX
      CSYMORB(I)=CSYMORB(I)*CMX
   58 CONTINUE
      LND=MIN0(IND+8,KND)
      WRITE(*,2060) IRPSOR(IM),REPN(IRPSOR(IM)),(CSYMORA(I),I=IND,LND)
      IF(LND.LT.KND) WRITE(*,2070) '   ',       (CSYMORA(I),I=LND+1,KND)
      WRITE(*,2070)                ' B ',       (CSYMORB(I),I=IND,LND)
      IF(LND.LT.KND) WRITE(*,2070) '   ',       (CSYMORB(I),I=LND+1,KND)
   60 CONTINUE
   61 WRITE (*,2053) (I,I=1,NSUBSP)
      WRITE (*,2054) (NBSYMX(I,1),I=1,NSUBSP)
      WRITE (*,2056) (NBSYMX(I,2),I=1,NSUBSP)
C
      NBLLH = 0
      NBSSH = 0
      DO 336 II = 1, NSUBSP
      NBLLH = NBLLH + NBSYMX(II,1) * (NBSYMX(II,1) + 1) / 2
      NBSSH = NBSSH + NBSYMX(II,2) * (NBSYMX(II,2) + 1) / 2
 336  CONTINUE
C
      IF (PRNT.GE.0) THEN
      PRINT*
      PRINT*
      PRINT*, ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
      PRINT*, ' SYMMETRY INTEGRALS'
      PRINT*, ' NUMBER OF (LL/LL) INTEGRALS: ', NBLLH * (NBLLH + 1) / 2
      PRINT*, ' NUMBER OF (SS/LL) INTEGRALS: ', NBSSH * NBLLH
      PRINT*, ' NUMBER OF (SS/SS) INTEGRALS: ', NBSSH * (NBSSH + 1) / 2
      PRINT*, ' NUMBER OF (SL/SL) INTEGRALS: ', NBSSH * NBLLH * 2
      PRINT*
      PRINT*, ' MAXIMUM LENGTH (IN MEGABYTE) OF (LL) FILES : ',
     +          NINT (DBLE(NBLLH * (NBLLH + 1) / 2 ) * 8.D-6)
      PRINT*, ' MAXIMUM LENGTH (IN MEGABYTE) OF (SL) FILES : ',
     +          NINT (DBLE(NBLLH * NBSSH ) * 8.D-6)
      PRINT*, ' MAXIMUM LENGTH (IN MEGABYTE) OF (SS) FILES : ',
     +          NINT (DBLE(NBSSH * (NBSSH + 1) / 2 ) * 8.D-6)
      PRINT*, ' MAXIMUM LENGTH (IN MEGABYTE) OF (BR) FILES : ',
     +          NINT (DBLE(NBLLH * NBSSH * 2 ) * 8.D-6 )
      PRINT*, ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
      PRINT*
      PRINT*
      ENDIF
      DO 64 I = 1, NEQNUC
      DO 63 IC = 1, 2
      BASFIL = BASFLS(IC,I)
      IF (BASFIL(1:12).EQ.'UNCONTRACTED'
     &   .OR.BASFIL(1:10).EQ.'CONTRACTED'
     &   .OR.BASFIL(1:7).EQ.'KINETIC'
     &   .OR.BASFIL(1:5).EQ.'EMPTY'
     &   .OR.BASFIL(1:4).EQ.'EVEN') GOTO 63
      OPEN (10,FILE=BASFIL,FORM='FORMATTED',STATUS='OLD')
      REWIND (10)
      READ(10,2140) TEXT,DATE1,TIME1,FORMATE,NE,FORMATC,NC
      WRITE (*,2150)  I,COMP(IC),BASFIL,TEXT,DATE1,TIME1
      CLOSE (10)
   63 CONTINUE
   64 CONTINUE
      IF (PRNT.LE.-1) GOTO 85
      WRITE (*,2080)
      IPT=0
      IPC=0
      IPM=0
      DO 80 IM=1,NEQUIV
      ITA=KCOMB(IM)
      IND=INDCOM(ITA)+1
      IT=ITYPE(IND)
      IC=ICENT(IND)
      IF (IC.EQ.IPC.AND.IT.EQ.IPT) GOTO 80
      IF (IPM.NE.0) THEN
         WRITE (*,2101) KND-JND+1,IM-IPM
         WRITE (*,2102) INDCPRI(IPM),INDCCON(IPM)
         IF (PRNT.GE.2) THEN
            PRINT*, 'CONTRACTION COEFFICIENTS IN GENBAS FORMAT ',FORMATC
            WRITE (*,FORMATC) (COEFF(I),I=INDBAS(IPM)+1,INDBAS(IM))
            PRINT*
         ENDIF
      ENDIF
      IPC=IC
      IPT=IT
      IPM=IM
      CN=CENTRN(IC)
      JND=INDBAS(IM)+1
      KND=INDBAS(IM+1)
      IF (INDCPRI(IM).GT.0) THEN
         INCORE = 1
      ELSE
         INCORE = 0
      ENDIF
      WRITE (*,2090) ITA,FNAME(IT),CN,IM,JND,ALPHA(JND),XNORM(JND),
     &               INCORE
      DO 70 I=JND+1,KND
      IF (INDCPRI(IM).GE.I-JND+1) THEN
         INCORE = 1
      ELSE
         INCORE = 0
      ENDIF
      WRITE (*,2100) IM,I,ALPHA(I),XNORM(I),INCORE
   70 CONTINUE
   80 CONTINUE
      IF (IPM.NE.0) THEN
         WRITE (*,2101) KND-JND+1,NEQUIV-IPM+1
         WRITE (*,2102) INDCPRI(IPM),INDCCON(IPM)
         IF (PRNT.GE.2) THEN
           PRINT*, 'CONTRACTION COEFFICIENTS IN GENBAS FORMAT ',FORMATC
           WRITE (*,FORMATC) (COEFF(I),I=INDBAS(IPM)+1,INDBAS(NEQUIV+1))
           PRINT*
         ENDIF
      ENDIF
   85 IF (PRNT.LT.2) GOTO 97
      WRITE (*,2110)
      DO 95 IM=1,NEQUIV
      IND=INDSYM(IM)+1
      KND=INDSYM(IM+1)
      DO 90 I=IND,KND
      ICT=INDORB(I)+1
      KCT=INDORB(I+1)
      WRITE (*,2120) IM,KSYMRP(I),LSYMRP(I),(ITRAN(K),CTRANA(K),
     $               CTRANB(K),K=ICT,KCT)
   90 CONTINUE
   95 CONTINUE
   97 CONTINUE
      endif
C
C     ---------------------------------------------------------------
C     END OF PRINT OUTPUT SECTION. NEXT WRITE DATA ON FILE IN BINARY
C     ---------------------------------------------------------------
C
      NWRDLL=0
      NWRDSS=0
      NWRDLS=0
      NBLTOT=0
      NBSTOT=0
      DO 110 IM=1,NEQ(1)
      IT=KCOMB(IM)
      NBAS=INDCOM(IT+1)-INDCOM(IT)
      NBLTOT=NBLTOT+NBAS
      DO 100 KM=1,IM
      KT=KCOMB(KM)
      MBAS=INDCOM(KT+1)-INDCOM(KT)
      NWRDLL=NWRDLL+NBAS*MBAS
  100 CONTINUE
  110 CONTINUE
      DO 140 IM=NEQ(1)+1,NEQ(2)
      IT=KCOMB(IM)
      NBAS=INDCOM(IT+1)-INDCOM(IT)
      NBSTOT=NBSTOT+NBAS
      DO 120 KM=1,NEQ(1)
      KT=KCOMB(KM)
      MBAS=INDCOM(KT+1)-INDCOM(KT)
      NWRDLS=NWRDLS+MBAS*NBAS
  120 CONTINUE
      DO 130 KM=NEQ(1),IM
      KT=KCOMB(KM)
      MBAS=INDCOM(KT+1)-INDCOM(KT)
      NWRDSS=NWRDSS+MBAS*NBAS
  130 CONTINUE
  140 CONTINUE
C
C     ----------------------------------------------------
C     MAKE ARRAY ICOMOP READY FOR USE IN NORMAL POINTGROUP
C     SYMMETRY ENVIRONMENT (BETHAMATRIX -> UNITMATRIX)
C     ----------------------------------------------------
C
      DO 170 IM=NQV1+1,NQV2
      IND=INDCOM(IM)+1
      KND=INDCOM(IM+1)
      DO 160 I=IND,KND
      DO 150 IOP=IFIOP,NSYMOP
      ICOMOP(I,IOP)=-ICOMOP(I,IOP)
  150 CONTINUE
  160 CONTINUE
  170 CONTINUE
      CALL DAYTIME(DATUM,TIJD)
      WRITE(3)      TIJD,DATUM,VERS,NAME
      WRITE(3)      N14,N1,N3,N5,N7,N10,N12,N16
C     WRITE(3)      NEQ,NEC(2),NGA(2),NCO(2),NSYMOP,NNUCL,NSYMF,NCOEFF,
      WRITE(3)      NEQ,NEC,NGA(2),NCO(2),NSYMOP,NNUCL,NSYMF,NCOEFF,
     $              NSUBSP,NWRDLL,NWRDLS,NWRDSS,NBLTOT,NBSTOT,POTNUC
      WRITE(3)      (CHARGE(I),XCORD(I),YCORD(I),ZCORD(I),
     $              RNUC(I),I=1,NNUCL)
      WRITE(3)      ((NUCOP(I,J),I=1,NNUCL),(XMAT(I,J),I=2,4),
     $              J=1,NSYMOP)
      WRITE(3)      (KCOMB(I),INDBAS(I),INDSYM(I),I=1,NEQUIV),
     $              (INDCPRI(I),INDCCON(I),I=1,NEQUIV)
      WRITE(3)      (ALPHA(I),COEFF(I)*XNORM(I),I=1,NGAUS),
     $              (COEFFH(I),I=1,NGAUS)
      WRITE(3)      (ICENT(I),ITYPE(I),(ICOMOP(I,J),J=1,NSYMOP),
     $              I=1,NCOMB),(INDCOM(I),I=1,NQV2)
      WRITE(3)      (INDORB(I),INDB(I),KSYMRP(I),LSYMRP(I),KSYMF(I),
     $              I=1,NSYMF)
      WRITE(3)      (ITRAN(I),CTRANA(I),CTRANB(I),I=1,NCOEFF)
      WRITE(3)      (NBSYM(I),NBSYMX(I,1),NBSYMX(I,2),I=1,NSUBSP)
      WRITE(3)      IGRID(1),(REPN(I),I=1,NSUBSP)
      if (mpirnk.eq.0) WRITE (*,2300)
 1900 FORMAT(A,A10)
 2000 FORMAT(4A)
 2001 FORMAT(A,3(A,:,'-->'))
 2002 FORMAT(A,I4)
 2023 FORMAT(A,L1)
 2003 FORMAT(/' GROUP MULTIPLICATION TABLE ',A/)
 2004 FORMAT(I3,2X,16(1X,A1,A4))
 2005 FORMAT(/' EFFECT OF SYMMETRY OPERATIONS ON THE NUCLEI',A,//
     $       ' CENTER NAME',4X,24(A,1X))
 2006 FORMAT(I5,4X,A,2X,24I5)
 2008 FORMAT(/' EFFECT OF SYMMETRY OPERATIONS ON THE ORBITAL TYPES'
     $       ,' (IRRESPECTIVE OF CENTER)',A,//' TYPE  NAME',5X,24(A,1X))
 2009 FORMAT(I4,1X,A6,2X,24I5)
 2010 FORMAT(/' MOLECULAR GEOMETRY',A,F7.5,A//' SEQ.NO.',1X,'CENTER',
     1   2X,'X-COORDINATE',3X,'Y-COORDINATE',3X,'Z-COORDINATE',2X,
     2  'CHARGE  NUCL. EXP.'/(2X,I4,2X,A4,2X,3(F12.6,3X),F8.4,2X,G10.4))
 2011 FORMAT(/' COMBINED CENTER-TYPE TRANSFORMATIONS (DIRECT ',
     $       'PRODUCT OF PREVIOUS TABLES)',A,/,
     $       '   (INCLUDES BETHA-MATRIX AS INVERSION OPERATOR',
     $       ' IF APPLICABLE)'//' COMB.',5X,24(A,1X))
 2012 FORMAT(I4,4X,24I5)
 2013 FORMAT(/4(13X,A4,13X))
 2014 FORMAT(8(1X,'(',F6.3,',',F6.3,')':))
 2015 FORMAT(/2(' COMBINAT. CENTER    TYPE',T50)/)
 2016 FORMAT(//' DISTANCE TABEL ',A/T10,13(I4,1X,A2))
 2017 FORMAT(I4,1X,A4,(T10,13F7.3))
 2018 FORMAT(/' NUCLEAR REPULSION POTENTIAL : ',F15.8)
 2020 FORMAT(/)
 2030 FORMAT(A,T50,A)
 2040 FORMAT(A,T50,I4,I3,I5,1X,A4,1X,A6)
 2050 FORMAT(I4,I3,I5,1X,A4,1X,A6,T49,A,I4,I3,I5,1X,A4,1X,A6)
 2051 FORMAT(/' REPRESENTATIONS SPANNED BY THE ORBITALTYPES OF THE',
     $        ' BASIS ARE '//10(1X,A,:,','))
 2052 FORMAT(/' REDUCTION OF THE REPRESENTATIONS ALONG THE GROUP',
     $        ' CHAIN '//7X,A3,2X,A3,2X,A3//(I5,1X,A))
 2053 FORMAT(/' NUMBER OF BASISFUNCTIONS PER SYMMETRY REPRESENTATION'
     $       //'  REPRESENTATION ',16I4)
 2054 FORMAT(/' LARGE COMPONENT ',16I4)
 2056 FORMAT(/' SMALL COMPONENT ',16I4/)
 2055 FORMAT(/' SYMMETRY ADAPTED ORBITALS IN TERMS OF ABOVE TYPED'
     $,' FUNCTIONS'//' (OPERATION ',A4,' (',A4,') IS USED TO FIX ',
     $'THE PHASES BETWEEN BASISFUNCTIONS'/' OF TWO-(FOUR-) DIMEN',
     $'SIONAL REPRESENTATIONS)'//2X,3(A4,1X))
 2060 FORMAT(I4,A,' A ',9('(',F5.1,',',F5.1,')':))
 2070 FORMAT(18X,A3,9('(',F5.1,',',F5.1,')':))
 2080 FORMAT(/' EXPONENTS (AND COEFFICIENTS)'//
     &       ' COMB. REF.   REF. FUNCTION'
     &  ,' EXP.   EXPONENT',13X,'NORM',6X,'INCORE'
     & /7X,'TYPE   CENT.   NR.    NR.'/)
 2090 FORMAT(I4,1X,A6,3X,A2,I8,I5,F18.8,F20.8,I3)
 2100 FORMAT(16X,I8,I5,F18.8,F20.8,I3)
 2101 FORMAT(21X,'(',I3,' PRIMITIVES CONTRACTED TO',
     &      I3,' BASISFUNCTIONS)')
 2102 FORMAT(21X,'(',I3,' PRIMITIVES IN CORE      ',
     &      I3,' CONTR. IN CORE)'/)
 2110 FORMAT(/' NORMALIZED SYMMETRY ORBITALS'/' BASISF.  SYMM. REPR.'
     $      ,2X,2('COMB.       COEFF.ALFA            COEFF.BETA',10X)/)
 2120 FORMAT(I4,I8,I6,4X,2(I4,2X,2('(',F10.7,',',F10.7,')',1X))/
     $       (22X,2(I4,2X,2('(',F10.7,',',F10.7,')',1X))))
 2140 FORMAT (A80/A10,A8,2(A10,I4))
 2150 FORMAT (/' ---- NUCLEI GROUP ',I2,' ---'/
     &        1X,A5,' COMPONENT BASISSET FROM FILE : ',A20/' HEADER :'
     &        ,13X,A80/' DATE AND TIME :',6X,A10,2X,A8)
 2300 FORMAT(//T35,'-- END OF PROGRAM --')
      IF (AREP) CLOSE(11)
      RETURN
      END
