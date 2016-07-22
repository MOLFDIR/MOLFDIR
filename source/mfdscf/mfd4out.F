C
C     ==================
      SUBROUTINE MFD4OUT(H,HO,HOC,G,GO,GOC,D,E)
C     ==================
C
      IMPLICIT REAL*8 (A-H, O-Z)
C     
      INCLUDE 'paracom.inc'
      INCLUDE 'general.inc'
      INCLUDE 'memory.inc'
C
      REAL*8 G (*)
      REAL*8 H (*)
      REAL*8 GO (*), GOC (*)
      REAL*8 HO (*), HOC (*)
      REAL*8 D(*), E(*)
      CHARACTER*10 BRTEX*28,DATES,TIMEX*8
C
C     --------------
C     GET PROPERTIES
C     --------------
C
      IF (ONLYBRT) GOTO 100
      IF (BREIT) THEN
         BRTEX = ' (WITH BREIT-CORRECTION)    '
      ELSE
         BRTEX = ' (WITHOUT BREIT-CORRECTION) '
      ENDIF
C
      CALL MFDPROP (G(IPR1), G(IPR2), G(IPR3), G(IPR4), G(IPR5),
     +              G(IPR6), GO(IPR1), GO(IPR2), GO(IPR3), GO(IPR4),
     +              GO(IPR5), GO(IPR6), 3,H(1),H((2*LENGTH)+1),
     +              HO(1),HO((2*LENGTH)+1),HOC(1),HOC((2*LENGTH)+1),
     +              D)
C
      IF (TWOC) THEN
C
      IF (OPENSH) THEN
	IF (NOPEN.LT.2) THEN
        WRITE(*,1002) BRTEX, ETOTAL,
     +                POTNUC, POTEN, VIRIAL (1) * 0.5D0,
     +                ELREPC (1),             ELREPC (2),
     +                ELREPO (1,1),           ELREPO (2,1),
     +                ONELEN (1),             ONELEN (2),
     +                ATTEN (1),              ATTEN (2),
     +                RKINEN (1),             RKINEN (2)
      ELSE
      WRITE(*,1004) BRTEX, ETOTAL,
     +                POTNUC, POTEN, VIRIAL (1) * 0.5D0,
     +                ELREPC (1),             ELREPC (2),
     +                ELREPC (3),
     +                ELREPO (1,1),           ELREPO(2,1),
     +                ELREPO (3,1),
     +                ELREPO (1,2),           ELREPO(2,2),
     +                ELREPO(3,2),
     +                ONELEN (1),             ONELEN (2),
     +                ONELEN (3),
     +                ATTEN (1),              ATTEN (2),
     +                ATTEN (3),
     +                RKINEN (1),             RKINEN (2),
     +                RKINEN (3)
      ENDIF
      ELSE
        WRITE(*,1003) BRTEX, ETOTAL,
     +                POTNUC, POTEN, VIRIAL (1) * 0.5D0,
     +                ELREPC (1),
     +                ONELEN (1),
     +                ATTEN (1),
     +                RKINEN (1)
C
      ENDIF
      NMO = 0
C
      WRITE(*,1080)
C
      DO 16 IRP=1,NSYMRP
       IF (IAVIRP(IRP).NE.IRP) THEN
           NMO = NMO + NOC(IRP)
           GOTO 16
        ENDIF
        WRITE(*,1030) REPN(IRP)
	IF (NOPEN.GT.1) THEN
        WRITE(*,1091)
        DO 15 MO=1,NOC(IRP)
          NMO = NMO + 1
          WRITE(*,1101) IRP,MO,ONELENO(NMO),RKINENO(NMO),REPCO(NMO),
     +                  REPOO(NMO,1),REPOO(NMO,2),
     +                  ATTENO(NMO)
 15     CONTINUE
        WRITE(*,1111) IRP,ONELENR(IRP),RKINENR(IRP),REPCR(IRP),
     +                REPOR (IRP,1),REPOR (IRP,2),
     +                ATTENR(IRP)
	ELSE
	WRITE(*,1092)
	DO 17 MO=1,NOC(IRP)
	  NMO = NMO + 1
	  WRITE(*,1102) IRP,MO,ONELENO(NMO),RKINENO(NMO),REPCO(NMO),
     +                  REPOO(NMO,1),
     +                  ATTENO(NMO)
 17     CONTINUE
        WRITE(*,1112) IRP,ONELENR(IRP),RKINENR(IRP),REPCR(IRP),
     +                REPOR (IRP,1),
     +                ATTENR(IRP)
        ENDIF
 16   CONTINUE
C
      ELSE
C
      IF (OPENSH) THEN
	IF (NOPEN.LT.2) THEN
        WRITE(*,1000) BRTEX, ETOTAL,
     +                POTNUC, POTEN, VIRIAL (1), VIRIAL (2),
     +                ELREPC (1),             ELREPC (2),
     +                ELREPO (1,1),           ELREPO (2,1),
     +                ONELEN (1),             ONELEN (2),
     +                ATTEN (1),              ATTEN (2),
     +                RKINEN (1),             RKINEN (2),
     +                BMCON (1),              BMCON (2),
     +                RKINEN (1) + BMCON (1), RKINEN (2) + BMCON (2)
      ELSE
      WRITE(*,1005) BRTEX, ETOTAL,
     +                POTNUC, POTEN, VIRIAL (1) ,VIRIAL (2),
     +                ELREPC (1),             ELREPC (2),
     +                ELREPC (3),
     +                ELREPO (1,1),           ELREPO(2,1),
     +                ELREPO (3,1),
     +                ELREPO (1,2),           ELREPO(2,2),
     +                ELREPO(3,2),
     +                ONELEN (1),             ONELEN (2),
     +                ONELEN (3),
     +                ATTEN (1),              ATTEN (2),
     +                ATTEN (3),
     +                RKINEN (1),             RKINEN (2),
     +                RKINEN (3),
     +                BMCON (1),              BMCON (2),
     +                BMCON (3),
     +                RKINEN (1) + BMCON (1), RKINEN (2) + BMCON (2),
     +                RKINEN (3) + BMCON (3)
       ENDIF
      ELSE
        WRITE(*,1001) BRTEX, ETOTAL,
     +                POTNUC, POTEN, VIRIAL (1), VIRIAL (2),
     +                ELREPC (1),
     +                ONELEN (1),
     +                ATTEN (1),
     +                RKINEN (1),
     +                BMCON (1),
     +                RKINEN (1) + BMCON (1)
      ENDIF
C
      NMO = 0
C
      WRITE(*,1080)
C
      DO 6 IRP=1,NSYMRP
        IF (NOC(IRP).EQ.0) GOTO 6
        IF (IAVIRP(IRP).NE.IRP) THEN
           NMO = NMO + NOC(IRP)
           GOTO 6
        ENDIF
        WRITE(*,1030) REPN(IRP)
	IF (NOPEN.GT.1) THEN
        WRITE(*,1090)
        DO 5 MO=1,NOC(IRP)
          NMO = NMO + 1
          WRITE(*,1100) IRP,MO,ONELENO(NMO),RKINENO(NMO),REPCO(NMO),
     +                  REPOO(NMO,1),REPOO(NMO,2),
     +                  ATTENO(NMO),BMCONO(NMO),BMCONO(NMO)+RKINENO(NMO)
 5      CONTINUE
        WRITE(*,1110) IRP,ONELENR(IRP),RKINENR(IRP),REPCR(IRP),
     +                REPOR (IRP,1),REPOR(IRP,2),
     +                ATTENR(IRP),BMCONR(IRP),BMCONR(IRP)+RKINENR(IRP)
	ELSE
        WRITE(*,1093)
        DO 7 MO=1,NOC(IRP)
          NMO = NMO + 1
          WRITE(*,1103) IRP,MO,ONELENO(NMO),RKINENO(NMO),REPCO(NMO),
     +                  REPOO(NMO,1),
     +                  ATTENO(NMO),BMCONO(NMO),BMCONO(NMO)+RKINENO(NMO)
 7      CONTINUE
        WRITE(*,1113) IRP,ONELENR(IRP),RKINENR(IRP),REPCR(IRP),
     +                REPOR (IRP,1),
     +                ATTENR(IRP),BMCONR(IRP),BMCONR(IRP)+RKINENR(IRP)
	ENDIF
 6    CONTINUE
      ENDIF
C
C     --------------------
C     GET BREIT CORRECTION
C     --------------------
C
 100  IF ((.NOT.TWOC) .AND. (.NOT. ONLYONE)) THEN
      EMEM = ETOTAL
      IF (.NOT.BREIT.AND..NOT.ONLYBRT.AND..NOT.BREXIST) GOTO 200
      IPTR1 = 1
      IPTR2 = IPTR1 + LENGTH
      IPTR3 = IPTR2 + LENGTH
      IPTR4 = IPTR3 + LENGTH
      CALL MFDENSN(H(IPTR1),H(IPTR2),H(IPTR3),H(IPTR4),
     +             HO(IPTR1),HO(IPTR2),HO(IPTR3),HO(IPTR4),
     +             HOC(IPTR1),HOC(IPTR2),HOC(IPTR3),HOC(IPTR4))
      CALL MFDBREIT(H(IPTR1),H(IPTR2),H(IPTR3),H(IPTR4),
     +              HO(IPTR1),HO(IPTR2),HO(IPTR3),HO(IPTR4),
     +              HOC(IPTR1),HOC(IPTR2),HOC(IPTR3),HOC(IPTR4),
     +              G(IPTR1),G(IPTR2),G(IPTR3),G(IPTR4),
     +              GO(IPTR1),GO(IPTR2),GO(IPTR3),GO(IPTR4),
     +              GOC(IPTR1),GOC(IPTR2),GOC(IPTR3),GOC(IPTR4),E)
C
      CALL MFDFMCP(G(IPTR1),G(IPTR2),G(IPTR3),G(IPTR4),
     +             H(IPTR1),H(IPTR3))
C
      IF (OPENSH) THEN
        CALL MFDFMCP(GO(IPTR1),GO(IPTR2),GO(IPTR3),GO(IPTR4),
     +               HO(IPTR1),HO(IPTR3))
	IF (NOPEN.GT.1) THEN
	CALL MFDFMCP(GOC(IPTR1),GOC(IPTR2),GOC(IPTR3),GOC(IPTR4),
     +               HOC(IPTR1),HOC(IPTR3))
	ENDIF
      ENDIF
      CALL MFDPROP (G(IPR1), G(IPR2), G(IPR3), G(IPR4), G(IPR5),
     +              G(IPR6), GO(IPR1), GO(IPR2), GO(IPR3), GO(IPR4),
     +              GO(IPR5), GO(IPR6), 3,H(1),H((2*LENGTH)+1),
     +              HO(1),HO((2*LENGTH)+1),HOC(1),HOC((2*LENGTH)+1),
     +              D)
C
      IF (OPENSH) THEN
        BRCOR = ELREPC(1)+ELREPO(2,1)+(ELREPC (2)+ELREPO(1,1))/2.
	IF (NOPEN.GT.1) THEN
	  BRCOR=BRCOR+ELREPO(3,2)+(ELREPC(3)+ELREPO(2,2)+
     +          ELREPO(1,2)+ELREPO(3,1))/2.
          IF (BREIT) THEN
             ETOTAL = EMEM
          ELSE
             ETOTAL = EMEM + BRCOR
          ENDIF
	  WRITE(*,2002) ETOTAL,BRCOR,ELREPC(1),ELREPC(2),ELREPC(3),
     +                  ELREPO(1,1),ELREPO(2,1),ELREPO(3,1),
     +                  ELREPO(1,2),ELREPO(2,2),ELREPO(3,2)
        ELSE
          IF (BREIT) THEN
             ETOTAL = EMEM
          ELSE
             ETOTAL = EMEM + BRCOR
          ENDIF
	  WRITE(*,2000) ETOTAL,BRCOR,ELREPC(1),ELREPC(2),ELREPO(1,1),
     +                  ELREPO(2,1)
	ENDIF
      ELSE
        BRCOR = ELREPC (1)
        IF (BREIT) THEN
           ETOTAL = EMEM
        ELSE
           ETOTAL = EMEM + BRCOR
        ENDIF
        WRITE(*,2001) ETOTAL,BRCOR, ELREPC (1)
      ENDIF
C
      NMO = 0
C
      WRITE(*,2080)
C
      DO 26 IRP=1,NSYMRP
        IF (NOC(IRP).EQ.0) GOTO 26
        IF (IAVIRP(IRP).NE.IRP) THEN
           NMO = NMO + NOC(IRP)
           GOTO 26
        ENDIF
        WRITE(*,1030) REPN(IRP)
	IF (NOPEN.GT.1) THEN
        WRITE(*,2090)
        DO 25 MO=1,NOC(IRP)
          NMO = NMO + 1
          WRITE(*,2100) IRP,MO,REPCO(NMO),REPOO(NMO,1),REPOO(NMO,2)
25      CONTINUE
        WRITE(*,2110) IRP,REPCR(IRP),REPOR (IRP,1),REPOR (IRP,2)
	ELSE
	WRITE(*,2091)
        DO 27 MO=1,NOC(IRP)
          NMO = NMO + 1
          WRITE(*,2101) IRP,MO,REPCO(NMO),REPOO(NMO,1)
27      CONTINUE
        WRITE(*,2111) IRP,REPCR(IRP),REPOR (IRP,1)
	ENDIF
26    CONTINUE
      ETOTAL = EMEM
      ENDIF
C
#if !(defined (PARALLEL))
      CLOSE (INTFILE (4))
      CLOSE (LBLFILE (4))
#endif
C
 200  CONTINUE
      IF (ITER0) RETURN
C
      ENTRY MFDMORB
C
C     ---------------------------
C     PRINT THE OCCUPIED ORBITALS
C     ---------------------------
C
      OPEN(MFDVECA,FILE=FNAM(9),FORM='FORMATTED')
      REWIND MFDVECA
C
      LENREC=8 * MAX0(4*(NL+NS)+4,80)
C
      OPEN(MFDVECB,FILE=FNAM(2),ACCESS='DIRECT',
     +     RECL=LENREC)
C
      IF (ONLYBRT) GOTO 300
      IF (PRTVC) WRITE(*,1020)
      WRITE(MFDVECA,1050) SCFTXT
      CALL DAYTIME(DATES,TIMEX)
      WRITE(MFDVECA,1060) DATES,TIMEX
      WRITE(MFDVECA,1061) ICYCLS,BREIT,TWOC
      IF (TWOC) BRCOR=0.0D0
      WRITE(MFDVECA,1062) ETOTAL,EPREV,EMDIF,WDIF,TOTDIF,BRCOR
      WRITE(MFDVECA,1050) FORMAT(1:20)
      WRITE(MFDVECA,1063) NSYMRP
      WRITE(MFDVECA,1064) (NBLR(I),I=1,NSYMRP)
      WRITE(MFDVECA,1064) (NBSR(I),I=1,NSYMRP)
      MAVREC=2*NOCT+2
      DO 20 IRP=1,NSYMRP
      NOCC=NOC(IRP)
      IF (TWOC) THEN
        NB = NBSIM (IRP)
        NBL = NBSIM1 (IRP)
      ELSE
        NB  =NBSYM  (IRP)
        NBL =NBSYM1 (IRP)
      ENDIF
      IVEC=2*NBL*(NBL+1)
      KVEC=IVEC+NB
      IF (PRTVC) WRITE(*,1030) REPN(IRP)
      NBS = NBSIM2(IRP)
      NBLRIRP = NBLR(IRP)
      NBSRIRP = NBSR(IRP)
      NBTRIRP = NBLRIRP + NBSRIRP
      IF (IAVIRP(IRP).EQ.IRP) WRITE(MFDVECA, 1210) INDSRP(IRP),
     +                             REPN (IRP),NBLRIRP,NB
      DO 10 NA = 1, NBTRIRP
        MAVREC=MAVREC+1
	    READ (MFDVECB,REC=MAVREC)
     +       IDUM,IDUM,IDUM,EW,(G(IVEC+I),G(KVEC+I),I=1,NB)
        IF (IAVIRP(IRP).NE.IRP) GO TO 9
        IF (NA.LE.NBSRIRP) THEN
           GOTO 10
        ELSE IF (NA-NBSRIRP.LE.NCL(IRP)) THEN
           OCCUP=1.0D0
           THECOP=1.0D0
        ELSE IF (NA-NBSRIRP.LE.NCL(IRP)+NOP(IRP,1)) THEN
           OCCUP=OCOPN(IRP,1)
           THECOP=COPCOF(1)
        ELSE IF (NA-NBSRIRP.LE.NOC(IRP)) THEN
           OCCUP=OCOPN(IRP,2)
           THECOP=COPCOF(2)
        ELSE
           OCCUP=0.D0
           THECOP=0.D0
        ENDIF
        WRITE(MFDVECA, 1211) NA-NBSRIRP, EW , OCCUP, THECOP
        WRITE(MFDVECA,FORMAT) (G(IVEC+I),G(KVEC+I),I=1,NB)
    9   IF (PRTVC.AND.(NA-NBSRIRP.LE.NOC(IRP)).AND.(NA.GT.NBSRIRP)) 
     +     WRITE(*,1040) INDSRP(IRP),NA-NBSRIRP,(G(IVEC+I),
     +                   G(KVEC+I),I=1,NB)
   10   CONTINUE
C
   20 CONTINUE
C
  300 CONTINUE
      IF (.NOT.ENDRUN) RETURN
      TEXT='--- NORMAL END OF SCF PROGRAM ---'
c     WRITE(*,1010) TEXT
      CLOSE(MFDVECB)
      CLOSE(MFDVECA)
c     WRITE(*,1010)
      RETURN
 1000 FORMAT(//' PROPERTIES AND EXPECTATION VALUES'/
     +        /' TOTAL ENERGY',A28,T45,G30.15/
     +        /' NUCLEAR REPULSION ENERGY',T45,G30.15,
     +        /' TOTAL POTENTIAL ENERGY',T45,G30.15,
     +        /' VIRIAL THEOREM 2.*<[V]>/<C*[A.P]>',T45,G30.15,
     +        /16X,'<[V]>-<C*[A.P]>/2.',T45,G30.15,
     +        //T45,'    CLOSED SHELL', T65, '      OPEN SHELL'/
     +        /' ELECTRON REPULSION EN. (WITH C-SHELL) ',T45,2G20.10,
     +        /' ELECTRON REPULSION EN. (WITH O-SHELL) ',T45,2G20.10,
     +        /' ONE-ELECTRON ENERGY',T45,2G20.10,
     +        /' NUCLEAR-ELECTRON ATTRACTION EN.',T45,2G20.10,
     +        /' DIRAC KINETIC ENERGY <C*[A.P]>',T45,2G20.10,
     +        /' BETHA MATRIX EXPECTATION VALUE',T45,2G20.10,
     +        /' KINETIC ENERGY <C*[A.P]>+<C*C*[B]>',T45,2G20.10)
 1001 FORMAT(//' PROPERTIES AND EXPECTATION VALUES'/
     +        /' TOTAL ENERGY',A28,T45,G30.15/
     +        /' NUCLEAR REPULSION ENERGY',T45,G30.15,
     +        /' TOTAL POTENTIAL ENERGY',T45,G30.15,
     +        /' VIRIAL THEOREM 2.*<[V]>/<C*[A.P]>',T45,G30.15,
     +        /16X,'<[V]>-<C*[A.P]>/2.',T45,G30.15,
     +        //T45,'    CLOSED SHELL',/
     +        /' ELECTRON REPULSION EN. (WITH C-SHELL) ',T45,G20.10,
     +        /' ONE-ELECTRON ENERGY',T45,G20.10,
     +        /' NUCLEAR-ELECTRON ATTRACTION EN.',T45,G20.10,
     +        /' DIRAC KINETIC ENERGY <C*[A.P]>',T45,G20.10,
     +        /' BETHA MATRIX EXPECTATION VALUE',T45,G20.10,
     +        /' KINETIC ENERGY <C*[A.P]>+<C*C*[B]>',T45,G20.10)
 1002 FORMAT(//' PROPERTIES AND EXPECTATION VALUES'/
     +        /' TOTAL ENERGY',A28,T45,G30.15/
     +        /' NUCLEAR REPULSION ENERGY',T45,G30.15,
     +        /' TOTAL POTENTIAL ENERGY',T45,G30.15,
     +        /' VIRIAL THEOREM 2.*<[V]>/<C*[A.P]>',T45,G30.15,
     +        //T45,'    CLOSED SHELL', T65, '      OPEN SHELL'/
     +        /' ELECTRON REPULSION EN. (WITH C-SHELL) ',T45,2G20.10,
     +        /' ELECTRON REPULSION EN. (WITH O-SHELL) ',T45,2G20.10,
     +        /' ONE-ELECTRON ENERGY',T45,2G20.10,
     +        /' NUCLEAR-ELECTRON ATTRACTION EN.',T45,2G20.10,
     +        /' KINETIC ENERGY',T45,2G20.10)
 1003 FORMAT(//' PROPERTIES AND EXPECTATION VALUES'/
     +        /' TOTAL ENERGY',A28,T45,G30.15/
     +        /' NUCLEAR REPULSION ENERGY',T45,G30.15,
     +        /' TOTAL POTENTIAL ENERGY',T45,G30.15,
     +        /' VIRIAL THEOREM 2.*<[V]>/<C*[A.P]>',T45,G30.15,
     +        //T45,'    CLOSED SHELL',/
     +        /' ELECTRON REPULSION EN. (WITH C-SHELL) ',T45,G20.10,
     +        /' ONE-ELECTRON ENERGY',T45,G20.10,
     +        /' NUCLEAR-ELECTRON ATTRACTION EN.',T45,G20.10,
     +        /' KINETIC ENERGY ',T45,G20.10)
 1004 FORMAT(//' PROPERTIES AND EXPECTATION VALUES'/
     +        /' TOTAL ENERGY',A28,T45,G30.15/
     +        /' NUCLEAR REPULSION ENERGY',T45,G30.15,
     +        /' TOTAL POTENTIAL ENERGY',T45,G30.15,
     +        /' VIRIAL THEOREM 2.*<[V]>/<C*[A.P]>',T45,G30.15,
     +        //T45,'    CLOSED SHELL', T65, '    OPEN SHELL 2',
     +          T85,'    OPEN SHELL 2'/,
     +        /' ELECTRON REPULSION EN. (WITH C-SHELL) ',T45,3G20.10,
     +        /' ELECTRON REPULSION EN. (WITH O-SHELL1)',T45,3G20.10,
     +        /' ELECTRON REPULSION EN. (WITH O-SHELL2)',T45,3G20.10,
     +        /' ONE-ELECTRON ENERGY',T45,3G20.10,
     +        /' NUCLEAR-ELECTRON ATTRACTION EN.',T45,3G20.10,
     +        /' KINETIC ENERGY',T45,3G20.10)
 1005 FORMAT(//' PROPERTIES AND EXPECTATION VALUES'/
     +        /' TOTAL ENERGY',A28,T45,G30.15/
     +        /' NUCLEAR REPULSION ENERGY',T45,G30.15,
     +        /' TOTAL POTENTIAL ENERGY',T45,G30.15,
     +        /' VIRIAL THEOREM 2.*<[V]>/<C*[A.P]>',T45,G30.15,
     +        /16X,'<[V]>-<C*[A.P]>/2.',T45,G30.15,
     +        //T45,'    CLOSED SHELL', T65, '    OPEN SHELL 1',
     +          T85,'    OPEN SHELL 2'/,
     +        /' ELECTRON REPULSION EN. (WITH C-SHELL) ',T45,3G20.10,
     +        /' ELECTRON REPULSION EN. (WITH O-SHELL1)',T45,3G20.10,
     +        /' ELECTRON REPULSION EN. (WITH O-SHELL2)',T45,3G20.10,
     +        /' ONE-ELECTRON ENERGY',T45,3G20.10,
     +        /' NUCLEAR-ELECTRON ATTRACTION EN.',T45,3G20.10,
     +        /' DIRAC KINETIC ENERGY <C*[A.P]>',T45,3G20.10,
     +        /' BETHA MATRIX EXPECTATION VALUE',T45,3G20.10,
     +        /' KINETIC ENERGY <C*[A.P]>+<C*C*[B]>',T45,3G20.10)
 1010 FORMAT(//26X,A)
 1020 FORMAT(//' OCCUPIED VECTORS IN THE NON-ORTHOGONAL BASIS'/)
 1030 FORMAT(//' REPRESENTATION ',A14/)
 1210 FORMAT(I4,1X, '***** ', A14, '*****',2I4)
 1211 FORMAT(1X,'MO:',I2,' EW:',G20.10,' OCC:',G15.10,' COPCOF:',
     +       G20.10)
 1040 FORMAT(' (',I2,',',I2,') ',(T11,'(',F9.6,',',F9.6,')',:,1X,
     $     '(',F9.6,',',F9.6,')',:,1X,'(',F9.6,',',F9.6,')',:,1X,
     $     '(',F9.6,',',F9.6,')',:,1X,'(',F9.6,',',F9.6,')')    )
 1050 FORMAT(A20)
 1060 FORMAT(2A10)
 1061 FORMAT(I4,2(3X,L1))
 1062 FORMAT(6G20.10)
 1063 FORMAT(I4)
 1064 FORMAT(16I4)
 1080 FORMAT('1      PROPERTIES AND EXPECTATION VALUES OF THE ORBITALS')
 1090 FORMAT(T13, 'E(ONE-E)         ',
     +       T30, 'E(KIN,DIRAC)     ',
     +       T47, 'E(REP,CLOSE)     ',
     +       T64, 'E(REP,OPEN 1)    ',
     +       T81, 'E(REP,OPEN 2)    ',
     +       T98, 'E(ATTR,NUC)      ',
     +       T115,'E(BETHA)         ',
     +       T132,'E(KIN,TOT)       ')
 1091 FORMAT(T13, 'E(ONE-E)         ',
     +       T30, 'E(KIN)           ',
     +       T47, 'E(REP,CLOSE)     ',
     +       T64, 'E(REP,OPEN 1)    ',
     +       T81, 'E(REP,OPEN 2)    ',
     +       T98, 'E(ATTR,NUC)      ')
 1093 FORMAT(T13, 'E(ONE-E)         ',
     +       T30, 'E(KIN,DIRAC)     ',
     +       T47, 'E(REP,CLOSE)     ',
     +       T64, 'E(REP,OPEN)      ',
     +       T81, 'E(ATTR,NUC)      ',
     +       T98, 'E(BETHA)         ',
     +       T115,'E(KIN,TOT)       ')
 1092 FORMAT(T13, 'E(ONE-E)         ',
     +       T30, 'E(KIN)           ',
     +       T47, 'E(REP,CLOSE)     ',
     +       T64, 'E(REP,OPEN)      ',
     +       T81, 'E(ATTR,NUC)      ')
 1100 FORMAT(1X,'(',I3,',',I3,')',2X,8(G17.9))
 1101 FORMAT(1X,'(',I3,',',I3,')',2X,6(G17.9))
 1102 FORMAT(1X,'(',I3,',',I3,')',2X,5(G17.9))
 1103 FORMAT(1X,'(',I3,',',I3,')',2X,7(G17.9))
 1110 FORMAT(/' SUM OF ',I3,1X,8(G17.9))
 1111 FORMAT(/' SUM OF ',I3,1X,6(G17.9))
 1112 FORMAT(/' SUM OF ',I3,1X,5(G17.9))
 1113 FORMAT(/' SUM OF ',I3,1X,7(G17.9))
 2000 FORMAT(//' TOTAL ENERGY WITH BREIT CORRECTION',T45,G30.15
     +        /' BREIT (GAUNT) CORRECTION ',T45,G30.15,
     +        //T40,'    CLOSED SHELL', T60, '      OPEN SHELL'/
     +        /' MAGNETIC BREIT ENERGY  (WITH C-SHELL) ',T40,2G20.10,
     +        /' MAGNETIC BREIT ENERGY  (WITH O-SHELL) ',T40,2G20.10)
 2001 FORMAT(//' TOTAL ENERGY WITH BREIT CORRECTION',T45,G30.15
     +        /' BREIT (GAUNT) CORRECTION ',T45,G30.15,/
     +        //T40,'    CLOSED SHELL',/
     +        /' MAGNETIC BREIT ENERGY  (WITH C-SHELL) ',T40,G20.10)
 2002 FORMAT(//' TOTAL ENERGY WITH BREIT CORRECTION',T45,G30.15
     +        /' BREIT (GAUNT) CORRECTION ',T45,G30.15,/
     +        //T40,'    CLOSED SHELL', T60, '    OPEN SHELL 1',
     +          T80, '    OPEN SHELL 2'/,
     +        /' MAGNETIC BREIT ENERGY  (WITH C-SHELL) ',T40,3G20.10,
     +        /' MAGNETIC BREIT ENERGY  (WITH O-SHELL1)',T40,3G20.10,
     +        /' MAGNETIC BREIT ENERGY  (WITH O-SHELL2)',T40,3G20.10)
 2080 FORMAT('1     BREIT CORRECTION ON THE ORBITAL EXPECTATION VALUES')
 2090 FORMAT(T15, 'E(MAG,CLOSE)     ',
     +       T32, 'E(MAG,OPEN 1)    ',
     +       T49, 'E(MAG,OPEN 2)    ')
 2091 FORMAT(T15, 'E(MAG,CLOSE)     ',
     +       T32, 'E(MAG,OPEN)      ')
 2100 FORMAT(1X,'(',I3,',',I3,')',2X,3(G17.9))
 2101 FORMAT(1X,'(',I3,',',I3,')',2X,2(G17.9))
 2110 FORMAT(/' SUM OF ',I3,1X,3(G17.9))
 2111 FORMAT(/' SUM OF ',I3,1X,2(G17.9))
      END
