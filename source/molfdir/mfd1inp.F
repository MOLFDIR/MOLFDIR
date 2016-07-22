C     ==================
      SUBROUTINE MFD1INP
C     ==================
C
C     READ INPUT DATA FROM CENTRAL FILE DIRECTORY
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'paracom.inc'
#if defined (PARALLEL)
      INCLUDE 'mpidef.inc'
#else
      PARAMETER (mpirnk = 0)
#endif
C
      CHARACTER*5 CENTR*2,CARD*80
      CHARACTER*10 FILEMFO,FILESYM,FILESCR
      CHARACTER*7 FNAME1,FNAME2,FNAME3
      LOGICAL SUPDCK
      NAMELIST/GENERAL/TWOC
      NAMELIST/MOLFDIR/NAME,NEQNUC,GROUPN,AUANG,IFRLC,PRNT,SUPDCK,
     $                 IDEFNUC
      FNAME1='MFD1OUT'
      FNAME2='MFDSYMC'
      FNAME3='SCRATCH'
      IF (mpirnk .eq. 0) THEN
         WRITE (FILEMFO,'(A7)') FNAME1
         WRITE (FILESYM,'(A7)') FNAME2
         WRITE (FILESCR,'(A7)') FNAME3
      ELSEIF (mpirnk .LT. 10) THEN
         WRITE (FILEMFO,'(A7,A1,I1)') FNAME1,'.',mpirnk
         WRITE (FILESYM,'(A7,A1,I1)') FNAME2,'.',mpirnk
         WRITE (FILESCR,'(A7,A1,I1)') FNAME3,'.',mpirnk
      ELSEIF (mpirnk .LT. 100) THEN
         WRITE (FILEMFO,'(A7,A1,I2)') FNAME1,'.',mpirnk
         WRITE (FILESYM,'(A7,A1,I2)') FNAME2,'.',mpirnk
         WRITE (FILESCR,'(A7,A1,I2)') FNAME3,'.',mpirnk
      ELSE
         WRITE (FILEMFO,'(A7,A1,I3)') FNAME1,'.',mpirnk
         WRITE (FILESYM,'(A7,A1,I3)') FNAME2,'.',mpirnk
         WRITE (FILESCR,'(A7,A1,I3)') FNAME3,'.',mpirnk
      ENDIF
      if (mpirnk.eq.0) PRINT 1100, VERS
C
C     OPEN ALL FILES TO BE USED AND REWIND THEM
C
C     FILES IN USE :
C     NAME----UNIT------------PURPOSE-----------
C     MFD1OUT  3     BINARY OUTPUTFILE FOR SUBSEQUENT PROGRAMS
C     MFDSYMC 15     BINARY COEFFICIENTS FOR SYMMETRY-TRANSFORMATION
C
      OPEN(3,FILE=FILEMFO,FORM='UNFORMATTED')
      OPEN(15,FILE=FILESYM,FORM='UNFORMATTED')
      REWIND 3
      REWIND 15
C
C     ------------------------------------
C     READ MOLECULAR GEOMETRY AND RUN-TEXT
C     ------------------------------------
C
C     NAME :       COMMENT TEXT
C     NEQNUC :     NUMBER OF UNIQUE NUCLEI
C     GROUPN :     1-3 GROUPNAME(S)
C     AUANG :      COORDINATE UNITS (AU OR ANG; AU DEFAULT)
C     IFRLC :      REMOVE NON-Y(L,M) COMBINATIONS'
C                        0= YES FOR LARGE, SMALL : S FROM G ONLY
C                        1= YES FOR LARGE,YES FOR SMALL COMPONENT
C                        2= YES FOR LARGE, NO FOR SMALL COMPONENT
C                       -1= NO  FOR LARGE, NO FOR SMALL COMPONENT
C     PRNT :      PRINT-OPTION
C     IDEFNUC :   FORMULA USED TO GENERATE NUCLEAR EXPONENT
C
      TWOC=.FALSE.
C
      CALL LOCATE('GENERAL',7,ITEST)
      IF (ITEST .GT. 0) THEN
         READ(5,GENERAL,ERR=100)
      ELSE
         NAME=' NO TITLE GIVEN ON MOLFDIR INPUT'
      ENDIF
      NEQNUC = 0
      GROUPN(1) = '   '
      GROUPN(2) = '   '
      GROUPN(3) = '   '
      AUANG = 'AU '
      IFRLC = 0
      PRNT = 0
      SUPDCK = .FALSE.
      IDEFNUC = 2
      CALL LOCATE('MOLFDIR',7,ITEST)
      IF (ITEST .GT. 0) THEN
         READ(5,MOLFDIR,ERR=10000)
      ELSE
         GOTO 10000
      ENDIF
      CALL LSHIFTC(3,GROUPN(1))
      CALL LSHIFTC(3,GROUPN(2))
      CALL LSHIFTC(3,GROUPN(3))
      IF (AUANG.NE.'AU'.AND.AUANG.NE.'ANG')
     $CALL Q2JOB(2,'UNKNOWN COORDINATE UNITS : '//AUANG,' ',0)
      IF (IABS(IFRLC).GT.2)
     $CALL Q2JOB(2,'NON-Y(L,M) OPTION : VALUE UNDEFINED',' ',0)
      IF (IFRLC.EQ. 2) FRLC(2)=.FALSE.
      IF (IFRLC.EQ. 1) FRLC(3)=.TRUE.
      IF (IFRLC.EQ.-1) FRLC(1)=.FALSE.
      IF (IFRLC.EQ.-1) FRLC(2)=.FALSE.
      CALL EASYINP
      AURAD=1.0/BOHRRAD
      IND=0
      INDNUC(1)=0
      DO 7 I=1,NEQNUC
      IND=IND+1
      READ (*,1040,END=10020) CENTRN(IND),XCORD(IND),YCORD(IND),
     &                        ZCORD(IND),CHARGE(IND),RNUC(IND)
      CENTR = CENTRN(IND)
      CALL LSHIFTC(2,CENTR)
      IF (AUANG.EQ.'ANG') THEN
          XCORD(IND)=XCORD(IND)*AURAD
          YCORD(IND)=YCORD(IND)*AURAD
          ZCORD(IND)=ZCORD(IND)*AURAD
      ENDIF
C
C     -------------------------------------------------------------
C     SET UP NUCLEAR INDEX ARRAYS AND FIND SYMM. EQUIVALENT CENTERS
C     "IND" WILL BE CHANGED IF NEW CENTRA ARE GENERATED
C     -------------------------------------------------------------
C
      CALL SETINDN(IND)
      INDNUC(I+1)=IND
    7 CONTINUE
      NNUCL=IND
C
C     ---------------------------
C     END OF SECTION GENERAL DATA
C     ---------------------------
C
      NQV = 0
      KND = 0
      LND = 0
      NEQCOM=0
      INDNC=1
      MINTYP=1
      MAXTYP=1
C
C     -------------------------
C     INPUT SECTION BASISSETS :
C     IC=1 : LARGE COMPONENT
C     IC=2 : SMALL COMPONENT
C     -------------------------
C
      IF (.NOT.TWOC) 
     &   OPEN (55,FILE=FILESCR,STATUS='NEW',FORM='FORMATTED')
      DO 24 IC=1,2
      FRLCC=FRLC(IC)
      DO 23 NUC=1,NEQNUC
      CARD(1:5)='EMPTY'
      IF ((.NOT.TWOC).OR.(IC.LT.2)) READ (*,1010) CARD
      BASFLS (IC,NUC) = CARD
      IOPT=3
      IF (CARD(1:12).EQ.'UNCONTRACTED') THEN
         IOPT = 1
      ELSEIF (CARD(1:10).EQ.'CONTRACTED') THEN
         IOPT = 2
      ELSEIF (CARD(1:7).EQ.'KINETIC') THEN
         IF (IC.NE.2) 
     &      CALL Q2JOB(2,CARD//' IS NOT A GOOD FILENAME',' ',0)
         IOPT = 4
      ELSEIF (CARD(1:5).EQ.'EMPTY') THEN
	 IOPT = 5
      ELSEIF (CARD(1:4).EQ.'EVEN') THEN
	 IOPT = 6
      ENDIF
      IF (IOPT.EQ.3) IDEFNUC=3
      CALL BASIS (NUC,KND,INDNC,LND,NQV,NEQCOM,IC,IOPT,CARD,DCHRG,DEXPO)
C
C     Set nuclear charge and exponent
C
      IF (IC.EQ.1) THEN
C
C     Look up symbol of this element
C
      IND = INDNUC(NUC) + 1
      CENTR = CENTRN(IND)
      CALL LSHIFTC(2,CENTR)
      DO 4 J=0,N2
    4 IF (CENTR.EQ.ATOMS(J)) GOTO 5
      CALL Q2JOB(2,' UNKNOWN ATOMIC NAME GIVEN : '//CENTR,' ',0)
    5 IF (IOPT.NE.3.OR.J.EQ.0) THEN
         DCHRG = J
C
C        Use model to generate nuclear exponent
C
         IF (IDEFNUC.EQ.1) THEN
C
C        Reference : Visser et al. Chem. Phys. Lett. 134, 34 (1987).
C
           IF (J.EQ.0) THEN
              DEXPO = 1.0D10
           ELSEIF (J.LT.108) THEN
#if defined (BIT64)
              DEXPO = 3.88E9 * (RNWGHT (J) ** (-0.666667) )
#else
              DEXPO = 3.88D9 * (RNWGHT (J) ** (-0.666667) )
#endif
           ELSE
              IF ((RNUC(IND).EQ.0.0).and.(mpirnk.eq.0)) THEN
              PRINT*, "WARNING : NUCLEAR MASS UNKNOWN FOR ELEMENT ",
     &        CENTRN
              PRINT*, "USING VALUE FOR ELEMENT ",ATOMS(108),
     &                " TO CALCULATE NUCLEAR EXPONENT"
            
              ENDIF
#if defined (BIT64)
              DEXPO = 3.88E9 * (RNWGHT (108) ** (-0.666667) )
#else
              DEXPO = 3.88D9 * (RNWGHT (108) ** (-0.666667) )
#endif
           ENDIF
         ELSE
C
C        Reference : Formula's from GRASP
C
           IF (J.GT.109) THEN
              if (mpirnk.eq.0)
     &        PRINT*,"ERROR: NUCLEAR ISOTOPE MASS UNKNOWN FOR ",
     &        "ELEMENT ",J
              CALL Q2JOB(6,'STOPPING',' ',0)
           ENDIF
#if defined (BIT64)
           RMS = (0.836E0 * ( INWGHT(J) ** (1.0E0/3.0E0))+ 0.570E0 )
           DEXPO = 1.5E0 / ( (RMS / BOHRFM ) ** 2.0E0 )
#else
           RMS = (0.836D0 * ( INWGHT(J) ** (1.0D0/3.0D0))+ 0.570D0 )
           DEXPO = 1.5D0 / ( (RMS / BOHRFM ) ** 2.0D0 )
#endif
         ENDIF
      ENDIF
      IF (CHARGE(IND).EQ.0.0) CHARGE(IND) = DCHRG
      IF (RNUC(IND).EQ.0.0) RNUC(IND) = DEXPO
      DO INDALL = IND+1, INDNUC(NUC+1)
         CHARGE(INDALL) = CHARGE(IND)
         RNUC(INDALL) = RNUC(IND)
      ENDDO
      ENDIF
   23 CONTINUE
      NCO(IC) = INDNC - 1
      NEQ(IC) = NQV
      NGA(IC) = KND
      NEC(IC) = NEQCOM
   24 CONTINUE
      IF (.NOT.TWOC) CLOSE (55,STATUS='DELETE')
      MINTYP=(MINTYP-1)*MINTYP*(MINTYP+1)/6+1
      MAXTYP=MAXTYP*(MAXTYP+1)*(MAXTYP+2)/6
C
C     -----------------------------
C     END OF SECTION BASISSET INPUT
C     -----------------------------
C
C
C     ---------------------------
C     CALCULATE NUCLEAR POTENTIAL
C     ---------------------------
C
      POTNUC=0.0
      DO 26 I=1,NNUCL
      C1=CHARGE(I)
      X1=XCORD(I)
      Y1=YCORD(I)
      Z1=ZCORD(I)
      DO 25 J=1,I-1
      CC=C1*CHARGE(J)
      XX=X1-XCORD(J)
      YY=Y1-YCORD(J)
      ZZ=Z1-ZCORD(J)
      DIST=XX*XX+YY*YY+ZZ*ZZ
      IF (DIST.LT.0.01.AND..NOT.SUPDCK) CALL Q2JOB
     $(2,'FOUND DIFFERENT NUCLEI WITHIN 0.1 AU DISTANCE',' ',0)
#if defined (BIT64)
      POTNUC=POTNUC+CC/SQRT(DIST)
#else
      POTNUC=POTNUC+CC/DSQRT(DIST)
#endif
   25 CONTINUE
   26 CONTINUE
      RETURN
C
 1000 FORMAT('>> ',A,I2,A)
 1010 FORMAT(A)
 1040 FORMAT(A2,4F12.8,G12.8)
C
 1100 FORMAT('1P R O G R A M   F O R   R E L A T I V I S T I C   M O L '
     $,'E C U L A R   F O C K - D I R A C   C A L C U L A T I O N S'/
     $33X,'V E R S I O N  ',A//
     $//50X,'BY'/40X,'PATRICK J.C. AERTS'/25X,'THEORETICAL CHEMISTRY',
     $' DEPARTMENT      UNIVERSITY OF GRONINGEN'///)
C
  100 CALL Q2JOB(1,'ERROR READING GENERAL NAMELIST',' ',0)
10000 CALL Q2JOB(1,'--TEXTCARD',' ',0)
10020 CALL Q2JOB(1,'--ATOM NAME,COORDINATES',' ',0)
      END
