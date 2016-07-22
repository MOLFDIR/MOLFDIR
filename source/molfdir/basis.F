C     ==============================================================
      SUBROUTINE BASIS (NUC,KND,INDNC,LND,NQV,NEQCOM,IC,IOPT,BASFIL,
     &                  DCHRG,DEXPO)
C     ==============================================================
C
C     IOPT  1)  UNCONTRACTED BASIS : READ ONLY THE EXPONENTS
C           2)  CONTRACTED BASIS :   READ EXPONENTS AND COEFFICIENTS
C           3)  BASIS FROM GENBAS :  READ EXPONENTS AND COEFFICIENTS FROM FILE
C           4)  KINETIC BALANCE :    SMALL COMPONENT IS @.P LARGE COMPONENT
C                                    IF UNI THEN NORMAL KIN. BAL.
C                                    IF SUB THEN D SUBSET OF S AND F SUBSET
C                                           OF P, ECT.
C           5)  EMPTY BASIS :        MAKE SOME VARIABLES ZERO
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
      CHARACTER*19 LINE
      CHARACTER*8 TEXT*80,DATE1*10,TIME1,FORMATE*15,FORMATC*15,BASFIL*80
      CHARACTER*5 TYP*1,KIN*3
      DIMENSION NORB(N5),EXPON(N17),CONTR(N16,N17),NCORB(N5),NCPRIM(N5)
      INTEGER NUCN,KINTYPE,KLTYPE(N5,3)
      DIMENSION ALP(N14)
      LOGICAL MULFAC,TOBE,EXIST,EXISTU,EXISTL
C
C     --------------------------------
      IF (IOPT.EQ.1.OR.IOPT.EQ.6) THEN
C     --------------------------------
C
      IF (.NOT.TWOC.AND.IC.EQ.1) WRITE (55,'(A19,10X,I4)') 
     &   'BASIS SET FROM NUC ',NUC
      READ (5,1000,END=11000) (TYPE(I),I=1,N5)
      IF (.NOT.TWOC.AND.IC.EQ.1) WRITE (55,1000) (TYPE(I),I=1,N5)
C
C     ------------------------------------------------------------------
C     CHECK FOR INVALID TYPES AND DETERMINE TYPENUMBER AND NUMB.OF TYPES
C     ------------------------------------------------------------------
C
      DO 20 I = 1,N5
      TYP = TYPE(I)
      DO 10 J = 0,N5
      IF (TYP.EQ.TYPES(J)) THEN
      IF (J.EQ.0) GOTO 30
      KTYPE(I)=J
      GOTO 20
      ENDIF
   10 CONTINUE
      CALL Q2JOB
     $(2,'UNKNOWN SYMBOL GIVEN : '//TYP//'.  S,P,D,F..EXPECTED',' ',0)
   20 CONTINUE
   30 NTYP = I - 1
      IF (NTYP.EQ.0) CALL Q2JOB
     $(2,'EMPTY BASISSET...(IS FORMAT CORRECT?!) N(3X,A)',' ',0)
      DO 40 J=1,NTYP
      MAXTYP=MAX0(MAXTYP,KTYPE(J))
      MINTYP=MIN0(MINTYP,KTYPE(J))
   40 CONTINUE
      READ (5,1010,END=11010) (NTYPE(I),I=1,NTYP),(NCPRIM(I),I=1,NTYP)
      IF (.NOT.TWOC.AND.IC.EQ.1) 
     &   WRITE (55,1010) (NTYPE(I),I=1,NTYP),(NCPRIM(I),I=1,NTYP)
      DO 70 I = 1 , NTYP
      NORB (I) = NTYPE(I)
C     Check on errors
      IF (NTYPE(I).GT.N17) CALL Q2JOB(3,'BASIS_IOPT_1','N17',NTYPE(I))
      IF (NORB(I).GT.N16) CALL Q2JOB(3,'BASIS_IOPT_1','N16',NORB(I))
      IF (NCPRIM(I).LT.0.OR.NCPRIM(I).GT.NTYPE(I)) THEN
         if (mpirnk.eq.0) 
     &   PRINT*,"WRONG NUMBER OF DEL. PRIMITIVES IN INPUT FOR ",TYPE(I)
         CALL Q2JOB(6,'INPUT ERROR IN BASIS',' ',0)
      ENDIF
C     Number of functions to be used in integral evaluation
      NCPRIM(I) = NTYPE(I) - NCPRIM(I)
      NCORB(I)  = NCPRIM(I)
      JND = KND
      IND = JND + 1
      KND = KND + NTYPE(I)**2
      IF (KND.GT.N15) CALL Q2JOB(3,'BASIS_IOPT_1','N15',KND)
      DO 60 J = 1, NTYPE(I)
      JND = JND + 1
      IF (IOPT.EQ.1) THEN
          READ (5,*,END=11030) ALPHA (JND)
      ELSE
          IF (J.EQ.1) THEN
             READ (5,*,END=11030) EVENCT,EVENRAT
#if defined (BIT64)
             ALPHA(JND) = EVENCT*(EVENRAT**(0.50 *REAL(NTYPE(I)-1)))
#else
             ALPHA(JND) = EVENCT*(EVENRAT**(0.5D0*DBLE(NTYPE(I)-1)))
#endif
             EVENRAT = 1.D0 / EVENRAT
          ELSE
             ALPHA(JND) = ALPHA(JND-1) * EVENRAT
          ENDIF
      ENDIF
      IF (.NOT.TWOC.AND.IC.EQ.1) WRITE (55,*) ALPHA (JND)
      COEFF (JND) = 0.0D0
      DO 50 JJ = 1, NORB (I) - 1
      JJND = JND + JJ * NTYPE(I)
      ALPHA (JJND) = ALPHA (JND)
      COEFF (JJND) = 0.0D0
   50 CONTINUE
   60 CONTINUE
      DO 65 J = 1, NORB(I)
      JJND = IND + (J  - 1)  * (NTYPE (I) + 1)
      NORBIT(J)=NTYPE(I)
      COEFF (JJND) = 1.0D0
   65 CONTINUE
      CALL SETINDA(NUC,KND,NTYPE(I)*NORB(I),INDNC,LND,NQV,NEQCOM,
     $KTYPE(I),NORB(I),IC,NCORB(I),NCPRIM(I))
   70 CONTINUE
C     -----------------------
      ELSEIF (IOPT.EQ.2) THEN
C     -----------------------
      READ (5,1000,END=11000) (TYPE(I),I=1,N5)
C
C     ------------------------------------------------------------------
C     CHECK FOR INVALID TYPES AND DETERMINE TYPENUMBER AND NUMB.OF TYPES
C     ------------------------------------------------------------------
C
      DO 120 I=1,N5
      TYP = TYPE(I)
      DO 110 J=0,N5
      IF (TYP.EQ.TYPES(J)) THEN
      IF (J.EQ.0) GOTO 130
      KTYPE(I)=J
      GOTO 120
      ENDIF
  110 CONTINUE
      CALL Q2JOB
     $(2,'UNKNOWN SYMBOL GIVEN : '//TYP//'.  S,P,D,F..EXPECTED',' ',0)
  120 CONTINUE
  130 NTYP = I - 1
      IF (NTYP.EQ.0) CALL Q2JOB
     $(2,'EMPTY BASISSET...(IS FORMAT CORRECT?!) N(3X,A)',' ',0)
      DO 140 J=1,NTYP
      MAXTYP=MAX0(MAXTYP,KTYPE(J))
      MINTYP=MIN0(MINTYP,KTYPE(J))
  140 CONTINUE
      READ (5,1010,END=11010) (NTYPE(I),I=1,NTYP),(NCPRIM(I),I=1,NTYP)
      READ (5,1010,END=11020) (NORB(I),I=1,NTYP),(NCORB(I),I=1,NTYP)
      DO 180 I=1,NTYP
      IF (NORB(I).LT.0) THEN
          NORB(I)=ABS(NORB(I))
          MULFAC=.TRUE.
      ELSE
          MULFAC=.FALSE.
      ENDIF
C     Check on errors
      IF (NTYPE(I).GT.N17) CALL Q2JOB(3,'BASIS_IOPT_2','N17',NTYPE(I))
      IF (NORB(I).GT.N16) CALL Q2JOB(3,'BASIS_IOPT_2','N16',NORB(I))
      IF (NORB(I).GT.NTYPE(I)) CALL Q2JOB
     $(2,'MORE CONTRACTED FUNCTIONS THAN PRIMITIVES. TYPE : '//TYPE(I)
     $,' ',0)
      IF (NCPRIM(I).LT.0.OR.NCPRIM(I).GT.NTYPE(I)) THEN
         if (mpirnk.eq.0)
     &   PRINT*,"WRONG NUMBER OF DEL. PRIMITIVES IN INPUT FOR ",TYPE(I)
         CALL Q2JOB(6,'INPUT ERROR IN BASIS',' ',0)
      ENDIF
      IF (NCORB(I).LT.0.OR.NCORB(I).GT.NORB(I)) THEN
         if (mpirnk.eq.0)
     &   PRINT*,"WRONG NUMBER OF DEL. FUNCTIONS IN INPUT FOR ",TYPE(I)
         CALL Q2JOB(6,'INPUT ERROR IN BASIS',' ',0)
      ENDIF
C     Number of functions to be used in integral evaluation
      NCPRIM(I) = NTYPE(I) - NCPRIM(I)
      NCORB(I)  = NORB(I) - NCORB(I)
      NORBS=1
      NORBF=MIN0(NORB(I),5)
  149 CONTINUE
      DO 150 J=1,NTYPE(I)
      IF (NORBS.EQ.1) THEN
      READ (5,1020,END=11040) EXPON(J),(CONTR(JC,J),JC=NORBS,NORBF)
      ELSE
      READ (5,1030,END=11040) (CONTR(JC,J),JC=NORBS,NORBF)
      ENDIF
  150 CONTINUE
      IF (NORBF.NE.NORB(I)) THEN
          NORBS=NORBF+1
          NORBF=MIN0(NORB(I),NORBF+5)
          GOTO 149
      ENDIF
      JND=KND
      IND=JND+1
      KND=KND+NTYPE(I)*NORB(I)
      IF (KND.GT.N15) CALL Q2JOB(3,'BASIS_IOPT_2','N15',KND)
C     --------------------------------
C     CONVERSION TO OLD DATA STRUCTURE
C     --------------------------------
      DO 170 JN=1,NORB(I)
      NORBIT(JN)=NTYPE(I)
      DO 160 JM=1,NTYPE(I)
      JND = JND + 1
      ALPHA(JND)=EXPON(JM)
      COEFF(JND)=CONTR(JN,JM)
      IF (ALPHA(JND).LE.0.0) CALL Q2JOB
     $(2,'EXPONENT LESS/EQUAL ZERO (0.0) IN BASIS',' ',0)
      IF (.NOT.MULFAC) GOTO 160
#if defined (BIT64)
      COEFF(JND) = COEFF(JND)*SQRT(ALPHA(JND))
#else
      COEFF(JND) = COEFF(JND)*DSQRT(ALPHA(JND))
#endif
  160 CONTINUE
  170 CONTINUE
      CALL SETINDA(NUC,KND,NTYPE(I)*NORB(I),INDNC,LND,NQV,NEQCOM,
     $KTYPE(I),NORB(I),IC,NCORB(I),NCPRIM(I))
  180 CONTINUE
C     -----------------------
      ELSEIF (IOPT.EQ.3) THEN
C     -----------------------
      NTYP = 0
      INQUIRE (FILE = BASFIL,EXIST = TOBE )
      IF (.NOT.TOBE) CALL Q2JOB(1,'FILE : '//BASFIL,' ',0)
      OPEN (10,FILE=BASFIL,FORM='FORMATTED',STATUS='OLD')
      REWIND (10)
      READ(10,1040,END=11050) TEXT,DATE1,TIME1,FORMATE,NE,FORMATC,NC,
     &                        DCHRG,DEXPO
  301 READ (10,1010,END=390) ICOMP,ITYP,NPRIM,NCONTR,
     &                       NCPRIM(ITYP),NCORB(ITYP)
      IF (ICOMP.NE.IC) THEN
         IF (IC.EQ.2) THEN
            NSTEP = NPRIM/NE + (NPRIM * NCONTR)/NC
            IF (MOD(NPRIM,NE).NE.0) NSTEP = NSTEP + 1
            IF (MOD(NCONTR*NPRIM,NC).NE.0) NSTEP = NSTEP + 1
            DO 310 INIT = 1, NSTEP
            READ (10,*,END=11040)
  310       CONTINUE
            GOTO 301
         ELSE
            CLOSE (10)
            RETURN
         ENDIF
      ENDIF
C     Check on errors
      IF (NPRIM.GT.N17) CALL Q2JOB(3,'BASIS_IOPT_3','N17',NPRIM)
      IF (NCONTR.GT.N16) CALL Q2JOB(3,'BASIS_IOPT_3','N16',NCONTR)
      IF (NCPRIM(ITYP).LT.0.OR.NCPRIM(ITYP).GT.NPRIM) THEN
         if (mpirnk.eq.0)
     &   PRINT*,"WRONG NUMBER OF DEL. PRIMITIVES IN INPUT ",TYPE(ITYP)
         CALL Q2JOB(6,'INPUT ERROR IN BASIS',' ',0)
      ENDIF
      IF (NCORB(ITYP).LT.0.OR.NCORB(ITYP).GT.NCONTR) THEN
         if (mpirnk.eq.0)
     &   PRINT*,"WRONG NUMBER OF DEL. FUNCTIONS IN INPUT ",TYPE(ITYP)
         CALL Q2JOB(6,'INPUT ERROR IN BASIS',' ',0)
      ENDIF
C     Number of functions to be used in integral evaluation
      NCPRIM(ITYP) = NPRIM - NCPRIM(ITYP)
      NCORB(ITYP)  = NCONTR - NCORB(ITYP)
      NTYP = NTYP + 1
      KTYPE(NTYP) = ITYP
      MAXTYP = MAX0(MAXTYP,ITYP)
      MINTYP = MIN0(MINTYP,ITYP)
      NTYPE(NTYP) = NPRIM
      NORB (NTYP) = NCONTR
      JND=KND
      IND=JND+1
      KND=KND+NPRIM*NCONTR
      IF (KND.GT.N15) CALL Q2JOB(3,'BASIS_IOPT_3','N15',KND)
      READ (10,FORMATE,END=11060) (ALPHA(JND),JND = IND,IND+NPRIM-1)
      READ (10,FORMATC,END=11070) (COEFF(JJND),
     &                             JJND=IND,IND+NPRIM*NCONTR-1)
      JND = IND - 1
      DO 360 JM = 1,NPRIM
      JND = JND + 1
      DO 350 J = 1, NCONTR
      JJND = JND + (J - 1) * NPRIM
      ALPHA (JJND) = ALPHA (JND)
      NORBIT(J)=NPRIM
  350 CONTINUE
  360 CONTINUE
      CALL SETINDA(NUC,KND,NPRIM*NCONTR,INDNC,LND,NQV,NEQCOM,
     $ITYP,NCONTR,IC,NCORB(ITYP),NCPRIM(ITYP))
      GOTO 301
  390 CONTINUE
      CLOSE (10)
C     -----------------------
      ELSEIF (IOPT.EQ.4) THEN
C     -----------------------
      KIN='UNI'
      IF (BASFIL(9:11).EQ.'SUB') KIN='SUB'
      REWIND 55
  345 READ (55,'(A19,10X,I4)',END=11111) LINE,NUCN
      IF ((LINE.NE.'BASIS SET FROM NUC ').OR.
     &   (NUCN.NE.NUC)) GOTO 345
      READ (55,1000,END=11000) (TYPE(I),I=1,N5)
C
C     ------------------------------------------------------------------
C     CHECK FOR INVALID TYPES AND DETERMINE TYPENUMBER AND NUMB.OF TYPES
C     ------------------------------------------------------------------
C
      DO 719 I = 1,N5+1
 719  KTYPE(I) = 0
      DO 720 I = 1,N5
      TYP = TYPE(I)
      DO 710 J = 0,N5
      IF (TYP.EQ.TYPES(J)) THEN
      IF (J.EQ.0) GOTO 730
      KTYPE(I)=J
      GOTO 720
      ENDIF
  710 CONTINUE
  720 CONTINUE
  730 NTYP = I - 1
      EXIST=.FALSE.
      DO 735 J=1,NTYP
  735    IF (KTYPE(J)+1.GT.N5) EXIST=.TRUE.
      IF (.NOT.EXIST) THEN
         DO 731 J=1,NTYP
	    KLTYPE(J,1)=KTYPE(J)
	    KLTYPE(J,2)=0    
	    IF (KTYPE(J).NE.1) KLTYPE(J,2)=KTYPE(J)-1
	    KLTYPE(J,3)=KTYPE(J)+1
  731    CONTINUE
	 NKLTYP=0
	 DO 732 J=1,NTYP
	    EXISTL=.FALSE.
	    EXISTU=.FALSE.
            IF (KLTYPE(J,2).EQ.0) EXISTL=.TRUE.
   	    DO 733 K=1,NKLTYP
	       EXISTU=.FALSE.
	       IF ((KLTYPE(J,2).EQ.0).OR.(KLTYPE(J,2).EQ.KTYPE(K)))
     $            EXISTL=.TRUE.
               IF (KLTYPE(J,3).EQ.KTYPE(K)) EXISTU=.TRUE.
  733       CONTINUE
	 IF (.NOT.EXISTL) THEN
	    NKLTYP=NKLTYP+1
	    KTYPE(NKLTYP)=KLTYPE(J,2)
	 ENDIF
	 IF (.NOT.EXISTU) THEN
	    NKLTYP=NKLTYP+1
            KTYPE(NKLTYP)=KLTYPE(J,3)
	 ENDIF
  732    CONTINUE
  745    EXISTU=.FALSE.
	 DO 734 J=1,NKLTYP
	    IF (KTYPE(J).GT.KTYPE(J+1).AND.(KTYPE(J+1).NE.0)) THEN
  	       EXISTU=.TRUE.
	       IDUM=KTYPE(J)
	       KTYPE(J)=KTYPE(J+1)
	       KTYPE(J+1)=IDUM
	    ENDIF
  734    CONTINUE
         IF (EXISTU) GOTO 745
	 DO 736 J=1,NKLTYP
  736      IF (KTYPE(J).GT.0) TYPE(J)=TYPES(KTYPE(J))
      ELSE
	 CALL Q2JOB(2,'I- OR HIGHER TYPE OF FUNCTION WANTED',' ',0)
      ENDIF
      DO 740 J=1,NKLTYP 
      MAXTYP=MAX0(MAXTYP,KTYPE(J))
      MINTYP=MIN0(MINTYP,KTYPE(J))
  740 CONTINUE
      DO 739 L=1,N5
  739    NTYPE(I)=0
      READ (55,1010,END=11010) (NTYPE(I),I=1,NTYP),(NCPRIM(I),I=1,NTYP)
      DO I = 1, NTYP
         IF (NCPRIM(I).NE.0) THEN
            if (mpirnk.eq.0)
     &      PRINT*, "SORRY : DELETION OPTION NOT AVAILABLE WITH KINETIC"
            CALL Q2JOB(6,'HIT UNIMPLEMENTED FEATURE',' ',0)
         ENDIF
      ENDDO
      IXPL=0
      DO 742 L=1,NTYP
  742    IXPL=IXPL+NTYPE(L)
      IF (IXPL.GT.N14) CALL Q2JOB(3,'BASIS_IOPT_4','N14',IXPL)
      DO 743 L=1,IXPL
  743    READ(55,*,END=11030) ALP(L)
      DO 770 L = 1 , NKLTYP
	 KINTYPE=0
	 DO 771 M=1,NTYP
	    IF (KLTYPE(M,3).EQ.KTYPE(L)) KINTYPE=KINTYPE+NTYPE(M)
	    IF (KLTYPE(M,2).EQ.KTYPE(L).AND.(KIN.EQ.'UNI'.OR.
     $         KINTYPE.EQ.0)) KINTYPE=KINTYPE+NTYPE(M)
  771    CONTINUE
      IF (KINTYPE.GT.N17) CALL Q2JOB(3,'BASIS_IOPT_4','N17',
     &   KINTYPE)
      NORB (L) = KINTYPE
      IF (NORB(L).GT.N16) CALL Q2JOB(3,'BASIS_IOPT_4','N16',NORB(L))
      JND = KND
      IND = JND + 1
      KND = KND + KINTYPE**2
      IF (KND.GT.N15) CALL Q2JOB(3,'BASIS_IOPT_4','N15',KND)
      IF (KTYPE(L).EQ.1) THEN 
	 IDUM=0
	 JDUM=0
	 DO 772 M=1,NTYP
  772    IF (KLTYPE(M,2).EQ.1) IDUM=M
	 DO 773 M=1,IDUM-1
  773    JDUM=JDUM+NTYPE(M)
         DO 761 J = 1, NTYPE(M)
         JND = JND + 1
         ALPHA (JND)=ALP(J+JDUM) 
         COEFF (JND) = 0.0D0
         DO 751 JJ = 1, NORB (L) - 1
         JJND = JND + JJ * KINTYPE
         ALPHA (JJND) = ALPHA (JND)
         COEFF (JJND) = 0.0D0
  751    CONTINUE
  761    CONTINUE
      ELSE IF (KTYPE(L).EQ.MAXTYP) THEN
	 IDUM=0
	 JDUM=0
	 DO 774 M=1,NTYP
  774    IF (KLTYPE(M,3).EQ.MAXTYP) IDUM=M
	 DO 775 M=1,IDUM-1
  775    JDUM=JDUM+NTYPE(M)
	 DO 762 J=1,NTYPE(M)
	 JND=JND+1
	 ALPHA(JND)=ALP(J+JDUM)
	 COEFF (JND)=0.0D0
         DO 752 JJ = 1, NORB (L) - 1
         JJND = JND + JJ * KINTYPE
         ALPHA (JJND) = ALPHA (JND)
         COEFF (JJND) = 0.0D0
  752    CONTINUE
  762    CONTINUE
      ELSE IF (KTYPE(L).LT.MAXTYP.AND.KTYPE(L).GT.MINTYP) THEN
	 IDUM=0
	 JDUM=0
	 DO 776 M=1,NTYP
  776    IF (KLTYPE(M,3).EQ.KTYPE(L)) IDUM=M
	 DO 777 M=1,IDUM-1
  777    JDUM=JDUM+NTYPE(M)
         IF (IDUM.EQ.0) GOTO 784
	 DO 763 J=1,NTYPE(M)
	 JND=JND+1
	 ALPHA(JND)=ALP(JDUM+J)
         COEFF(JND)=0.0D0
	 DO 753 JJ = 1, NORB (L) - 1 
	 JJND = JND + JJ * KINTYPE
	 ALPHA (JJND) = ALPHA (JND)
	 COEFF (JJND) = 0.0D0
  753    CONTINUE
  763    CONTINUE
  784    IF (KIN.EQ.'UNI') THEN
	    IDUM=0
	    JDUM=0
	    DO 778 M=1,NTYP
  778       IF (KLTYPE(M,2).EQ.KTYPE(L)) IDUM=M
	    DO 779 M=1,IDUM-1
  779       JDUM=JDUM+NTYPE(M)
            IF (IDUM.EQ.0) GOTO 781
	    DO 764 J=1,NTYPE(M)
	    JND=JND+1
	    ALPHA(JND)=ALP(J+JDUM)
	    COEFF(JND)=0.0D0
            DO 754 JJ = 1, NORB (L) - 1
            JJND = JND + JJ * KINTYPE
            ALPHA (JJND) = ALPHA (JND)
            COEFF (JJND) = 0.0D0
  754       CONTINUE
  764       CONTINUE
  781       CONTINUE
	 ENDIF
      ENDIF
      DO 765 J = 1, NORB(L)
      JJND = IND + (J  - 1)  * (KINTYPE + 1)
      NORBIT(J)=KINTYPE
      COEFF (JJND) = 1.0D0
  765 CONTINUE
      CALL SETINDA(NUC,KND,KINTYPE*NORB(L),INDNC,LND,NQV,NEQCOM,
     $KTYPE(L),NORB(L),IC,NORB(L),KINTYPE)
  770 CONTINUE
C     -----------------------
      ELSE
C     -----------------------
C     WHEN (IOPT.GT.4)
C     WE DO NOTHING
C     -----
      ENDIF
C     -----
C
      RETURN
 1000 FORMAT(8(3X,A))
 1010 FORMAT(20I4)
 1011 FORMAT(4I4,F6.1,F20.5)
 1020 FORMAT(F16.8,5F11.8)
 1030 FORMAT(16X,5F11.8)
 1040 FORMAT (A80/A10,A8,2(A10,I4),F6.1,F20.5)
11000 CALL Q2JOB(1,'--L-TYPE IDENTIFIERS',' ',0)
11010 CALL Q2JOB(1,'--NUMB.OF PRIMITIVES',' ',0)
11020 CALL Q2JOB(1,'--NUMB.OF CONTRACTED',' ',0)
11030 CALL Q2JOB(1,'--EXPONENTS',' ',0)
11040 CALL Q2JOB(1,'--EXPONENT,COEFFICIENT',' ',0)
11050 CALL Q2JOB(1,BASFIL(1:10)//' EMPTY',' ',0)
11060 CALL Q2JOB(1,'--EXPONENTS ON '//BASFIL(1:10),' ',0)
11070 CALL Q2JOB(1,'--COEFFICIENTS ON '//BASFIL(1:10),' ',0)
11111 CALL Q2JOB(1,'--LARGE COMPONENT BASIS IS CONTRACTED',' ',0)
      END
