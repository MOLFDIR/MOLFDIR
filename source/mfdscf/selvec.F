C
C     ===============================================
      SUBROUTINE SELVEC(IRP,DC,DO,DOC,NF,FCR,FCI,FOR,FOI,FOCR,FOCI)
C     ===============================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     SELECT AND ORHOGONALIZE THE OCCUPIED VECTORS
C
C     IF SELECT: SELECT BY MAXIMUM OVERLAP WITH PREV ITERATION
C     ELSE IF OVLSLCT: USE FILE SELVECF FOR SELECTION (BY CHARACTER)
C                      (AT THIS MOMENT, ONLY THE LARGE COMPONENT PART
C                       OF THE VECTORS ARE CONSIDERED)
C     ELSE: SELECT BY COUNTING
C
C
C     ORTHOGONALIZATION: S = <A|B>
C                                  *
C                        A' = A - S   B
C
C                        <A'|A'> = 1.0 - RE(S)^2 - IM(S)^2
C
C
C     THE DATA IN FCR, FCI, FOR AND FOI MIGHT BE ALTERED DO TO THE
C     ORTHONORMALIZATION STEP
C
C     THE SELECTED EIGENVECTORS AND EIGENVALUES WILL BE RETURNED
C     IN DC, FCR AND FCI (FIRST THE CLOSED SHELL VECTORS, FOLL0WED
C     BY THE OPEN SHELL VECTORS, FOLLOWED BY THE VIRTUALS)
C
      INCLUDE 'paracom.inc'
      INCLUDE 'general.inc'
      INCLUDE 'ossel.inc'
C
      REAL*8 FCR(NF,*),FCI(NF,*),FOR(NF,*),FOI(NF,*),DC(*),DO(*)
      REAL*8 FOCR(NF,*),FOCI(NF,*), DOC(*)
C
      REAL*8 CHC(N28),CHO(N28,2),CVR(N20)
      REAL*8 QLLR(N28,N28),QLLI(N28,N28)
      REAL*8 FR(N20,N28),FI(N20,N28),D(N28),GR(N20,N28),GI(N20,N28)
      REAL*8 GCR(N20,N28),GCI(N20,N28)
      INTEGER IV(N28)
      LOGICAL USEDCL(N20),USEDOP(N20,2),OS,NOP1,NOP2
      CHARACTER CARD*80
C
      NLR=NBLR(IRP)
      NSR=NBSR(IRP)
      NTR=NLR+NSR
      NBASL=NBSIM1(IRP)
      NBASS=NBSIM2(IRP)
      NBAS=NBASL+NBASS
      IF (NBAS.GT.N20) CALL Q2JOB(3,'SELVECF','N20',NBAS)
      IF (NLR.GT.N28) CALL Q2JOB(3,'SELVECF','N28',NLR)
      IF (NBASL.GT.N28) CALL Q2JOB(3,'SELVECF','N28',NBASL)
C
      DO 204 I=1,N20
         DO 207 J=1,N28
            FR(I,J)=0.0D0
            FI(I,J)=0.0D0
            GR(I,J)=0.0D0
            GI(I,J)=0.0D0
            GCR(I,J)=0.0D0
            GCI(I,J)=0.0D0
 207     CONTINUE
 204  CONTINUE
C
      DO 205 I=1,NLR
        USEDOP(I,1)=.FALSE.
	USEDOP(I,2)=.FALSE.
        USEDCL(I)=.FALSE.
 205    CONTINUE
C
      IF (HARDSEL) THEN
	 WRITE(*,*) 
	 WRITE(*,*) 'SORTING THE SPINORS THE HARD WAY'
	 WRITE(*,*)
C
C     ---------------------
C     REORDERING OF SPINORS
C     DOING IT THE HARD WAY
C     ---------------------
C
         DO 206 I=1,NCL(IRP)+NOP(IRP,1)+NOP(IRP,2)
            II = IVVAL(I,IRP)
            IV(I) = II
            USEDCL(II) = .TRUE.
            IF (NOP(IRP,1).GT.0) USEDOP(II,1) = .TRUE.
            IF (NOP(IRP,2).GT.0) USEDOP(II,2) = .TRUE.
 206     CONTINUE
         GOTO 499
      ENDIF
C
      IF (SELECT) THEN
         IF (KCYCLS.EQ.0) THEN
            IF (OVLSLCT) GOTO 10
         ELSE
            GOTO 11
         ENDIF
      ENDIF
      IF (OVLSLCT) GOTO 10
C
C     ----------------
C     NORMAL SELECTION
C     ----------------
C
      DO 100 I=1,NCL(IRP)
        IV(I)=I
        USEDCL(I)=.TRUE.
        IF (NOP(IRP,1).EQ.0) GOTO 101
        IOMAX = 0
        OMAX = 0.D0
        DO 110 J = 1, NLR
           IF (USEDOP(J,1)) GOTO 110
           SUMR=XDOT(FCR(1,I),FOR(1,J),NTR)+XDOT(FCI(1,I),FOI(1,J),NTR)
           SUMI=XDOT(FCR(1,I),FOI(1,J),NTR)-XDOT(FCI(1,I),FOR(1,J),NTR)
           OVLAP=SUMR*SUMR+SUMI*SUMI
           IF (OVLAP.GT.OMAX) THEN
              IOMAX = J
              OMAX = OVLAP
           ENDIF
 110       CONTINUE
        USEDOP(IOMAX,1) = .TRUE.
 101    IF (NOP(IRP,2).EQ.0) GOTO 100
	IOMAX = 0
	OMAX = 0.D0
	DO 711 J = 1, NLR
	   IF (USEDOP(J,2)) GOTO 711
           SUMR=XDOT(FCR(1,I),FOCR(1,J),NTR)+XDOT(FCI(1,I),
     +          FOCI(1,J),NTR)
           SUMI=XDOT(FCR(1,I),FOCI(1,J),NTR)-XDOT(FCI(1,I),
     +          FOCR(1,J),NTR)
           OVLAP=SUMR*SUMR+SUMI*SUMI
  	   IF (OVLAP.GT.OMAX) THEN
	      IOMAX = J
	      OMAX = OVLAP
	   ENDIF
 711       CONTINUE
        USEDOP(IOMAX,2) = .TRUE.
 100    CONTINUE
C
      IF (NOP(IRP,1).NE.0) THEN
         MO = NCL(IRP)
         DO 120 I = 1, NLR
           IF (.NOT.USEDOP(I,1)) THEN
              MO = MO + 1
              IV(MO) = I
              USEDOP(I,1) = .TRUE.
              ICMAX = 0
              CMAX = 0.D0
              DO 111 J = 1, NLR
                 IF (USEDCL(J)) GOTO 111
                 SUMR = XDOT(FOR(1,I),FCR(1,J),NTR)
     &                + XDOT(FOI(1,I),FCI(1,J),NTR)
                 SUMI = XDOT(FOR(1,I),FCI(1,J),NTR)
     &                - XDOT(FOI(1,I),FCR(1,J),NTR)
                 OVLAP=SUMR*SUMR+SUMI*SUMI
                 IF (OVLAP.GT.CMAX) THEN
                    ICMAX = J
                    CMAX = OVLAP
                 ENDIF
 111             CONTINUE
                 USEDCL(ICMAX) = .TRUE.
 112          IF (MO.GE.NOC(IRP)) GOTO 130
           ENDIF
 120     CONTINUE
         IF (MO.NE.NOC(IRP)) CALL Q2JOB(6,
     &      'SELVECF: Error in OPEN SHELL 1 SELECTION',' ',1)
 130     CONTINUE
      ENDIF
      IF (NOP(IRP,2).NE.0) THEN
         MO = NCL(IRP)
         DO 121 I = 1, NLR
   	   IF (.NOT.USEDOP(I,2)) THEN
	      MO = MO + 1
	      IV(MO) = I
	      USEDOP(I,2) = .TRUE.
	      ICMAX = 0
	      CMAX = 0.D0
	      DO 114 J = 1, NLR
	         IF (USEDCL(J)) GOTO 114
                 SUMR = XDOT(FOCR(1,I),FCR(1,J),NTR)
     &                + XDOT(FOCI(1,I),FCI(1,J),NTR)
                 SUMI = XDOT(FOCR(1,I),FCI(1,J),NTR)
     &                - XDOT(FOCI(1,I),FCR(1,J),NTR)
 	 	 OVLAP=SUMR*SUMR+SUMI*SUMI
		 IF (OVLAP.GT.CMAX) THEN
		    ICMAX = J
		    CMAX = OVLAP
		 ENDIF
 114             CONTINUE
   	         USEDCL(ICMAX) = .TRUE.
 115          IF (MO.GE.NOC(IRP)) GOTO 131
           ENDIF
 121     CONTINUE
         IF (MO.NE.NOC(IRP)) CALL Q2JOB(6,
     &      'SELVECF: Error in OPEN SHELL 2 SELECTION',' ',1)
 131     CONTINUE
      ENDIF
C
      GOTO 499
C
C     -----------------------------
C     SELECT BY CHARACTER (OVLSLCT)
C     -----------------------------
C
 10   CONTINUE
C
C     ---------------------
C     POSITION FILE SELVECF
C     ---------------------
C
      REWIND(LVECF)
      READ(LVECF,1000) CARD
 1000 FORMAT(A)
C
      DO 208 I=1,IRP-1
        IF (IAVIRP(I).NE.I) GOTO 208
        IF (IAVIRP(IRP).EQ.I) GOTO 208
        READ(LVECF,1000) CARD
        DO 209 J=1,NOC(I)
          READ(LVECF,1000) CARD
          IBAS=0
 733      READ(LVECF,1010) IDUM,JDUM
          IBAS=IBAS+IDUM
          IF (IBAS.NE.NBSIM1(I)) GOTO 733
 1010     FORMAT(2I4)
 209      CONTINUE
 208    CONTINUE
      READ(LVECF,1000) CARD
C
C     ------------------------------------
C     TRANSFORM VECTORS TO NON-ORTHO BASIS
C     ------------------------------------
C
      IREC=(IRP-1)*NREC
      READ(MFDOEMR,REC=IREC+8)
     +    ((QLLR(I,J),I=1,NBASL),J=1,NLR)
      READ(MFDOEMI,REC=IREC+8)
     +    ((QLLI(I,J),I=1,NBASL),J=1,NLR)
      DO 203 MO=1,NLR
        CALL HMV (NBASL,NLR,N28,QLLR,QLLI,FCR(1,MO),FCI(1,MO),
     +            FR(1,MO),FI(1,MO))
        CALL HMV (NBASL,NLR,N28,QLLR,QLLI,FOR(1,MO),FOI(1,MO),
     +            GR(1,MO),GI(1,MO))
        CALL HMV (NBASL,NLR,N28,QLLR,QLLI,FOCR(1,MO),FOCI(1,MO),
     +            GCR(1,MO),GCI(1,MO))
 203    CONTINUE
C
      NOPP=0
      NCPP=0
      NCLP=0
C
C     ------------------------------------------
C     SELECT VECTOR FOR EACH VECTOR FROM SELVECF
C     ------------------------------------------
C
      DO 200 MO=1,NOC(IRP)
        READ(LVECF,1015) E,O,C
 1015 FORMAT(10X,G20.10,5X,G15.10,8X,G20.10)
        IF (O.NE.1.0D0) THEN
          OS=.TRUE.
        ELSE
          OS=.FALSE.
        ENDIF
        IBAS=0
 201    READ(LVECF,1010) NDAT,IDAT
        DO 202 I=1,NDAT
          CVR(I+IBAS)=IDAT
 202      CONTINUE
        IBAS=IBAS+NDAT
        IF (IBAS.LT.NBASL) GOTO 201
        DO 210 I=1,NLR
          CHC(I)=0.0D0
          CHO(I,1)=0.0D0
	  CHO(I,2)=0.0D0
          DO 240 J=1,NBASL
            R=CVR(J)*GCR(J,I)
            A=CVR(J)*GCI(J,I)
            CHO(I,2)=CHO(I,2)+R*R+A*A
	    R=CVR(J)*GR(J,I)
	    A=CVR(J)*GI(J,I)
	    CHO(I,1)=CHO(I,1)+R*R+A*A
            R=CVR(J)*FR(J,I)
            A=CVR(J)*FI(J,I)
            CHC(I)=CHC(I)+R*R+A*A
 240        CONTINUE
 210      CONTINUE
C
        CHCMX=0.0D0
        CHOMX=0.0D0
	CHOCMX=0.0D0
        ICHC=0
        ICHO=0
	ICHOC=0
        NOP1=(NOP(IRP,1).GT.0).AND.OS
        NOP2=(NOP(IRP,2).GT.0).AND.OS
C
        DO 220 I=NLR,1,-1
          IF ((CHC(I).GT.SMALL).AND.(.NOT.USEDCL(I)).AND.(.NOT.OS)) THEN
            CHCMX=CHC(I)
            ICHC=I
          ENDIF
          IF (NOP1.AND.(CHO(I,1).GT.SMALL).AND.(.NOT.USEDOP(I,1))) THEN
            CHOMX=CHO(I,1)
            ICHO=I
          ENDIF
          IF (NOP2.AND.(CHO(I,2).GT.SMALL).AND.(.NOT.USEDOP(I,2))) THEN
            CHOCMX=CHO(I,2)
            ICHOC=I
          ENDIF
 220      CONTINUE
C
C
        IF (OS) THEN
C         IF ((ICHO.GT.0).OR.(ICHOC.GT.0)) THEN
          IF (ICHO.GT.0) THEN
              NOPP=NOPP+1
              IV(NCL(IRP)+NOPP)=ICHO
              USEDOP(ICHO,1)=.TRUE.
              ICMAX = 0
              CMAX = 0.D0
              DO 221 J = 1, NLR
                 IF (USEDCL(J)) GOTO 221
                 SUMR = XDOT(FOR(1,ICHO),FCR(1,J),NTR)
     &                + XDOT(FOI(1,ICHO),FCI(1,J),NTR)
                 SUMI = XDOT(FOR(1,ICHO),FCI(1,J),NTR)
     &                - XDOT(FOI(1,ICHO),FCR(1,J),NTR)
                 OVLAP=SUMR*SUMR+SUMI*SUMI
                 IF (OVLAP.GT.CMAX) THEN
                    ICMAX = J
                    CMAX = OVLAP
                 ENDIF
 221          CONTINUE
              USEDCL(ICMAX) = .TRUE.
          ELSE
            IF (ICHOC.GT.0) THEN
              NCPP=NCPP+1
              IV(NCL(IRP)+NOP(IRP,1)+NCPP)=ICHOC
              USEDOP(ICHOC,2)=.TRUE.
              ICMAX = 0
              CMAX = 0.D0
              DO 222 J = 1, NLR
                 IF (USEDCL(J)) GOTO 222
                 SUMR = XDOT(FOCR(1,ICHOC),FCR(1,J),NTR)
     &                + XDOT(FOCI(1,ICHOC),FCI(1,J),NTR)
                 SUMI = XDOT(FOCR(1,ICHOC),FCI(1,J),NTR)
     &                - XDOT(FOCI(1,ICHOC),FCR(1,J),NTR)
                 OVLAP=SUMR*SUMR+SUMI*SUMI
                 IF (OVLAP.GT.CMAX) THEN
                    ICMAX = J
                    CMAX = OVLAP
                 ENDIF
 222          CONTINUE
              USEDCL(ICMAX) = .TRUE.
          ELSE
            PRINT*,'WARNING: NO OS VECT SELECTED,IRP=',IRP,',MO=',MO
          ENDIF
         ENDIF
        ELSE
          IF (ICHC.GT.0) THEN
              NCLP=NCLP+1
              IV(NCLP)=ICHC
              USEDCL(ICHC)=.TRUE.
              IF (NOP(IRP,1).EQ.0) GOTO 224
              IOMAX = 0
              OMAX = 0.D0
              DO 223 J = 1, NLR
                 IF (USEDOP(J,1)) GOTO 223
                 SUMR = XDOT(FCR(1,ICHC),FOR(1,J),NTR)
     &                + XDOT(FCI(1,ICHC),FOI(1,J),NTR)
                 SUMI = XDOT(FCR(1,ICHC),FOI(1,J),NTR)
     &                - XDOT(FCI(1,ICHC),FOR(1,J),NTR)
                 OVLAP = SUMR*SUMR+SUMI*SUMI
                 IF (OVLAP.GT.OMAX) THEN
                    IOMAX = J
                    OMAX = OVLAP
                 ENDIF
 223          CONTINUE
              USEDOP(IOMAX,1) = .TRUE.
 224          IF (NOP(IRP,2).EQ.0) GOTO 226
              IOMAX = 0
              OMAX = 0.D0
              DO 225 J = 1, NLR
                 IF (USEDOP(J,2)) GOTO 225
                 SUMR = XDOT(FCR(1,ICHC),FOCR(1,J),NTR)
     &                + XDOT(FCI(1,ICHC),FOCI(1,J),NTR)
                 SUMI = XDOT(FCR(1,ICHC),FOCI(1,J),NTR)
     &                - XDOT(FCI(1,ICHC),FOCR(1,J),NTR)
                 OVLAP = SUMR*SUMR+SUMI*SUMI
                 IF (OVLAP.GT.OMAX) THEN
                    IOMAX = J
                    OMAX = OVLAP
                 ENDIF
 225          CONTINUE
              USEDOP(IOMAX,2) = .TRUE.
 226          CONTINUE
          ELSE
            PRINT*,'WARNING: NO CL VECT SELECTED,IRP=',IRP,',MO=',MO
          ENDIF
        ENDIF
C
 200    CONTINUE
      IF (NCLP.NE.NCL(IRP)) 
     &   CALL Q2JOB(6,'SELVECF: NCLP <> NCL',' ',1)
      IF (NOP1.AND.NOPP.NE.NOP(IRP,1)) 
     &   CALL Q2JOB(6,'SELVECF: NOPP <> NOP 1',' ',1)
      IF (NOP2.AND.NCPP.NE.NOP(IRP,2)) 
     &   CALL Q2JOB(6,'SELVECF: NCPP <> NOP 2',' ',1)
      GOTO 499
C
C     -----------------------------------------
C     SELECT BY OVERLAP WITH PREVIOUS ITERATION
C     -----------------------------------------
C
 11   CONTINUE
      DO 300 MO=1,NOC(IRP)
        READ(MFDVECB,REC=INDNEV(IRP)+NOCT+2+MO) RDUM,RDUM,RDUM,RDUM,
     +      (GR(I,1),GI(I,1),RDUM,RDUM,I=1,NBAS)
        DO 310 NO=1,NLR
          IF (USEDCL(NO)) THEN
            CHC(NO)=0.0D0
          ELSE
            SUMR=XDOT(FCR(1,NO),GR(1,1),NTR)+XDOT(FCI(1,NO),GI(1,1),NTR)
            SUMI=XDOT(FCR(1,NO),GI(1,1),NTR)-XDOT(FCI(1,NO),GR(1,1),NTR)
            CHC(NO)=SUMR*SUMR+SUMI*SUMI
          ENDIF
          IF (USEDOP(NO,1)) THEN
            CHO(NO,1)=0.0D0
          ELSE
            SUMR=XDOT(FOR(1,NO),GR(1,1),NTR)+XDOT(FOI(1,NO),GI(1,1),NTR)
            SUMI=XDOT(FOR(1,NO),GI(1,1),NTR)-XDOT(FOI(1,NO),GR(1,1),NTR)
            CHO(NO,1)=SUMR*SUMR+SUMI*SUMI
          ENDIF
	  IF (USEDOP(NO,2)) THEN
   	    CHO(NO,2)=0.0D0
	  ELSE
            SUMR=XDOT(FOCR(1,NO),GR(1,1),NTR)+
     &           XDOT(FOCI(1,NO),GI(1,1),NTR)
            SUMI=XDOT(FOCR(1,NO),GI(1,1),NTR)-
     &           XDOT(FOCI(1,NO),GR(1,1),NTR)
   	    CHO(NO,2)=SUMR*SUMR+SUMI*SUMI
	  ENDIF
 310      CONTINUE
        IOMAX=0
	IOCMAX=0
        ICMAX=0
        OMAX=0.0D0
	OCMAX=0.0D0
        CMAX=0.0D0
        DO 320 I=1,NLR
          IF (CHC(I).GT.CMAX) THEN
            ICMAX=I
            CMAX=CHC(I)
          ENDIF
          IF (CHO(I,1).GT.OMAX) THEN
            IOMAX=I
            OMAX=CHO(I,1)
          ENDIF
          IF (CHO(I,2).GT.OCMAX) THEN
            IOCMAX=I
            OCMAX=CHO(I,2)
          ENDIF
 320      CONTINUE
        IF (MO.GT.(NCL(IRP)+NOP(IRP,1))) THEN
          IF (IOCMAX.EQ.0) CALL Q2JOB(6,
     &       'SELVECF: NO OPEN SHELL 2 VECTOR SELECTED',' ',1)
          IV(MO)=IOCMAX
          USEDOP(IOCMAX,2)=.TRUE.
          USEDCL(ICMAX)=.TRUE.
        ELSEIF (MO.GT.NCL(IRP)) THEN
	  IF (IOMAX.EQ.0) CALL Q2JOB(6,
     &       'SELVECF: NO OPEN SHELL 1 VECTOR SELECTED',' ',1)
	  IV(MO)=IOMAX
	  USEDOP(IOMAX,1)=.TRUE.
	  USEDCL(ICMAX)=.TRUE.
          ELSE
            IF (ICMAX.EQ.0) CALL Q2JOB(6,
     &       'SELVECF: NO CLOSED SHELL 2 VECTOR SELECTED',' ',1)
            IV(MO)=ICMAX
            USEDCL(ICMAX)=.TRUE.
	    IF (NOP(IRP,1).GT.0) USEDOP(IOMAX,1)=.TRUE.
	    IF (NOP(IRP,2).GT.0) USEDOP(IOCMAX,2)=.TRUE.
        ENDIF
 300    CONTINUE
      GOTO 499
C
C     ----------------------------------------------
C     REORDER AND ORTHOGONALIZE THE SELECTED VECTORS
C     ----------------------------------------------
C
 499  CONTINUE
C
      IF (DEBUG) THEN
        DO 570 MO=1,NOC(IRP)
          WRITE(6,1100) IRP,MO,IV(MO)
 570      CONTINUE
 1100   FORMAT (' IRP = ',I4,', MO = ',I4,', IV(MO) = ',I4)
      ENDIF
C
      DO 500 MO=1,NCL(IRP)
        IF (NOORTHO) GOTO 501
        DO 510 NO=1,MO-1
          SUMR=XDOT(FCR(1,IV(MO)),FR(1,NO),NTR) +
     +         XDOT(FCI(1,IV(MO)),FI(1,NO),NTR)
          SUMI=XDOT(FCR(1,IV(MO)),FI(1,NO),NTR) -
     +         XDOT(FCI(1,IV(MO)),FR(1,NO),NTR)
#if defined (BIT64)
          RNORM=1.0/SQRT(1.0-SUMR*SUMR-SUMI*SUMI)
#else
          RNORM=1.0D0/DSQRT(1.0D0-SUMR*SUMR-SUMI*SUMI)
#endif
          DO 520 I=1,NTR
            FCR(I,IV(MO))=(FCR(I,IV(MO))-
     +                     SUMR*FR(I,NO)+SUMI*FI(I,NO))*RNORM
            FCI(I,IV(MO))=(FCI(I,IV(MO))-
     +                     SUMR*FI(I,NO)+SUMI*FR(I,NO))*RNORM
 520        CONTINUE
 510      CONTINUE
 501    DO 530 I=1,NTR
          FR(I,MO)=FCR(I,IV(MO))
          FI(I,MO)=FCI(I,IV(MO))
 530      CONTINUE
        D(MO)=DC(IV(MO))
 500    CONTINUE
      DO 505 MO=NCL(IRP)+1,NOC(IRP)-NOP(IRP,2)
        IF (NOORTHO) GOTO 506
        DO 515 NO=1,MO-1
          SUMR=XDOT(FOR(1,IV(MO)),FR(1,NO),NTR) +
     +         XDOT(FOI(1,IV(MO)),FI(1,NO),NTR)
          SUMI=XDOT(FOR(1,IV(MO)),FI(1,NO),NTR) -
     +         XDOT(FOI(1,IV(MO)),FR(1,NO),NTR)
#if defined (BIT64)
          RNORM=1.0/SQRT(1.0-SUMR*SUMR-SUMI*SUMI)
#else
          RNORM=1.0D0/DSQRT(1.0D0-SUMR*SUMR-SUMI*SUMI)
#endif
          DO 525 I=1,NTR
            FOR(I,IV(MO))=(FOR(I,IV(MO))-
     +                     SUMR*FR(I,NO)+SUMI*FI(I,NO))*RNORM
            FOI(I,IV(MO))=(FOI(I,IV(MO))-
     +                     SUMR*FI(I,NO)+SUMI*FR(I,NO))*RNORM
 525        CONTINUE
 515      CONTINUE
 506    DO 535 I=1,NTR
          FR(I,MO)=FOR(I,IV(MO))
          FI(I,MO)=FOI(I,IV(MO))
 535      CONTINUE
        D(MO)=DO(IV(MO))
 505    CONTINUE
      DO 605 MO=NCL(IRP)+NOP(IRP,1)+1,NOC(IRP)
        IF (NOORTHO) GOTO 606
        DO 615 NO=1,MO-1
          SUMR=XDOT(FOCR(1,IV(MO)),FR(1,NO),NTR) +
     +         XDOT(FOCI(1,IV(MO)),FI(1,NO),NTR)
          SUMI=XDOT(FOCR(1,IV(MO)),FI(1,NO),NTR) -
     +         XDOT(FOCI(1,IV(MO)),FR(1,NO),NTR)
#if defined (BIT64)
          RNORM=1.0/SQRT(1.0-SUMR*SUMR-SUMI*SUMI)
#else
          RNORM=1.0D0/DSQRT(1.0D0-SUMR*SUMR-SUMI*SUMI)
#endif
          DO 625 I=1,NTR
            FOCR(I,IV(MO))=(FOCR(I,IV(MO))-
     +                     SUMR*FR(I,NO)+SUMI*FI(I,NO))*RNORM
            FOCI(I,IV(MO))=(FOCI(I,IV(MO))-
     +                     SUMR*FI(I,NO)+SUMI*FR(I,NO))*RNORM
 625        CONTINUE
 615      CONTINUE
 606    DO 635 I=1,NTR
          FR(I,MO)=FOCR(I,IV(MO))
          FI(I,MO)=FOCI(I,IV(MO))
 635      CONTINUE
        D(MO)=DOC(IV(MO))
 605    CONTINUE
C
C     --------------------------------------------------------------
C     PUT VIRTUALS IN PLACE, IN AN OPEN SHELL CASE OSVIRT DETERMINES
C     WHICH VIRTUALS ARE USED. ORTHOGONALISATION ON THE OCCUPIED
C     ORBITALS IS DONE IN MFDSEIV (AFTER EXTRAPOLATIONS ETC.)
C     --------------------------------------------------------------
C
      MO = NOC (IRP)
      DO 650 IMOV = 1, NLR
      IF (OSVIRT.AND.(NOP(IRP,1)+NOP(IRP,2).NE.0)) THEN
	 IF (NOP(IRP,1).NE.0) THEN
           IF (.NOT.USEDOP(IMOV,1)) THEN
              MO = MO + 1
              DO 660 I=1,NTR
               FR(I,MO) = FOR(I,IMOV)
               FI(I,MO) = FOI(I,IMOV)
 660          CONTINUE
              D(MO) = DO(IMOV)
           ENDIF
         ELSEIF (NOP(IRP,2).NE.0) THEN
           IF (.NOT.USEDOP(IMOV,2)) THEN
              MO = MO + 1
              DO 661 I=1,NTR
               FR(I,MO) = FOCR(I,IMOV)
               FI(I,MO) = FOCI(I,IMOV)
 661          CONTINUE
              D(MO) = DOC(IMOV)
           ENDIF
         ENDIF
      ELSE
         IF (.NOT.USEDCL(IMOV)) THEN
            MO = MO + 1
            DO 670 I=1,NTR
             FR(I,MO) = FCR(I,IMOV)
             FI(I,MO) = FCI(I,IMOV)
 670        CONTINUE
            D(MO) = DC(IMOV)
         ENDIF
      ENDIF
 650  CONTINUE
      IF (MO.NE.NLR) PRINT*, 'WARNING IRP:',IRP,' LAST ',NLR-MO,
     &                         ' VIRTUALS ARE RUBBISH'
C
C     ------------------------------
C     PUT THE ORBITALS IN FCR AND DC
C     ------------------------------
C
      DO 750 MO=1,NLR
        DO 760 I=1,NTR
          FCR(I,MO)=FR(I,MO)
          FCI(I,MO)=FI(I,MO)
 760      CONTINUE
        DC(MO)=D(MO)
 750    CONTINUE
C
      RETURN
      END
