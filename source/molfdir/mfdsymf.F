C
C     ==================
      SUBROUTINE MFDSYMF
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
      COMMON/GROUPR/PROJOP(48,16),UU(2,2,48)
      DIMENSION FASE(16)
      DIMENSION CSYMA(N8*2),CSYMB(N8*2)
      EQUIVALENCE (CSYMORA,CSYMA),(CSYMORB,CSYMB)
      EQUIVALENCE (NEC(2),NEQCOM)
      COMPLEX*16 PROJOP,UU,SN
      COMPLEX*16 VPROD,FASE,FASEKR,CFA(N3),CFB(N3)
      COMPLEX*16 XNOR1,XNOR2,XOVLAP,CAFA(N3),CAFB(N3)
      COMPLEX*16 XSQRT
      REAL*8 RNORM1
      LOGICAL FRLCD
      INTEGER IFNINV(N6+1)
C
      FASEKR = (0.0,0.0)
C
      DO I = 1, 16
         FASE(I) = (0.0,0.0)
      ENDDO
C
      DO 1 I=1,N8
      CSYMORA(I)=(0.0,0.0)
      CSYMORB(I)=(0.0,0.0)
    1 CONTINUE
C     -----------------------------------------------------------------
C     SET UP INVERSE ARRAY OF IFN, WHICH GIVES POSITION OF FUNCTION ITY
C     IN ARRAY IFN
C     ------------------------------------------------------------------
      DO 11 ITY=1,N6
      IFNINV(IABS(IFN(ITY))) = ITY*ISIGN(1,IFN(ITY))
   11 CONTINUE
      NEQSYMF=0
      INDSOR(1)=0
      INDSC(1)=0
      IADR =0
      IMB=0
      DO 105 IC=1,2
      IMA=IMB+1
      IMB=NEC(IC)
      FRLCC=FRLC(IC)
      FRLCD=(.NOT.FRLC(3)).AND.(IC.EQ.2)
C     ---------------
      IF (FRLCC) THEN
C     ---------------
C
C     -----------------------------------------------------------------
C     ALL ATOMIC ORBITALS (S,PX,PY,PZ,X**2,Y**2,Z**2,...) ARE GROUPED
C     TOGETHER IN FIXED COMBINATIONS (F.E. X**2,Y**2,Z**2 ) SO FORMING
C     ATOMIC SYMMETRY GROUPS (ASG). THE NUMBER OF AO'S IN THE IT-TH
C     ASG IS LBASC(IT)
C     FROM THESE AO'S MAY BE CONSTRUCTED A NUMBER OF LINEARLY INDEPEN-
C     DENT CONTRACTED AO'S (CAO) (F.I. FROM X**2,Y**2,Z**2 ONLY TWO COM-
C     BINATIONS REMAIN). FOR THE IT-TH ASG THIS NUMBER IS LSYMC(IT)
C     THE COEFFICIENTS FOR THE CONTRACTION (F.I. +1.,-1.,0. FOR
C     X**2-Y**2) OF THE FIRST CAO IN THE IT-TH ASG START AT POSITION
C     INDYC(IT) IN THE LIST OF COEFFICIENTS Y(Z)LMC( ).
C     FOR THE INDIVIDUAL AO 'I' (S,PX,...) THE NUMBER OF THE ASG IT BE-
C     LONGS TO IS STORED IN INDYC(I)
C     -----------------------------------------------------------------
C
      IT=0
      ITT=0
      N=1
      NO=1
      INDYC(0)=1
      DO 2 I=1,N6
      IF(IFN(I).GT.0) ITT=ITT+1
      IF(IFN(I+1).LT.0) THEN
          N=N+1
      ELSE
          IT=IT+1
#if defined (BIT64)
          LSYMC(IT)=IFIX((SQRT(1.0+8.0*REAL(N))-1.0))/2
#else
          LSYMC(IT)=IDINT((DSQRT(1.0d0+8.0d0*DBLE(N))-1.0d0))/2
#endif
          IF (FRLCD) THEN
             LSYMC(IT)=N
             IF (N.EQ.6) LSYMC(IT)=5
          ENDIF
          LBASC(IT)=N
          IF(N.EQ.NO) THEN
            INDYC(IT)=INDYC(IT-1)
          ELSE
            INDYC(IT)=INDYC(IT-1)+LBASC(IT-1)*LSYMC(IT-1)
          ENDIF
          NO=N
          N=1
      ENDIF
      INDTYP(I)=ITT
    2 CONTINUE
C
C     ----
      ELSE
C     ----
C
C     --------------------------------------------------------------
C     NO GROUPS OF FIXED COMBINATIONS ARE FORCED AND NO COMBINATIONS
C     ARE REMOVED FROM THE BASIS OF THIS COMPONENT
C     --------------------------------------------------------------
C
      DO 4 I=1,N6
      LSYMC(I)=1
      LBASC(I)=1
      INDYC(I)=1
      INDTYP(I)=I
    4 CONTINUE
C
C     -----
      ENDIF
C     -----
C
C     ---------------------------------------------------------------
C     DETERMINE THE NUMBER OF MO'S THAT CAN BE FORMED BY COMBINING
C     THE EQUIVALENT AO'S ON THEIR RESPECTIVE CENTERS FOR EACH MOLE-
C     CULAR SYMMETRY GROUP (MSG).
C     AN MSG CONSISTS OF ALL THE AO'S IN AN ASG OF ALL EQUIVALENT
C     CENTERS IN THE MOLECULE.
C     THE NUMBER OF MO'S IN THE IM-TH MSG IS THE NUMBER OF CAO'S PER
C     AO IN THE ASG TIMES THE NUMBER OF AO'S IN THE MSG
C     A FACTOR OF TWO APPEARS HERE, TO ACCOUNT FOR THE SPIN
C     THE MO'S FORMED FROM THE CAO'S ARE CALLED CONTRACTED MO'S (CMO)
C     THE NUMBER OF THE FIRST CMO THAT CAN BE FORMED FROM THE CAO'S
C     THE IM-TH MSG IS STORED IN INDSC(IM)
C     ---------------------------------------------------------------
C
      DO 6 IM=IMA,IMB
      IND=INDCOM(IM)+1
      ITT=ITYPE(IND)
      IT=INDTYP(IABS(IFNINV(ITT)))
      IF (.NOT.FRLCC) IT=INDTYP(ITT)
      NBAS=INDCOM(IM+1)-INDCOM(IM)
      NSYMC(IM)=2*NBAS*LSYMC(IT)/LBASC(IT)
      IF (.NOT.FRLCC) NSYMC(IM)=2*NBAS
      INDSC(IM+1)=INDSC(IM)+NSYMC(IM)/2
    6 CONTINUE
C
C     ---------------------------------------------------------------
C     NOW WE ESTABLISHED A SET OF FUNCTIONS (CMO'S) WHICH CAN SERVE
C     AS A BASIS FROM WHICH PRIMITIVE SYMMETRY ADAPTED FUNCTIONS
C     (PSF) CAN BE FORMED, AND THEIR NUMBER EQUALS THE NUMBER OF
C     CMO'S (NOT THE NUMBER OF MO'S)
C     A PROJECTION OPERATOR FOR EACH SUBSPECIES OF A REPRESENTATION
C     IS ALLOWED TO OPERATE ON ALL CMO'S, TO FINF ALL REPRESENTATIONS
C     AND PSF'S SPANNED BY THIS BASIS.
C     ---------------------------------------------------------------
C
      DO 100 IM=IMA,IMB
      ISYMF=NEQSYMF+1
      IND=INDCOM(IM)+1
      KND=INDCOM(IM+1)
      NBAS=KND-IND+1
      IREP=1
      LSYMF=NEQSYMF+1
      IF (IADR+NSYMC(IM)*NBAS.GT.N8) CALL Q2JOB
     $(3,'P.MFDSYMF_CSYMORA & _CSYMORB','N8',IADR+NSYMC(IM)*NBAS)
C     ----------------------------------
C     LOOP OVER SYMMETRY REPRESENTATIONS
C     ----------------------------------
      DO 90 IR=1,NSYMRP
      ID=IDIMRP(IR)
C     -------------------------
C     LOOP OVER BASIS FUNCTIONS
C     -------------------------
      DO 70 IF=IND,KND
      ITY=ITYPE(IF)
      IT=INDTYP(IABS(IFNINV(ITY)))
      IF (.NOT.FRLCC) IT=INDTYP(ITY)
      ICY=ICENT(IF)
      IF (IFNINV(ITY).LT.0 .AND. FRLCC) GOTO 70
      IBC=LBASC(IT)-1
      ISC=LSYMC(IT)-1
      INDY=INDYC(IT) - (IBC+1)
C     ------------------------------------------
C     LOOP OVER CARTESIANS IN THE BASIS FUNCTION
C     ------------------------------------------
      DO 65 ICX=0,ISC
      INDY=INDY + (IBC+1)
C     --------------
C     LOOP OVER SPIN
C     --------------
      DO 60 IS=1,2
      DO 7 I=1,NBAS
      CFA(I)=(0.0,0.0)
      CFB(I)=(0.0,0.0)
      CAFA(I)=(0.0,0.0)
      CAFB(I)=(0.0,0.0)
    7 CONTINUE
      DO 15 IB=0,IBC
      IFF=IABS(IFN(IFNINV(ITY) + IB) )
      IF (.NOT.FRLCC) IFF=ITY
      DO 8 I=IND,KND
    8 IF (IFF.EQ.ITYPE(I) .AND. ICY.EQ.ICENT(I) ) GOTO 9
      if (mpirnk.eq.0)
     &WRITE(*,'(A,I3,A,I3)') ' CANNOT FIND CENTER ',ICY,
     +' WITH FUNCTION ',IFF
      CALL Q2JOB(6,'ERROR AT P.MFDSYMF_IFF',' ',0)
    9 IFF = I
      FAC=YLMC(INDY+IB)
      IF (IS.EQ.1) THEN
         CFA(IFF-IND+1) = YLMC(INDY+IB)
         IF (FRLCD) CFA(IFF-IND+1) = ZLMC(INDY+IB)
      ELSE
         CFB(IFF-IND+1) = YLMC(INDY+IB)
         IF (FRLCD) CFB(IFF-IND+1) = ZLMC(INDY+IB)
      ENDIF
   15 CONTINUE
C
C     ------------------------------------------------------------
C     WE USE THE FUNCTION (R+K) PSI TO START THE PROJECTION
C     R IS THE PHASE DEFINING OPERATOR, K IS THE KRAMERS' OPERATOR
C     FOR ALL GROUPS BUT D3 AND C3V WE MIGHT HAVE STARTED WITH
C     JUST A SCALAR FUNCTION MULTIPLIED WITH A OR B, BUT THIS WILL
C     LEAD TO MIXED KRAMERS' PAIRS IN C3V AND D3. 
C     ------------------------------------------------------------
C
C
      CALL XROTATE (IFASOP(1),ICOMOP,N7,UU,CFA,CFB,IND,NBAS,CAFA,CAFB)
C
      DO I = 1, NBAS
#if defined (BIT64)
         CAFB(I) = CAFB(I) +  CONJG(CFA(I))
         CAFA(I) = CAFA(I) -  CONJG(CFB(I))
#else
         CAFB(I) = CAFB(I) +  DCONJG(CFA(I))
         CAFA(I) = CAFA(I) -  DCONJG(CFB(I))
#endif
      ENDDO
C
      CALL XPROJOP (IREP,NSYMOP,ICOMOP,N7,PROJOP,UU,
     &              CAFA,CAFB,IND,NBAS,CFA,CFB)
C
C     ---------------------------------------------------------------
C     LOOK FOR NON ZERO COEFFICIENTS. IF THERE ARE NONE FUNCTION 'IFF'
C     WITH SPIN 'IS' (ALPHA OR BETHA) IS NOT IN A BASIS FOR REPRESEN-
C     TATION 'IR'
C     ---------------------------------------------------------------
C
      XNOR1 = XOVLAP(NBAS,IND,ICENT,ITYPE,CFA,CFB,CFA,CFB)
C
#if defined (BIT64)
      IF (CABS(XNOR1).LT.1.E-10) GOTO 60
#else
      IF (CDABS(XNOR1).LT.1.D-10) GOTO 60
#endif
C
#if defined (BIT64)
      RNORM1 = SQRT(1.0/CABS(XNOR1))
      CALL CSSCAL (NBAS,RNORM1,CFA,1)
      CALL CSSCAL (NBAS,RNORM1,CFB,1)
#else
      RNORM1 = DSQRT(1.D0/CDABS(XNOR1))
      CALL ZDSCAL (NBAS,RNORM1,CFA,1)
      CALL ZDSCAL (NBAS,RNORM1,CFB,1)
#endif
      XNOR1 = (1.0,0.0)
C
C     ----------------------------------------------------------------
C     CHECK ON ORTHOGONALITY
C     IF THE ACTUAL PSF IS NOT ORTHOGONAL TO ALL PSF'S FORMED FROM THE
C     SAME SET OF CMO'S THEN THIS PSF HAS ALREADY BEEN FOUND
C     ----------------------------------------------------------------
C
      DO 50 I=ISYMF,NEQSYMF
      IZ=(NEQSYMF-I+1)*NBAS
      VPROD = XOVLAP(NBAS,IND,ICENT,ITYPE,CSYMORA(IADR-IZ+1),
     &               CSYMORB(IADR-IZ+1),CFA,CFB)
      XNOR2 = XOVLAP(NBAS,IND,ICENT,ITYPE,CSYMORA(IADR-IZ+1),
     &               CSYMORB(IADR-IZ+1),CSYMORA(IADR-IZ+1),
     &               CSYMORB(IADR-IZ+1))
#if defined (BIT64)
      IF (CABS(VPROD).GT.1.E-10) THEN
#else
      IF (CDABS(VPROD).GT.1.D-10) THEN
#endif
          IF (IRPSOR(I).NE.IREP) CALL Q2JOB
     $    (6,'FOUND NON-ORTHOGONAL SYM.FIES OF DIFFERENT IREPS',' ',0)
          SN=VPROD/XNOR2
#if defined (BIT64)
          IF (CABS(XNOR1/VPROD-CONJG(SN)).LT.1.E-10) GOTO 60
#else
          IF (CDABS(XNOR1/VPROD-DCONJG(SN)).LT.1.D-10) GOTO 60
#endif
          DO 45 J=1,NBAS
          CFA(J)=CFA(J)-CSYMORA(IADR+J-IZ)*SN
          CFB(J)=CFB(J)-CSYMORB(IADR+J-IZ)*SN
   45     CONTINUE
          XNOR1 = XOVLAP(NBAS,IND,ICENT,ITYPE,CFA,CFB,CFA,CFB)
#if defined (BIT64)
          IF (CABS(XNOR1).LT.1.E-10) CALL Q2JOB(6,'P.SYMF.S50',' ',0)
#else
          IF (CDABS(XNOR1).LT.1.D-10) CALL Q2JOB(6,'P.SYMF.S50',' ',0)
#endif
      ENDIF
   50 CONTINUE
C
C     -----------------------------------------------------------------
C     THIS ONE IS ORTHOGONAL TO ALL PREVIOUS FUNCTIONS OF THIS SYMMETRY
C     Make first non-zero coefficient real and add him to the list
C     -----------------------------------------------------------------
C
      DO I = 1, NBAS
#if defined (BIT64)
         YNORM = CFA(I)*CONJG(CFA(I)) 
#else
         YNORM = CFA(I)*DCONJG(CFA(I)) 
#endif
         IF (YNORM.GT.1E-10) THEN
#if defined (BIT64)
            VPROD =  CONJG(CFA(I)) / SQRT(YNORM)
#else
            VPROD = DCONJG(CFA(I)) / DSQRT(YNORM)
#endif
            GOTO 51
         ENDIF
#if defined (BIT64)
         YNORM = CFB(I)*CONJG(CFB(I)) 
         IF (YNORM.GT.1E-10) THEN
            VPROD =  CONJG(CFB(I)) / SQRT(YNORM)
#else
         YNORM = CFB(I)*DCONJG(CFB(I)) 
         IF (YNORM.GT.1D-10) THEN
            VPROD = DCONJG(CFB(I)) / DSQRT(YNORM)
#endif
            GOTO 51
         ENDIF
      ENDDO
   51 CONTINUE
      IADR1 = IADR
      DO  I=1,NBAS
         CSYMORA(IADR+I)=CFA(I)*VPROD
         CSYMORB(IADR+I)=CFB(I)*VPROD
      ENDDO
      IADR = IADR + NBAS
      NEQSYMF=NEQSYMF+1
      IF (NEQSYMF.GT.N9) CALL Q2JOB(4,'P.MFDSYMF_INDSOR',' ',0)
      INDSOR(NEQSYMF+1)=INDSOR(NEQSYMF)+NBAS
      IRPSOR(NEQSYMF)=IREP
C     -----------------------------
C     GENERATE THE KRAMERS' PARTNER
C     -----------------------------
      DO I = 1, NBAS
#if defined (BIT64)
         CFA(I)=- CONJG(CSYMORB(IADR1+I))
         CFB(I)=  CONJG(CSYMORA(IADR1+I))
#else
         CFA(I)=-DCONJG(CSYMORB(IADR1+I))
         CFB(I)= DCONJG(CSYMORA(IADR1+I))
#endif
      ENDDO
C     --------------------------------------
C     Determine symmetry of Kramers' partner
C     --------------------------------------
      JREP = 0
      KREP = 0
      DO JR = 1, NSYMRP
      DO JRP = 1, IDIMRP(JR)
         JREP = JREP + 1
         CALL XPROJOP (JREP,NSYMOP,ICOMOP,N7,PROJOP,UU,
     &        CFA,CFB,IND,NBAS,CAFA,CAFB)
         XNOR1 = XOVLAP(NBAS,IND,ICENT,ITYPE,CAFA,CAFB,CAFA,CAFB)
#if defined (BIT64)
         IF (CABS(XNOR1).GT.1.E-10) THEN
#else
         IF (CDABS(XNOR1).GT.1.D-10) THEN
#endif
            IF (KREP.NE.0) THEN
               if (mpirnk.eq.0)
     &         WRITE (6,*)  "Kramers' partner spans 2 repr: ",KREP,JREP
               CALL Q2JOB(6,'KRAMERS PARTNER',' ',0)
            ELSE
               KREP = JREP
            ENDIF
         ENDIF
      ENDDO
      ENDDO
      IF (KREP.EQ.0) THEN
         if (mpirnk.eq.0)
     &   WRITE (6,*)  "Kramers' partner does not span repr: "
         CALL Q2JOB (6,'KRAMERS PARTNER',' ',0)
      ENDIF 
      KADR1=IADR
      DO I=1,NBAS
         CSYMORA(IADR+I)=CFA(I)
         CSYMORB(IADR+I)=CFB(I)
      ENDDO
      IADR = IADR + NBAS
      NEQSYMF=NEQSYMF+1 
      IF (NEQSYMF.GT.N9) CALL Q2JOB(4,'P.MFDSYMF_INDSOR',' ',0)
      INDSOR(NEQSYMF+1)=INDSOR(NEQSYMF)+NBAS
      IRPSOR(NEQSYMF)=KREP
C
      IF (ID.EQ.1) GOTO 60
C
C     ------------------------------------------------------------
C     FOR MULTIDIMENSIONAL IRREPS WE NEED TO GENERATE THE PARTNERS
C     ------------------------------------------------------------
C
C     ----------------------------------------------------------
C     TWO CASES : 1) KRAMERS' PARTNER IN THIS (2-D OR 4-D) IRREP
C                 2) KRAMERS' PARTNER IN OTHER (2-D) IRREP
C     ----------------------------------------------------------
C
      IF (KREP.GE.IREP.AND.KREP.LT.IREP+ID) THEN
C
C     -----------------------------------------------------------------
C     IF THE KRAMERS' PARTNER IS ALSO IN THIS IRREP THE PHASE MUST BE
C     IDENTICAL TO THE ONE FOUND PREVIOUSLY TO HAVE CONSISTENT REPR.
C     MATRICES (AND REAL INTEGRALS IN THE MS BASIS)
C     -----------------------------------------------------------------
C
         CALL XROTATE (IFASOP(1),ICOMOP,N7,UU,CSYMORA(IADR1+1),
     &                 CSYMORB(IADR1+1),IND,NBAS,CFA,CFB)
         XNOR1 = XOVLAP(NBAS,IND,ICENT,ITYPE,CFA,CFB,CFA,CFB)
#if defined (BIT64)
         IF (REAL(XNOR1).LT.1.E-10) CALL Q2JOB
#else
         IF (DBLE(XNOR1).LT.1.D-10) CALL Q2JOB
#endif
     $   (6,' ERROR IN CALCULATION OF NORM IN P.MFDSYMF_XNOR1',' ',0)
#if defined (BIT64)
         XNOR1 = CMPLX(1./SQRT(REAL(XNOR1)))
#else
         XNOR1 = DCMPLX(1./DSQRT(DBLE(XNOR1)))
#endif
         VPROD = XOVLAP(NBAS,IND,ICENT,ITYPE,CSYMORA(KADR1+1),
     &                  CSYMORB(KADR1+1),CFA,CFB)
         XNOR2 = XOVLAP(NBAS,IND,ICENT,ITYPE,CSYMORA(KADR1+1),
     &                  CSYMORB(KADR1+1),CSYMORA(KADR1+1),
     &                  CSYMORB(KADR1+1))
#if defined (BIT64)
         IF (CABS(XNOR2).LT.1.E-10) CALL Q2JOB
#else
         IF (CDABS(XNOR2).LT.1.D-10) CALL Q2JOB
#endif
     $   (6,' ERROR IN CALCULATION OF NORM IN P.MFDSYMF_XNOR2',' ',0)
#if defined (BIT64)
         XNOR2=XNOR1/CMPLX(SQRT(REAL(XNOR2)))
#else
         XNOR2=XNOR1/DCMPLX(DSQRT(DBLE(XNOR2)))
#endif
         VPROD=VPROD*XNOR2
#if defined (BIT64)
         IF (CABS(VPROD).LT.1.E-10)  CALL Q2JOB
#else
         IF (CDABS(VPROD).LT.1.D-10)  CALL Q2JOB
#endif
     $   (6,' ERROR IN DETERMINATION OF PHASE FACTOR P.MFDSYMF',' ',0)
         IF (FASEKR.NE.(0.0,0.0)) THEN
             VPROD=FASEKR/VPROD
             VPROD=XSQRT(VPROD)
             DO I=1,NBAS
                CSYMORA(IADR1+I)=CSYMORA(IADR1+I)*VPROD
                CSYMORB(IADR1+I)=CSYMORB(IADR1+I)*VPROD
#if defined (BIT64)
                CSYMORA(KADR1+I)=CSYMORA(KADR1+I)* CONJG(VPROD)
                CSYMORB(KADR1+I)=CSYMORB(KADR1+I)* CONJG(VPROD)
#else
                CSYMORA(KADR1+I)=CSYMORA(KADR1+I)*DCONJG(VPROD)
                CSYMORB(KADR1+I)=CSYMORB(KADR1+I)*DCONJG(VPROD)
#endif
             ENDDO
         ELSE
             FASEKR=VPROD
         ENDIF
C
         IF (ID.EQ.2) GOTO 60
C
C     -----------------------------------------------------------------
C     GENERATE THE REMAINING PARTNERS OF THIS KRAMERS' FUNCTION TO FORM
C     A BASIS OF THE REPRESENTATION BY PROJECTING OUT CMO'S FOR ALL
C     OTHER SUBREPRESENTATIONS
C     -----------------------------------------------------------------
C
         CALL XROTATE (IFASOP(2),ICOMOP,N7,UU,CSYMORA(IADR1+1),
     &                 CSYMORB(IADR1+1),IND,NBAS,CFA,CFB)
         XNOR1 = XOVLAP(NBAS,IND,ICENT,ITYPE,CFA,CFB,CFA,CFB)
#if defined (BIT64)
         IF (REAL(XNOR1).LT.1.E-10) CALL Q2JOB
#else
         IF (DBLE(XNOR1).LT.1.D-10) CALL Q2JOB
#endif
     $   (6,' ERROR IN CALCULATION OF NORM IN P.MFDSYMF_XNOR1',' ',0)
#if defined (BIT64)
         XNOR1 = CMPLX(1./SQRT(REAL(XNOR1)))
#else
         XNOR1 = DCMPLX(1./DSQRT(DBLE(XNOR1)))
#endif
         KREP2 = 0
         IREP2=IREP+2
         IADR2 = IADR
         CALL XPROJOP (IREP2,NSYMOP,ICOMOP,N7,PROJOP,UU,
     &        CFA,CFB,IND,NBAS,CSYMORA(IADR+1),CSYMORB(IADR+1))
         IADR = IADR + NBAS
         NEQSYMF=NEQSYMF+1
         IF (NEQSYMF.GT.N9) CALL Q2JOB(4,'P.MFDSYMF_INDSOR',' ',0)
         INDSOR(NEQSYMF+1)=INDSOR(NEQSYMF)+NBAS
         IRPSOR(NEQSYMF)=IREP2
C        ---------------------------------------
C        GENERATE THE PARTNERS' KRAMERS' PARTNER
C        ---------------------------------------
         KADR2=IADR
         DO I = 1, NBAS
#if defined (BIT64)
            CSYMORA(IADR+I)=- CONJG(CSYMORB(IADR2+I))
            CSYMORB(IADR+I)=  CONJG(CSYMORA(IADR2+I))
#else
            CSYMORA(IADR+I)=-DCONJG(CSYMORB(IADR2+I))
            CSYMORB(IADR+I)= DCONJG(CSYMORA(IADR2+I))
#endif
         ENDDO
         IADR = IADR + NBAS
C        ---------------------------------------------------------
C        Check the symmetry : should be remaining row of 4-d irrep
C        ---------------------------------------------------------
         KREP2 = IREP2 + 1
         CALL XPROJOP (KREP2,NSYMOP,ICOMOP,N7,PROJOP,UU,
     &        CSYMORA(KADR2+1),CSYMORB(KADR2+1),IND,NBAS,CAFA,CAFB)
         XNOR2 = XOVLAP(NBAS,IND,ICENT,ITYPE,CAFA,CAFB,CAFA,CAFB)
#if defined (BIT64)
         IF (CABS(XNOR2).LT.1.E-10) THEN
#else
         IF (CDABS(XNOR2).LT.1.D-10) THEN
#endif
            if (mpirnk.eq.0) WRITE (6,*)  
     &         "Second Kramers' partner does not span 4th row"
            CALL Q2JOB (6,'KRAMERS PARTNER',' ',0)
         ENDIF 
         NEQSYMF=NEQSYMF+1
         IF (NEQSYMF.GT.N9) CALL Q2JOB(4,'P.MFDSYMF_INDSOR',' ',0)
         INDSOR(NEQSYMF+1)=INDSOR(NEQSYMF)+NBAS
         IRPSOR(NEQSYMF)=KREP2
C
C        --------------------------------------
C        Check and correct phases if necessary.
C        First the Kramers' phase D(R)[3,4].
C        --------------------------------------
C
         CALL XROTATE (IFASOP(1),ICOMOP,N7,UU,CSYMORA(IADR2+1),
     &                 CSYMORB(IADR2+1),IND,NBAS,CFA,CFB)
         XNOR1 = XOVLAP(NBAS,IND,ICENT,ITYPE,CFA,CFB,CFA,CFB)
#if defined (BIT64)
         IF (REAL(XNOR1).LT.1.E-10) CALL Q2JOB
#else
         IF (DBLE(XNOR1).LT.1.D-10) CALL Q2JOB
#endif
     $   (6,' ERROR IN CALCULATION OF NORM IN P.MFDSYMF_XNOR1',' ',0)
#if defined (BIT64)
         XNOR1 = CMPLX(1./SQRT(REAL(XNOR1)))
#else
         XNOR1 = DCMPLX(1./DSQRT(DBLE(XNOR1)))
#endif
         VPROD = XOVLAP(NBAS,IND,ICENT,ITYPE,CSYMORA(KADR2+1),
     &                  CSYMORB(KADR2+1),CFA,CFB)
         XNOR2 = XOVLAP(NBAS,IND,ICENT,ITYPE,CSYMORA(KADR2+1),
     &                  CSYMORB(KADR2+1),CSYMORA(KADR2+1),
     &                  CSYMORB(KADR2+1))
#if defined (BIT64)
         IF (CABS(XNOR2).LT.1.E-10) CALL Q2JOB
#else
         IF (CDABS(XNOR2).LT.1.D-10) CALL Q2JOB
#endif
     $   (6,' ERROR IN CALCULATION OF NORM IN P.MFDSYMF_XNOR2',' ',0)
#if defined (BIT64)
         XNOR2=XNOR1/CMPLX(SQRT(REAL(XNOR2)))
#else
         XNOR2=XNOR1/DCMPLX(DSQRT(DBLE(XNOR2)))
#endif
         VPROD=VPROD*XNOR2
#if defined (BIT64)
         IF (CABS(VPROD).LT.1.E-10)  CALL Q2JOB
#else
         IF (CDABS(VPROD).LT.1.D-10)  CALL Q2JOB
#endif
     $   (6,' ERROR IN PHASE FACTOR P.MFDSYMF',' ',0)
C        --------------------------------------------------------
C        Rotate first function and counterrotate second function.
C        --------------------------------------------------------
         VPROD=FASEKR/VPROD
         VPROD=XSQRT(VPROD)
         DO I=1,NBAS
            CSYMORA(IADR2+I)=CSYMORA(IADR2+I)*VPROD
            CSYMORB(IADR2+I)=CSYMORB(IADR2+I)*VPROD
#if defined (BIT64)
            CSYMORA(KADR2+I)=CSYMORA(KADR2+I)* CONJG(VPROD)
            CSYMORB(KADR2+I)=CSYMORB(KADR2+I)* CONJG(VPROD)
#else
            CSYMORA(KADR2+I)=CSYMORA(KADR2+I)*DCONJG(VPROD)
            CSYMORB(KADR2+I)=CSYMORB(KADR2+I)*DCONJG(VPROD)
#endif
         ENDDO
C
C        -----------------------------------------------------------
C        Check phase with previous pair. Use D(R)[1,4]
C        There is the only degree of freedom for the phases. 
C        In the first pass the coefficient of the first function in
C        the first row of the first representation is chosen real.
C        This defines the phases for all further functions.
C        Inconsistencies may be corrected by combining 
C        the current 4 functions with the previous ones.
C        This is NOT implemented, I guess the current method 
C        should work in almost all cases.
C        -----------------------------------------------------------
C
         CALL XROTATE (IFASOP(2),ICOMOP,N7,UU,CSYMORA(IADR1+1),
     &                 CSYMORB(IADR1+1),IND,NBAS,CFA,CFB)
         XNOR1 = XOVLAP(NBAS,IND,ICENT,ITYPE,CFA,CFB,CFA,CFB)
#if defined (BIT64)
         IF (REAL(XNOR1).LT.1.E-10) CALL Q2JOB
#else
         IF (DBLE(XNOR1).LT.1.D-10) CALL Q2JOB
#endif
     $   (6,' ERROR IN CALCULATION OF NORM IN P.MFDSYMF_XNOR1',' ',0)
#if defined (BIT64)
         XNOR1 = CMPLX(1./SQRT(REAL(XNOR1)))
#else
         XNOR1 = DCMPLX(1./DSQRT(DBLE(XNOR1)))
#endif
         VPROD = XOVLAP(NBAS,IND,ICENT,ITYPE,CSYMORA(KADR2+1),
     &                  CSYMORB(KADR2+1),CFA,CFB)
         XNOR2 = XOVLAP(NBAS,IND,ICENT,ITYPE,CSYMORA(KADR2+1),
     &                  CSYMORB(KADR2+1),CSYMORA(KADR2+1),
     &                  CSYMORB(KADR2+1))
#if defined (BIT64)
         IF (CABS(XNOR2).LT.1.E-10) CALL Q2JOB
#else
         IF (CDABS(XNOR2).LT.1.D-10) CALL Q2JOB
#endif
     $   (6,' ERROR IN CALCULATION OF NORM IN P.MFDSYMF_XNOR2',' ',0)
#if defined (BIT64)
         XNOR2=XNOR1/CMPLX(SQRT(REAL(XNOR2)))
#else
         XNOR2=XNOR1/DCMPLX(DSQRT(DBLE(XNOR2)))
#endif
         VPROD=VPROD*XNOR2
#if defined (BIT64)
         IF (CABS(VPROD).LT.1.E-10)  CALL Q2JOB
#else
         IF (CDABS(VPROD).LT.1.D-10)  CALL Q2JOB
#endif
     $   (6,' ERROR IN PHASE FACTOR P.MFDSYMF',' ',0)
         IF (FASE(KREP2).EQ.(0.0,0.0)) THEN
            FASE(KREP2) = VPROD
         ELSE
#if defined (BIT64)
            IF ( CABS(FASE(KREP2)-VPROD).GT.1.E-10) THEN
#else
            IF (CDABS(FASE(KREP2)-VPROD).GT.1.D-10) THEN
#endif
            if (mpirnk.eq.0)
     &      WRITE (*,*) "Problems with definition of rep. matrices"
            CALL Q2JOB (6,' ERROR IN PHASE FACTOR P.MFDSYMF',' ',0)
            ENDIF
         ENDIF
C
C     ----
      ELSE
C     ----
C
C     -----------------------------------------------------------------
C     THE KRAMERS' PARTNER IS IN ANOTHER 2-D IRREP : THE PHASE OF EACH
C     IRREP SHOULD BE IDENTICAL TO THE ONES FOUND PREVIOUSLY TO HAVE 
C     CONSISTENT REPR. MATRICES (AND REAL INTEGRALS IN THE MS BASIS)
C     CONTINUE BY PROJECTING OUT SYMMETRY PARTNER OF FIRST FUNCTION AND
C     COUNTERROTATE KRAMERS' PAIR IF NECESSARY
C     START BY MOVING THE KRAMERS' PARTNER ONE DOWN FOR COMPATIBILITY 
C     WITH THE OLDER GENERATING SCHEME (PROJECTING PARTNERS WITHOUT 
C     USING TR SYMMETRY)
C     ----------------------------------------------------------------
C
         DO I = 1, NBAS
            CSYMORA(IADR+I)=CSYMORA(KADR1+I)
            CSYMORB(IADR+I)=CSYMORB(KADR1+I)
         ENDDO
         IADR2 = KADR1
         KADR1 = IADR
         IADR = IADR+NBAS
         NEQSYMF=NEQSYMF+1
         IF (NEQSYMF.GT.N9) CALL Q2JOB(4,'P.MFDSYMF_INDSOR',' ',0)
         INDSOR(NEQSYMF+1)=INDSOR(NEQSYMF)+NBAS
         IRPSOR(NEQSYMF)=KREP
C
         CALL XROTATE (IFASOP(1),ICOMOP,N7,UU,CSYMORA(IADR1+1),
     &                 CSYMORB(IADR1+1),IND,NBAS,CFA,CFB)
         XNOR1 = XOVLAP(NBAS,IND,ICENT,ITYPE,CFA,CFB,CFA,CFB)
#if defined (BIT64)
         IF (REAL(XNOR1).LT.1.E-10) CALL Q2JOB
#else
         IF (DBLE(XNOR1).LT.1.D-10) CALL Q2JOB
#endif
     $   (6,' ERROR IN CALCULATION OF NORM IN P.MFDSYMF_XNOR1',' ',0)
#if defined (BIT64)
         XNOR1 = CMPLX(1./SQRT(REAL(XNOR1)))
#else
         XNOR1 = DCMPLX(1./DSQRT(DBLE(XNOR1)))
#endif
         IREP2 = IREP + 1
         CALL XPROJOP (IREP2,NSYMOP,ICOMOP,N7,PROJOP,UU,
     &        CFA,CFB,IND,NBAS,CSYMORA(IADR2+1),CSYMORB(IADR2+1))
         IRPSOR(NEQSYMF-1)=IREP2
         XNOR2 = XOVLAP(NBAS,IND,ICENT,ITYPE,CSYMORA(IADR2+1),
     &                  CSYMORB(IADR2+1),CSYMORA(IADR2+1),
     &                  CSYMORB(IADR2+1))
         VPROD = XOVLAP(NBAS,IND,ICENT,ITYPE,CSYMORA(IADR2+1),
     &                  CSYMORB(IADR2+1),CFA,CFB)
#if defined (BIT64)
         IF (CABS(XNOR2).LT.1.E-10) CALL Q2JOB
#else
         IF (CDABS(XNOR2).LT.1.D-10) CALL Q2JOB
#endif
     $   (6,' ERROR IN CALCULATION OF NORM IN P.MFDSYMF_XNOR2',' ',0)
#if defined (BIT64)
         XNOR2=XNOR1/CMPLX(SQRT(REAL(XNOR2)))
         IF (CABS(VPROD).LT.1.E-10)  CALL Q2JOB
#else
         XNOR2=XNOR1/DCMPLX(DSQRT(DBLE(XNOR2)))
         IF (CDABS(VPROD).LT.1.D-10)  CALL Q2JOB
#endif
     $   (6,' ERROR IN PHASE FACTOR P.MFDSYMF',' ',0)
         VPROD=VPROD*XNOR2
         IF (FASE(IREP2).NE.(0.0,0.0)) THEN
             VPROD=FASE(IREP2)/VPROD
             DO I=1,NBAS
#if defined (BIT64)
                CSYMORA(IADR2+I)=CSYMORA(IADR2+I)* CONJG(VPROD)
                CSYMORB(IADR2+I)=CSYMORB(IADR2+I)* CONJG(VPROD)
#else
                CSYMORA(IADR2+I)=CSYMORA(IADR2+I)*DCONJG(VPROD)
                CSYMORB(IADR2+I)=CSYMORB(IADR2+I)*DCONJG(VPROD)
#endif
             ENDDO
         ELSE
             FASE(IREP2)=VPROD
         ENDIF
C
C     --------------------------------------------------------------
C     GENERATE THE PARTNER IN THE OTHER IRREP BY MAKING THE KRAMERS'
C     PARTNER. CHECK THE PHASES. D(R)[1,2] = D*(R)[3,4]
C     FIND OUT WHETHER THE 2-D REPS ARE "CROSS-LINKED" 
C     --------------------------------------------------------------
C
         IF (KREP.EQ.IREP2+1) THEN
            KREP2 = KREP + 1
            KADR2 = IADR
            DO I = 1, NBAS
#if defined (BIT64)
               CSYMORA(IADR+I)=- CONJG(CSYMORB(IADR2+I))
               CSYMORB(IADR+I)=  CONJG(CSYMORA(IADR2+I))
#else
               CSYMORA(IADR+I)=-DCONJG(CSYMORB(IADR2+I))
               CSYMORB(IADR+I)= DCONJG(CSYMORA(IADR2+I))
#endif
            ENDDO
            IADR = IADR+NBAS
            NEQSYMF=NEQSYMF+1
            IF (NEQSYMF.GT.N9) CALL Q2JOB(4,'P.MFDSYMF_INDSOR',' ',0)
            INDSOR(NEQSYMF+1)=INDSOR(NEQSYMF)+NBAS
            IRPSOR(NEQSYMF)=KREP2
         ELSEIF (KREP.EQ.IREP2+2) THEN
            KREP2 = KREP - 1
            KADR2 = KADR1
            DO I = 1, NBAS
               CSYMORA(IADR+I)=CSYMORA(KADR1+I)
               CSYMORB(IADR+I)=CSYMORB(KADR1+I)
            ENDDO
            KADR1 = IADR
            DO I = 1, NBAS
#if defined (BIT64)
               CSYMORA(KADR2+I)=- CONJG(CSYMORB(IADR2+I))
               CSYMORB(KADR2+I)=  CONJG(CSYMORA(IADR2+I))
#else
               CSYMORA(KADR2+I)=-DCONJG(CSYMORB(IADR2+I))
               CSYMORB(KADR2+I)= DCONJG(CSYMORA(IADR2+I))
#endif
            ENDDO
            IRPSOR(NEQSYMF)=KREP2
            IADR = IADR+NBAS
            NEQSYMF=NEQSYMF+1
            IF (NEQSYMF.GT.N9) CALL Q2JOB(4,'P.MFDSYMF_INDSOR',' ',0)
            INDSOR(NEQSYMF+1)=INDSOR(NEQSYMF)+NBAS
            IRPSOR(NEQSYMF)=KREP
         ELSE
            CALL Q2JOB (6,' ERROR IN KRAMERS-LINKED 2-D IRREPS',' ',0)
         ENDIF
C
         CALL XROTATE (IFASOP(1),ICOMOP,N7,UU,CSYMORA(KADR1+1),
     &                 CSYMORB(KADR1+1),IND,NBAS,CFA,CFB)
         VPROD = XOVLAP(NBAS,IND,ICENT,ITYPE,CSYMORA(KADR2+1),
     &                  CSYMORB(KADR2+1),CFA,CFB)
#if defined (BIT64)
         IF (CABS(VPROD).LT.1.E-10)  CALL Q2JOB
#else
         IF (CDABS(VPROD).LT.1.D-10)  CALL Q2JOB
#endif
     $   (6,' ERROR IN PHASE FACTOR P.MFDSYMF',' ',0)
         VPROD=VPROD*XNOR2
C        ---------------------------
C        Rotate pairs to fix phases.
C        ---------------------------
         IF (FASE(KREP2).EQ.(0.0,0.0)) THEN
#if defined (BIT64)
            VPROD= CONJG(FASE(IREP2))/VPROD
#else
            VPROD=DCONJG(FASE(IREP2))/VPROD
#endif
            VPROD=XSQRT(VPROD)
#if defined (BIT64)
            FASE(IREP2) = FASE(IREP2) * CONJG(VPROD)
            FASE(KREP2) = CONJG(FASE(IREP2))
#else
            FASE(IREP2) = FASE(IREP2) * DCONJG(VPROD)
            FASE(KREP2) = DCONJG(FASE(IREP2))
#endif
            VPROD=XSQRT(VPROD)
            DO I=1,NBAS
               CSYMORA(KADR1+I)=CSYMORA(KADR1+I)*VPROD
               CSYMORB(KADR1+I)=CSYMORB(KADR1+I)*VPROD
#if defined (BIT64)
               CSYMORA(KADR2+I)=CSYMORA(KADR2+I)* CONJG(VPROD)
               CSYMORB(KADR2+I)=CSYMORB(KADR2+I)* CONJG(VPROD)
               CSYMORA(IADR1+I)=CSYMORA(IADR1+I)* CONJG(VPROD)
               CSYMORB(IADR1+I)=CSYMORB(IADR1+I)* CONJG(VPROD)
#else
               CSYMORA(KADR2+I)=CSYMORA(KADR2+I)*DCONJG(VPROD)
               CSYMORB(KADR2+I)=CSYMORB(KADR2+I)*DCONJG(VPROD)
               CSYMORA(IADR1+I)=CSYMORA(IADR1+I)*DCONJG(VPROD)
               CSYMORB(IADR1+I)=CSYMORB(IADR1+I)*DCONJG(VPROD)
#endif
               CSYMORA(IADR2+I)=CSYMORA(IADR2+I)*VPROD
               CSYMORB(IADR2+I)=CSYMORB(IADR2+I)*VPROD
            ENDDO
         ELSE
#if defined (BIT64)
            IF ( CABS(FASE(KREP2)-VPROD).GT.1.E-10) THEN
#else
            IF (CDABS(FASE(KREP2)-VPROD).GT.1.D-10) THEN
#endif
            if (mpirnk.eq.0)
     &      WRITE (*,*) "Problems with definition of rep. matrices"
            CALL Q2JOB (6,' ERROR IN PHASE FACTOR P.MFDSYMF',' ',0)
            ENDIF
         ENDIF
C
C     -----
      ENDIF
C     -----
C
   60 CONTINUE
   65 CONTINUE
   70 CONTINUE
C
C     -----------------------------------------------------------
C     CHECK ORTHOGONALITY OF ALL FUNCTIONS FORMED FROM THIS GROUP
C     -----------------------------------------------------------
C
      DO 89 I=LSYMF,NEQSYMF
      INS=INDSOR(I)
      DO 88 J=LSYMF,I-1
      KNS=INDSOR(J)
      VPROD=(0.0,0.0)
      VPROD = XOVLAP(NBAS,IND,ICENT,ITYPE,CSYMORA(KNS+1),
     &               CSYMORB(KNS+1),CSYMORA(INS+1),CSYMORB(INS+1))
#if defined (BIT64)
      IF (CABS(VPROD).GT.0.001) CALL Q2JOB(6,'P.MFDSYMF_S.86',' ',0)
#else
      IF (CDABS(VPROD).GT.0.001) CALL Q2JOB(6,'P.MFDSYMF_S.86',' ',0)
#endif
   88 CONTINUE
   89 CONTINUE
      IREP=IREP+ID
   90 CONTINUE
      IF (NEQSYMF-LSYMF+1.NE.NSYMC(IM))
     +CALL Q2JOB(6,' TOO MANY/FEW SYMMETRY ADAPTED FUNCTIONS FOUND',
     +           ' ',0)
  100 CONTINUE
  105 CONTINUE
C
C     ---------------------------------------------------------
C     THE NUMBER OF SUBSPECIES SPANNED BY THE BASISSET EMPLOYED
C     EQUALS THE HIGHEST SEQUENCE NUMBER OF THE SUBSPECIES EN-
C     COUNTERED
C     ---------------------------------------------------------
C
      NSUBRP=0
      DO 120 J=1,NSUBSP
      DO 110 I=1,NEQSYMF
      IF (IRPSOR(I).NE.J) GOTO 110
      NSUBRP=NSUBRP+1
      IREPS(NSUBRP)=J
      GOTO 120
  110 CONTINUE
  120 CONTINUE
      NC=2*INDSOR(NEQSYMF+1)
      DO 130 I=1,NC,2
#if defined (BIT64)
      IF (ABS(CSYMA(I  )).LT.1.E-10) CSYMA(I  )=0.0
      IF (ABS(CSYMA(I+1)).LT.1.E-10) CSYMA(I+1)=0.0
      IF (ABS(CSYMB(I  )).LT.1.E-10) CSYMB(I  )=0.0
      IF (ABS(CSYMB(I+1)).LT.1.E-10) CSYMB(I+1)=0.0
#else
      IF (DABS(CSYMA(I  )).LT.1.D-10) CSYMA(I  )=0.0
      IF (DABS(CSYMA(I+1)).LT.1.D-10) CSYMA(I+1)=0.0
      IF (DABS(CSYMB(I  )).LT.1.D-10) CSYMB(I  )=0.0
      IF (DABS(CSYMB(I+1)).LT.1.D-10) CSYMB(I+1)=0.0
#endif
  130 CONTINUE
      RETURN
      END
