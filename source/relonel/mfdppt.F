C
C     ==============================================================
      SUBROUTINE MFDPPT(NA,AAR,AAI,NB,BBR,BBI,NC,CCR,CCI,ND,DDR,DDI,
     $                  NE,ER,EI,NF,FR,FI,M,EER,EEI,N,D2,E)
C     ==============================================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     FIRST PURPOSE OF THIS ROUTINE IS TO CALCULATE THE KINETIC BALANCE
C     IN THE GIVEN BASIS. IF THE BASIS SHOWS LINEAR DEPENDENCIES THE BASIS
C     IS REDUCED
C     WHILE DOING THIS (FOR EACH SUBSPECIES SEPERATELY) THE OVERLAP (S)
C     MATRICES ARE DIAGONALISED TO GIVE S**-1/2 AND THE INTEGRAL FILES
C     ARE REBUILT
C     NA ---NUMBER OF MATRIX-ELEMENTS IN THE LOWER TRIANGLE FOR THE
C           LARGE COMPONENT BASISSET FOR THE LARGEST REPRESENTATION
C     AAR---REAL PART OF THAT LOWER TRIANGLE
C     AAI---IMAGINAIRY PART OF THAT LOWER TRIANGLE
C     NB ---NUMBER OF MATRIX-ELEMENTS IN THE RECTANGLE OVER THE LARGE-
C           SMALL COMPONENT BASISSETS FOR THE LARGEST REPRESENTATION
C     BBR---REAL PART OF THAT RECTANGLE
C     BBI---IMAGINARY PART OF THAT RECTANGLE
C     NC ---MAXIMUM OVER (NA+NB) AND NE
C     CCR---REAL PART OF MATRIX LIKE AR BUT LARGE ENOUGH TO CONTAIN
C           THE LOWER TRIANGLE OVER THE SMALL-COMPONENT BASISSET BY
C           OVERWRITING DR
C     CCI---IMAGINARY PART THAT BELONGS TO CR
C     ND ---MAXIMUM OVER NB AND NA
C     DDR---MATRIX LIKE BR BUT DR IS OVERWRITTEN IF CR IS TO CONTAIN
C           THE LOWER TRIANGLE OF THE SMALL COMPONENT BASIS
C     DDI---IMAGINARY PART THAT BELONGS TO DR
C     NE ---NUMBER OF MATRIX-ELEMENTS IN THE LOWER TRIANGLE FOR THE
C           SMALL COMPONENT BASISSET FOR THE LARGEST REPRESENTATION
C     ER ---REAL PART OF THAT LOWER TRIANGLE.
C     EI ---IMAGINARY PART THAT BELONGS TO ER
C     NF ---AS NE
C     FR ---REAL PART OF LOWER TRIANGLE OF MATRIX-ELEMENTS OVER SMALL
C           COMPONENT BASISSET
C     FI ---IMAGINARY PART THAT BELONGS TO FR
C     M ----M=N+1
C     EER --REAL PART OF TWO DIMENSIONAL MATRIX EER(N,M)
C           EER AND ER...FR SHARE THE SAME MEMORY LOCATIONS
C           USING EER MEANS OVERWRITING ER AND FR
C     EEI --IMAGINARY PART THAT BELONGS TO EER
C     N ----MAXIMUM (OVER THE COMPONENTS) OF BASISFUNCTIONS OF THE
C           LARGEST REPRESENTATION
C     D2 ---ARRAY USED BY DIAGONALISATION PROCEDURES TO CONTAIN THE
C           EIGENVALUES
C     E ----ARRAY USED BY DIAGONALISATION PROCEDURES TO CONTAIN THE
C           SUBDIAGONAL ELEMENTS
C
      INCLUDE 'paracom.inc'
#if defined (PARALLEL)
      INCLUDE 'mpidef.inc'
#else
      PARAMETER (mpirnk = 0)
#endif
C
      DIMENSION TT(2*N2),SS(2*N2),HH(2*N2)
      DIMENSION AAR(NA),AAI(NA),BBR(NB),BBI(NB),CCR(NC),CCI(NC)
      DIMENSION DDR(ND),DDI(ND),ER(NE),EI(NE),FR(NF),FI(NF)
      DIMENSION EER(N,M),EEI(N,M),D2(N),E(N)
      DIMENSION SER (NEX, NEX), SEI (NEX, NEX)
      DIMENSION TEMPR (NEX * NEX), TEMPI (NEX * NEX)
      DIMENSION TEMPE (NEX * 2)
      EQUIVALENCE (B1,TT),(B2,SS),(B3,HH)
C
      REWIND 10
      REWIND 11
      REWIND 12
      LSTART=0
      MSTART=0
      NSTART=0
      DO 200 IRP=1,NSYMRP
#if defined (LINUX)
      if (irp.gt.1) BACKSPACE 10
      if (irp.gt.1) BACKSPACE 11
      if (irp.gt.1) BACKSPACE 12
#else
      BACKSPACE 10
      BACKSPACE 11
      BACKSPACE 12
#endif
      NB1=NBSYM1(IRP)
      NB2=NBSYM2(IRP)
      IF (NEX .LT. NB1) CALL Q2JOB(3,'MFDPPT','NB1',NEX)
      IF (NEX .LT. NB2) CALL Q2JOB(3,'MFDPPT','NB2',NEX)
      NWLLH=NB1*(NB1+1)/2
      NWSSH=NB2*(NB2+1)/2
      NWLS=NB1*NB2
C
C     -------------------------------------------------------------------
C     READ LARGE COMP. KIN.EN (T), NUCL.ATTR.EN (V) AND OVERL.(S) INTGRLS
C     -------------------------------------------------------------------
C
C     -------------------------
C     PUT KINETIC ENERGY IN 'A'
C     PUT OVERLAP INTEGR IN 'C'
C     PUT POT.IAL ENERGY IN 'D'
C     -------------------------
C
C
C     -----------------------------------------------------------
C     INTEGRALS IN SS,TT,HH ARE STORED AS A LOWER TRINGLE IN A
C     LINEAR ARRAY; KEEP THIS ORDER IN THE ARRAYS 'A','B',AND,'C'
C     -----------------------------------------------------------
C
      NN=0
      LL=1
      KK=0
      LREST=NWLLH+LSTART
      LSTART=2*LSTART+1
      LMAX=N2
   10 READ(10) TT,SS,HH
      IF(LMAX.GT.LREST) LMAX=LREST
      DO 20 L=LSTART,2*LMAX,2
      NN=NN+1
      AAR(NN)=TT(L)
      AAI(NN)=TT(L+1)
      CCR(NN)=SS(L)
      CCI(NN)=SS(L+1)
      DDR(NN)=HH(L)
      DDI(NN)=HH(L+1)
   20 CONTINUE
      LSTART=1
      LREST=LREST-N2
      IF(LREST.GT.0) GOTO 10
      LSTART=LMAX
C
C     -------------------------------------------------
C     READ SMALL-LARGE COMPONENT  IMPULS-TYPE INTEGRALS
C     -------------------------------------------------
C
C     ---------------------------
C     PUT IMPULS INTEGRALS IN 'B'
C     ---------------------------
C
C
C     ---------------------------------------------------------
C     INTEGRALS IN HH ARE STORED AS RECTANGULAR MATRIX, ROWWISE
C     IN A LINAER ARRAY; CHANGE THIS ORDER TO COLUMWISE IN 'B'
C     ---------------------------------------------------------
C
      KK=1
      LL=0
      LREST=NWLS+MSTART
      MSTART=2*MSTART+1
      LMAX=N2
   30 READ(11) HH
      IF (LMAX.GT.LREST) LMAX=LREST
      DO 40 L=MSTART,LMAX*2,2
      NN=LL*NB2+KK
      BBR(NN)=HH(L)
      BBI(NN)=HH(L+1)
      LL=LL+1
      IF(LL.LT.NB1) GOTO 40
      LL=0
      KK=KK+1
   40 CONTINUE
      MSTART=1
      LREST=LREST-N2
      IF(LREST.GT.0) GOTO 30
      MSTART=LMAX
C     PRINT*,'---P-MATRIX ',(BBR(I),BBI(I),I=1,NWLS)
C
C     ----------------------------------------------------------------
C     READ SMALL COMPONENT NUCL.ATTR.EN (H) AND OVERLAP (S)  INTEGRALS
C     ----------------------------------------------------------------
C
C     ---------------------------------------------------------------
C     PUT POT.IAL ENERGY IN 'EE' UPPER TRIANGLE SHIFTED BY ONE COLUMN
C     PUT OVERLAP INTEGR IN 'EE' LOWER TRIANGE
C     ---------------------------------------------------------------
C
      K=0
      I=1
      LREST=NWSSH+NSTART
      NSTART=2*NSTART+1
      LMAX=N2
   50 READ(12) SS,HH
      IF(LMAX.GT.LREST) LMAX=LREST
      DO 60 L=NSTART,2*LMAX,2
      K=K+1
      EER(I,K)=SS(L)
      EEI(I,K)=SS(L+1)
      EER(K,I+1)=HH(L)
      EEI(K,I+1)=HH(L+1)
      IF(K.LT.I) GOTO 60
      I=I+1
      K=0
   60 CONTINUE
      NSTART=1
      LREST=LREST-N2
      IF(LREST.GT.0) GOTO 50
      NSTART=LMAX
C
C     ------------------------------------------------------------
C     WRITE ONE-ELECTRON DIRAC-MATRIX AND OVERLAPINTEGRALS TO FILE
C     ------------------------------------------------------------
C
C
C     -----------------------
C     LARGE COMPONENT OVERLAP
C     -----------------------
C
      WRITE (14) (CCR(I),CCI(I),I=1,NWLLH)
C
C     ------------------------------------------
C     DIAGONALIZE LARGE COMPONENT OVERLAP MATRIX
C     TO CHECK LINEAR DEPENDENCIES
C     ------------------------------------------
C
      DO 71 I = 1, NB1
      DO 70 J = 1, I
      SER (I, J) = CCR (I*(I-1)/2 + J)
      SEI (I, J) = CCI (I*(I-1)/2 + J)
      SER (J, I) = CCR (I*(I-1)/2 + J)
      SEI (J, I) = -CCI (I*(I-1)/2 + J)
  70  CONTINUE
  71  CONTINUE
      IFAIL = 0
      CALL CRDIAG (NEX,NB1,SER,SEI,D2,TEMPR,TEMPI,E,E,TEMPE,IFAIL)
      IF (IFAIL .NE. 0) 
     &   CALL Q2JOB(6,'MFDPPT: CRDIAG FAILED',' ',0)
      NBR = 0
      DO 72 L=1,NB1
      IF (D2(L).LT.OVLAPL) NBR = NBR + 1
   72 CONTINUE
      IF (IEIGEN.EQ.1.AND.NBR.GT.0.and.mpirnk.eq.0)
     &                    WRITE (*,1010) IRP,NBR,(D2(L),L=1,NBR)
      IF (IEIGEN.GE.2.AND.NB1.GT.0.and.mpirnk.eq.0)
     &                    WRITE (*,1010) IRP,NBR,(D2(L),L=1,NB1)
C
C     ------------------------------
C     LARGE COMPONENT KINETIC ENERGY
C     ------------------------------
C
      WRITE (14) (AAR(I),AAI(I),I=1,NWLLH)
C
C     --------------------------------
C     LARGE COMPONENT POTENTIAL ENERGY
C     --------------------------------
C
      WRITE (14) (DDR(I),DDI(I),I=1,NWLLH)
C
C     ------------------------------------------
C     LARGE-SMALL COMPONENT KINETIC ENERGY [@.P]
C     ------------------------------------------
C
      WRITE (14) (BBR(I),BBI(I),I=1,NWLS)
C
C     ---------------------------------
C     SMALL COMPONENT POTENTIAL ENERGY
C     SMALL COMPONENT OVERLAP INTEGRALS
C     ---------------------------------
C
      WRITE (14) ((EER(I,J+1),EEI(I,J+1),EER(J,I),EEI(J,I),I=1,J),
     $           J=1,NB2)
C
C     -----------------------------------------------------
C     CALCULATE D**-1/2 MATRIX FOR SMALL COMPONENT BASISSET
C     CALCULATE ALSO S**-1/2 AND WRITE IT TO FILE
C     -----------------------------------------------------
C
      IFAIL=0
C     PRINT *,((EER(I,K),K=1,I),I=1,NB2)
C     PRINT *,((EEI(I,K),K=1,I),I=1,NB2)
      CALL CRDIAG (N,NB2,EER,EEI,D2,TEMPR,TEMPI,E,E,TEMPE,IFAIL)
      IF (IFAIL.NE.0)
     &   CALL Q2JOB(6,'MFDPPT: CRDIAG FAILED',' ',0)
      NBR = 0
      DO 90 L=1,NB2
      IF (D2(L).LT.OVLAPS) NBR = NBR + 1
   90 CONTINUE
      IF (IEIGEN.EQ.1.AND.NBR.GT.0.and.mpirnk.eq.0)
     &                    WRITE (*,1011) IRP,NBR,(D2(L),L=1,NBR)
      IF (IEIGEN.GE.2.AND.NB2.GT.0.and.mpirnk.eq.0)
     &                    WRITE (*,1011) IRP,NBR,(D2(L),L=1,NB2)
      DO 91 L = 1, NB2
#if defined (BIT64)
      D2(L)=1.0/SQRT(D2(L))
#else
      D2(L)=1.0/DSQRT(D2(L))
#endif
   91 CONTINUE
      NB2R = NB2 - NBR
      CALL PMMBKTR(NB2,EER,EEI,N,D2,CCR,CCI)
C     CALL PRTMAT(NB2,CCR,CCI)
C
C     ----------------------------------------------
C     WRITE SMALL COMPONENT [S**-1/2] MATRIX TO FILE
C     ----------------------------------------------
C
      WRITE (14) (CCR(I),CCI(I),I=1,NWSSH)
C
C     --------------------------------------------
C     CALCULATE (REDUCTION-) TRANSFORMATION MATRIX
C     --------------------------------------------
C
      DO 100 J = 1, NB2R
      DO 100 I = 1, NB2
      EER (I,J) = EER (I,J+NBR) * D2(J+NBR)
      EEI (I,J) = EEI (I,J+NBR) * D2(J+NBR)
 100  CONTINUE
C
C     --------------------------------------------------------
C     TRANSFORM FROM THE LEFT TO REDUCED SMALL COMPONENT BASIS
C     --------------------------------------------------------
C
      CALL PMMCMM(N,NB2,EER,EEI,NB2R,NB2,BBR,BBI,NB1,NB2R,DDR,DDI,NB1)
C
C     ---------------------------------------------------
C     MULTIPLY WITH COMPLEX CONJUGATE TO GET [@.P]+ [@.P]
C     ---------------------------------------------------
C
      CALL PMCMMT(NB2R,NB2R,DDR,DDI,NB1,NB2R,DDR,DDI,NB1,NB1,CCR,CCI)
      DIFFT = 0.D0
      SUMA = 0.D0
      SUMC = 0.D0
      L = 0
      IF (NB1.EQ.0) THEN
         IF (NB2.NE.0.and.mpirnk.eq.0) WRITE (*,1050) IRP
         GOTO 200
      ENDIF
      IF (IKINBL.GE.1.and.mpirnk.eq.0) PRINT*
      DO 111 I = 1, NB1
      DO 110 J = 1, I
      L = L + 1
      DIFFR = AAR (L) - 0.5D0 * CCR (L)
      DIFFI = AAI (L) - 0.5D0 * CCI (L)
      DIFF = DIFFR * DIFFR + DIFFI * DIFFI
      DIFFT = DIFFT + DIFF
#if defined (BIT64)
      DIFF = SQRT (DIFF)
#else
      DIFF = DSQRT (DIFF)
#endif
      SUMA = SUMA + AAR (L) * AAR (L) + AAI (L) * AAI (L)
#if defined (BIT64)
      SUM = SQRT (AAR (L) * AAR (L) + AAI (L) * AAI (L))
#else
      SUM = DSQRT (AAR (L) * AAR (L) + AAI (L) * AAI (L))
#endif
 110  CONTINUE
      DIFF = DIFF / SUM
      IF (IKINBL.GE.1.and.mpirnk.eq.0) WRITE (*,1020) IRP,I,DIFF
 111  CONTINUE
#if defined (BIT64)
      DIFFT = SQRT ( DIFFT / SUMA )
#else
      DIFFT = DSQRT ( DIFFT / SUMA )
#endif
      if (mpirnk.eq.0) WRITE (*,1030) IRP,DIFFT
 200  CONTINUE
      REWIND 14
      RETURN
 1010 FORMAT (/1X,'REPR. ',I3,' HAS ',I3,' EIGENVALUE(S) BELOW '
     &,'TRESHOLD IN LARGE COMP. OVERLAP MATRIX :'//(7E10.2))
 1011 FORMAT (/1X,'REPR. ',I3,' HAS ',I3,' EIGENVALUE(S) BELOW '
     &,'TRESHOLD IN SMALL COMP. OVERLAP MATRIX :'//(7E10.2))
 1020 FORMAT (1X,'(',I2,', ',I2,'): <T-(@.P)+.(@.P)> = ', G8.2)
 1030 FORMAT (/1X,'TOTAL DIFFERENCE [T] - [@.P]+.[@.P] IN REPR.'
     &,I3,' IS ',G10.4)
 1050 FORMAT (/1X,'NO LARGE COMPONENT FUNCTIONS IN REPRESENTATION ',I3)
      END
