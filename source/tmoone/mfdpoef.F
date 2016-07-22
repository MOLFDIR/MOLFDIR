C
C     ===========================================================
      SUBROUTINE MFDPOEF (ALLR, ALLI, BLLR, BLLI, CR, CI, DR, DI,
     +                    IA, IB, IC)
C     ===========================================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     CREATE ACCORDING TO INPUT SPECIFICATION A ONE-ELECTRON FILE FOR
C     USE DURING THE SCF-CYCLES. POSSIBILITIES DIFFER IN TYPE OF MOD-
C     IFICATION OF THE DIRAC MATRIX-REPRESENTATION AND/OR VELOCITY OF
C     LIGHT TO BE USED. ALL INFORMATION NECCESARY EXISTS ON FILES
C     CREATED BY PROGRAM 'RELONEL'
C
C     THE NEW ONE ELECTRON FILE HAS THE NAME FNAM(1) (ONE ELECTRON
C     MATRIX ELEMENTS) AND IS A 'DIRECT ACCESS' FILE. THIS IS TO ENABLE
C     RANDOM USE OF THE ONE ELECTRON MATRICES
C     RECORDS ARE WRITTEN AS FOLLOWS :
C     REC  1 : IDENTITICATION RECORD; FIRST WORD IS THE RECORD LENGTH
C     REC  2 : OVERLAP MATRIX LARGE COMPONENT
C     REC  3 : OVERLAP MATRIX SMALL COMPONENT
C     REC  4 : POTENTIAL ENERGY (V) LARGE COMPONENT
C     REC  5 : POTENTIAL ENERGY (V) SMALL COMPONENT 
C     REC  6 : C*[@.P] 
C     REC  7 : V(SS)-2*C*C*V(SS) --OF-- T + V
C     REC  8 : Q (L) OR THE FOCK MATRIX
C     REC  9 : Q (S) OR THE FOCK MATRIX
C     REC 10 : KINETIC ENERGY T(LL)
C     REC 11 : NOT USED                           
C     REC 12 : NOT USED                        
C     REC 13 : Q (L) FOR THE DENSITY MATRIX
C     REC 14 : Q (S) FOR THE DENSITY MATRIX         
C     REC 15 : AS RECORD 2 FOR NEXT REPRESENTATION
C
C     NREC IS THE NUMBER OF RECORDS OF ONE-ELECTRON MATRIXELEMENTS FOR
C          ANY REPRESENTATION
C
      INCLUDE 'param.inc'
      INCLUDE 'common.inc'
C
      DIMENSION ALLR (MS, MS), ALLI (MS, MS)
      DIMENSION BLLR (NL, NL), BLLI (NL, NL)
      DIMENSION CR(MLS),CI(MLS),DR(MSS+MS),DI(MSS+MS)
      DIMENSION D(N22*2),E(N22*2)
      REAL*8 DL(N22*2),DS(N22*2)
      REAL*8 TEMPE(N22*2)
C
      NQ2 = NEQUIV -NQ1
      LENREC =8 * (    MSS + 1)
      OPEN(MFDOEMR,FILE=FNAM(1),ACCESS='DIRECT',RECL=LENREC,
     $     FORM='UNFORMATTED')
      OPEN(MFDOEMI,FILE=FNAM(5),ACCESS='DIRECT',RECL=LENREC,
     $     FORM='UNFORMATTED')
C
      OVLSLCT = .FALSE.
      LRP=0
      THRESH=1.0D-12
C
C     ======================
      DO 100 IRP = 1, NSYMRP
C     ======================
C
C     -----------------------------------------
C     SKIP THE RECORDS ON MFDONE FOR
C     THE UNOCCUPIED REPRESENTATIONS
C     -----------------------------------------
C
      REWIND (MFDONE)
      NSKIP = 6
      DO 10 I = 1, 11 + (INDSRP (IRP) - 1) * NSKIP
      READ (MFDONE)
 10   CONTINUE
      NB1=NBSYM1(IRP)
      NB2=NBSYM2(IRP)
      IF (TWOC .AND. (NB1. GT. N22)) 
     +   CALL Q2JOB(3, 'MFDPOEF','N22',NB1)
      NBLLH=NB1*(NB1+1)/2
      NBSSH=NB2*(NB2+1)/2
      NBLS =NB1*NB2
      IREC=(IRP-1)*NREC
C
C     --------------------------
C     LARGE/LARGE OVERLAP MATRIX
C     --------------------------
C
      READ (MFDONE) ((ALLR(I,J),ALLI(I,J),J=1,I),I=1,NB1)
C
      WRITE(MFDOEMR,REC= 2+IREC)
     +      ((ALLR (I, J), J = 1, I), I = 1, NB1)
      WRITE(MFDOEMI,REC= 2+IREC)
     +      ((ALLI (I, J), J = 1, I), I = 1, NB1)
C
      IF (TWOC) THEN
C
C     ------------------------------
C     LARGE COMPONENT KINETIC ENERGY
C     ------------------------------
C
      READ (MFDONE) ((BLLR(I,J),BLLI(I,J),J=1,I),I=1,NB1)
      WRITE(MFDOEMR,REC=IREC+10)
     +      ((BLLR(I,J),J=1,I),I=1,NB1)
      WRITE(MFDOEMI,REC=IREC+10)
     +      ((BLLI(I,J),J=1,I),I=1,NB1)
      ELSE
      READ (MFDONE)
      ENDIF
C
      IF (NB1.EQ.0) CALL Q2JOB(6, 'NB1.EQ.0 IN MFDPOEF',' ',0)
C
C     ------------------------------------
C     DIAGONALIZE LARGE COMPONENT S-MATRIX
C     ------------------------------------
C
      CALL CHDIAG (MS,NB1,ALLR,ALLI,DL,DR,DI,TEMPE,TEMPE,E,IFAIL)
C
      NBLR(IRP)=NB1
      DO 20 I=NB1,1,-1
        IF (DL(I).LT.THRESH) THEN
          NBLR(IRP)=NB1-I
          WRITE(6,2000) IRP, DL(I), I
 2000     FORMAT(1X,'WARNING: REP',I4,': S-EV ',
     +           G12.6,' LARGE BASIS REDUCED BY ',I4,' FIES')
          GOTO 25
        ENDIF
#if defined (BIT64)
        D(I)=1.0D0/SQRT(DL(I))
#else
        D(I)=1.0D0/DSQRT(DL(I))
#endif
   20   CONTINUE
   25 CONTINUE
C
      WRITE (MFDOEMR, REC = IREC + 8)
     +  ((ALLR(I,J)*D(J), I=1,NB1), J=NB1-NBLR(IRP)+1, NB1)
      WRITE (MFDOEMI, REC = IREC + 8)
     +  ((ALLI(I,J)*D(J), I=1,NB1), J=NB1-NBLR(IRP)+1, NB1)
C
C     --------------------------------------------
C     READ LARGE COMPONENT POTENTIAL ENERGY MATRIX
C     --------------------------------------------
C
      READ (MFDONE) ((ALLR(I,J),ALLI(I,J),J=1,I),I=1,NB1)
      WRITE(MFDOEMR,REC= 4+IREC)
     +      ((ALLR (I, J), J = 1, I), I = 1, NB1)
      WRITE(MFDOEMI,REC= 4+IREC)
     +      ((ALLI (I, J), J = 1, I), I = 1, NB1)
C
C     -----------------
C     READ [@.P] MATRIX
C     -----------------
C
      READ (MFDONE) (CR(I),CI(I),I=1,NBLS)
      IF (.NOT. TWOC) THEN
        DO 30 I=1,NBLS
          CR(I)=CR(I)*C
          CI(I)=CI(I)*C
   30   CONTINUE
      ENDIF
      WRITE(MFDOEMR,REC= 6+IREC) (CR(I),I=1,NBLS)
      WRITE(MFDOEMI,REC= 6+IREC) (CI(I),I=1,NBLS)
C
C     -----------------------------------------------------
C     READ OVERLAP AND POTENTIAL ENERGY FOR SMALL COMPONENT
C     -----------------------------------------------------
C
      READ (MFDONE) (DR(I),DI(I),DR(I+1),DI(I+1),I=1,NBSSH*2,2)
      WRITE(MFDOEMR,REC= 3+IREC) (DR(I+1),I=1,NBSSH*2,2)
      WRITE(MFDOEMI,REC= 3+IREC) (DI(I+1),I=1,NBSSH*2,2)
      WRITE(MFDOEMR,REC= 5+IREC) (DR(I),I=1,NBSSH*2,2)
      WRITE(MFDOEMI,REC= 5+IREC) (DI(I),I=1,NBSSH*2,2)
C
      IF (TWOC) THEN
        IND = -1
        DO 130 I = 1, NB2
          DO 140 J = 1, I
            IND = IND + 2
            ALLR (I, J) = DR (IND)
            ALLI (I, J) = DI (IND)
            ALLR (J, I) = DR (IND)
            ALLI (J, I) = - DI (IND)
 140      CONTINUE
 130    CONTINUE
      ELSE
       DO 40 I=1,NBSSH*2,2
        DR(I)=DR(I)-TWOCSQ*DR(I+1)
        DI(I)=DI(I)-TWOCSQ*DI(I+1)
   40   CONTINUE
C
       WRITE(MFDOEMR,REC= 7+IREC) (DR(I),I=1,NBSSH*2,2)
       WRITE(MFDOEMI,REC= 7+IREC) (DI(I),I=1,NBSSH*2,2)
      ENDIF
C
C     -----------
C     S(SS)**-1/2
C     -----------
C
      READ (MFDONE) (DR(I),DI(I),I=1,NBSSH)
      WRITE(MFDOEMR,REC= 9+IREC) ((DR( I*(I-1)/2 + J ),I=J,NB2),J=1,NB2)
      WRITE(MFDOEMI,REC= 9+IREC) ((DI( I*(I-1)/2 + J ),I=J,NB2),J=1,NB2)
C
C
      IF (TWOC) THEN
C
      READ (MFDOEMR, REC = IREC + 4)
     +     ((ALLR (I, J), J = 1, I), I = 1, NB1)
      READ (MFDOEMI, REC = IREC + 4)
     +     ((ALLI (I, J), J = 1, I), I = 1, NB1)
C
      READ (MFDOEMR, REC = IREC + 10)
     +     ((BLLR (I, J), J = 1, I), I = 1, NB1)
      READ (MFDOEMI, REC = IREC + 10)
     +     ((BLLI (I, J), J = 1, I), I = 1, NB1)
C
      DO 930 I = 1, NB1
      DO 940 J = 1, I
      BLLR (I, J) = BLLR (I, J) + ALLR (I, J)
      BLLI (I, J) = BLLI (I, J) + ALLI (I, J)
 940  CONTINUE
 930  CONTINUE
C
      WRITE (MFDOEMR, REC = IREC + 7)
     +      ((BLLR (I, J), J = 1, I), I = 1, NB1)
      WRITE (MFDOEMI, REC = IREC + 7)
     +      ((BLLI (I, J), J = 1, I), I = 1, NB1)
      ENDIF
C
C     ---------------
C     CALCULATE Q (S)
C     ---------------
C
      READ (MFDOEMR,REC= 3+IREC)
     +      ((ALLR (I, J), J = 1, I), I = 1, NB2)
      READ (MFDOEMI,REC= 3+IREC)
     +      ((ALLI (I, J), J = 1, I), I = 1, NB2)
C
      IF (NB2.GT.0) 
     &CALL CHDIAG (MS,NB2,ALLR,ALLI,DS,DR,DI,TEMPE,TEMPE,E,IFAIL)
C
      NBSR(IRP)=NB2
      DO 820 I=NB2,1,-1
        IF (DS(I).LT.THRESH) THEN
          NBSR(IRP)=NB2-I
          WRITE(6,2010) IRP, DS(I), I
 2010     FORMAT(1X,'WARNING: REP',I4,': S-EV ',
     +           G12.6,' SMALL BASIS REDUCED BY ',I4,' FIES')
          GOTO 825
        ENDIF
#if defined (BIT64)
        D(I)=1.0D0/SQRT(DS(I))
#else
        D(I)=1.0D0/DSQRT(DS(I))
#endif
  820   CONTINUE
  825 CONTINUE
C
      WRITE (MFDOEMR, REC = IREC + 9)
     +  ((ALLR(I,J)*D(J), I = 1, NB2), J = NB2-NBSR(IRP)+1, NB2)
      WRITE (MFDOEMI, REC = IREC + 9)
     +  ((ALLI(I,J)*D(J), I = 1, NB2), J = NB2-NBSR(IRP)+1, NB2)
C
C     ----------------------------------------------------
C     PREPARE BACK TRANSFORMATION MATRIX FOR USE IN SELVEC
C     ----------------------------------------------------
C
      NLR=NBLR(IRP)
      READ (MFDOEMR,REC= 8+IREC)
     +        ((ALLR (I, J), I = 1,NB1), J = 1, NLR)
      READ (MFDOEMI,REC= 8+IREC)
     +        ((ALLI (I, J), I = 1,NB1), J = 1, NLR)
      DO 730 I=1,NB1
        DO 735 J=1,NLR
          ALLR(I,J)=ALLR(I,J)*DL(J+NB1-NLR)
          ALLI(I,J)=ALLI(I,J)*DL(J+NB1-NLR)
 735      CONTINUE
 730    CONTINUE
      WRITE (MFDOEMR, REC = IREC + 13)
     +        ((ALLR (I, J), I = 1,NB1), J = 1, NLR)
      WRITE (MFDOEMI, REC = IREC + 13)
     +        ((ALLI (I, J), I = 1,NB1), J = 1, NLR)
C
  100 CONTINUE
      CLOSE(MFDONE)
      RETURN
      END
