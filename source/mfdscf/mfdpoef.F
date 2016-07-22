C
C     ===========================================================
      SUBROUTINE MFDPOEF (ALLR, ALLI, BLLR, BLLI, CR, CI, DR, DI,
     +                    IA, IB, IC, VR, VI)
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
C     REC  7 : V(SS)-2*C*C*S(SS) --OF-- T + V
C     REC  8 : Q (L) FOR THE FOCK MATRIX
C     REC  9 : Q (S) FOR THE FOCK MATRIX
C     REC 10 : KINETIC ENERGY T(LL)
C     REC 11 : NOT USED
C     REC 12 : NOT USED
C     REC 13 : Q (L) FOR THE DENSITY MATRIX
C     REC 14 : Q (S) FOR THE DENSITY MATRIX
C     REC 15 : AS RECORD 2 FOR NEXT REPRESENTATION
C     REC 16 : ETC.
C
C     NREC IS THE NUMBER OF RECORDS OF ONE-ELECTRON MATRIXELEMENTS FOR
C          ANY REPRESENTATION
C
      INCLUDE 'paracom.inc'
      INCLUDE 'general.inc'
      INCLUDE 'memory.inc'
C
      DIMENSION ALLR (MS, MS), ALLI (MS, MS)
      DIMENSION BLLR (NL, NL), BLLI (NL, NL)
      DIMENSION CR(MLS),CI(MLS),DR(MSS+MS),DI(MSS+MS)
      DIMENSION D(N22*2),E(N22*2)
      REAL*8 VR(*), VI(*)
C
      REAL*8 DL(N22*2),DS(N22*2)
      REAL*8 TEMPE(N22*2)
      CHARACTER*80 TITLE, FMT
C
      IF (NNL .LT. NL) CALL Q2JOB(3,'POEF','NNL',NL)
      IF (NNS .LT. NS) CALL Q2JOB(3,'POEF','NNS',NS)
      IF (NS*NS .GT. LENGTH) CALL Q2JOB(3,'POEF','LENGTH',NS*NS)
C
      NQ2 = NEQUIV -NQ1
      LENREC =8 * (    MSS + 1)
      OPEN(MFDOEMR,FILE=FNAM(1),ACCESS='DIRECT',RECL=LENREC,
     $     FORM='UNFORMATTED')
      OPEN(MFDOEMI,FILE=FNAM(6),ACCESS='DIRECT',RECL=LENREC,
     $     FORM='UNFORMATTED')
C
      OPEN (LVECF, FILE = FNAM (5), FORM = 'FORMATTED')
      REWIND (LVECF)
      READ (LVECF, 1000, END = 600, ERR = 600) TITLE
      WRITE (6, 1010) TITLE
      READ (LVECF, 1000, END = 600, ERR = 600) TITLE
      READ (LVECF, 1001, END = 600, ERR = 600) FMT
 1001 FORMAT(20A)
      OVLSLCT = .TRUE.
      GOTO 610
 1000 FORMAT (A)
 1010 FORMAT (//1X,'OPEN SHELL SELECTION BY OVERLAP; TITLE OF FILE:',
     +        /1X,A80//)
C
 600  CONTINUE
      CLOSE(LVECF,STATUS='DELETE')
      OVLSLCT = .FALSE.
 610  CONTINUE
C
      LRP=0
C
C     ======================
      DO 100 IRP = 1, NSYMRP
C     ======================
C
      IF (ABS(IKRIRP(IRP)).EQ.IRP) THEN
c     IF (IAVIRP(IRP) .EQ. IRP) THEN
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
      IF (TWOC .AND. (NB1. GT. N22)) CALL Q2JOB(3,
     &   'POEF','N22',NB1)   
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
C     ------------------------------
C     LARGE COMPONENT KINETIC ENERGY
C     ------------------------------
C
      READ (MFDONE) ((BLLR(I,J),BLLI(I,J),J=1,I),I=1,NB1)
      WRITE(MFDOEMR,REC=IREC+10)
     +      ((BLLR(I,J),J=1,I),I=1,NB1)
      WRITE(MFDOEMI,REC=IREC+10)
     +      ((BLLI(I,J),J=1,I),I=1,NB1)
C
      IF (NB1.EQ.0) CALL Q2JOB(6,'NB1.EQ.0 IN MFDPOEF',' ',1)
C
C     ------------------------------------
C     DIAGONALIZE LARGE COMPONENT S-MATRIX
C     ------------------------------------
C
      CALL CRDIAG (MS,NB1,ALLR,ALLI,DL,DR,DI,TEMPE,TEMPE,E,IFAIL)
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
      WRITE (MFDOEMR, REC = IREC + 13)
     +  ((ALLR(I,J)/D(J), I=1,NB1), J=NB1-NBLR(IRP)+1, NB1)
      WRITE (MFDOEMI, REC = IREC + 13)
     +  ((ALLI(I,J)/D(J), I=1,NB1), J=NB1-NBLR(IRP)+1, NB1)
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
      DO 40 I=1,NBSSH*2,2
       DR(I)=DR(I)-TWOCSQ*DR(I+1)
       DI(I)=DI(I)-TWOCSQ*DI(I+1)
   40 CONTINUE
C
      WRITE(MFDOEMR,REC= 7+IREC) (DR(I),I=1,NBSSH*2,2)
      WRITE(MFDOEMI,REC= 7+IREC) (DI(I),I=1,NBSSH*2,2)
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
      CALL CRDIAG (MS,NB2,ALLR,ALLI,DS,DR,DI,TEMPE,TEMPE,E,IFAIL)
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
      WRITE (MFDOEMR, REC = IREC + 14)
     +  ((ALLR(I,J)/D(J), I = 1, NB2), J = NB2-NBSR(IRP)+1, NB2)
      WRITE (MFDOEMI, REC = IREC + 14)
     +  ((ALLI(I,J)/D(J), I = 1, NB2), J = NB2-NBSR(IRP)+1, NB2)
C
      IF (RSKINB) CALL RESKB (MFDOEMR,MFDOEMI,IREC,NB1,NB2,NBLR(IRP),
     &   NBSR(IRP),C,ALLR,ALLI,DR,DI,VR,VI,TEMPE,E)                    
C
C     ---------------------------------------------------
C     MAKE 1-ELECTRON MATRIX FOR 2-COMPONENT CALCULATIONS
C     ---------------------------------------------------
C
      IF (TWOC) THEN
C
      READ (MFDOEMR, REC = IREC + 4)
     +     ((BLLR (I, J), J = 1, I), I = 1, NB1)
      READ (MFDOEMI, REC = IREC + 4)
     +     ((BLLI (I, J), J = 1, I), I = 1, NB1)
C
      READ (MFDOEMR, REC = IREC + 10)
     +     ((ALLR (I, J), J = 1, I), I = 1, NB1)
      READ (MFDOEMI, REC = IREC + 10)
     +     ((ALLI (I, J), J = 1, I), I = 1, NB1)
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
C
      NBSR(IRP) = 0
C
      ENDIF 
C
C
      ELSE
C
C === FORCE TIME REVERSAL SYMMETRY ON THE ONE ELECTRON LEVEL
C
      IREC = (IRP - 1) * NREC
      IRECP = (ABS(IKRIRP(IRP)) - 1) * NREC
      ISKR = SIGN(1,IKRIRP(IRP))
      ICOPL = LENREC / 8
      DO 950 I = 2, NREC
      READ(MFDOEMR,REC=IRECP+I) (DR(J),J=1,ICOPL)
      READ(MFDOEMI,REC=IRECP+I) (DI(J),J=1,ICOPL)
      WRITE(MFDOEMR,REC=IREC+I) (DR(J),J=1,ICOPL)
      WRITE(MFDOEMI,REC=IREC+I) (ISKR*DI(J),J=1,ICOPL)
  950 CONTINUE
      NBLR(IRP) = NBLR(ABS(IKRIRP(IRP)))
      NBSR(IRP) = NBSR(ABS(IKRIRP(IRP)))
      ENDIF
C
  100 CONTINUE
      CLOSE(MFDONE)
      RETURN
      END
