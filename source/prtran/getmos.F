C
C     =================
      SUBROUTINE GETMOS
C     =================
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE "molf.inc"
      INCLUDE "files.inc"
      INCLUDE "sym.inc"
      INCLUDE "inpt.inc"
      INCLUDE "vecs.inc"
C
      REAL*8 OCCP(N40)
      COMPLEX*16 VMO(N22*2,N40)
      COMPLEX*16 CTRANA(N10),CTRANB(N10)
C
      CHARACTER*80 TITLE,CARD,FORMAT
      CHARACTER*8 D0*10,T0
C
      COMPLEX*16 CDUM
C
      LOGICAL JDUM
      INTEGER IDUM(17)
C
C     ---------------------------------------------------------------
C     GET THE TRANSFORMATION COEFFICIENTS FOR THE BACK-TRANSFORMATION
C     ---------------------------------------------------------------
C
      OPEN(MFDSYMC,FILE=FNAME(1),FORM='UNFORMATTED',STATUS='OLD',
     +             ERR=10040)
      REWIND MFDSYMC
      ITB = INDORB(NSYMF+1)
      IF (ITB.GT.N10) CALL Q2JOB (3,'R.CTRAN','N10',ITB)
      DO 75 IT=1,NSYMF
        ITA=INDORB(IT)+1
        ITB=INDORB(IT+1)
        READ(MFDSYMC,END=10000) (CTRANA(I),CTRANB(I),I=ITA,ITB)
 75     CONTINUE
      CLOSE(MFDSYMC)
C
C     -------------------------------------------------------------
C     READ FORMATTED VECTORS FROM MFDVECA
C     -------------------------------------------------------------
C
      MFDVEC = MFDVECA
      OPEN(MFDVEC,FILE=FNAME(3),FORM='FORMATTED',
     +     ERR=10010)
      REWIND MFDVEC
C
      READ (MFDVEC,1000,END=10020,ERR=10020) TITLE
      READ (MFDVEC,1005,END=10020,ERR=10020) D0,T0
      READ (MFDVEC,1006,END=10020,ERR=10020) ICYCLS,JDUM,JDUM
      READ (MFDVEC,1007,END=10020,ERR=10020) ETOTAL,EPREV,EMDIF,
     +                                       WDIFP,TOTDIF,RDUM
      READ (MFDVEC,1000,END=10020) CARD
      READ (MFDVEC,1008,END=10020,ERR=10020) IDUM(1)
      READ (MFDVEC,1009,END=10020,ERR=10020) (IDUM(I+1),I=1,IDUM(1))
      READ (MFDVEC,1009,END=10020,ERR=10020) (IDUM(I+1),I=1,IDUM(1))
      WRITE(6,1010) FNAME(3),TITLE,D0,T0,ICYCLS,ETOTAL,EPREV,
     +      EMDIF,WDIFP,TOTDIF
C
      READ(CARD,1001,ERR=10020) FORMAT
      IF (FORMAT.EQ.' ') FORMAT='(6F22.16)'
C
      INBB=0
C
      IF (NSC.GT.N22) STOP 'NSC.GT.N22 IN GETMOS'
C
      IMO = 0
C
C     -------------------
      DO 140 IRP=1,NSYMRP
C     -------------------
C
        NB1=NBSYM1(IRP)
        IF (TWOC) THEN
          NBSM=NBSYM1(IRP)
        ELSE
          NBSM=NBSYM(IRP)
        ENDIF
        INBA=INBB+1
        INBB=INBB+NBSYM(IRP)
C
        NORP=NOC(IRP)
C
        IF ((NB1.GT.0).AND.IPRTNR(IRP).LE.1) THEN
           NSKIP=NSKIP1(IRP)
           READ (MFDVEC,1003,END=10020,ERR=10020) IDUMMY,NSPIN,IDUMMY
           NSKIP2(IRP)=NSPIN-NSKIP-NORP
           DO 145 MO=1,NSKIP
             READ (MFDVEC,1000,END=10020,ERR=10020) CARD
             READ (MFDVEC,FORMAT,END=10020,ERR=10020)
     +         (CDUM, I = 1, NBSM)
 145       CONTINUE
        ENDIF
C
C       ----------------
        DO 130 MO=1,NORP
C       ----------------
C
          IF (MO.GT.N40) STOP 'MO .GT. N40 IN GETMOS'
          IMO=IMORAS(MO,IRP)
C
C         ----------------------------------------
C         READ IN SYMMETRY ADAPTED SOLUTION VECTOR
C         ----------------------------------------
C
C            -----------------------------------------------------
C            IF THIS SUBREP IS THE PARTNER OF AN EARLIER SUBREP,
C            USE SAME VECTORS AS IN THE FIRST PARTNER (SO SKIPPING
C            VECTORS INSTEAD OF READING VECTORS)
C            -----------------------------------------------------
C
          IF (IPRTNR(IRP).LE.1) THEN
            READ(MFDVEC,1000,END=10020,ERR=10020) CARD
            READ(CARD,'(35X,G15.10)') OCCP(MO)
            READ (MFDVEC,FORMAT,END=10020,ERR=10020)
     +           (VMO (I,MO), I = 1, NBSM)
          ENDIF
          OCCMO(IMO) = OCCP(MO)
C
C         ----------------------
C         INITIALIZE LONG VECTOR
C         ----------------------
C
          DO 80 I=1,NSC
            QA(I,IMO)=0.0D0
            QB(I,IMO)=0.0D0
 80         CONTINUE
C
          INBBB=INBB
          IF (TWOC) INBBB=INBB-NBSYM2(IRP)
C
C         ------------------------
C         TRANSFORM TO LONG VECTOR
C         ------------------------
C
          DO 100 I=INBA,INBBB
            IT=KSYMF(I)
            IM=INDB(IT)
            INDV=NEM(IM)
            IND=INDORB(IT)+1
            KND=INDORB(IT+1)
            II=I-INBA+1
            DO 90 J=IND,KND
              JJ=J-IND+1
              QA(INDV+JJ,IMO)=QA(INDV+JJ,IMO)+CTRANA(J)*VMO(II,MO)
              QB(INDV+JJ,IMO)=QB(INDV+JJ,IMO)+CTRANB(J)*VMO(II,MO)
 90           CONTINUE
 100        CONTINUE
 130      CONTINUE
C
        IF (IPRTNR(IRP).LE.1.AND.NB1.GT.0) THEN
        DO 135 MO=1,NSKIP2(IRP)
           READ (MFDVEC,1000,END=10020,ERR=10020) CARD
           READ (MFDVEC,FORMAT,END=10020,ERR=10020)
     +          (CDUM, I = 1, NBSM)
 135    CONTINUE
        ENDIF
 140    CONTINUE
C
      CLOSE(MFDVEC)
C
      RETURN
 1000 FORMAT(A)
 1001 FORMAT(20A)
 1003 FORMAT(I4,26X,2I4)
 1005 FORMAT(2A10)
 1006 FORMAT(I4,3X,L1,3X,L1)
 1007 FORMAT(6G20.10)
 1008 FORMAT(I4)
 1009 FORMAT(16I4)
 1010 FORMAT(//' FORMATTED VECTORS FROM FILE ',A7//
     +         ' FILE HEADING : ',A/
     +         ' SCF RUN DATE : ',A10/
     +         ' SCF RUN TIME : ',A8//
     +         ' FILE INFORMATION: '/
     +         ' CYCLE COUNT ',T25,I20/
     +         ' ETOTAL',T25,G20.10/
     +         ' EPREV ',T25,G20.10/
     +         ' EMDIF',T25,G20.10/
     +         ' WDIF ',T25,G20.10/
     +         ' TOTDIF',T25,G20.10///)
10000 CALL Q2JOB(1,'END OF ERROR ON MFDSYMC',' ',0)
10010 CALL Q2JOB(1,'ERROR OPENING MFDVECA',' ',0)
10020 CALL Q2JOB(1,'END OR ERROR ON MFDVECA',' ',0)
10040 CALL Q2JOB(1,'ERROR OPENING MFDSYMC',' ',0)
      END
