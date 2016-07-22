C
C     ================
      SUBROUTINE POPAN
C     ================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     ===========================================
C     Calculate necessary population quantities :
C     Net, Overlap and Gross populations.
C     ===========================================
C
      INCLUDE 'paracom.inc'
C
      DIMENSION VMOR (2*N25), VMOI (2*N25)
      DIMENSION OVLR (N25,N25), OVLI (N25,N25)
      DIMENSION OVSR (N25,N25), OVSI (N25,N25)
      DIMENSION QOORB(N7,N7),QOREP(N7,N7),QOTOT(N7,N7)
      DIMENSION QGORB(N7),QGREP(N7),QGTOT(N7)
      CHARACTER FORMAT*80,CARD*80
C
      DO 10 I = 1, N7
        QGTOT(I) = 0.D0
        DO 9  J = 1, N7
          QOTOT(I,J) = 0.D0
    9     CONTINUE
   10   CONTINUE
C
      OPEN (MFDONE, FILE = 'MFDONEE', FORM = 'UNFORMATTED')
      OPEN (MFDVECA, FILE = FILENAM, FORM = 'FORMATTED',ERR=10020)
      REWIND MFDVECA
      READ (MFDVECA, *,END=10020)
      READ (MFDVECA, *,END=10020)
      READ (MFDVECA, *,END=10020)
      READ (MFDVECA, *,END=10020)
      READ(MFDVECA,1000,END=10020) CARD
 1000 FORMAT(A)
      READ(CARD,1001,ERR=10020) FORMAT
 1001 FORMAT(20A)
      IF (FORMAT.EQ.' ') FORMAT='(6F22.16)'
      READ (MFDVECA,*,END=10020)
      READ (MFDVECA,*,END=10020)
      READ (MFDVECA,*,END=10020)
C
C     Loop over unique representations only.
C
C     -------------------------------
      DO 500 IRP = 1 , NSYMRP
      IF (IPRTNR(IRP).NE.1) GOTO 500
C     ------------------------------
C
      DO 70 I =1 ,N7
         QGREP(I) = 0.D0
         DO 69 J = 1, N7
            QOREP(I,J) = 0.D0
  69        CONTINUE
  70     CONTINUE
C
      NB1 = NBSYM1(IRP)
      IF (NB1.GT.N25) CALL Q2JOB(3,'POPAN','N25',NB1)
      NB2 = NBSYM2(IRP)
      IF (NB2.GT.N25) CALL Q2JOB(3,'POPAN','N25',NB2)
      NB = NBSYM(IRP)
      IF (TWOC) NB = NBSYM1(IRP)
C
C     Read Overlap matrices from file
C
      REWIND (MFDONE)
      NSKR = 6
      DO 100 I = 1, 11 + (IRP - 1) * NSKR
        READ (MFDONE)
  100   CONTINUE
      READ (MFDONE) ((OVLR(I,J),OVLI(I,J),J=1,I),I=1,NB1)
C
      IF (.NOT.TWOC) THEN
         READ (MFDONE)
         READ (MFDONE)
         READ (MFDONE)
         READ (MFDONE) ((DUM,DUM,OVSR(I,J),OVSI(I,J),J=1,I),I=1,NB2)
      ENDIF
C
      NV=NVAL(IRP)+NSKIP1(IRP)+NSKIP2(IRP)
      IF (NV.GT.0) THEN
         READ (MFDVECA,1000,END=10020,ERR=10020) CARD
         READ(CARD,1002) IDUMMY,NSPINOR,IDUMMY
      ELSE
         GOTO 500
      ENDIF
 1002 FORMAT(I4,26X,2I4)
      IF (NSPINOR.LT.NVAL(IRP)+NSKIP1(IRP)) THEN
         STOP 'I find less spinors then you want to analyze !'
      ENDIF
      NSKIP2(IRP)=NSPINOR-NSKIP1(IRP)-NVAL(IRP)
      IF (NVAL(IRP).NE.0) WRITE (6,'(/A26)') CARD(1:26)
C
C     -------------------------
      DO 110 MO = 1, NSKIP1(IRP)
C     -------------------------
C
        READ(MFDVECA,1000,END=10020,ERR=10020) CARD
        READ (MFDVECA,FORMAT,END=10020,ERR=10020)
     +       (VMOR (I), VMOI (I), I = 1, NB)
 110    CONTINUE
C
C     --------------------------
      DO 200 MO = 1 , NVAL (IRP)
C     --------------------------
C
      DO 120 I = 1, N7
         QGORB(I) = 0.D0
         DO 119 J = 1, N7
            QOORB(I,J) = 0.D0
 119     CONTINUE
 120  CONTINUE
C
C     ---------------------------------------------------------------
C     READ IN SYMMETRY ADAPTED SOLUTION VECTOR, CALCULATE POPULATIONS
C     ---------------------------------------------------------------
C
      READ(MFDVECA,1211,END=10020,ERR=10020)
     +       NO, EPS, OCCUP, THECOP
 1211 FORMAT(4X,I2,4X,G20.10,5X,G15.10,8X,G20.10)
      READ (MFDVECA,FORMAT,END=10020,ERR=10020)
     +     (VMOR (I), VMOI (I), I = 1, NB)
C
      DO 150 I = 1, NB1
         IND = INDGR(I,IRP)
         DO 140 J = 1, I
            JND = INDGR(J,IRP)
            CONTR = 2.D0 *
     &      ((VMOR(I) * VMOR(J) + VMOI(I) * VMOI(J)) * OVLR(I,J) +
     &       (VMOI(I) * VMOR(J) - VMOR(I) * VMOI(J)) * OVLI(I,J))
              IF (I.EQ.J) CONTR = CONTR * 0.5D0
              QOORB(IND,JND) = QOORB(IND,JND) + CONTR
              IF (IND.NE.JND)
     &        QOORB(JND,IND) = QOORB(JND,IND) + CONTR
  140       CONTINUE
  150    CONTINUE
C
      IF (.NOT.TWOC) THEN
        DO 151 I = NB1 + 1, NB
           IND = INDGR(I,IRP)
           II = I - NB1
           DO 141 J = NB1 + 1, I
              JND = INDGR(J,IRP)
              JJ = J - NB1
              CONTR = 2.D0 *
     &        ((VMOR(I) * VMOR(J) + VMOI(I) * VMOI(J)) * OVSR(II,JJ) +
     &         (VMOR(I) * VMOI(J) - VMOI(I) * VMOR(J)) * OVSI(II,JJ))
              IF (I.EQ.J) CONTR = CONTR * 0.5D0
              QOORB(IND,JND) = QOORB(IND,JND) + CONTR
              IF (IND.NE.JND)
     &        QOORB(JND,IND) = QOORB(JND,IND) + CONTR
  141         CONTINUE
  151      CONTINUE
      ENDIF
C
      DO 180 I = 1, NGR(IRP)
         IND = KNDGR(I,IRP)
         QGORB(IND) = QOORB(IND,IND)
         DO 170 J = 1, NGR(IRP)
            JND = KNDGR(J,IRP)
            QGORB(IND) = QGORB(IND) + QOORB(IND,JND)
            QOREP(IND,JND) = QOREP(IND,JND) + QOORB(IND,JND) * OCCUP
  170       CONTINUE
         QGORB(IND) = QGORB(IND) * 0.5D0
         QGREP(IND) = QGREP(IND) + QGORB(IND) * OCCUP
  180    CONTINUE
C
      IF (PRTOP) THEN
         IF (PRTNET) THEN
           WRITE (6,1100) NO,EPS,OCCUP
           WRITE (6,1110) (GRLAB(KNDGR(I,IRP)),I=1,NGR(IRP))
           DO 190 I = 1, NGR(IRP)
              IND = KNDGR(I,IRP)
              WRITE (6,1120) GRLAB(IND),
     &                       (QOORB(IND,KNDGR(J,IRP)),J=1,NGR(IRP))
  190         CONTINUE
          ENDIF
         WRITE (6,1130)  NO,EPS,OCCUP
         WRITE (6,1110)  (GRLAB(KNDGR(I,IRP)),I=1,NGR(IRP))
         WRITE (6,1140)  (QGORB(KNDGR(I,IRP)),I=1,NGR(IRP))
      ENDIF
  200 CONTINUE
C
C
C     --------------------------
      DO 210 MO = 1, NSKIP2(IRP)
C     --------------------------
C
         READ (MFDVECA,1000,END=10020,ERR=10020) CARD
         READ (MFDVECA,FORMAT,END=10020,ERR=10020)
     +        (VMOR (I), VMOI (I), I = 1, NB)
  210    CONTINUE
C
      FACTOR = REAL(IDIM(IRP))
      DO 280 I = 1, NGR(IRP)
         IND = KNDGR(I,IRP)
         DO 270 J = 1, NGR(IRP)
            JND = KNDGR(J,IRP)
            QOTOT(IND,JND) = QOTOT(IND,JND) + QOREP(IND,JND) * FACTOR
  270       CONTINUE
         QGTOT(IND) = QGTOT(IND) + QGREP(IND) * FACTOR
  280    CONTINUE
C
      IF (PRTRP.AND.NVAL(IRP).NE.0) THEN
         IF (PRTNET) THEN
           WRITE (6,1101) IRP
           WRITE (6,1110) (GRLAB(KNDGR(I,IRP)),I=1,NGR(IRP))
           DO 290 I = 1, NGR(IRP)
              IND = KNDGR(I,IRP)
              WRITE (6,1120) GRLAB(IND),
     &                      (QOREP(IND,KNDGR(J,IRP)),J=1,NGR(IRP))
  290         CONTINUE
          ENDIF
         WRITE (6,1131)  IRP
         WRITE (6,1110)  (GRLAB(KNDGR(I,IRP)),I=1,NGR(IRP))
         WRITE (6,1140)  (QGREP(KNDGR(I,IRP)),I=1,NGR(IRP))
      ENDIF
  500 CONTINUE
      CLOSE (MFDVECA)
      IF (PRTTP) THEN
         IF (PRTNET) THEN
           WRITE (6,1102)
           WRITE (6,1110) (GRLAB(IND),IND=1,NLABEL)
           DO 590 IND = 1, NLABEL
              WRITE (*,1120) GRLAB(IND),(QOTOT(IND,JND),JND=1,NLABEL)
  590         CONTINUE
          ENDIF
         WRITE (6,1132)
         WRITE (6,1110)  (GRLAB(IND),IND=1,NLABEL)
         WRITE (6,1140)  (QGTOT(IND),IND=1,NLABEL)
      ENDIF
      QTOT = 0.D0
      DO 600 IND = 1, NLABEL
         QTOT = QTOT + QGTOT(IND)
  600    CONTINUE
      WRITE (6,1150) QTOT
C
      RETURN
 1100 FORMAT (/' Net and Overlap population orbital',i3,' energy ',
     &f12.6,' occupation ',f6.4)
 1101 FORMAT (/' Net and Overlap population representation',i3)
 1102 FORMAT (/' Total Net and Overlap population')
 1110 FORMAT (/8X,9(1X,A6,1X))
 1120 FORMAT (1X,A6,1X,9(F7.4,1X))
 1130 FORMAT (/' Gross population orbital',i3,' energy ',f12.6,
     &' occupation ',f6.4)
 1131 FORMAT (/' Gross population representation',i3)
 1132 FORMAT (/' Total Gross population')
 1140 FORMAT (8X,9(F7.4,1X))
 1150 FORMAT (/' Total number of electrons ',F14.8)
C
10020 CALL Q2JOB(1,'END OR ERROR ON VECTOR FILE',' ',0)
      END
