      SUBROUTINE SORT1(IPASS,INDPQ,INDPQU,INDPQS,IPQMIN,IPQMAX,
     &                 IPSYM,NPQBUF,NPQSIZ,NG,
     &                 LBRCPQ,LABEL1,LABEL2,NBUCK,LBUCK,RINT,RBUCK)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE "files.inc"
      INCLUDE "inpt.inc"
C
      INTEGER IPQMIN(*),IPQMAX(*),INDPQ(*),INDPQU(*),INDPQS(*)
      INTEGER IPSYM(NSC,*)
      INTEGER LABEL1(NG),LABEL2(NG)
      INTEGER LBUCK(NPQSIZ,NPQBUF),NBUCK(NPQBUF),LBRCPQ(0:NPQBUF)
      REAL*8 RBUCK(NPQSIZ,NPQBUF),RINT(NG)
      INTEGER LREC, NB
      LOGICAL READY, GETTW
C
#if defined (BIT64) 
      DATA MASK /Z"000000000000FFFF"/
#else
      DATA MASK /'0000FFFF'X/
#endif
C
      SAVE MASK
C
#if defined (CRAY)
      ISHIFT (I, J) = SHIFT (I, J)
#else
      ISHIFT (I, J) = ISHFT (I, J)
#endif
C
      MINPQ = IPQMIN(IPASS)
      MAXPQ = IPQMAX(IPASS)
      INPQMIN = INDPQ(MINPQ)
      LREC = 0
C
C     CREATE DIRECT ACCESS FILE FOR SORTED INTEGRALS
C     EACH RECORDS CONTAINS NPQSIZ INTEGRALS PLUS LABELS AND A POINTER
C     TO THE PREVIOUS RECORD OF THIS PQ
C
#if defined (BIT64) 
      IRECL = 8 + 8 + NPQSIZ*8 + NPQSIZ*8
#else
      IRECL = 4 + 4 + NPQSIZ*8 + NPQSIZ*4
#endif
      OPEN (INTSRT,FILE=FNAME(1),ACCESS='DIRECT',RECL=IRECL)
C
      LBRCPQ(0)=0
      DO I=1,NPQBUF
         NBUCK(I)=0
         LBRCPQ(I)=0
      ENDDO
C
      IFILMN=1
      IFILMX=MFIL
      IF (BREIT) IFILMN=4
      IF (BREIT) IFILMX=4
C
C     -------------------------
      DO 10 IFILE=IFILMN,IFILMX
C     -------------------------
C
      OPEN(SRTTW(IFILE),FILE=FNAME(IFILE+1),FORM='UNFORMATTED',
     +    ERR=999,STATUS='OLD')
      REWIND(SRTTW(IFILE))
      OPEN(SRTLB(IFILE),FILE=FNAME(IFILE+5),FORM='UNFORMATTED',
     +    ERR=999,STATUS='OLD')
      REWIND(SRTLB(IFILE))
C
 20   READY=GETTW(IFILE,SRTTW(IFILE),SRTLB(IFILE),NINT,RINT,
     &            LABEL1,LABEL2,NG)
C
C     --------------
      DO 30 I=1,NINT
C     --------------
C
#if defined (BIT64) 
            IP=AND(ISHIFT(LABEL1(I),16),MASK)
            IQ=AND(ISHIFT(LABEL1(I),32),MASK)
#else
            IP=IAND(ISHIFT(LABEL1(I),-16),MASK)
            IQ=IAND(LABEL1(I),MASK)
#endif
            IPQ=IP*(IP-1)/2+IQ
C
C Apply symmetry operation to map to list of unique pq's
C
            IPQU = INDPQU(IPQ)
C
C Check whether we handle this integrals in this pass
C
            INPQ=INDPQ(IPQU)
            IF ((IPQU.LT.MINPQ).OR.(IPQU.GT.MAXPQ)) GOTO 30
            IF (INPQ.EQ.0) GOTO 30
C
C We need to rewrite the other labels since we rotated the integral
C
            ISYMOP = INDPQS(IPQ)
#if defined (BIT64) 
            IR=AND(ISHIFT(LABEL1(I),48),MASK)
            IS=AND(LABEL1(I),MASK)
#else
            IR=IAND(ISHIFT(LABEL2(I),-16),MASK)
            IS=IAND(LABEL2(I),MASK)
#endif
            IRT = IPSYM(IR,ABS(ISYMOP))
            IST = IPSYM(IS,ABS(ISYMOP))
            IF (IRT*IST*ISYMOP.LT.0) RINT(I) = - RINT(I)
            IRTA = ABS(IRT)
            ISTA = ABS(IST)
            IRT = MAX(IRTA,ISTA)
            IST = MIN(IRTA,ISTA)
#if defined (BIT64) 
            LABEL1(I) = ISHIFT (IRT,16)
            LABEL1(I) = OR (IST,LABEL1(I))
#else
            LABEL2(I) = ISHIFT (IRT,16)
            LABEL2(I) = IOR (IST,LABEL2(I))
#endif
            IPQ = IPQU
C
C Now the integrals belong to a unique pq block : sort into buffers
C
            JND=INPQ-INPQMIN+1
            NB=NBUCK(JND)+1
            RBUCK(NB,JND)=RINT(I)
#if defined (BIT64) 
            LBUCK(NB,JND)=LABEL1(I)
#else
            LBUCK(NB,JND)=LABEL2(I)
#endif
            IF (NB.EQ.NPQSIZ) THEN
              LREC=LBRCPQ(0)+1
              WRITE(INTSRT,REC=LREC) NB,LBRCPQ(JND),
     +                               (RBUCK(J,JND),J=1,NB),
     +                               (LBUCK(J,JND),J=1,NB)
C
              LBRCPQ(0)=LREC
              LBRCPQ(JND)=LREC
              NB=0
            ENDIF
            NBUCK(JND)=NB
 30         CONTINUE
          IF (.NOT.READY) GOTO 20
          CLOSE (SRTTW(IFILE))
          CLOSE (SRTLB(IFILE))
 10       CONTINUE
C
        DO 60 IPQ=MINPQ,MAXPQ
          INPQ=INDPQ(IPQ)
          IF (INPQ.EQ.0) GOTO 60
          JND=INPQ-INPQMIN+1
          NB=NBUCK(JND)
          IF (NB.EQ.0) GOTO 60
          LREC=LBRCPQ(0)+1
          WRITE(INTSRT,REC=LREC) NB,LBRCPQ(JND),
     +                           (RBUCK(J,JND),J=1,NB),
     +                           (LBUCK(J,JND),J=1,NB)
C
          LBRCPQ(0)=LREC
          LBRCPQ(JND)=LREC
          NBUCK(JND)=0
 60       CONTINUE
C
      CLOSE (INTSRT,STATUS='KEEP')
      OPEN (INTSRT,ACCESS='DIRECT',RECL=IRECL,FILE=FNAME(1))
C
      RETURN
 999  CALL Q2JOB(1,'UNABLE TO OPEN SRT FILE',' ',0)
      RETURN
      END
