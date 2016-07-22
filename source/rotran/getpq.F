      LOGICAL FUNCTION GETPQ
     +       (IPQL,NINT,ITOP,NSCTR,IRDCF,G,INDG,RBUF,LBUF)
C
C ***************************************************************
C ***
C *** PURPOSE:  READ THE DIRECT ACCESS FILE COMPLETELY AND
C ***           PROVIDE THE CALLER WITH THE (RS|PQ) INTEGRALS
C ***           THE PQ-PAIR READ IS IN IPQL
C ***
C ***************************************************************
C ***        I/O VARIABLES:
C ***
C *** IPQL: contains label of actually read pq stream
C *** NINT: contains # of actually read integrals of this stream
C *** ITOP:  actual value of the top record in the DA file
C *** NSCTR:  leading dimension of the two dimensional label array INDG
C *** IRDCF : record data control field. every read record gets a -1
C *** G  : contains integral values of this stream
C *** LABR: index array with the corresponding R labels
C *** LABS: index array with the corresponding S labels
C *** RBUF : cache integral buffer (of NBUFSZ size)
C *** LBUF : cache buffer for packed labels !
C ***
C ***************************************************************

      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'files.inc'
      INCLUDE 'inpt.inc'
C
      INTEGER IPQL,NINT,ITOP,NSCTR
      INTEGER IRDCF(*)
      REAL*8 G(*),RBUF(*)
      INTEGER INDG(NSCTR,2),LBUF(*)

C *** LOCAL VARIABLES

      INTEGER IACT,INXT,IR,IS
      INTEGER IP,IQ,IT
      REAL*8 RI
      LOGICAL ISBRT

C
#if defined (BIT64)
      DATA MASK /Z"000000000000FFFF"/
#else
      DATA MASK /'0000FFFF'X/
#endif
#if defined (CRAY)
      ISHIFT (I, J) = SHIFT (I, J)
#else
      ISHIFT (I, J) = ISHFT (I, J)
#endif
C

C     ---------- enter record reading loop ----

      IACT=ITOP

      NINT=0
 30   READ(SRTPQIL,REC=IACT) IPQL,NT,INXT,
     +    (RBUF(IJ),IJ=1,NT),(LBUF(IJ),IJ=1,NT)

C **  check which type this record belongs to (Coulomb or Breit)
C **  if this is not the  appropriate record we skip it !

      IR=0.5D0+0.5D0*DSQRT(DBLE(8*IPQL-7))
      IS=IPQL-IR*(IR-1)/2
      ISBRT=.FALSE.
c **  Test below should only be true if Breit and TWOC=T
      IF(.NOT.TWOC.AND.(IR.GE.ISSTA).AND.(IS.LE.ILEND)) ISBRT=.TRUE.
      IF(   (BREIT.AND.(.NOT.ISBRT)) .OR.
     +      ((.NOT.BREIT).AND.ISBRT)      )  GOTO 35


C ****************************     unpack and transfer this record
C ****************************   applies only if it is the right type !
C ***  
C ***  
        DO IV=1,NT
          NINT=NINT+1
#if defined (BIT64)
          IP=AND(ISHIFT(LBUF(IV),-16),MASK)
          IQ=AND(LBUF(IV),MASK)
#else
          IP=IAND(ISHIFT(LBUF(IV),-16),MASK)
          IQ=IAND(LBUF(IV),MASK)
#endif
          INDG(NINT,1)=IP
          INDG(NINT,2)=IQ
          IF(IP.GT.IR.OR.(IP.EQ.IR.AND.IQ.GE.IS)) THEN
            IT=INTTYP(IP,IQ,IR,IS)
          ELSE
            IT=INTTYP(IR,IS,IP,IQ)
          ENDIF
          RI=RBUF(IV)
          IF (IT.EQ.1) THEN
            RI=RI*0.125D0
          ELSEIF (IT.EQ.2) THEN
            RI=RI*0.5D0
          ELSEIF (IT.EQ.3) THEN
            RI=RI*0.25D0
          ELSEIF (IT.EQ.4) THEN
            RI=RI*0.5D0
          ELSEIF (IT.LE.8) THEN
            RI=RI*0.5D0
          ENDIF
          IF(ISBRT) RI=-RI
          G(NINT)=RI
        ENDDO
C ***
C ***
C *****************************

 35   CONTINUE
      IRDCF(IACT)=-1
      IF(INXT.EQ.0) GOTO 40  !ADJUST ITOP AND LOOK FOR NEXT AVAILABLE RECORD
      IACT=INXT
      GOTO 30

 40   ITOP=ITOP-1
      IF(ITOP.EQ.0) GOTO 50    ! WE ARE THROUGH THE DA FILE, NO MORE PQ-PAIRS !
      IF(IRDCF(ITOP).EQ.-1) GOTO 40   ! THIS RECORD WAS ALREADY READ IN !
      IACT=ITOP
      GETPQ=.TRUE.
      RETURN


 50   GETPQ=.FALSE.
      RETURN

 8888 CONTINUE
      WRITE(*,*) 'READ ERROR ON FILE SRTPQIL.*'
C     CALL MPI_FINALIZE(IERR)
      STOP 1
 
      END


