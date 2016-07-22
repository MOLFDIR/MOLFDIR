C
C     ==================
      SUBROUTINE MFDFSLV(DAAR,DBAR,DAAI,DBAI,DOAAR,DOBAR,DOAAI,DOBAI,
     +                   DOACR,DOBCR,DOACI,DOBCI,FAAR,FBAR,FAAI,FBAI,
     +               FOAAR,FOBAR,FOAAI,FOBAI,FOACR,FOBCR,FOACI,FOBCI)
C     ==================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     BUILD THE SKELETON FOCK-MATRIX FROM LIST OF INTEGRALS AND LABELS
C
C     COMMON/DMT/: ON ENTRY CLOSED SHELL DENSITY MATRIX (DAAR)
C     COMMON/FMT/: ON EXIT CLOSED SHELL INCOMPLETE FOCK MATRIX (FAAR)
C                   OPEN SHELL NR.1
C     COMMON/DOMT/: ON ENTRY OPEN SHELL DENSITY MATRIX (DOAAR)
C     COMMON/FOMT/: ON EXIT OPEN SHELL INCOMPLETE FOCK MATRIX (FOAAR)
C                   OPEN SHELL NR.2
C     COMMON/DOMD/: ON ENTRY OPEN SHELL DENSITY MATRIX (DOACR)
C     COMMON/FOMD/: ON EXIT OPEN SHELL INCOMPLETE FOCK MATRIX (FOACR)
C
C     OPENSH: .TRUE. IF OPEN SHELL CALCULATION
C
      INCLUDE 'paracom.inc'
      INCLUDE 'general.inc'
      INCLUDE 'memory.inc'
      INCLUDE 'mpif.h'
      INCLUDE 'mpidef.inc'
C
      COMMON/INT/ RINT(N2)
C
      REAL*8 DAAR(*),DBAR(*),DAAI(*),DBAI(*)
      REAL*8 DOAAR(*),DOBAR(*),DOAAI(*),DOBAI(*)
      REAL*8 DOACR(*),DOBCR(*),DOACI(*),DOBCI(*)
      REAL*8 FAAR(*),FBAR(*),FAAI(*),FBAI(*)
      REAL*8 FOAAR(*),FOBAR(*),FOAAI(*),FOBAI(*)
      REAL*8 FOACR(*),FOBCR(*),FOACI(*),FOBCI(*)
C
      DIMENSION ILAB ( N2 ) , JLAB ( N2 ) , KLAB ( N2 ) , LLAB ( N2 )
      DIMENSION ISTAT(MPI_STATUS_SIZE)
      INTEGER LBUCKP(N2,14),LBUCKQ(N2,14),LBUCKR(N2,14),LBUCKS(N2,14)
      INTEGER NBUCK(14)
      REAL*8  RBUCK(N2,14)
      INTEGER LBUF(NPQSIZ)
      REAL*8  RBUF(NPQSIZ)
      LOGICAL ISOS
C     INTEGER UNITI,UNITL
C     CHARACTER*12 FILNMI,FILNML
      CHARACTER*7  FNAME1,FNAME2
      CHARACTER*12 FILNM1,FILNM2
      
C
#if defined (BIT64)
      DATA MASK /Z"000000000000FFFF"/
#else
      DATA MASK /"0000FFFF"X/
#endif
      SAVE MASK
#if defined (CRAY)
      ISHIFT (I, J) = SHIFT (I, J)
#else
      ISHIFT (I, J) = ISHFT (I, J)
#endif
C
      ISOS = OPENSH
C
C  ** CONSTRUCT L/I FILENAME FOR SPECIFIC NODE **
c
      IUNIT=SRTPQIL
      FNAME1=FNAM(10)
      FNAME2=FNAM(11)
      IF (mpirnk  .EQ. 0) THEN
         WRITE (FILNM1,'(A7)') FNAME1
         WRITE (FILNM2,'(A7)') FNAME2
      ELSEIF (mpirnk  .LT. 10) THEN
         WRITE (FILNM1,'(A7,A1,I1)') FNAME1,'.',mpirnk
         WRITE (FILNM2,'(A7,A1,I1)') FNAME2,'.',mpirnk
      ELSEIF (mpirnk  .LT. 100) THEN
         WRITE (FILNM1,'(A7,A1,I2)') FNAME1,'.',mpirnk
         WRITE (FILNM2,'(A7,A1,I2)') FNAME2,'.',mpirnk
      ELSE
         WRITE (FILNM1,'(A7,A1,I3)') FNAME1,'.',mpirnk
         WRITE (FILNM2,'(A7,A1,I3)') FNAME2,'.',mpirnk
      ENDIF

C
C     ----------------------------------
C     GET DENSITY MATRIX/CES FROM MASTER
C     ----------------------------------
C
      CALL MPI_BCAST(DAAR,LENGTH,MPI_DOUBLE_PRECISION,0,
     &               MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(DAAI,LENGTH,MPI_DOUBLE_PRECISION,0,
     &               MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(DBAR,LENGTH,MPI_DOUBLE_PRECISION,0,
     &               MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(DBAI,LENGTH,MPI_DOUBLE_PRECISION,0,
     &               MPI_COMM_WORLD,IERR)
      IF (ISOS) THEN
          CALL MPI_BCAST(DOAAR,LENGTH,MPI_DOUBLE_PRECISION,0,
     &                   MPI_COMM_WORLD,IERR)
          CALL MPI_BCAST(DOAAI,LENGTH,MPI_DOUBLE_PRECISION,0,
     &                   MPI_COMM_WORLD,IERR)
          CALL MPI_BCAST(DOBAR,LENGTH,MPI_DOUBLE_PRECISION,0,
     &                   MPI_COMM_WORLD,IERR)
          CALL MPI_BCAST(DOBAI,LENGTH,MPI_DOUBLE_PRECISION,0,
     &                   MPI_COMM_WORLD,IERR)
      ENDIF
      IF (ISOS.AND.NOPEN.GT.1) THEN
          CALL MPI_BCAST(DOACR,LENGTH,MPI_DOUBLE_PRECISION,0,
     &                   MPI_COMM_WORLD,IERR)
          CALL MPI_BCAST(DOACI,LENGTH,MPI_DOUBLE_PRECISION,0,
     &                   MPI_COMM_WORLD,IERR)
          CALL MPI_BCAST(DOBCR,LENGTH,MPI_DOUBLE_PRECISION,0,
     &                   MPI_COMM_WORLD,IERR)
          CALL MPI_BCAST(DOBCI,LENGTH,MPI_DOUBLE_PRECISION,0,
     &                   MPI_COMM_WORLD,IERR)
      ENDIF
C
C     -------------------------------
C     INITIALIZE FOCK MATRIX ELEMENTS
C     -------------------------------
C
      DO 5 IV = 1, LENGTH 
        FAAR (IV) = 0.0D0
        FAAI (IV) = 0.0D0
        FBAR (IV) = 0.0D0
        FBAI (IV) = 0.0D0
        FOAAR (IV) = 0.0D0
        FOAAI (IV) = 0.0D0
        FOBAR (IV) = 0.0D0
        FOBAI (IV) = 0.0D0
    5 CONTINUE
      IF (ISOS.AND.NOPEN.GT.1) THEN
      DO 6 IV = 1, LENGTH
        FOACR (IV) = 0.0D0
        FOACI (IV) = 0.0D0
        FOBCR (IV) = 0.0D0
        FOBCI (IV) = 0.0D0
    6 CONTINUE
      ENDIF
C

C     ---------------------------------------------------------------
C     --- FROM HERE READ THE DIRECT ACCESS FILE AND PREPARE EVERYTHING
C     --- FOR THE COMPUTATION OF THE PARTIAL FOCK MATRICES. ALSO CREATE
C     --- INTTYP BATCHES !
C     ---------------------------------------------------------------
C
C     --- GET DA-CONTROL-FILE CHARACTERISTICS
C
      OPEN(UNIT=IUNIT,FILE=FILNM2,ACCESS='SEQUENTIAL',
     +   STATUS='UNKNOWN',FORM='UNFORMATTED')
      READ(IUNIT) LRECCO,IRECLT,ILSTA,ILEND,ISSTA,ISEND
      CLOSE(IUNIT)
#if defined (BIT64)
      IRECL=8 + 8 + 8 + NPQSIZ*8 + NPQSIZ*8
#else
      IRECL=4 + 4 + 4 + NPQSIZ*8 + NPQSIZ*4
#endif
      IF(IRECL.NE.IRECLT) THEN
        WRITE(*,*) 'NON-MATCHING RECORD LENGTH IN DA-FILE !'
        CALL MPI_FINALIZE(IERR)
        STOP 1
      ENDIF
C
C     --- OPEN DA FILE

      OPEN(UNIT=IUNIT,FILE=FILNM1,ACCESS='DIRECT',RECL=IRECL)

C     --- CLEAR THE INTTYPE BUCKETS
      DO I=1,14
        NBUCK(I)=0
      ENDDO

      DO 1000 LREC=1,LRECCO
C ** read in a record
        READ(IUNIT,REC=LREC,ERR=8888) IPQL,NT,IDUM,
     +         (RBUF(IJ),IJ=1,NT),(LBUF(IJ),IJ=1,NT)
C ** unpack, check for (SL/SL) range, make canonical and distribute
        IRL=0.5D0+0.5D0*DSQRT(DBLE(8*IPQL-7))
        ISL=IPQL-IRL*(IRL-1)/2
c ** if this is a relativistic calculation, then...
c ** if this integral block is of breit type, read next record
        IF(.NOT.TWOC.AND.(IRL.GE.ISSTA).AND.(ISL.LE.ILEND)) GOTO 1000
C ** treat record, make sure no upper variables are changed !!!
        DO I=1,NT
#if defined (BIT64)
          IPL = AND (ISHIFT(LBUF(I),-16), MASK)
          IQL = AND (LBUF(I), MASK)
#else
          IPL = IAND (ISHIFT(LBUF(I),-16), MASK)
          IQL = IAND (LBUF(I), MASK)
#endif
          IF(IPL.GT.IRL.OR.(IPL.EQ.IRL.AND.IQL.GE.ISL)) THEN
            IPLM=IPL
            IQLM=IQL
            IRLM=IRL
            ISLM=ISL
          ELSE
            IPLM=IRL
            IQLM=ISL
            IRLM=IPL
            ISLM=IQL
          ENDIF
 17       ITT=INTTYP(IPLM,IQLM,IRLM,ISLM)
C         write(*,*) ' ---> ',iplM,iqlM,irlM,islM
          INDX=NBUCK(ITT)+1
          LBUCKP(INDX,ITT)=IPLM
          LBUCKQ(INDX,ITT)=IQLM
          LBUCKR(INDX,ITT)=IRLM
          LBUCKS(INDX,ITT)=ISLM
          RBUCK(INDX,ITT)=RBUF(I)
C         write(88,'(4I4,E25.16)') iplM,iqlM,irlM,islM,rbuf(i)
          NBUCK(ITT)=INDX
        ENDDO 
C
C ** check if in every bucket there is enough space for the next record of
C ** length NPQSIZ (= maximum increment) otherwise flush corresponding buckets
C ** by calculating corresponding fock matrix elements.
C
        DO 950 IT=1,14
          NT=NBUCK(IT)
          IF( (NT+NPQSIZ).GT.N2) THEN   ! THIS BUCKET WILL OVERFLOW, FLUSH !

            DO IV = 1, NT
              ILAB (IV) = LBUCKP(IV,IT)
              JLAB (IV) = LBUCKQ(IV,IT)
              KLAB (IV) = LBUCKR(IV,IT)
              LLAB (IV) = LBUCKS(IV,IT)
              RINT (IV) = RBUCK (IV,IT)
            ENDDO
            CALL FOCK (IT,NT,ILAB,JLAB,KLAB,LLAB,RINT,
     +            DAAR,DBAR,DAAI,DBAI,
     +            FAAR,FBAR,FAAI,FBAI,FAAR)
C
            IF (ISOS) THEN
            CALL FOCK (IT,NT,ILAB,JLAB,KLAB,LLAB,RINT,
     +              DOAAR,DOBAR,DOAAI,DOBAI,
     +              FOAAR,FOBAR,FOAAI,FOBAI,FOAAR)
            IF (NOPEN.GT.1) CALL FOCK (IT,NT,ILAB,JLAB,KLAB,
     +                  LLAB,RINT,DOACR,DOBCR,DOACI,DOBCI,
     +                  FOACR,FOBCR,FOACI,FOBCI,FOACR)
            ENDIF  !(ISOS)
            NBUCK(IT)=0
          ENDIF
 950    CONTINUE

C **  treat next record
 1000 CONTINUE
C ** all records are read. we are ready for the final flush

C     CLOSE(88)


C     -------------------------------------
C     ---  FINAL FLUSH OF ALL BUCKETS   ---
C     -------------------------------------
C

      DO 100 IT=1,14

        NT=NBUCK(IT)
        IF(NT.EQ.0) GOTO 100

        DO IV = 1, NT
          ILAB (IV) = LBUCKP(IV,IT)
          JLAB (IV) = LBUCKQ(IV,IT)
          KLAB (IV) = LBUCKR(IV,IT)
          LLAB (IV) = LBUCKS(IV,IT)
          RINT (IV) = RBUCK (IV,IT)
        ENDDO
C
C     --------------------------------------------------------------------
C     DISTRIBUTION OF THIS BATCH OF INTEGRALS OVER THE SKELETON MATRIX/CES
C     --------------------------------------------------------------------
C
      CALL FOCK (IT,NT,ILAB,JLAB,KLAB,LLAB,RINT,
     +           DAAR,DBAR,DAAI,DBAI,
     +           FAAR,FBAR,FAAI,FBAI,FAAR)
C
      IF (ISOS) THEN
      CALL FOCK (IT,NT,ILAB,JLAB,KLAB,LLAB,RINT,
     +             DOAAR,DOBAR,DOAAI,DOBAI,
     +             FOAAR,FOBAR,FOAAI,FOBAI,FOAAR)
      IF (NOPEN.GT.1) CALL FOCK (IT,NT,ILAB,JLAB,KLAB,
     +                 LLAB,RINT,DOACR,DOBCR,DOACI,DOBCI,
     +                 FOACR,FOBCR,FOACI,FOBCI,FOACR)
      ENDIF  !(ISOS)

 100  CONTINUE

C     WRITE(*,*) 'NODE ',mpirnk,' HAS FINISHED DA FILE (FOCK).'

C
C     ------------------------------------------------
C     ALL DONE REPORT TO MASTER AND SEND FOCK MATRICES
C     ------------------------------------------------
      CALL MPI_SEND(mpirnk,1,MPI_INTEGER,0,18,
     &              MPI_COMM_WORLD,IERR)
      CALL MPI_RECV(ITEST,1,MPI_INTEGER,0,19,
     &              MPI_COMM_WORLD,ISTAT,IERR)
      IF (ITEST.EQ.3.AND..NOT.BREIT) THEN
      CALL MPI_REDUCE(FAAR,DAAR,LENGTH,MPI_DOUBLE_PRECISION,
     &                MPI_SUM,0,MPI_COMM_WORLD,IERR)
      CALL MPI_REDUCE(FAAI,DAAI,LENGTH,MPI_DOUBLE_PRECISION,
     &                MPI_SUM,0,MPI_COMM_WORLD,IERR)
      CALL MPI_REDUCE(FBAR,DBAR,LENGTH,MPI_DOUBLE_PRECISION,
     &                MPI_SUM,0,MPI_COMM_WORLD,IERR)
      CALL MPI_REDUCE(FBAI,DBAI,LENGTH,MPI_DOUBLE_PRECISION,
     &                MPI_SUM,0,MPI_COMM_WORLD,IERR)
      IF (ISOS) THEN
      CALL MPI_REDUCE(FOAAR,DOAAR,LENGTH,MPI_DOUBLE_PRECISION,
     &                MPI_SUM,0,MPI_COMM_WORLD,IERR)
      CALL MPI_REDUCE(FOAAI,DOAAI,LENGTH,MPI_DOUBLE_PRECISION,
     &                MPI_SUM,0,MPI_COMM_WORLD,IERR)
      CALL MPI_REDUCE(FOBAR,DOBAR,LENGTH,MPI_DOUBLE_PRECISION,
     &                MPI_SUM,0,MPI_COMM_WORLD,IERR)
      CALL MPI_REDUCE(FOBAI,DOBAI,LENGTH,MPI_DOUBLE_PRECISION,
     &                MPI_SUM,0,MPI_COMM_WORLD,IERR)
      ENDIF
      IF (ISOS.AND.NOPEN.GT.1) THEN
      CALL MPI_REDUCE(FOACR,DOACR,LENGTH,MPI_DOUBLE_PRECISION,
     &                MPI_SUM,0,MPI_COMM_WORLD,IERR)
      CALL MPI_REDUCE(FOACI,DOACI,LENGTH,MPI_DOUBLE_PRECISION,
     &                MPI_SUM,0,MPI_COMM_WORLD,IERR)
      CALL MPI_REDUCE(FOBCR,DOBCR,LENGTH,MPI_DOUBLE_PRECISION,
     &                MPI_SUM,0,MPI_COMM_WORLD,IERR)
      CALL MPI_REDUCE(FOBCI,DOBCI,LENGTH,MPI_DOUBLE_PRECISION,
     &                MPI_SUM,0,MPI_COMM_WORLD,IERR)
      ENDIF
      ENDIF
C
      RETURN

 8888 WRITE(*,*) ' **  ERROR READING DIRECT ACCESS FILE **'
      CALL MPI_FINALIZE(IERR)
      STOP

      END
