C
C     ==================
      SUBROUTINE MFDBSLV
C     (SLAVE ROUTINE)
C     ==================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     BUILD THE SKELETON BREIT-MATRIX FROM LIST OF INTEGRALS AND LABELS
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
      INCLUDE 'param.inc'
      INCLUDE 'common.inc'
      INCLUDE 'densfock.inc'
      INCLUDE 'mpidef.inc'
      INCLUDE 'mpif.h'
C
      DIMENSION ILAB ( N2 ) , JLAB ( N2 ) , KLAB ( N2 ) , LLAB ( N2 )
      DIMENSION ISTAT(MPI_STATUS_SIZE)
      INTEGER LBUCKP(N2,14),LBUCKQ(N2,14),LBUCKR(N2,14),LBUCKS(N2,14)
      INTEGER NBUCK(14)
      REAL*8  RINT(N2),RBUCK(N2,14)
      INTEGER LBUF(NPQSIZ)
      REAL*8  RBUF(NPQSIZ)
      LOGICAL ISOS,EX1,EX2
      CHARACTER*7  FNAME1,FNAME2
      CHARACTER*12 FILNM1,FILNM2
      INTEGER IRCV,LENGTH
      INTEGER INCAR(MAXNOD)
      
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
      FNAME1=FNAM(9)
      FNAME2=FNAM(10)
      IF (mpirnk .EQ. 0) THEN
         WRITE (FILNM1,'(A7)') FNAME1
         WRITE (FILNM2,'(A7)') FNAME2
      ELSEIF (mpirnk .LT. 10) THEN
         WRITE (FILNM1,'(A7,A1,I1)') FNAME1,'.',mpirnk
         WRITE (FILNM2,'(A7,A1,I1)') FNAME2,'.',mpirnk
      ELSEIF (mpirnk .LT. 100) THEN
         WRITE (FILNM1,'(A7,A1,I2)') FNAME1,'.',mpirnk
         WRITE (FILNM2,'(A7,A1,I2)') FNAME2,'.',mpirnk
      ELSE
         WRITE (FILNM1,'(A7,A1,I3)') FNAME1,'.',mpirnk
         WRITE (FILNM2,'(A7,A1,I3)') FNAME2,'.',mpirnk
      ENDIF

C     ----------------------------------
C     CHECK IF NECESSARY FILES ARE THERE
c     AND IF IT IS POSSIBLE TO CONTINUE
C     ----------------------------------
      IXX=0
      INQUIRE (FILE = FILNM1, EXIST = EX1)
      INQUIRE (FILE = FILNM2, EXIST = EX2)
      IF(.NOT.(EX1.AND.EX2)) IXX=-1
C
C ** signal file status to master
C
      CALL MPI_GATHER(IXX,1,MPI_INTEGER,INCAR,1,MPI_INTEGER,0,
     +                  MPI_COMM_WORLD,IERR)
C
C ** get overall status from master via bcast
C
      CALL MPI_BCAST(IXX,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
      IF(IXX.NE.0) CALL Q2JOB(6,
     +              'Incomplete execution. Program aborted.',0,0)
C
C ** eveything should be allright. start work
C
      OPEN(UNIT=SRTPQLN,FILE=FILNM2,ACCESS='SEQUENTIAL',
     +   STATUS='UNKNOWN',FORM='UNFORMATTED')
      READ(SRTPQLN) LRECCO,IRECL,ILSTA,ILEND,ISSTA,ISEND
      CLOSE(SRTPQLN)
      OPEN(UNIT=SRTPQIL,FILE=FILNM1,ACCESS='DIRECT',RECL=IRECL)

C     WRITE(*,*) 'NODE: ',mpirnk,' OPENED FILE ',FILNM1
C     WRITE(*,*) 'FILE DATA: ',LRECCO,IRECL

C
C
C     ----------------------------------
C     GET DENSITY MATRIX/CES FROM MASTER
C     ----------------------------------
C
      LENGTH=N21
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

C     ---------------------------------------------------------------
C     --- FROM HERE READ THE DIRECT ACCESS FILE AND PREPARE EVERYTHING
C     --- FOR THE COMPUTATION OF THE PARTIAL FOCK MATRICES. ALSO CREATE
C     --- INTTYP BATCHES !
C     ---------------------------------------------------------------

      DO I=1,14
        NBUCK(I)=0
      ENDDO

      DO 1000 LREC=1,LRECCO
C ** read in a record
        READ(SRTPQIL,REC=LREC,ERR=8888) IPQL,NT,IDUM,
     +         (RBUF(IJ),IJ=1,NT),(LBUF(IJ),IJ=1,NT)
C ** unpack, check for (SL/SL) range, make canonical and distribute
        IRL=0.5D0+0.5D0*DSQRT(DBLE(8*IPQL-7))
        ISL=IPQL-IRL*(IRL-1)/2
c ** if this integral block is not of breit type, read next record
        IF(.NOT.((IRL.GE.ISSTA).AND.(ISL.LE.ILEND))  ) GOTO 1000
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
            CALL GAUNT (IT,NT,ILAB,JLAB,KLAB,LLAB,RINT,
     +            DAAR,DBAR,DAAI,DBAI,
     +            FAAR,FBAR,FAAI,FBAI,FAAR)
C
            IF (ISOS) THEN
            CALL GAUNT (IT,NT,ILAB,JLAB,KLAB,LLAB,RINT,
     +              DOAAR,DOBAR,DOAAI,DOBAI,
     +              FOAAR,FOBAR,FOAAI,FOBAI,FOAAR)
            ENDIF  !(ISOS)
            NBUCK(IT)=0
          ENDIF
 950    CONTINUE

C **  treat next record
 1000 CONTINUE
C ** all records are read. we are ready for the final flush


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
      CALL GAUNT (IT,NT,ILAB,JLAB,KLAB,LLAB,RINT,
     +           DAAR,DBAR,DAAI,DBAI,
     +           FAAR,FBAR,FAAI,FBAI,FAAR)
C
      IF (ISOS) THEN
      CALL GAUNT (IT,NT,ILAB,JLAB,KLAB,LLAB,RINT,
     +             DOAAR,DOBAR,DOAAI,DOBAI,
     +             FOAAR,FOBAR,FOAAI,FOBAI,FOAAR)
      ENDIF  !(ISOS)

 100  CONTINUE

      CLOSE(SRTPQIL)

C     WRITE(*,*) 'NODE ',mpirnk,' HAS FINISHED DA FILE (BREIT).'

C
C     ------------------------------------------------
C     ALL DONE REPORT TO MASTER AND SEND FOCK MATRICES
C     ------------------------------------------------
      IXX=0
      CALL MPI_SEND(IXX,1,MPI_INTEGER,0,M_JODO_T,
     &              MPI_COMM_WORLD,IERR)

C ** communicate Fock matrix

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
C
      RETURN

 8888 WRITE(*,*) ' **  ERROR READING DIRECT ACCESS FILE **'
      WRITE(*,*) ' **  NODE:',mpirnk,' **'
      CALL MPI_FINALIZE(IERR)
      STOP 1

      END
