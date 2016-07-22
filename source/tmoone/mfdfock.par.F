C
C     ==================
      SUBROUTINE MFDFOCK
C     (MASTER ROUTINE)
C     ==================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     BUILD THE SKELETON FOCK-MATRIX FROM LIST OF INTEGRALS AND LABELS
C
C     COMMON/DMT/: ON ENTRY CLOSED SHELL DENSITY MATRIX (DAAR)
C     COMMON/FMT/: ON EXIT CLOSED SHELL INCOMPLETE FOCK MATRIX (FAAR)
C     COMMON/DOMT/: ON ENTRY OPEN SHELL DENSITY MATRIX (DOAAR)
C     COMMON/FOMT/: ON EXIT OPEN SHELL INCOMPLETE FOCK MATRIX (FOAAR)
C     all the matrices are linear arrays of length N21 !
C
C     OPENSH: .TRUE. IF OPEN SHELL CALCULATION
C
      INCLUDE 'param.inc'
      INCLUDE 'common.inc'
      INCLUDE 'densfock.inc'
      INCLUDE 'mpidef.inc'
      INCLUDE 'mpif.h'
C
      LOGICAL ISOS
      INTEGER IERR,ISTAT(MPI_STATUS_SIZE),IRCV,IV,LENGTH
      INTEGER IXX,IDUM,INCAR(MAXNOD)
C
C ** before we start building the Fock matrix we collect
C ** status from each node, if files are ok

      DO IV=1,MAXNOD
        INCAR(IV)=1
      ENDDO

      IDUM=0  ! becomes the first entry in the incar array !
      CALL MPI_GATHER(IDUM,1,MPI_INTEGER,INCAR,1,MPI_INTEGER,0,
     +                MPI_COMM_WORLD,IERR)
      IXX=0
      DO I=1,mpisiz
        IF(INCAR(I).NE.0) IXX=-1
      ENDDO

      CALL MPI_BCAST(IXX,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
      IF(IXX.NE.0) CALL Q2JOB(6,'ERROR ON A NODE !',0,0)


C ** files on the nodes should be ok, we start building the Fock matrix

      ISOS = OPENSH
C     -------------------------------
C     INITIALIZE FOCK MATRIX ELEMENTS
C     -------------------------------
      
      LENGTH=N21
      DO IV = 1, N21
        FOAAR (IV) = 0.0D0
        FOAAI (IV) = 0.0D0
        FOBAR (IV) = 0.0D0
        FOBAI (IV) = 0.0D0
        FAAR (IV) = 0.0D0
        FAAI (IV) = 0.0D0
        FBAR (IV) = 0.0D0
        FBAI (IV) = 0.0D0
        SC1 (IV) = 0.0D0
      ENDDO
C
C     --------------------------------------------
C     SEND SLAVES A COPY OF THE DENSITY MATRIX/CES
C     --------------------------------------------
C
C     write(*,*) 'master sending DM to slaves'
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
C
C     ----------------------------------------
C     WAIT FOR ALL SLAVES TO FINISH THEIR WORK
C     ----------------------------------------
C
      IXX=0
      DO I=1,mpisiz-1
        CALL MPI_RECV(IRCV,1,MPI_INTEGER,MPI_ANY_SOURCE,
     &     M_JODO_T,MPI_COMM_WORLD,ISTAT,IERR)
        IF(IERR.NE.0) CALL Q2JOB(6,'MPI_RECV ERROR IN MFDFOCK',0,0)
        NFROM=ISTAT(MPI_SOURCE)
        INCAR(NFROM)=0
        IF(IRCV.NE.0) IXX=-1
      ENDDO
      DO I=1,mpisiz-1
        IF(INCAR(I).NE.0) THEN
          IXX=-1
          WRITE(*,*) 'NO RESPONSE FROM NODE',I
        ENDIF
      ENDDO
C     write(*,*) 'master got confirmation from all nodes'


C     ------------------------------------ 
C     DO A GLOBAL SUM OF THE FOCK MATRICES 
C     ------------------------------------ 
C 
C     IF (.NOT.BREIT) THEN
      CALL MPI_REDUCE(SC1,FAAR,LENGTH,MPI_DOUBLE_PRECISION,
     &                MPI_SUM,0,MPI_COMM_WORLD,IERR)
      CALL MPI_REDUCE(SC1,FAAI,LENGTH,MPI_DOUBLE_PRECISION,
     &                MPI_SUM,0,MPI_COMM_WORLD,IERR)
      CALL MPI_REDUCE(SC1,FBAR,LENGTH,MPI_DOUBLE_PRECISION,
     &                MPI_SUM,0,MPI_COMM_WORLD,IERR)
      CALL MPI_REDUCE(SC1,FBAI,LENGTH,MPI_DOUBLE_PRECISION,
     &                MPI_SUM,0,MPI_COMM_WORLD,IERR)
      IF (ISOS) THEN
      CALL MPI_REDUCE(SC1,FOAAR,LENGTH,MPI_DOUBLE_PRECISION,
     &                MPI_SUM,0,MPI_COMM_WORLD,IERR)
      CALL MPI_REDUCE(SC1,FOAAI,LENGTH,MPI_DOUBLE_PRECISION,
     &                MPI_SUM,0,MPI_COMM_WORLD,IERR)
      CALL MPI_REDUCE(SC1,FOBAR,LENGTH,MPI_DOUBLE_PRECISION,
     &                MPI_SUM,0,MPI_COMM_WORLD,IERR)
      CALL MPI_REDUCE(SC1,FOBAI,LENGTH,MPI_DOUBLE_PRECISION,
     &                MPI_SUM,0,MPI_COMM_WORLD,IERR)
      ENDIF
C     ENDIF
C     write(*,*) 'master did reduce.'

      RETURN
      END
