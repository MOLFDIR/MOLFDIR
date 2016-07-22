      PROGRAM TMOONE

      IMPLICIT REAL*8 (A-H, O-Z)

      EXTERNAL SCFDAT

C
C     ======================================================================
C     FORM EFFECTIVE ONE-ELECTRON MATRIX ELEMENTS FOR USE IN CI CALCULATIONS
C     BUILD FOCKMATRIX WITH DENSITY FROM THE FROZEN ORBITALS,THEN TRANSFORM
C     TO ACTIVE ORBITAL SPACE.
C     SCF PROGRAM STRIPPED TO DO THIS IN SEPT.1991 BY  L. VISSCHER
C     THEORETICAL CHEMISTRY DEPARTMENT,
C     UNIVERSITY OF GRONINGEN (RUG),
C     NIJENBORGH 16, 9747 AG GRONINGEN, THE NETHERLANDS
C     =====================================================================
C
      INCLUDE 'param.inc'
      INCLUDE 'common.inc'
#if defined (PARALLEL)
      INCLUDE 'mpidef.inc'
      INCLUDE 'mpif.h'
C
      INTEGER IERR
      DIMENSION ISTAT(MPI_STATUS_SIZE)
C
C     MPI Initialization section
C
      IERR = 0
      CALL MPI_INIT(IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,mpisiz,IERR)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD,mpirnk,IERR)
      IF (IERR.NE.0) call mpiqt('Error initializing MPI',0)
      IF (mpisiz.LT.2) THEN
         WRITE (*,'(A)') 'Please use serial code for this !'
         call MPI_FINALIZE(IERR)
         stop
      ENDIF
      IF (mpisiz.GT.MAXNOD) THEN
         WRITE (*,'(A)') 'Please enlarge MAXNOD parameter'
         WRITE (*,'(A)') 'in the file mpidat.inc to ',mpisiz
         call MPI_FINALIZE(IERR)
         stop
      ENDIF

C     ================================
C     COMMUNICATION CHECK START
C     ================================

      IF(mpirnk.eq.0) THEN     ! MASTER POLLS THROUGH THE NODES

        WRITE(*,'(A)') '   ================================='
        WRITE(*,'(A)') '   ===  PARALLEL  TMOONE         ==='
        WRITE(*,'(A)') '   ===  COMMUNICATION  CHECK     ==='
        WRITE(*,'(A)') '   ================================='
        WRITE(*,'(A)') ' '
        DO I=1,mpisiz-1
          CALL MPI_SEND(mpirnk,1,MPI_INTEGER,i,M_BELL_T,
     +    MPI_COMM_WORLD,IERR)
          if(IERR.ne.0) call mpiqt('MPI_SEND',8)
          CALL MPI_RECV(IRCV,1,MPI_INTEGER,i,M_IMHE_T,
     +    MPI_COMM_WORLD,ISTAT,IERR)
          if(IERR.ne.0) call mpiqt('MPI_RECV',8)
          if(IRCV.ne.i) call mpiqt('MPI information corrupted !',27)
          WRITE(*,'(A,I6,A)') '  ** NODE',i,'  READY.'
        ENDDO

      ELSE                 ! EVERY NODE COMMUNICATES ITS NODE # (RANK)

        CALL MPI_RECV(IRCV,1,MPI_INTEGER,0,M_BELL_T,MPI_COMM_WORLD,
     +  ISTAT,IERR)
        if(IERR.ne.0) call mpiqt('MPI_RECV',8)
        if(IRCV.ne.0) call mpiqt('MPI information corrupted !',27)
        CALL MPI_SEND(mpirnk,1,MPI_INTEGER,0,M_IMHE_T,MPI_COMM_WORLD,
     +  IERR)
        if(IERR.ne.0) call MPIQT('MPI_SEND',8)

      ENDIF

C     ================================
C     COMMUNICATION CHECK END
C     ================================

C ** the necessary files for the transformation step are on the
C ** master node !
#else
      PARAMETER (mpirnk=0)
#endif

      CALL CPUUSED(SEC)
      CALL MFDATA4
      CALL MFD4INP
      CALL MFDTABL
      CALL MFDDRIV
      CALL CPUUSED(SEC1)

C ** from here only master node deletes the files
C ** because the slaves did not create them on local nodes !
      IF(mpirnk.EQ.0) THEN
        PRINT*
        PRINT*, ' TOTAL CPU TIME: ',SEC1-SEC,' SECONDS'
        PRINT*
        PRINT*, ' (NORMAL END OF TRANSFORMATION PROGRAM)'
        OPEN (MFDVECB,FILE=FNAM(2),ACCESS='DIRECT',
     +        FORM='UNFORMATTED',RECL=8)
        OPEN (MFDENSM,FILE=FNAM(6),ACCESS='DIRECT',
     +        FORM='UNFORMATTED',RECL=8)
        CLOSE (MFDVECB,STATUS='DELETE')
        CLOSE (MFDENSM,STATUS='DELETE')
        CLOSE (MFDOEMI,STATUS='DELETE')
        CLOSE (MFDOEMR,STATUS='DELETE')
      ENDIF

#if defined (PARALLEL)
      CALL MPI_FINALIZE(IERR)
#endif

      END
