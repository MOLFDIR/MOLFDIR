      PROGRAM RELTWEL

      IMPLICIT REAL*8 (A-H, O-Z)

      EXTERNAL MFDATA3
C
C     =================================================================
C     CALCULATE TWO-ELECTRON INTEGRALS FOR MOLECULAR DIRAC-HARTREE-FOCK
C     SCF WITH SEPERATE LARGE AND SMALL COMPONENT BASISSETS. INTEGRALS
C     ARE OVER CARTESIAN GAUSSIANS AND SYMMETRY OPERATIONS ARE USED TO
C     DETERMINE WETHER INTEGRALS ARE UNIQUE OR NOT. ONLY UNIQUE INTEGRALS
C     ARE STORED IF THEIR VALUE IS ABOVE SOME THRESHOLD, TOGETHER WITH A
C     LABEL.
C     FIRST VERSION WRITTEN BY P.J.C.AERTS
C     VECTORIZED GENERAL CONTRACTION VERSION (4.0) BY L. VISSCHER
C     THEORETICAL CHEMISTRY DEPARTMENT,
C     UNIVERSITY OF GRONINGEN (RUG), GRONINGEN,THE NETHERLANDS
C     PARALLEL VERSION BY M. PERNPOINTNER
C     THEORETICAL CHEMISTRY DEPARTMENT,
C     UNIVERSITY OF GRONINGEN (RUG), GRONINGEN,THE NETHERLANDS
C     =================================================================
C
C

C     according to this implemetation the large (alpha/beta) and small (alpha/
C     beta) part are described by the same set of primitives !!
C     a unitary double-group symmetry transformation mixes alpha/beta parts of the
C     spatial basis functions !

      INCLUDE 'mpif.h'
      INCLUDE 'mpidef.inc'
C
      INTEGER IERR, ISTAT(MPI_STATUS_SIZE)
C
C     --- initialize MPI ---
C
      IERR = 0
      CALL MPI_INIT(IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,mpisiz,IERR)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD,mpirnk,IERR)
      IF (IERR.NE.0) call mpiqt('Error initializing MPI',0)
      IF (mpisiz.LE.1) THEN
         WRITE (*,'(A)') 'Please use the serial code for this !!'
         CALL MPI_FINALIZE(IERR)
         STOP
      ENDIF
C
C     --------------------------------
C     CHECK MASTER/SLAVE COMMUNICATION
C     --------------------------------

      IF(mpirnk.EQ.0) THEN

C     this process is master process

        WRITE(*,'(/A,/A/)') 'RELTWEL:',
     +       'Checking master/slave communication'
        DO i=1,mpisiz-1
          CALL MPI_SEND(mpirnk,1,MPI_INTEGER,i,M_BELL_T,
     +    MPI_COMM_WORLD,IERR)
          if(ierr.ne.0) call mpiqt('MPI_SEND',8)
          CALL MPI_RECV(ircv,1,MPI_INTEGER,i,M_IMHE_T,
     +    MPI_COMM_WORLD,istat,ierr)
          if(ierr.ne.0) call mpiqt('MPI_RECV',8)
          if(ircv.ne.i) call mpiqt('MPI information corrupted !',27)
          WRITE(*,'(A,I6,A)') '  ** NODE',i,'  OK.'
        ENDDO

      ELSE

C     this process is slave process

        CALL MPI_RECV(ircv,1,MPI_INTEGER,0,M_BELL_T,MPI_COMM_WORLD,
     +  istat,ierr)
        if(ierr.ne.0) call mpiqt('MPI_RECV',8)
        if(ircv.ne.0) call mpiqt('MPI information corrupted !',27)
        CALL MPI_SEND(mpirnk,1,MPI_INTEGER,0,M_IMHE_T,MPI_COMM_WORLD,
     +  IERR)
        if(ierr.ne.0) call mpiqt('MPI_SEND',8)

      ENDIF
C
C     ------------------------------------------
C     END COMMUNICATION CHECK, START COMPUTATION
C     ------------------------------------------
C
      CALL MFD3INP
      CALL GENTAB
      CALL MFDTWOE
   
      CALL MPI_FINALIZE(IERR)
      if (mpirnk.eq.0) WRITE(*,*) '    -- NORMAL END OF PROGRAM --'

      END
