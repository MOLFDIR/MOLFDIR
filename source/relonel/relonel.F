      PROGRAM RELONEL
C
      IMPLICIT REAL*8 (A-H, O-Z)
      EXTERNAL DRELON
C
C     ==============================================================
C     CALCULATE ALL ONE ELECTRON INTEGRALS OVER THE DIRAC OPERATOR
C     AND PERFORM TRANSFORMATIONS TO GARANTY A PROPER REPRESENTATION
C     OF ALL PARTS OF THE DIRAC OPERATOR IN A FINITE BASIS
C     FIRST VERSION WRITTEN BY P.J.C.AERTS
C     THEORETICAL CHEMISTRY DEPARTMENT,
C     UNIVERSITY OF GRONINGEN (RUG), GRONINGEN,THE NETHERLANDS
C     ==============================================================
C
#if defined (PARALLEL)
      INCLUDE 'mpif.h'
      INCLUDE 'mpidef.inc'
      CALL MPI_INIT(mpierr)
      if(mpierr.ne.0) call mpiqt('MPI_INIT',8)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,mpisiz,mpierr)
      if(mpierr.ne.0) call mpiqt('MPI_COMM_WORLD',14)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD,mpirnk,mpierr)
      if(mpierr.ne.0) call mpiqt('MPI_COMM_RANK',13)
#else
      PARAMETER (mpirnk = 0)
#endif

C     all processes generate the one-electron integral file
C     this is not time consuming and need not be parallelised !

      CALL CPUUSED(SEC0)
      CALL MFDATA2
      CALL MFD2INP
      CALL GENTAB1
      CALL MFDONE
C   MFDONEE file still open here !
      CLOSE(14)
      CALL CPUUSED(SEC)
      if (mpirnk.eq.0) then
      print*,' '
      Write (6,1000) Sec-Sec0
 1000 Format (/' CPU Time used :',F12.4
     &//T20,'-- Normal End of Program --')
      endif

C **  close the scratch files
      CLOSE(10,STATUS='DELETE')
      CLOSE(11,STATUS='DELETE')
      CLOSE(12,STATUS='DELETE')

#if defined (PARALLEL)
      CALL MPI_FINALIZE(mpierr)
      if((mpierr.ne.0).and.(mpirnk.eq.0)) then
        write(*,*) ' ** MPI_FINALIZE failed ! **'
      endif
#endif

      END
