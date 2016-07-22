      PROGRAM MOLFDIR

      IMPLICIT REAL*8 (A-H, O-Z)
 
      EXTERNAL MOLFDAT
C
C     ==========================================================
C     EASY INPUT PROGRAM FOR GENERATING ALL NECESSARY DATA TO
C     PERFORM SUBSEQUENT INTEGRAL EVALUATION FOR MOLECULAR
C     HARTREE-FOCK-DIRAC CALCULATIONS
C     WRITTEN SEPT'81-JUN'82 BY P.J.C.AERTS
C     THEORETICAL CHEMISTRY DEPARTMENT,
C     UNIVERSITY OF GRONINGEN (RUG), GRONINGEN,THE NETHERLANDS
C     ==========================================================
C
C     GENERAL CONTRACTION VERSION (2.0) FEBRUARY 1990 BY L. VISSCHER
C
#if defined (PARALLEL)
      INCLUDE 'mpif.h'
      INCLUDE 'mpidef.inc'
C
      INTEGER llen, mpierr
      CHARACTER*256 procnm
C
      CALL MPI_INIT(mpierr)
      if(mpierr.ne.0) call mpiqt('MPI_INIT',8)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,mpisiz,mpierr)
      if(mpierr.ne.0) call mpiqt('MPI_COMM_SIZE',0)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD,mpirnk,mpierr)
      if(mpierr.ne.0) call mpiqt('MPI_COMM_RANK',0)
      CALL MPI_GET_PROCESSOR_NAME(procnm,llen,mpierr)
      if(mpierr.ne.0) call mpiqt('MPI_GET_PROCESSOR_NAME',0)
      write(*,*) 'MOLFDIR invoc. ',mpirnk,' running on host ',
     > procnm(1:llen)
#else
      PARAMETER (mpirnk = 0)
#endif
C
      OPEN (5, FILE='MOLFDIR.INP', FORM='FORMATTED')
      CALL MFD1INP
      CALL MFDSYMF
      CALL MFD1NRM
      CALL GMULTB
      CALL GREPMAT
      CALL MFD1OUT
      if (mpirnk.eq.0) CALL MOLECUL
C
      CLOSE(5)
C
#if defined (PARALLEL)
      CALL MPI_FINALIZE(mpierr)
      if(mpierr.ne.0) call mpiqt('MPI_FINALIZE',0)
#endif
C
      if (mpirnk .eq. 0)
     $   write(*,*) ' ** NORMAL END OF PROGRAM **'
      STOP 0
      END
