      PROGRAM SCF

      IMPLICIT REAL*8 (A-H, O-Z)

      EXTERNAL SCFDAT

C
C  =====================================================================
C  PERFORM SELF-CONSISTENT-FIELD (SCF) CALCULATION ON A MOLECULAR SYSTEM
C  USING THE MO-LCAO EXPANSION METHOD FOR THE HARTREE-FOCK-DIRAC
C  EQUATIONS.
C  METHOD FIRST DESCRIBED IN CHEM. PHYS. LETTERS 104 (1985) 28
C  FIRST PROGRAM WRITTEN AUGUST 1986 BY  P.J.C AERTS
C  THEORETICAL CHEMISTRY DEPARTMENT,
C  UNIVERSITY OF GRONINGEN (RUG),
C  NIJENBORGH 16, 9747 AG GRONINGEN, THE NETHERLANDS
C
C  Since then the following authors have contributed :
C  Olivier Visser, University of Groningen
C  Luuk Visscher, University of Groningen and NASA Ames Research Center
C  Hirzo Merenga, Technical University of Delft
C  Bert de Jong, University of Groningen
C
C  Most recent publication of the method :
C  L. Visscher, O. Visser, P.J.C. Aerts, H. Merenga and W.C. Nieuwpoort,
C  Comp. Phys. Comm. 81 (1994) 120-144.
C
C  =====================================================================
C
      INCLUDE 'paracom.inc'
      INCLUDE 'general.inc'
      INCLUDE 'memory.inc'
      INCLUDE 'diis.inc'
#if defined (PARALLEL)
      INCLUDE 'mpif.h'
      INCLUDE 'mpidef.inc'
      DIMENSION ISTAT(MPI_STATUS_SIZE)
#else
      PARAMETER (mpirnk=0)
      PARAMETER (mpisiz=0)
#endif
C
      PARAMETER (MXL=2 000 000)
      DIMENSION A(MXL),B(MXL),CC(MXL),D(MXL)
      DIMENSION E(MXL),F(MXL),G(MXL),H(MXL),M(MXL)
#if defined (PARALLEL)
C
C Initialize MPI stuff
C
      mpirnk = 0
      IERR = 0
      ISTOP = 0
      CALL MPI_INIT(IERR)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD,mpirnk,IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,mpisiz,IERR)
      IF (IERR.NE.0) THEN
         PRINT*,'Error initializing MPI'
         GOTO 112
      ENDIF
      IF (mpisiz.LE.1) THEN
         PRINT*,'For a single node run, please use the serial version'
         GOTO 112
      ENDIF
      mpisiz = mpisiz - 1
C
C Checking/synchronizing communication before we go further
C
      IF(mpirnk.EQ.0) THEN
        DO i=1,mpisiz
          CALL MPI_SEND(mpirnk,1,MPI_INTEGER,i,M_BELL_T,
     +    MPI_COMM_WORLD,IERR)
          if(ierr.ne.0) call mpiqt('MPI_SEND',8)
          CALL MPI_RECV(ircv,1,MPI_INTEGER,i,M_IMHE_T,
     +    MPI_COMM_WORLD,istat,ierr)
          if(ierr.ne.0) call mpiqt('MPI_RECV',8)
          if(ircv.ne.i) call mpiqt('MPI information corrupted !',27)
        ENDDO
      ELSE
        CALL MPI_RECV(ircv,1,MPI_INTEGER,0,M_BELL_T,MPI_COMM_WORLD,
     +  istat,ierr)
        if(ierr.ne.0) call mpiqt('MPI_RECV',8)
        if(ircv.ne.0) call mpiqt('MPI information corrupted !',27)
        CALL MPI_SEND(mpirnk,1,MPI_INTEGER,0,M_IMHE_T,MPI_COMM_WORLD,
     +  IERR)
        if(ierr.ne.0) call mpiqt('MPI_SEND',8)
      ENDIF
#endif
C
      CALL CPUUSED(SEC)
      CALL MFD4INP
      CALL MFDTABL
      CALL MFDATA4
      IF (4*LENGTH.GT.MXL) CALL Q2JOB
     & (3,'P.SCF','MXL',4*LENGTH)
      IF (4*LENGTH1.GT.MXL) CALL Q2JOB
     & (3,'P.SCF','MXL',4*LENGTH1)
C
      IF (mpirnk.EQ.0) THEN
C
C Start master SCF routine
C
      CALL MFDDRIV(A,B,CC,D,E,F,M,G,H)
      CALL CPUUSED(SEC4)
C
C Master generated output
C
      CALL MFD4OUT(A,B,G,CC,D,H,E,F)
      CALL CPUUSED(SEC5)
      SEC6 = SEC5 - SEC4
      TPROP = TPROP + SEC6
C
C Remove Master generated files not used in rest of the calculations  
C
      OPEN (MFDENSM, FILE='MFDENSM', FORM='UNFORMATTED',
     +      ACCESS='DIRECT',RECL=8)
      CLOSE (MFDENSM,STATUS='DELETE')
      CLOSE (MFDOEMI,STATUS='DELETE')
      CLOSE (MFDOEMR,STATUS='DELETE')
C
C Finish, get timings and print them out
C
      CALL CPUUSED(SEC1)
      SEC = SEC1 - SEC
      TVARI = SEC-TPOEF-TDENS-TFOCK-TGAUNT-TFMCP-TDIAG-TPROP
#if defined (PARALLEL)
      ISTOP=0
      DO I=1,mpisiz
         CALL MPI_SEND(ISTOP,1,MPI_INTEGER,I,16,
     &                 MPI_COMM_WORLD,IERR)
      ENDDO
      DMIN = 1.D+15
      DNIL = 0.D0
      CALL MPI_REDUCE(DNIL,TFAVE,1,MPI_DOUBLE_PRECISION,
     &                MPI_SUM,0,MPI_COMM_WORLD,IERR)
      CALL MPI_REDUCE(DMIN,TFMIN,1,MPI_DOUBLE_PRECISION,
     &                MPI_MIN,0,MPI_COMM_WORLD,IERR)
      CALL MPI_REDUCE(DNIL,TFMAX,1,MPI_DOUBLE_PRECISION,
     &                MPI_MAX,0,MPI_COMM_WORLD,IERR)
      CALL MPI_REDUCE(DNIL,TGAVE,1,MPI_DOUBLE_PRECISION,
     &                MPI_SUM,0,MPI_COMM_WORLD,IERR)
      CALL MPI_REDUCE(DMIN,TGMIN,1,MPI_DOUBLE_PRECISION,
     &                MPI_MIN,0,MPI_COMM_WORLD,IERR)
      CALL MPI_REDUCE(DNIL,TGMAX,1,MPI_DOUBLE_PRECISION,
     &                MPI_MAX,0,MPI_COMM_WORLD,IERR)
      TFOCK = TFOCK + TFAVE
      TGAUNT = TGAUNT + TGAVE
      TFAVE = TFAVE / DBLE(mpisiz)
      TGAVE = TGAVE / DBLE(mpisiz)
#endif
      IHOUR = SEC/3600.D0
      SEC = SEC - IHOUR*3600
      IMIN = SEC/60.D0
      SEC = SEC - IMIN*60
      ISEC = SEC
      IF (mpisiz.EQ.0) THEN
      WRITE (6,1000) IDIAGR,IDIAGC,TPOEF,TDENS,TFOCK,TGAUNT,TFMCP,
     &               TDIAG,TPROP,TVARI,IHOUR,IMIN,ISEC
      ELSE
      WRITE (6,1001) mpisiz+1,IDIAGR,IDIAGC,TPOEF,TDENS,TFOCK,TFMIN,
     &               TFMAX,TFAVE,TGAUNT,TGMIN,TGMAX,TGAVE,TFMCP,
     &               TDIAG,TPROP,TVARI,IHOUR,IMIN,ISEC
      ENDIF
C
      ELSE
C
C Start my slave process
C
#if defined (PARALLEL)
      CALL MFDDSLV(A,B,CC,D,G,H)
#endif
C
      ENDIF
C
C     In parallel case close off
C
#if defined (PARALLEL)
  112 CALL MPI_FINALIZE(IERR)
#endif
      IF (mpirnk.EQ.0) STOP 'NORMAL END OF PROGRAM'
      IF (mpirnk.NE.0) STOP
 1000 FORMAT (
     &/' Total number of real diagonalisations :',T50,I8
     &/' Total number of complex diagonalisations :',T50,I8
     &//' TIMINGS FOR THE VARIOUS ROUTINES'
     &/' Setting up 1-electron files (MFDPOEF):',T50,F16.4
     &/' Constructing and extrapolating densities:',T50,F16.4
     &/' Constructing Coulomb part of Fock matrix:',T50,F16.4
     &/' Constructing Gaunt part of Fock matrix:',T50,F16.4
     &/' Completion of skeleton Fock matrix:',T50,F16.4
     &/' Diagonalisation of Fock matrix:',T50,F16.4
     &/' Property calculation (includes Gaunt): ',T50,F16.4
     &/' Untimed other routines: ',T50,F16.4
     &//' TOTAL CPU TIME:',I4,' HOURS',I4,' MINUTES',I4,' SECONDS' 
     &//8X,'---  NORMAL END OF RUN ---')
 1001 FORMAT (
     &/' Number of processors used :',T60,I3
     &/' Total number of real diagonalisations :',T60,I8
     &/' Total number of complex diagonalisations :',T60,I8
     &//' TIMINGS FOR THE VARIOUS ROUTINES'
     &/' Setting up 1-electron files (MFDPOEF):',T60,F16.4
     &/' Constructing and extrapolating densities:',T60,F16.4
     &/' Constructing Coulomb part of Fock matrix (all nodes):',
     &   T60,F16.4,
     &/' Minimum time on slave processor for Coulomb part:',T60,F16.4
     &/' Maximum time on slave processor for Coulomb part:',T60,F16.4
     &/' Average time of slave processors for Coulomb part:',T60,F16.4
     &/' Constructing Gaunt part of Fock matrix (all nodes):',T60,F16.4
     &/' Minimum time on slave processor for Gaunt part:',T60,F16.4
     &/' Maximum time on slave processor for Gaunt part:',T60,F16.4
     &/' Average time of slave processors for Gaunt part:',T60,F16.4
     &/' Completion of skeleton Fock matrix:',T60,F16.4
     &/' Diagonalisation of Fock matrix:',T60,F16.4
     &/' Property calculation (includes Gaunt): ',T60,F16.4
     &/' Untimed other routines: ',T60,F16.4
     &//' Master CPU TIME:',I4,' HOURS',I4,' MINUTES',I4,' SECONDS' 
     &//8X,'---  NORMAL END OF RUN ---')
      END
