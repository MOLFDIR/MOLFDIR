C
C     ==================
      SUBROUTINE CRSCF(DAAR,DBAR,DAAI,DBAI,DOAAR,DOBAR,DOAAI,DOBAI,
     +                 DOACR,DOBCR,DOACI,DOBCI,FAAR,FBAR,FAAI,FBAI,
     +                 FOAAR,FOBAR,FOAAI,FOBAI,FOACR,FOBCR,FOACI,FOBCI,
     +                 SC1,SC2,SC3)
C     ==================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'paracom.inc'
      INCLUDE 'general.inc'
      INCLUDE 'memory.inc'
#if defined (PARALLEL)
      INCLUDE 'mpif.h'
      INCLUDE 'mpidef.inc'
C
      DIMENSION ISTAT(MPI_STATUS_SIZE)
      INTEGER YINT(2)
#endif
      REAL*8 DAAR(*),DBAR(*),DAAI(*),DBAI(*)
      REAL*8 DOAAR(*),DOBAR(*),DOAAI(*),DOBAI(*)
      REAL*8 DOACR(*),DOBCR(*),DOACI(*),DOBCI(*)
      REAL*8 FAAR(*),FBAR(*),FAAI(*),FBAI(*)
      REAL*8 FOAAR(*),FOBAR(*),FOAAI(*),FOBAI(*)
      REAL*8 FOACR(*),FOBCR(*),FOACI(*),FOBCI(*)
      REAL*8 SC1(*), SC2(*), SC3(*)
C
C     -----------------------
C     DRIVE ITERATION PROCESS
C     -----------------------
C
      FIRST=ONLYONE
      IF (ONLYONE) GOTO 200
      CALL CPUUSED(SEC1)
C
C Generate density matrices
C
      CALL MFDENSI(DAAR,FAAR,DBAR,DAAI,DBAI,FBAR,FAAI,FBAI)
      CALL CPUUSED(SEC2)          
      TDENS = TDENS + SEC2 - SEC1 
C
#if defined (PARALLEL)
C Let slave nodes check if the required integral files are there
C
      IDOIT=3
      DO I=1,mpisiz
         CALL MPI_SEND(IDOIT,1,MPI_INTEGER,I,16,
     &                 MPI_COMM_WORLD,IERN)
      ENDDO
      DO I=1,mpisiz
         CALL MPI_RECV(YINT,2,MPI_INTEGER,MPI_ANY_SOURCE,17,
     &                 MPI_COMM_WORLD,ISTAT,IERN)
         IF ((YINT(1).EQ.0).AND..NOT.ONLYBRT) IERN = 2
         IF (BREIT.AND.(YINT(2).EQ.0)) IERN = 2
      ENDDO
      IF (IERN.NE.0) RETURN
      IF (YINT(2).EQ.1.AND..NOT.TWOC) BREXIST=.TRUE.
#else
      CALL MFOPEN
#endif
      IF (ONLYBRT) RETURN
      IF (FIRST) GOTO 200
C
  100 CONTINUE
      IF (FIRST) FIRST = .FALSE.
      CALL CPUUSED(SEC1)
C
C Get density matrix and transform to non-symmetry adapted basis
C
      CALL MFDENST(DAAR,DBAR,DAAI,DBAI,FAAR,FAAI,
     +             DOAAR,DOBAR,DOAAI,DOBAI,FOAAR,FOAAI,
     +             DOACR,DOBCR,DOACI,DOBCI,FOACR,FOACI)
      CALL CPUUSED(SEC2)          
      TDENS = TDENS + SEC2 - SEC1 
      CALL CPUUSED(SEC1)
C
C Generate Coulomb type Fock matrix
C
      CALL MFDFOCK(DAAR,DBAR,DAAI,DBAI,DOAAR,DOBAR,DOAAI,DOBAI,
     +             DOACR,DOBCR,DOACI,DOBCI,FAAR,FBAR,FAAI,FBAI,
     +             FOAAR,FOBAR,FOAAI,FOBAI,FOACR,FOBCR,FOACI,FOBCI,
     +             SC1)
      CALL CPUUSED(SEC2)          
      TFOCK = TFOCK + SEC2 - SEC1 
      IF (BREIT) THEN
        CALL CPUUSED(SEC1)
C
C Generate Gaunt type Fock matrix
C
        CALL MFDBREIT(DAAR,DBAR,DAAI,DBAI,DOAAR,DOBAR,DOAAI,
     +          DOBAI,DOACR,DOBCR,DOACI,DOBCI,FAAR,FBAR,FAAI,FBAI,
     +          FOAAR,FOBAR,FOAAI,FOBAI,FOACR,FOBCR,FOACI,FOBCI,
     +          SC1)
        CALL CPUUSED(SEC2)          
        TGAUNT = TGAUNT + SEC2 - SEC1 
      ENDIF
C
      CALL CPUUSED(SEC1)
C
C Transform to symmetry basis
C
      CALL MFDFMCP (FAAR, FBAR, FAAI, FBAI, DAAR, DAAI)
C
      IF (OPENSH) THEN
        CALL MFDFMCP (FOAAR, FOBAR, FOAAI, FOBAI, DOAAR, DOAAI)
	IF (NOPEN.GT.1) CALL MFDFMCP (FOACR, FOBCR,
     +                  FOACI, FOBCI, DOACR, DOACI)
      ENDIF
      CALL CPUUSED(SEC2)          
      TFMCP = TFMCP + SEC2 - SEC1 
C
      CALL CPUUSED(SEC1)
C
C Compute energy
C
      CALL MFDPROP (FAAR(IPR1),FAAR(IPR2),FAAR(IPR3),FAAR(IPR4),
     +              FAAR(IPR5),FAAR(IPR6),FOAAR(IPR1),FOAAR(IPR2),
     +              FOAAR(IPR3),FOAAR(IPR4),FOAAR(IPR5),FOAAR(IPR6),
     +              1,DAAR,DAAI,DOAAR,DOAAI,DOACR,DOACI,SC1)
      CALL CPUUSED(SEC2)          
      TPROP = TPROP + SEC2 - SEC1 
      WRITE (6, 1000) ICYCLS, ETOTAL
      WRITE(DAYFTXT,1010) ICYCLS, ETOTAL
C
C Determine if calculation is converged
C
      CALL MFDCRIT
C
      IF (ENDRUN) THEN
#if !(defined (PARALLEL))
         DO  104 I= MFIL,1,-1
            CLOSE (INTFILE (I))
            CLOSE (LBLFILE (I))
  104    CONTINUE
#endif
         RETURN
      ENDIF

      IF (ENDRUN) RETURN
C
      IF (ITER0) THEN
          PRINT* , ' ITER0 (TEMPORARY?) DISABLED ...'
          ITER0=.FALSE.
      ENDIF
C
      EPREV = ETOTAL
C
  200 CONTINUE
C
      CALL CPUUSED(SEC1)
C
C Construct totoal Fock matrix and diagonalize
C
      CALL MFDIAGH (FAAR(ID1),FAAR(ID2),FAAR(ID3),FAAR(ID4),
     +              FAAR(ID5),FAAR(ID11),FAAR(ID12),FAAR(ID6),
     +              FAAR(ID7),FAAR(ID8),FAAR(ID9),FAAR(ID10),SC1(1),
     +              SC1(2*LENGTH+1),SC2(1),SC2(2*LENGTH+1),
     +              SC3(1),SC3(2*LENGTH+1),
     +              DAAR,DAAI,DOAAR,DOAAI,FOAAR,FOAAI,
     +              FAAR(2*LENGTH1+1),DOACR,DOACI,FOACR,FOACI)
      CALL CPUUSED(SEC2)          
      TDIAG = TDIAG + SEC2 - SEC1 
C
      ICYCLS = ICYCLS + 1
      KCYCLS = KCYCLS + 1
C
      CALL CPUUSED(SEC1)
C
C Form density in symmetry adapted basis
C
      CALL MFDENSS(DAAR,FAAR)
      CALL CPUUSED(SEC2)          
      TDENS = TDENS + SEC2 - SEC1 
C
C EXTRAPOLATE THE DENSITY IF THIS IS NEEDED AND ASKED FOR
C
      IF ((.NOT. FIRST) .AND. (KCYCLS .NE. 1)) THEN
      CALL CPUUSED(SEC1)
      CALL EXTRAP(DAAR,DOAAR,DOACR,FAAR,FOAAR,FOACR,SC1,SC2,SC3)
      CALL CPUUSED(SEC2)          
      TDENS = TDENS + SEC2 - SEC1 
      ENDIF
C     Print out eigenvalues if this is the last run
      IF (ENDRUN) PRTEW=.TRUE.
C
#if defined (CRAY) || defined (SGI) || defined (LINUX) || defined (SUN)
      CALL FLUSH (6)
#endif
#if defined (IBM)
      CALL FLUSH_(6)
#endif
C
      IF (.NOT. ONLYONE) GOTO 100
C
 1000 FORMAT (//' ITERATION ',I3,': TOTAL ENERGY = ',G29.14E4//)
 1010 FORMAT('E(TOTAL,',I3,'):',G29.14E4)
 1020 FORMAT (//' ITERATION ',I3,': ENERGY (ESTIMATED) = ',G29.14E4//)
 1030 FORMAT('E(STIMATE,',I3,'):',G29.14E4)
      RETURN
      END
