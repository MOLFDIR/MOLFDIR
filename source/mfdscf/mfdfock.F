C
C     ==================
      SUBROUTINE MFDFOCK(DAAR,DBAR,DAAI,DBAI,DOAAR,DOBAR,DOAAI,DOBAI,
     +                   DOACR,DOBCR,DOACI,DOBCI,FAAR,FBAR,FAAI,FBAI,
     +               FOAAR,FOBAR,FOAAI,FOBAI,FOACR,FOBCR,FOACI,FOBCI,
     +               SC1)
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
C
      COMMON/INT/ RINT(N2)
#if defined (BIT64) 
      COMMON/LAB/ LABEL(N2)
#else
      COMMON/LAB/ LABEL(N2), LABEL1(N2)
#endif
C
      REAL*8 DAAR(*),DBAR(*),DAAI(*),DBAI(*)
      REAL*8 DOAAR(*),DOBAR(*),DOAAI(*),DOBAI(*)
      REAL*8 DOACR(*),DOBCR(*),DOACI(*),DOBCI(*)
      REAL*8 FAAR(*),FBAR(*),FAAI(*),FBAI(*)
      REAL*8 FOAAR(*),FOBAR(*),FOAAI(*),FOBAI(*)
      REAL*8 FOACR(*),FOBCR(*),FOACI(*),FOBCI(*)
      REAL*8 SC1(*)
C
      DIMENSION ILAB ( N2 ) , JLAB ( N2 ) , KLAB ( N2 ) , LLAB ( N2 )
      LOGICAL READY,ISOS
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
C     LOOP OVER THE FILES LL, SL AND SS
C     ---------------------------------------------------------------
C
      DO 1000 IUNIT = MFIL, 1, -1
C
C
C     -----------------------------------
C     INITIALIZE READING FROM THESE FILES
C     -----------------------------------
C
      REWIND (INTFILE (IUNIT))
      REWIND (LBLFILE (IUNIT))
      READY = .FALSE.
C
   10 CONTINUE
C
C     ----------------------------------------
C     NOTE: ASYNC I/O IS TEMP. UNAVAILABLE
C     START READING NEXT BLOCK OF INFORMATION
C     ----------------------------------------
C
      CALL READLBL (LBLFILE(IUNIT),IT, NT, READY)
      CALL READINT (INTFILE(IUNIT),IT, NT, READY)
C
C     -------------------------------
C     SKIP TO NEXT RECORD WHEN NT = 0
C     -------------------------------
C
      IF ( NT .EQ. 0 ) GOTO 100
C
C     ------------------
C     START CALCULATIONS
C     ------------------
C
C .... UNPACKING OF THE LABELS
C
      DO 20 IV = 1, NT
#if defined (BIT64) 
      ILAB (IV) = AND (ISHIFT (LABEL (IV), 16), MASK)
      JLAB (IV) = AND (ISHIFT (LABEL (IV), 32), MASK)
      KLAB (IV) = AND (ISHIFT (LABEL (IV), 48), MASK)
      LLAB (IV) = AND (LABEL (IV), MASK)
#else
      ILAB (IV) = IAND (ISHIFT (LABEL (IV), -16), MASK)
      JLAB (IV) = IAND (LABEL (IV), MASK)
      KLAB (IV) = IAND (ISHIFT (LABEL1 (IV), -16), MASK)
      LLAB (IV) = IAND (LABEL1 (IV), MASK)
#endif
   20 CONTINUE
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
      ENDIF
C
  100 CONTINUE
C
C     ------------------------------------------------
C     IF NOT READY CONTINUE WITH NEXT SET OF INTEGRALS
C     ------------------------------------------------
C
      IF ( .NOT. READY ) GOTO 10
C
C     -----------------------
C     CONTINUE WITH NEXT FILE
C     -----------------------
C
 1000 CONTINUE
C
C     --------
C     ALL DONE
C     --------
C
      RETURN
      END
