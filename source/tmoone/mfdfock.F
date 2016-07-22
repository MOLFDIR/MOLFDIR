C
C     ==================
      SUBROUTINE MFDFOCK
C     ==================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     BUILD THE SKELETON FOCK-MATRIX FROM LIST OF INTEGRALS AND LABELS
C
C     COMMON/DMT/: ON ENTRY CLOSED SHELL DENSITY MATRIX (DAAR)
C     COMMON/FMT/: ON EXIT CLOSED SHELL INCOMPLETE FOCK MATRIX (FAAR)
C     COMMON/DOMT/: ON ENTRY OPEN SHELL DENSITY MATRIX (DOAAR)
C     COMMON/FOMT/: ON EXIT OPEN SHELL INCOMPLETE FOCK MATRIX (FOAAR)
C
C     OPENSH: .TRUE. IF OPEN SHELL CALCULATION
C
      INCLUDE 'param.inc'
      INCLUDE 'common.inc'
      INCLUDE 'densfock.inc'
C
      COMMON/INT/ RINT(N2)
#if defined (BIT64) 
      COMMON/LAB/ LABEL(N2)
#else
      COMMON/LAB/ LABEL(N2), LABEL1(N2)
#endif
C
      DIMENSION ILAB ( N2 ) , JLAB ( N2 ) , KLAB ( N2 ) , LLAB ( N2 )
C
      EQUIVALENCE (FILNAM1 (1), FNAMES (1))
      EQUIVALENCE (FILNAM2 (1), FNAMES (4))
C
      CHARACTER*7 FILNAM1(3),FILNAM2(3)
      LOGICAL READY
      LOGICAL ISOS
C
#if defined (BIT64) 
      DATA MASK /Z"000000000000FFFF"/
#else
      DATA MASK /'0000FFFF'X/
#endif
C
#if defined (CRAY)
      ISHIFT (I, J) = SHIFT (I, J)
#else
      ISHIFT (I, J) = ISHFT (I, J)
#endif
      ISOS = OPENSH
C
C     -------------------------------
C     INITIALIZE FOCK MATRIX ELEMENTS
C     -------------------------------
C
      DO 5 IV = 1, N21
        FOAAR (IV) = 0.0D0
        FOAAI (IV) = 0.0D0
        FOBAR (IV) = 0.0D0
        FOBAI (IV) = 0.0D0
        FAAR (IV) = 0.0D0
        FAAI (IV) = 0.0D0
        FBAR (IV) = 0.0D0
        FBAI (IV) = 0.0D0
    5   CONTINUE
C
C     ---------------------------------------------------------------
C     LOOP OVER THE FILES LL, SL AND SS
C     ---------------------------------------------------------------
C
      DO 1000 IUNIT = 1, MFIL
C
C
C     -----------------------------------
C     INITIALIZE READING FROM THESE FILES
C     -----------------------------------
C
      OPEN (INTFILE (IUNIT),FILE = FILNAM1 (IUNIT),FORM = 'UNFORMATTED')
      OPEN (LBLFILE (IUNIT),FILE = FILNAM2 (IUNIT),FORM = 'UNFORMATTED')
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
      IF (ISOS)   CALL FOCK (IT,NT,ILAB,JLAB,KLAB,LLAB,RINT,
     +             DOAAR,DOBAR,DOAAI,DOBAI,
     +             FOAAR,FOBAR,FOAAI,FOBAI,FOAAR)
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
      CLOSE (INTFILE (IUNIT))
      CLOSE (LBLFILE (IUNIT))
C
 1000 CONTINUE
C
C     --------
C     ALL DONE
C     --------
C
      RETURN
      END
