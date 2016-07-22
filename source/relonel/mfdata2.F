C
C     ==================
      SUBROUTINE MFDATA2
C     ==================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     INITIALIZE DATA,DEFINE PARAMETERS,STORE ALL COMMON BLOCKS USED
C     OPEN ALL FILES TO BE USED AND REWIND THEM
C
C     FILES IN USE :
C     NAME----UNIT------------PURPOSE-----------
C     MFD1OUT  3    BINARY INPUTFILE FROM MOLFDIR
C     TAPE10  10    SCRATCH FILE
C     TAPE11  11    SCRATCH FILE
C     TAPE12  12    SCRATCH FILE
C     MFDONE  14    BINARY OUTPUT WITH 'DIRAC'ONE-ELECTRON INTEGRALS
C
      INCLUDE 'paracom.inc'
#if defined (PARALLEL)
      INCLUDE 'mpidef.inc'
#else
      PARAMETER (mpirnk = 0)
#endif
C
      CHARACTER*7 FNAME1,FNAME2,FNAME3,FNAME4,FNAME5
      CHARACTER*10 FILE1,FILE2,FILE3,FILE4,FILE5
      FNAME1='MFD1OUT'
      FNAME2='SCRAFI1'
      FNAME3='SCRAFI2'
      FNAME4='SCRAFI3'
      FNAME5='MFDONEE'
      IF (mpirnk .EQ. 0) THEN
         WRITE (FILE1,'(A7)') FNAME1
         WRITE (FILE2,'(A7)') FNAME2
         WRITE (FILE3,'(A7)') FNAME3
         WRITE (FILE4,'(A7)') FNAME4
         WRITE (FILE5,'(A7)') FNAME5
      ELSEIF (mpirnk .LT. 10) THEN
         WRITE (FILE1,'(A7,A1,I1)') FNAME1,'.',mpirnk
         WRITE (FILE2,'(A7,A1,I1)') FNAME2,'.',mpirnk
         WRITE (FILE3,'(A7,A1,I1)') FNAME3,'.',mpirnk
         WRITE (FILE4,'(A7,A1,I1)') FNAME4,'.',mpirnk
         WRITE (FILE5,'(A7,A1,I1)') FNAME5,'.',mpirnk
      ELSEIF (mpirnk .LT. 100) THEN
         WRITE (FILE1,'(A7,A1,I2)') FNAME1,'.',mpirnk
         WRITE (FILE2,'(A7,A1,I2)') FNAME2,'.',mpirnk
         WRITE (FILE3,'(A7,A1,I2)') FNAME3,'.',mpirnk
         WRITE (FILE4,'(A7,A1,I2)') FNAME4,'.',mpirnk
         WRITE (FILE5,'(A7,A1,I2)') FNAME5,'.',mpirnk
      ELSE
         WRITE (FILE1,'(A7,A1,I3)') FNAME1,'.',mpirnk
         WRITE (FILE2,'(A7,A1,I3)') FNAME2,'.',mpirnk
         WRITE (FILE3,'(A7,A1,I3)') FNAME3,'.',mpirnk
         WRITE (FILE4,'(A7,A1,I3)') FNAME4,'.',mpirnk
         WRITE (FILE5,'(A7,A1,I3)') FNAME5,'.',mpirnk
      ENDIF
      OPEN( 3,FILE=FILE1,STATUS='OLD'    ,FORM='UNFORMATTED')
      OPEN(10,FILE=FILE2,STATUS='NEW',FORM='UNFORMATTED')
      OPEN(11,FILE=FILE3,STATUS='NEW',FORM='UNFORMATTED')
      OPEN(12,FILE=FILE4,STATUS='NEW',FORM='UNFORMATTED')
      OPEN(14,FILE=FILE5,STATUS='UNKNOWN',FORM='UNFORMATTED')
      REWIND  3
      REWIND 10
      REWIND 11
      REWIND 12
      REWIND 14
      RETURN
      END
