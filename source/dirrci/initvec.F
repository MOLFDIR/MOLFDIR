C
      SUBROUTINE InitVec
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     Initialises temporary file for expansion vectors
C
      INCLUDE 'param.inc'
      INCLUDE 'files.inc' 
      INCLUDE 'general.inc'
C
      IF (REALAR) THEN
         IRECL = NDET * 8
      ELSE
         IRECL = NDET * 16
      ENDIF
      Open (MRCVECS,File=FNAM(7),ACCESS='DIRECT',RECL=IRECL,
     &      IOSTAT=IOS,ERR=100)
      Return
 100  STOP 'Cannot open MRCVECS'
      End
