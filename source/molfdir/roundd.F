      REAL*8 FUNCTION ROUNDD(VAR,NDEC)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C---------------Description--------------------------------------------
C
C     Rounds off variabele to the same number of decimal places as when
C     written out as formatted output. 
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C     July 1996.
C
C---------------Calling variables--------------------------------------
C
      REAL*8 VAR
      INTEGER NDEC
C
C---------------Common Blocks--------------------------------------
C
C---------------Local variables--------------------------------------
C
      REAL*8 RVAR
      CHARACTER*8 FORMAT,CARD*40
C
C---------------Executable code--------------------------------------
C
      IF (0.LE.NDEC.AND.NDEC.LT.10) THEN
         WRITE (FORMAT,1000) NDEC
      ELSEIF (10.LE.NDEC.AND.NDEC.LE.20) THEN
         WRITE (FORMAT,1001) NDEC
      ELSE
         STOP 'NDEC SHOULD BE BETWEEN 0 AND 20 IN ROUND'
      ENDIF
C
      WRITE (CARD,FORMAT) VAR
      READ  (CARD,FORMAT) RVAR
      ROUNDD = RVAR
C
      RETURN
 1000 FORMAT ('(F40.',I1,') ')
 1001 FORMAT ('(F40.',I2,')')
      END
