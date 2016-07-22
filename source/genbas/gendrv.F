C
C     =================
      SUBROUTINE GENDRV
C     =================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     DRIVE BASIS GENERATION
C
      INCLUDE 'paracom.inc'
C
      CHARACTER SIOM,SITP
C
      CALL GENVEC
      DO 200 IOM = 1, 2
      DO 100 ITP = 0, N4
      WRITE (SIOM,'(I1)') IOM
      WRITE (SITP,'(I1)') ITP
      CALL EXPAND (IOM,ITP)
      IF (NCONTR(IOM,ITP).GT.0) THEN
         IF (IPROF.EQ.0) THEN
             CALL BASIS (IOM,ITP)
         ELSEIF (IPROF.EQ.1) THEN
             CALL PROFIL (IOM,ITP)
             GO TO 100
         ELSEIF (IPROF.EQ.2) THEN
             CALL PROFIL (IOM,ITP)
             CALL BASIS (IOM,ITP)
         ELSEIF (IPROF.GT.2) THEN
             CALL PROFIL (IOM,ITP)
             CALL BASIS (IOM,ITP)
             CALL PROFIL (IOM,ITP)
         ENDIF
         IF (NCONTR(IOM,ITP).GT.0) CALL GENFIL (IOM,ITP)
      ENDIF
  100 CONTINUE
  200 CONTINUE
      IF (IPROF.NE.1) CALL GENOUT2
      RETURN
      END
