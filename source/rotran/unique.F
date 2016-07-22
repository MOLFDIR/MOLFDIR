      SUBROUTINE UNIQUE (NKR,I,J,BREIT,IJ,CONJUG,IS)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     Find the Kramers unique triangular pair index
C
      INCLUDE "files.inc"
      INCLUDE "vecs.inc"
C
      INTEGER I,J,IJ,IS
      LOGICAL CONJUG,BREIT
C
      IKR = IKRMO(I)
      JKR = IKRMO(J)
C---------------------------------------
C Lower triangle in ij is stored : 
C upper triangle related by herm. conjug.
C---------------------------------------
      IF (ABS(IKRMO(I)).GE.ABS(IKRMO(J))) THEN
         IIKR = IKR
         JJKR = JKR
         CONJUG = .FALSE.
      ELSE
         IIKR = JKR
         JJKR = IKR
         CONJUG = .TRUE.
      ENDIF
C------------------------------------
C (ibar,j) and (ibar,jbar) are stored
C------------------------------------
      IS = 1
      IF (IIKR.GT.0) THEN
         IS = ISIGN(1,IIKR*JJKR)
         IF (BREIT) IS = - IS
         IIKR = - IIKR
         JJKR = - JJKR
         CONJUG = .NOT.CONJUG
      ENDIF
C-------------------------------------
C     Calculate record index
C-------------------------------------
      IJBAROFF = NKR*(NKR+1)/2
      IF (JJKR.GT.0) THEN
         IJ=(JJKR-1)*(2*NKR-JJKR)/2-IIKR
      ELSE
         IJ=IJBAROFF-(JJKR+1)*(2*NKR+JJKR)/2-IIKR
      ENDIF
C
      RETURN
      END
