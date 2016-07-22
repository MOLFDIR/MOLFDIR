C
      SUBROUTINE SELECV
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     ====================
C     SELECTS EIGENVECTORS
C     ====================
C
      INCLUDE 'param.inc'
      INCLUDE 'general.inc'
      INCLUDE 'iterat.inc'
C
      DO 20 IROOT = 1, NROOTS
         IF (SELECT) THEN
         OVMAX = 0.D0
         DO 10 I = 1, NVEC
            OV = EVECR(IROOT,I)*EVECR(IROOT,I)
            IF (.NOT.REALAR)
     &      OV = OV + EVECI(IROOT,I)*EVECI(IROOT,I)
            IF (OV.GT.OVMAX) THEN
               NSEL(IROOT) = I
               OVMAX = OV
            ENDIF
   10       CONTINUE
         ELSE
            NSEL(IROOT) = IROOT
         ENDIF
   20 CONTINUE
C
      RETURN
      END
