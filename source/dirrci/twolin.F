C
      SUBROUTINE TWOLIN (ENDFIL,TINTR,TINTI,IJEX,KL,NKL)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     ================================
C     READS BLOCK OF INTEGRALS (**|KL)
C     ** : ALL IJ >= KL
C     ================================
C
      INCLUDE 'param.inc'
      INCLUDE 'general.inc'
      INCLUDE 'files.inc'
C
      DIMENSION TINTR(*),TINTI(*),IJEX(*)
      LOGICAL ENDFIL
C
      ENDFIL = .FALSE.
      IF (REALAR) THEN
         READ (MRCTWOE) KL,NKL,(IJEX(IX),IX=1,NKL),(TINTR(IX),IX=1,NKL)
      ELSE
         READ (MRCTWOE) KL,NKL,(IJEX(IX),IX=1,NKL),(TINTR(IX),IX=1,NKL),
     &                  (TINTI(IX),IX=1,NKL)
      ENDIF
      IF (KL.LT.0) ENDFIL=.TRUE.
      RETURN
      END
