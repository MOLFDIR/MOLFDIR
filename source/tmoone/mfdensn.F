C
C     ==================
      SUBROUTINE MFDENSN
C     ==================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     TRANSFORM THE MO'S BACK TO THE NON SYMMETRY ADAPTED BASIS
C     AND GET DENSITYMATRIX IN THE NON-ADAPTED BASIS
C
      INCLUDE 'param.inc'
      INCLUDE 'common.inc'
      INCLUDE 'densfock.inc'
C
      CALL DENSN (DAAR,DAAI,DBAR,DBAI,.FALSE.)
      IF (OPENSH) CALL DENSN (DOAAR,DOAAI,DOBAR,
     +                        DOBAI,.TRUE.)
      RETURN
      END
