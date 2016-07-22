C
C     =====================
      SUBROUTINE DIAGH(IRP)
C     =====================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'param.inc'
      INCLUDE 'common.inc'
      INCLUDE 'general.inc'
C
      REAL*8 E(N2),EE(N2,2)
C
      CALL CRDIAG (N2,NDET,HR,HI,D(1,IRP),EE(1,1),EE(1,2),E,E,EE,IERR)
      RETURN
      END
