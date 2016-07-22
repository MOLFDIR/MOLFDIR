C
C     =============================================
      SUBROUTINE OVRLAP (RNT,NPR,EXPON,C1,C2,OVLAP)
C     =============================================
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'paracom.inc'
C
      DIMENSION C1(N17),C2(N17),EXPON(N17)
C
      OVLAP = 0.D0
      DO 20 J = 1, NPR
      ALFA = EXPON (J)
      DO 10 K = 1, NPR
      BETHA = EXPON (K)
      XNORM = (4.0D0*ALFA*BETHA/(ALFA+BETHA)**2)**(RNT/2.0D0+0.75D0)
      OVLAP = OVLAP + XNORM * C1 (J) * C2 (K)
   10 CONTINUE
   20 CONTINUE
      RETURN
      END
