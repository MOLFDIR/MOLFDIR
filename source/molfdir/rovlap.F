C
C     =============================
      FUNCTION ROVLAP (ITYP1,ITYP2)
C     =============================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     THIS ROUTINE COMPUTES THE RELATIVE OVERLAP BETWEEN TWO CARTESIAN
C     FUCTIONS ON THE SAME CENTER.
C
      INCLUDE 'paracom.inc'
C
      SAVE FACTOR
      REAL*8 FACTOR (0:12)
#if defined (BIT64)
      DATA FACTOR /1.,0.,1.,0.,3.,0.,15.,0.,105.,0.,945.,0.,10395./
#else
      DATA FACTOR /1.D0,0.D0,1.D0,0.D0,3.D0,0.D0,15.D0,0.D0,
     &             105.D0,0.D0,945.D0,0.D0,10395.D0/
#endif
      N1X = NX (ITYP1)
      N2X = NX (ITYP2)
      N1Y = NY (ITYP1)
      N2Y = NY (ITYP2)
      N1Z = NZ (ITYP1)
      N2Z = NZ (ITYP2)
      ROVLAP = FACTOR(N1X+N2X)*FACTOR(N1Y+N2Y)*FACTOR(N1Z+N2Z)
      RETURN
      END
