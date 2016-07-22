C
C     ===============
      BLOCK DATA INIT
C     ===============
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'param.inc'
      INCLUDE 'common.inc'
      INCLUDE 'general.inc'
C
      DATA REPNA / N5A * '    ' /
      DATA MULTB / N5B * 0 /
      DATA ONER, ONEI / N3 * 0.0D0, N3 * 0.0D0 /
      DATA TWOINR, TWOINI / N4A * 0.0D0, N4A * 0.0D0 /
      DATA INDTWR, INDTWI / N4 * 0, N4 * 0 /
      DATA INDGAR, INDGAI / N4 * 0, N4 * 0 /
      DATA IDET / N2 * 0 /
      END
