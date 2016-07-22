C
C     =============================
      INTEGER FUNCTION NBITS (JDET)
C     =============================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'param.inc'
      INCLUDE 'general.inc'
C
      ID = JDET
      NB = 0
      DO 10 I = 1, NORB
        NB = NB + MOD (ID, 2)
 10     ID = ID / 2
      NBITS = NB
      RETURN
      END
