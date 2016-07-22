C
C     =======================================
      SUBROUTINE READINT (UNIT,IT, NT, READY)
C     =======================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'paracom.inc'
C
      COMMON/INT/ RINT(N2)
#if defined (BIT64) 
      COMMON/LAB/ LABEL(N2)
#else
      COMMON/LAB/ LABEL(N2), LABEL1(N2)
#endif
C
      LOGICAL READY
      INTEGER UNIT
C
      READ (UNIT) NT, IT, READY, (RINT (I), I = 1, NT)
      RETURN
C
C     ===================================
      ENTRY READLBL (UNIT, IT, NT, READY)
C     ===================================
C
#if defined (BIT64) 
      READ (UNIT) NT, IT, READY, (LABEL (I), I = 1, NT)
#else
      READ (UNIT) NT, IT, READY, (LABEL (I), I = 1, NT),
     +                           (LABEL1 (I), I = 1, NT)
#endif
      RETURN
      END
