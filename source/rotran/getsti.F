      LOGICAL FUNCTION GETSTI(IPQ,G,NINT,INDG,NGD,LBRCPQ)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE "files.inc"
C
      REAL*8 G(*)
      INTEGER INDG(NGD,2),LBRCPQ(0:*)
C
#if defined (BIT64)
      DATA MASK /Z"000000000000FFFF"/
#else
      DATA MASK /'0000FFFF'X/
#endif
C
#if defined (CRAY)
      ISHIFT (I, J) = SHIFT (I, J)
#else
      ISHIFT (I, J) = ISHFT (I, J)
#endif
      NINT=0
C
      IREC=LBRCPQ(IPQ)
C
 20   IF (IREC.EQ.0) GOTO 100
      READ(INTSRT,REC=IREC) NT,IREC,(G(I),I=NINT+1,NINT+NT),
     +                              (INDG(I,2),I=NINT+1,NINT+NT)
C
      DO 30 I=1,NT
        NINT = NINT + 1
#if defined (BIT64)
        INDG(NINT,1)=AND(ISHIFT(INDG(NINT,2),48),MASK)
        INDG(NINT,2)=AND(INDG(NINT,2),MASK)
#else
        INDG(NINT,1)=IAND(ISHIFT(INDG(NINT,2),-16),MASK)
        INDG(NINT,2)=IAND(INDG(NINT,2),MASK)
#endif
 30     CONTINUE
C
      GOTO 20
C
 100  IF (NINT.EQ.0) THEN
        GETSTI=.FALSE.
      ELSE
        GETSTI=.TRUE.
      ENDIF
C
      RETURN
      END
