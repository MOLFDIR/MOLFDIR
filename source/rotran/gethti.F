      LOGICAL FUNCTION GETHTI(JI,RJ,NRJ,NINT,INDRJ,LBRCIJ)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE "files.inc"
C
      COMPLEX*16 RJ(*)
      INTEGER NRJ,NINT
      INTEGER INDRJ(NRJ,2),LBRCIJ(0:*)
C
#if defined (BIT64)
      DATA MASK /Z"000000000000FFFF"/
#else
      DATA MASK /'0000FFFF'X/
#endif
C
      SAVE MASK
C
#if defined (CRAY)
      ISHIFT (I, J) = SHIFT (I, J)
#else
      ISHIFT (I, J) = ISHFT (I, J)
#endif
      NINT=0
      IREC=LBRCIJ(JI)
C
 10   IF (IREC.EQ.0) GOTO 20
        READ(INTTWO,REC=IREC) NT,IREC,(RJ(II),II=NINT+1,NINT+NT),
     +                                (INDRJ(II,1),II=NINT+1,NINT+NT)
C
      DO 30  K=1,NT
        NINT = NINT + 1
#if defined (BIT64)
        INDRJ(NINT,2)=AND(INDRJ(NINT,1),MASK)
        INDRJ(NINT,1)=AND(ISHIFT(INDRJ(NINT,1),48),MASK)
#else
        INDRJ(NINT,2)=IAND(INDRJ(NINT,1),MASK)
        INDRJ(NINT,1)=IAND(ISHIFT(INDRJ(NINT,1),-16),MASK)
#endif
 30     CONTINUE
C
      GOTO 10
C
 20   IF (NINT.EQ.0) THEN
        GETHTI=.FALSE.
      ELSE
        GETHTI=.TRUE.
      ENDIF
C
      RETURN
      END
