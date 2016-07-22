
C     ===========================
      SUBROUTINE GENDET(IRP,IRRP)
C     ===========================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'param.inc'
      INCLUDE 'general.inc'
C
      INTEGER N(N2)
C
#if defined (CRAY)
      IAND(I,J)=AND(I,J)
      ISHIFT (I, J) = SHIFT (I, J)
#else
      ISHIFT (I, J) = ISHFT (I, J)
#endif
C
      IF (IPRNT.GE.10) WRITE(6,1000) REPNA(IRRP)
C
      NDET = 0
      DO 10 I = 1, 2 ** NORB
c       IF ((NBITS(I).EQ.NELEC).AND.(NBITS(I.AND.IREF).EQ.IREFE)
        IF ((NBITS(I).EQ.NELEC).AND.(NBITS(IAND(I,IREF)).EQ.IREFE)
     +     .AND.(IRPDET(I).EQ.IRRP))  THEN
          NDET = NDET + 1
          IF (NDET .GT. N2) STOP ' N2 TOO SMALL'
          IDET (NDET) = I
          DO 20 J=1,NORB
            N(J)=0
 20         IF (IAND (I, ISHIFT (1, J - 1)) .NE. 0)  N(J)=1
          IF (IPRNT.GE.10)
     +        WRITE(6,1010) NDET,(N(J),J=1,NORB)
        ENDIF
 10     CONTINUE
      RETURN
 1000 FORMAT(/1X,'LIST OF DETERMINANTS WITH SYMMETRY ',A4,':'//)
 1010 FORMAT(1X,I4,32I2)
      END
