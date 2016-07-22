C
C     ================
      SUBROUTINE MAKEH
C     ================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'param.inc'
      INCLUDE 'common.inc'
      INCLUDE 'general.inc'
C
      DO 10 I = 1, NDET
        DO 10 J = 1, I
          CALL HMAT (IDET (I), IDET (J), HR (I, J), HI (I, J))
          HR (J, I) = HR (I, J)
          HI (J, I) = - HI (I, J)
 10       CONTINUE

C     PRINT*, ' ** HR **'
C     DO 100 I=1,NDET
C       WRITE(6,1010) (HR(I,J),J=1,I)
C100    CONTINUE
C     PRINT*, ' ** HI **'
C     DO 110 I=1,NDET
C       WRITE(6,1010) (HI(I,J),J=1,I)
C110    CONTINUE
C1010 FORMAT(4F20.14)
      RETURN
      END
