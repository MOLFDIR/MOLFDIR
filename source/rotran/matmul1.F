C
C     ========================================================
      SUBROUTINE MATMUL1(A,NZA,INDNZ,NZD,B,MB,C,MC,IP,NSC,NSP)
C     ========================================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     A: G  (NONZERO INTEGRALS, LOWER TRIANGLE)
C     B: Q  (TRANSFORMATION MATRIX)
C     C: H  (RESULT, ONE INDEX TRANSFORMED)
C
      REAL*8 A(*)
      COMPLEX*16 B(MB,*),C(MC,*)
      INTEGER INDNZ(NZD,2)
C
      DO 40 I=1,NSP
        DO 50 IR=IP,NSC
#if defined (BIT64)
          C(IR,I)=(0.0,0.0)
#else
          C(IR,I)=(0.D0,0.D0)
#endif
 50       CONTINUE
 40     CONTINUE
C
      DO 10 IRS=1,NZA
        IR=INDNZ(IRS,1)
        IS=INDNZ(IRS,2)
        DO 30 I=1,NSP
           C(IR,I)=C(IR,I)+A(IRS)*B(IS,I)
 30        CONTINUE
 10     CONTINUE
      RETURN
      END
