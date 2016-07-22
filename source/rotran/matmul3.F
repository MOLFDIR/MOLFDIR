C
C     ===================================================
      SUBROUTINE MATMUL3(A,NZA,INDNZ,NZD,B,MB,C,MC,NA,NN)
C     ===================================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     A: J  (TWO INDEX TRANSFORMED INTEGRALS, LOWER TRIANGLE)
C     B: Q  (TRANSFORMATION MATRIX)
C     C: K  (RESULT, THREE INDEX TRANSFORMED INTEGRALS)
C
      INTEGER MB,MC,NA,NN,IP,IQ,K,IPQ
      COMPLEX*16 A(*),B(MB,*),C(MC,*)
      INTEGER INDNZ(NZD,2)
C
      DO I = 1, NA
         DO IP = 1, NN
#if defined (BIT64)
            C(IP,I) = CMPLX(0.0,0.0)
#else
            C(IP,I) = DCMPLX(0.D0,0.D0)
#endif
         ENDDO
      ENDDO
C
      DO 10 IPQ=1,NZA
        IP=INDNZ(IPQ,1)
        IQ=INDNZ(IPQ,2)
        DO 30 K=1,NA
           C(IP,K)=C(IP,K)+A(IPQ)*B(IQ,K)
           C(IQ,K)=C(IQ,K)+A(IPQ)*B(IP,K)
 30        CONTINUE
 10     CONTINUE
      RETURN
      END
