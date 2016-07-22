C
      FUNCTION TRC(A,B,L,K)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C === COMPUTE THE TRACE OF THE PRODUCT OF 2 UNDER TRIANGLE MATRICES
C === I.E. TR(A+*B)
C
#if defined (BIT64)
      COMPLEX*16 CDOTC
#else
      COMPLEX*16 ZDOTC
#endif
      COMPLEX*16 A(*), B(*), CSUM1, CSUM2
      REAL*8 TRC
C
#if defined (BIT64)
      CSUM1 = CDOTC(L,A,1,B,1)
#else
      CSUM1 = ZDOTC(L,A,1,B,1)
#endif
  100 CONTINUE
      DO 200 I = 1, K
      II = I * (I + 1) / 2
#if defined (BIT64)
      CSUM2 = CONJG(A(II)) * B(II)
#else
      CSUM2 = DCONJG(A(II)) * B(II)
#endif
  200 CONTINUE
      CSUM1 = 2 * CSUM1 - CSUM2
#if defined (BIT64)
      TRC = REAL(CSUM1)
#else
      TRC = DREAL(CSUM1)
#endif
      RETURN
      END
