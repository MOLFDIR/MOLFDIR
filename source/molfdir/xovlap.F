      COMPLEX*16 FUNCTION XOVLAP(NBAS,IND,ICENT,ITYPE,
     &                           CF1A,CF1B,CF2A,CF2B)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      COMPLEX*16 CFTA,CFTB,CF1A(*),CF1B(*),CF2A(*),CF2B(*)
      COMPLEX*16 XO
      INTEGER ICENT(*),ITYPE(*)
C
      XO = (0.,0.)
      DO I=1,NBAS
         II = IND + I - 1
         ICYT = ICENT(II)
         ITYT = ITYPE(II)
         CFTA = (0.,0.)
         CFTB = (0.,0.)
         DO J=1,NBAS
            JJ = IND + J - 1
            IF (ICENT(JJ).EQ.ICYT) THEN
                S = ROVLAP(ITYT,ITYPE(JJ))
#if defined (BIT64)
                CFTA = CFTA + CMPLX(S)*CF2A(J)
                CFTB = CFTB + CMPLX(S)*CF2B(J)
#else
                CFTA = CFTA + DCMPLX(S)*CF2A(J)
                CFTB = CFTB + DCMPLX(S)*CF2B(J)
#endif
            ENDIF
         ENDDO
#if defined (BIT64)
      XO = XO + CFTA*CONJG(CF1A(I)) + CFTB*CONJG(CF1B(I))
#else
      XO = XO + CFTA*DCONJG(CF1A(I)) + CFTB*DCONJG(CF1B(I))
#endif
      ENDDO
C
      XOVLAP = XO
C
      RETURN
      END
