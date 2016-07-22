C
      SUBROUTINE BSOLVE(A,IVA,RHS,N,C,B,IVB,WK1,WK2,IFAIL)
      IMPLICIT REAL*8 (A-H, O-Z)
C
C === THIS ROUTINE SOLVES THE LINEAR SET OF EQUATIONS AX=C
C === THE SOLUTION IS OBTAINED USING CROUT'S METHOD.
C === SEE E.G. : L. FOX, INTRODUCTION TO NUMERICAL LINEAR ALGEBRA,
C ===            P. 102, CLARENDON PRESS, OXFORD (1979)
C
C === ON INPUT A MUST CONTAIN THE MATRIX DEFINING THE EQUATIONS
C ===          IVA IS THE FIRST DIMENSION OF A AS DECLARED
C ===          IN THE CALLING SUBROUTINE
C ===          N IS THE ORDER OF THE MATRIX
C ===          RHS CONTAINS THE RHS VECTOR
C ===
C === ON EXIT  A IS UNCHANGED
C ===          RHS IS UNCHANGED
C ===          IVA AND N ARE UNALTERED
C ===          B CONTAINES THE LU DECOMPOSITION OF A
C ===          C CONTAINS THE SOLUTION VECTOR
C ===          WK1 AND WK2 ARE OVERWRITTEN
C ===          IFAIL = 0 IF EVERYTHING OK
C ===          IFAIL = 1 IF A IS SINGULAR
C ===          IFAIL = 2 IF A IS BADLY CONDITIONED
C
      PARAMETER(TOL=1.0D-14)
C
      DIMENSION A(IVA,N), B(IVB,N)
      DIMENSION RHS(N),C(N), WK1(N), WK2(N)
C
C === DETERMINE THE LU DECOMPOSITION OF A ACCORDING TO CROUT
C
      IFAIL = 0
      ERROR1 = 1.0D0
C
#if defined (BIT64)
      IF (ABS(A(1,1)) .LT. TOL) THEN
#else
      IF (DABS(A(1,1)) .LT. TOL) THEN
#endif
      IFAIL = 1
      RETURN
      ENDIF
C
      DO 50 I = 1, N
      DO 60 J = 1, N
      B(J,I) = A(J,I)
   60 CONTINUE
   50 CONTINUE
      DO 70 I = 1, N
      C(I) = RHS(I)
   70 CONTINUE
C
      DO 100 I = 2, N
      B(1,I) = B(1,I) / B(1,1)
  100 CONTINUE
C
      DO 110 I = 2, N
C
      DO 120 K = 1, I - 1
      DO 130 J = I, N
      B(J,I) = B(J,I) - B(J,K) * B(K,I)
  130 CONTINUE
  120 CONTINUE
C
#if defined (BIT64)
      IF (ABS(B(I,I)) .LT. TOL) THEN
#else
      IF (DABS(B(I,I)) .LT. TOL) THEN
#endif
      IFAIL = 1
      RETURN
      ENDIF
C
      DO 140 K = 1, I - 1
      DO 150 J = I + 1, N
      B(I,J) = B(I,J) - B(I,K) * B(K,J)
  150 CONTINUE
  140 CONTINUE
      DO 160 J = I + 1, N
      B(I,J) = B(I,J) / B(I,I)
  160 CONTINUE
C
  110 CONTINUE
C
C
C === CORRECT THE VECTOR C FOR THE PIVOTING OF A
C
      C(1) = C(1) / B(1,1)
      DO 200 I = 2, N
      DO 210 J = 1, I - 1
      C(I) = C(I) - B(I,J) * C(J)
  210 CONTINUE
      C(I) = C(I) / B(I,I)
  200 CONTINUE
C
C === COMPUTE THE SOLUTION VECTORS BY BACK SUBSTITUTION
C
      DO 300 I = N - 1, 1, -1
      DO 310 J = I + 1, N
      C(I) = C(I) - C(J) * B(I,J)
  310 CONTINUE
      WK2(I) = C(I)
  300 CONTINUE
      WK2(N) = C(N)
C
C === ENHANCE THE ACCURACY OF THE SOLUTION
C
  500 CONTINUE
C
      ERROR2 = ERROR1
      ERROR1 = 0D0
      DO 400 I = 1, N
      WK1(I) = RHS(I)
      DO 410 J = 1, N
      WK1(I) = WK1(I) - A(I,J) * C(J)
  410 CONTINUE
      ERROR1 = ERROR1 + WK1(I) * WK1(I)
  400 CONTINUE
#if defined (BIT64)
      ERROR1 = SQRT(ERROR1)
#else
      ERROR1 = DSQRT(ERROR1)
#endif
      IF (ERROR1 .LT. TOL) RETURN
      IF ((ERROR1 .GE. ERROR2) .AND.
#if defined (BIT64)
     &    (ABS(ERROR1-ERROR2) .GT. 1.0D-16)) THEN
#else
     &    (DABS(ERROR1-ERROR2) .GT. 1.0D-16)) THEN
#endif
      IFAIL = 2
      DO 420 I = 1, N
      C(I) = WK2(I)
  420 CONTINUE
      RETURN
      ENDIF
C
      WK1(1) = WK1(1) / B(1,1)
      DO 430 I = 2, N
      DO 440 J = 1, I - 1
      WK1(I) = WK1(I) - B(I,J) * WK1(J)
  440 CONTINUE
      WK1(I) = WK1(I) / B(I,I)
  430 CONTINUE
C
      DO 450 I = N - 1, 1, -1
      DO 460 J = I + 1, N
      WK1(I) = WK1(I) - WK1(J) * B(I,J)
  460 CONTINUE
  450 CONTINUE
C
      DO 470 I = 1, N
      WK2(I) = C(I)
  470 CONTINUE
      DO 480 I = 1, N
      C(I) = C(I) + WK1(I)
  480 CONTINUE
C
      GOTO 500
C
      END
