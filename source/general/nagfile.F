#if !(defined (CRAY))
      SUBROUTINE F01BCF(N,TOL,Z,IZ,W,IW,D,E,C,S)
C     MARK 3 RELEASE. NAG COPYRIGHT 1972.
C     MARK 4 REVISED.
C     MARK 4.5 REVISED
C     MARK 5C REVISED
C     MARK 6B REVISED IER-125 IER-127 (JUL 1978)
C     MARK 11 REVISED. VECTORISATION (JAN 1984).
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C
C
C     TRECX2
C     F01BCF REDUCES A COMPLEX HERMITIAN MATRIX TO REAL
C     TRIDIAGONAL FORM FROM WHICH THE EIGENVALUES AND EIGENVECTORS
C     CAN BE FOUND USING SUBROUTINE F02AYF,(CXTQL2). THE HERMITIAN
C     MATRIX A=A(1) IS REDUCED TO THE TRIDIAGONAL MATRIX A(N-1) BY
C     N-2 UNITARY TRANSFORMATIONS. THE HOUSEHOLDER REDUCTION ITSELF
C     DOES NOT GIVE A REAL TRIDIAGONAL MATRIX, THE OFF-DIAGONAL
C     ELEMENTS ARE COMPLEX. THEY ARE SUBSEQUENTLY MADE REAL BY A
C     DIAGONAL TRANSFORMATION.
C     APRIL 1ST. 1972
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION  TOL
      INTEGER           IW, IZ, N
C     .. Array Arguments ..
      DOUBLE PRECISION  C(N), D(N), E(N), S(N), W(IW,N), Z(IZ,N)
C     .. Local Scalars ..
      DOUBLE PRECISION  CO, F, FI, FR, G, GI, GR, H, HH, R, SI
      INTEGER           I, II, J, K, L
C     .. External Subroutines ..
      EXTERNAL          F01BCY, F01BCZ
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, SQRT
C     .. Executable Statements ..
      DO 20 I = 1, N
         D(I) = Z(N,I)
         E(I) = -W(N,I)
   20 CONTINUE
      IF (N.EQ.1) GO TO 540
      DO 360 II = 2, N
         I = N - II + 2
         L = I - 2
         G = 0.0D0
         FR = D(I-1)
         FI = E(I-1)
         IF (L.EQ.0) GO TO 60
         DO 40 K = 1, L
            G = G + D(K)*D(K) + E(K)*E(K)
   40    CONTINUE
   60    H = G + FR*FR + FI*FI
C        L IS NOW I-1
         L = L + 1
         IF (ABS(FR)+ABS(FI).NE.0.0D0) GO TO 80
         R = 0.0D0
         CO = 1.0D0
         C(I) = 1.0D0
         SI = 0.0D0
         S(I) = 0.0D0
         GO TO 140
   80    IF (ABS(FR).LT.ABS(FI)) GO TO 100
         R = ABS(FR)*SQRT(1.0D0+(FI/FR)**2)
         GO TO 120
  100    R = ABS(FI)*SQRT(1.0D0+(FR/FI)**2)
  120    SI = FI/R
         S(I) = -SI
         CO = FR/R
         C(I) = CO
  140    IF (G.LE.TOL) GO TO 280
         G = -SQRT(H)
         E(I) = G
C        E(I) HAS ITS FINAL REAL VALUE
         H = H - R*G
C        S*S + SR
         D(I-1) = (R-G)*CO
         E(I-1) = (R-G)*SI
         DO 160 J = 1, L
            Z(J,I) = D(J)
            W(J,I) = E(J)
  160    CONTINUE
         CALL F01BCZ(Z,IZ,W,IW,L,D,E,C,S)
C        FORM P
         DO 180 J = 1, L
            C(J) = C(J)/H
            S(J) = S(J)/H
  180    CONTINUE
         FR = 0.0D0
         DO 200 J = 1, L
            FR = FR + C(J)*D(J) + S(J)*E(J)
  200    CONTINUE
C        FORM K
         HH = FR/(H+H)
C        FORM Q
         DO 220 J = 1, L
            C(J) = C(J) - HH*D(J)
            S(J) = S(J) - HH*E(J)
  220    CONTINUE
C        NOW FORM REDUCED A
         DO 260 J = 1, L
            FR = D(J)
            FI = E(J)
            GR = C(J)
            GI = S(J)
            DO 240 K = J, L
               Z(K,J) = (((Z(K,J)-GR*D(K))-GI*E(K))-FR*C(K)) - FI*S(K)
               W(K,J) = (((W(K,J)-GR*E(K))+GI*D(K))-FR*S(K)) + FI*C(K)
  240       CONTINUE
            D(J) = Z(L,J)
            Z(I,J) = 0.0D0
            E(J) = -W(L,J)
            W(I,J) = 0.0D0
            W(J,J) = 0.0D0
  260    CONTINUE
         GO TO 340
  280    E(I) = R
         H = 0.0D0
         DO 300 J = 1, L
            Z(J,I) = D(J)
            W(J,I) = E(J)
  300    CONTINUE
         DO 320 J = 1, L
            Z(I,J) = 0.0D0
            D(J) = Z(I-1,J)
            W(I,J) = 0.0D0
            E(J) = -W(I-1,J)
  320    CONTINUE
  340    D(I) = H
  360 CONTINUE
C     WE NOW FORM THE PRODUCT OF THE
C     HOUSEHOLDER MATRICES, OVERWRITING
C     ON Z AND W
      DO 500 I = 2, N
         L = I - 1
         Z(N,L) = Z(L,L)
         Z(L,L) = 1.0D0
         W(N,L) = E(L)
         W(L,L) = 0.0D0
         H = D(I)
         IF (H.EQ.0.0D0) GO TO 460
         DO 380 K = 1, L
            D(K) = 0.0D0
            E(K) = 0.0D0
  380    CONTINUE
         CALL F01BCY(Z,IZ,W,IW,L,L,Z(1,I),W(1,I),D,E)
         DO 400 K = 1, L
            D(K) = D(K)/H
            E(K) = -E(K)/H
  400    CONTINUE
         DO 440 J = 1, L
            DO 420 K = 1, L
               Z(K,J) = Z(K,J) - Z(K,I)*D(J) + W(K,I)*E(J)
               W(K,J) = W(K,J) - Z(K,I)*E(J) - W(K,I)*D(J)
  420       CONTINUE
  440    CONTINUE
  460    DO 480 J = 1, L
            Z(J,I) = 0.0D0
            W(J,I) = 0.0D0
  480    CONTINUE
  500 CONTINUE
      W(N,N) = E(N)
      DO 520 I = 1, N
         D(I) = Z(N,I)
         Z(N,I) = 0.0D0
         E(I) = W(N,I)
         W(N,I) = 0.0D0
  520 CONTINUE
  540 Z(N,N) = 1.0D0
      W(N,N) = 0.0D0
      E(1) = 0.0D0
C     NOW WE MULTIPLY BY THE
C     COSTHETA + I SINTHETA COLUMN
C     FACTORS
      CO = 1.0D0
      SI = 0.0D0
      IF (N.EQ.1) RETURN
      DO 580 I = 2, N
         F = CO*C(I) - SI*S(I)
         SI = CO*S(I) + SI*C(I)
         CO = F
         DO 560 J = 1, N
            F = Z(J,I)*CO - W(J,I)*SI
            W(J,I) = Z(J,I)*SI + W(J,I)*CO
            Z(J,I) = F
  560    CONTINUE
  580 CONTINUE
      RETURN
      END
      SUBROUTINE F01BCY(AR,IAR,AI,IAI,M,N,BR,BI,CR,CI)
C     MARK 11 RELEASE. NAG COPYRIGHT 1983.
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C
C     COMPUTES  C = C +  (A**H)*B  (COMPLEX) WHERE
C     A IS RECTANGULAR M BY N.
C     C MUST BE DISTINCT FROM B.
C
C
C     .. Scalar Arguments ..
      INTEGER           IAI, IAR, M, N
C     .. Array Arguments ..
      DOUBLE PRECISION  AI(IAI,N), AR(IAR,N), BI(M), BR(M), CI(N), CR(N)
C     .. Local Scalars ..
      DOUBLE PRECISION  XI, XR
      INTEGER           I, J
C     .. Executable Statements ..
      DO 40 I = 1, N
         XR = CR(I)
         XI = CI(I)
         DO 20 J = 1, M
            XR = XR + AR(J,I)*BR(J) + AI(J,I)*BI(J)
            XI = XI + AR(J,I)*BI(J) - AI(J,I)*BR(J)
   20    CONTINUE
         CR(I) = XR
         CI(I) = XI
   40 CONTINUE
      RETURN
      END
      SUBROUTINE F01BCZ(AR,IAR,AI,IAI,N,BR,BI,CR,CI)
C     MARK 11 RELEASE. NAG COPYRIGHT 1983.
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C
C     COMPUTES  C = A*B  (COMPLEX) WHERE
C     A IS A HERMITIAN N-BY-N MATRIX,
C     WHOSE LOWER TRIANGLE IS STORED IN A.
C     C MUST BE DISTINCT FROM B.
C
C
C     .. Scalar Arguments ..
      INTEGER           IAI, IAR, N
C     .. Array Arguments ..
      DOUBLE PRECISION  AI(IAI,N), AR(IAR,N), BI(N), BR(N), CI(N), CR(N)
C     .. Local Scalars ..
      DOUBLE PRECISION  YI, YR
      INTEGER           I, IP1, J, NM1
C     .. Executable Statements ..
      DO 20 I = 1, N
         CR(I) = 0.0D0
         CI(I) = 0.0D0
   20 CONTINUE
      IF (N.EQ.1) GO TO 100
      NM1 = N - 1
      DO 80 I = 1, NM1
         DO 40 J = I, N
            CR(J) = CR(J) + AR(J,I)*BR(I) - AI(J,I)*BI(I)
            CI(J) = CI(J) + AR(J,I)*BI(I) + AI(J,I)*BR(I)
   40    CONTINUE
         YR = CR(I)
         YI = CI(I)
         IP1 = I + 1
         DO 60 J = IP1, N
            YR = YR + AR(J,I)*BR(J) + AI(J,I)*BI(J)
            YI = YI + AR(J,I)*BI(J) - AI(J,I)*BR(J)
   60    CONTINUE
         CR(I) = YR
         CI(I) = YI
   80 CONTINUE
  100 CR(N) = CR(N) + AR(N,N)*BR(N) - AI(N,N)*BI(N)
      CI(N) = CI(N) + AR(N,N)*BI(N) + AI(N,N)*BR(N)
      RETURN
      END
      SUBROUTINE F02AXF(AR,IAR,AI,IAI,N,WR,VR,IVR,VI,IVI,WK1,WK2,WK3,
     *                  IFAIL)
C     MARK 3 RELEASE. NAG COPYRIGHT 1972.
C     MARK 4.5 REVISED
C     MARK 9 REVISED. IER-327 (SEP 1981).
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C     MARK 13 REVISED. USE OF MARK 12 X02 FUNCTIONS (APR 1988).
C
C     EIGENVALUES AND EIGENVECTORS OF A COMPLEX HERMITIAN MATRIX
C     1ST APRIL 1972
C
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='F02AXF')
C     .. Scalar Arguments ..
      INTEGER           IAI, IAR, IFAIL, IVI, IVR, N
C     .. Array Arguments ..
      DOUBLE PRECISION  AI(IAI,N), AR(IAR,N), VI(IVI,N), VR(IVR,N),
     *                  WK1(N), WK2(N), WK3(N), WR(N)
C     .. Local Scalars ..
      DOUBLE PRECISION  A, B, MAX, SQ, SUM, XXXX
      INTEGER           I, ISAVE, J
C     .. Local Arrays ..
      CHARACTER*1       P01REC(1)
C     .. External Functions ..
      DOUBLE PRECISION  X02AJF, X02AKF
      INTEGER           P01ABF
      EXTERNAL          X02AJF, X02AKF, P01ABF
C     .. External Subroutines ..
      EXTERNAL          F01BCF, F02AYF
C     .. Intrinsic Functions ..
      INTRINSIC         SQRT
C     .. Executable Statements ..
      ISAVE = IFAIL
      DO 40 I = 1, N
         IF (AI(I,I).NE.0.0D0) GO TO 140
         DO 20 J = 1, I
            VR(I,J) = AR(I,J)
            VI(I,J) = AI(I,J)
   20    CONTINUE
   40 CONTINUE
      CALL F01BCF(N,X02AKF()/X02AJF(),VR,IVR,VI,IVI,WR,WK1,WK2,WK3)
      IFAIL = 1
      CALL F02AYF(N,X02AJF(),WR,WK1,VR,IVR,VI,IVI,IFAIL)
      IF (IFAIL.EQ.0) GO TO 60
      IFAIL = P01ABF(ISAVE,1,SRNAME,0,P01REC)
      RETURN
C     NORMALISE
   60 DO 120 I = 1, N
         SUM = 0.0D0
         MAX = 0.0D0
         DO 80 J = 1, N
            SQ = VR(J,I)*VR(J,I) + VI(J,I)*VI(J,I)
            SUM = SUM + SQ
            IF (SQ.LE.MAX) GO TO 80
            MAX = SQ
            A = VR(J,I)
            B = VI(J,I)
   80    CONTINUE
         IF (SUM.EQ.0.0D0) GO TO 120
         SUM = 1.0D0/SQRT(SUM*MAX)
         DO 100 J = 1, N
            SQ = SUM*(VR(J,I)*A+VI(J,I)*B)
            VI(J,I) = SUM*(VI(J,I)*A-VR(J,I)*B)
            VR(J,I) = SQ
  100    CONTINUE
  120 CONTINUE
      RETURN
  140 IFAIL = P01ABF(ISAVE,2,SRNAME,0,P01REC)
      RETURN
      END
      SUBROUTINE F02AYF(N,EPS,D,E,Z,IZ,W,IW,IFAIL)
C     MARK 3 RELEASE. NAG COPYRIGHT 1972.
C     MARK 4 REVISED.
C     MARK 4.5 REVISED
C     MARK 9 REVISED. IER-326 (SEP 1981).
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C
C     CXTQL2
C     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS OF A
C     HERMITIAN MATRIX, WHICH HAS BEEN REDUCED TO A REAL
C     TRIDIAGONAL MATRIX, T, GIVEN WITH ITS DIAGONAL ELEMENTS IN
C     THE ARRAY D(N) AND ITS SUB-DIAGONAL ELEMENTS IN THE LAST N
C     - 1 STORES OF THE ARRAY E(N), USING QL TRANSFORMATIONS. THE
C     EIGENVALUES ARE OVERWRITTEN ON THE DIAGONAL ELEMENTS IN THE
C     ARRAY D IN ASCENDING ORDER. THE REAL AND IMAGINARY PARTS OF
C     THE EIGENVECTORS ARE FORMED IN THE ARRAYS Z,W(N,N)
C     RESPECTIVELY, OVERWRITING THE ACCUMULATED TRANSFORMATIONS AS
C     SUPPLIED BY THE SUBROUTINE F01BCF. THE SUBROUTINE WILL FAIL
C     IF ALL EIGENVALUES TAKE MORE THAN 30*N ITERATIONS
C     1ST APRIL 1972
C
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='F02AYF')
C     .. Scalar Arguments ..
      DOUBLE PRECISION  EPS
      INTEGER           IFAIL, IW, IZ, N
C     .. Array Arguments ..
      DOUBLE PRECISION  D(N), E(N), W(IW,N), Z(IZ,N)
C     .. Local Scalars ..
      DOUBLE PRECISION  B, C, F, G, H, P, R, S
      INTEGER           I, I1, II, ISAVE, J, K, L, M, M1
C     .. Local Arrays ..
      CHARACTER*1       P01REC(1)
C     .. External Functions ..
      INTEGER           P01ABF
      EXTERNAL          P01ABF
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, SQRT
C     .. Executable Statements ..
      ISAVE = IFAIL
      IF (N.EQ.1) GO TO 40
      DO 20 I = 2, N
         E(I-1) = E(I)
   20 CONTINUE
   40 E(N) = 0.0D0
      B = 0.0D0
      F = 0.0D0
      J = 30*N
      DO 300 L = 1, N
         H = EPS*(ABS(D(L))+ABS(E(L)))
         IF (B.LT.H) B = H
C        LOOK FOR SMALL SUB-DIAG ELEMENT
         DO 60 M = L, N
            IF (ABS(E(M)).LE.B) GO TO 80
   60    CONTINUE
   80    IF (M.EQ.L) GO TO 280
  100    IF (J.LE.0) GO TO 400
         J = J - 1
C        FORM SHIFT
         G = D(L)
         H = D(L+1) - G
         IF (ABS(H).GE.ABS(E(L))) GO TO 120
         P = H*0.5D0/E(L)
         R = SQRT(P*P+1.0D0)
         H = P + R
         IF (P.LT.0.0D0) H = P - R
         D(L) = E(L)/H
         GO TO 140
  120    P = 2.0D0*E(L)/H
         R = SQRT(P*P+1.0D0)
         D(L) = E(L)*P/(1.0D0+R)
  140    H = G - D(L)
         I1 = L + 1
         IF (I1.GT.N) GO TO 180
         DO 160 I = I1, N
            D(I) = D(I) - H
  160    CONTINUE
  180    F = F + H
C        QL TRANSFORMATION
         P = D(M)
         C = 1.0D0
         S = 0.0D0
         M1 = M - 1
         DO 260 II = L, M1
            I = M1 - II + L
            G = C*E(I)
            H = C*P
            IF (ABS(P).LT.ABS(E(I))) GO TO 200
            C = E(I)/P
            R = SQRT(C*C+1.0D0)
            E(I+1) = S*P*R
            S = C/R
            C = 1.0D0/R
            GO TO 220
  200       C = P/E(I)
            R = SQRT(C*C+1.0D0)
            E(I+1) = S*E(I)*R
            S = 1.0D0/R
            C = C/R
  220       P = C*D(I) - S*G
            D(I+1) = H + S*(C*G+S*D(I))
C           FORM VECTOR
            DO 240 K = 1, N
               H = Z(K,I+1)
               Z(K,I+1) = S*Z(K,I) + C*H
               Z(K,I) = C*Z(K,I) - S*H
               H = W(K,I+1)
               W(K,I+1) = S*W(K,I) + C*H
               W(K,I) = C*W(K,I) - S*H
  240       CONTINUE
  260    CONTINUE
         E(L) = S*P
         D(L) = C*P
         IF (ABS(E(L)).GT.B) GO TO 100
  280    D(L) = D(L) + F
  300 CONTINUE
C     ORDER EIGEN VALUES ANS EIGENVECTORS
      DO 380 I = 1, N
         K = I
         P = D(I)
         I1 = I + 1
         IF (I1.GT.N) GO TO 340
         DO 320 J = I1, N
            IF (D(J).GE.P) GO TO 320
            K = J
            P = D(J)
  320    CONTINUE
  340    IF (K.EQ.I) GO TO 380
         D(K) = D(I)
         D(I) = P
         DO 360 J = 1, N
            P = Z(J,I)
            Z(J,I) = Z(J,K)
            Z(J,K) = P
            P = W(J,I)
            W(J,I) = W(J,K)
            W(J,K) = P
  360    CONTINUE
  380 CONTINUE
      IFAIL = 0
      RETURN
  400 IFAIL = P01ABF(ISAVE,1,SRNAME,0,P01REC)
      RETURN
      END
      SUBROUTINE F03AHF(N,A,IA,DETR,DETI,IDETE,RINT,IFAIL)
C     MARK 2 RELEASE. NAG COPYRIGHT 1972
C     MARK 3 REVISED.
C     MARK 4.5 REVISED
C     MARK 7 REVISED IER-143 (DEC 1978)
C     MARK 8 REVISED. IER-236 (APR 1980).
C     MARK 11 REVISED. VECTORISATION (JAN 1984).
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C     MARK 12 REVISED. EXTENDED BLAS (JUNE 1986)
C
C     COMPDET
C     THE COMPLEX UNSYMMETRIC MATRIX, A, IS STORED IN THE ARRAY
C     A(N,N). THE DECOMPOSITION A = LU, WHERE L IS A LOWER
C     TRIANGULAR MATRIX AND U IS A UNIT UPPER TRIANGULAR MATRIX,
C     IS PERFORMED AND OVERWRITTEN ON A,
C     OMITTING THE UNIT DIAGONAL OF U. A RECORD OF ANY
C     INTERCHANGES MADE TO THE ROWS OF A IS KEPT IN RINT(I), I=1,N,
C     SUCH THAT THE I-TH ROW AND THE RINT(I)-TH ROW WERE
C     INTERCHANGED AT THE I-TH STEP. THE DETERMINANT, (DETR + I *
C     DETI) * 2.0**IDETE, OF A IS ALSO COMPUTED. THE SUBROUTINE
C     WILL
C     FAIL IF A, MODIFIED BY THE ROUNDING ERRORS, IS SINGULAR.
C     1ST DECEMBER 1971
C
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='F03AHF')
C     .. Scalar Arguments ..
      DOUBLE PRECISION  DETI, DETR
      INTEGER           IA, IDETE, IFAIL, N
C     .. Array Arguments ..
      COMPLEX*16        A(IA,N)
      DOUBLE PRECISION  RINT(N)
C     .. Local Scalars ..
      COMPLEX*16        CZ
      DOUBLE PRECISION  W, X, Y, Z
      INTEGER           I, J, K, L
C     .. Local Arrays ..
      CHARACTER*1       P01REC(1)
C     .. External Functions ..
      INTEGER           P01ABF
      EXTERNAL          P01ABF
C     .. External Subroutines ..
      EXTERNAL          ZGEMV, ZTRSV
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DIMAG, DBLE
C     .. Executable Statements ..
      DO 20 I = 1, N
         RINT(I) = 0.0D0
   20 CONTINUE
      DO 60 J = 1, N
         DO 40 I = 1, N
            RINT(I) = RINT(I) + DBLE(A(I,J))**2 + DIMAG(A(I,J))**2
   40    CONTINUE
   60 CONTINUE
      DO 80 I = 1, N
         IF (RINT(I).LE.0.0D0) GO TO 240
   80 CONTINUE
      DETR = 1.0D0
      DETI = 0.0D0
      IDETE = 0
      DO 220 K = 1, N
         L = K
         Z = 0.0D0
         DO 100 I = K, N
            X = DBLE(A(I,K))
            Y = DIMAG(A(I,K))
            X = (X*X+Y*Y)/RINT(I)
            IF (X.LE.Z) GO TO 100
            Z = X
            L = I
  100    CONTINUE
         IF (L.EQ.K) GO TO 140
         DETR = -DETR
         DETI = -DETI
         DO 120 J = 1, N
            CZ = A(K,J)
            A(K,J) = A(L,J)
            A(L,J) = CZ
  120    CONTINUE
         RINT(L) = RINT(K)
  140    RINT(K) = L
         X = DBLE(A(K,K))
         Y = DIMAG(A(K,K))
         Z = X*X + Y*Y
         W = X*DETR - Y*DETI
         DETI = X*DETI + Y*DETR
         DETR = W
         IF (ABS(DETR).LE.ABS(DETI)) W = DETI
         IF (W.EQ.0.0D0) GO TO 240
  160    IF (ABS(W).LT.1.0D0) GO TO 180
         W = W*0.0625D0
         DETR = DETR*0.0625D0
         DETI = DETI*0.0625D0
         IDETE = IDETE + 4
         GO TO 160
  180    IF (ABS(W).GE.0.0625D0) GO TO 200
         W = W*16.0D0
         DETR = DETR*16.0D0
         DETI = DETI*16.0D0
         IDETE = IDETE - 4
         GO TO 180
  200    IF (K.LT.N) THEN
            CALL ZTRSV('L','N','N',K,A,IA,A(1,K+1),1)
            CALL ZGEMV('N',N-K,K,(-1.0D0,0.0D0),A(K+1,1),IA,A(1,K+1),1,
     *                 (1.0D0,0.0D0),A(K+1,K+1),1)
         END IF
  220 CONTINUE
      IFAIL = 0
      RETURN
  240 IDETE = 0
      IFAIL = P01ABF(IFAIL,1,SRNAME,0,P01REC)
      RETURN
      END
      INTEGER FUNCTION P01ABF(IFAIL,IERROR,SRNAME,NREC,REC)
C     MARK 11.5(F77) RELEASE. NAG COPYRIGHT 1986.
C     MARK 13 REVISED. IER-621 (APR 1988).
C     MARK 13B REVISED. IER-668 (AUG 1988).
C
C     P01ABF is the error-handling routine for the NAG Library.
C
C     P01ABF either returns the value of IERROR through the routine
C     name (soft failure), or terminates execution of the program
C     (hard failure). Diagnostic messages may be output.
C
C     If IERROR = 0 (successful exit from the calling routine),
C     the value 0 is returned through the routine name, and no
C     message is output
C
C     If IERROR is non-zero (abnormal exit from the calling routine),
C     the action taken depends on the value of IFAIL.
C
C     IFAIL =  1: soft failure, silent exit (i.e. no messages are
C                 output)
C     IFAIL = -1: soft failure, noisy exit (i.e. messages are output)
C     IFAIL =-13: soft failure, noisy exit but standard messages from
C                 P01ABF are suppressed
C     IFAIL =  0: hard failure, noisy exit
C
C     For compatibility with certain routines included before Mark 12
C     P01ABF also allows an alternative specification of IFAIL in which
C     it is regarded as a decimal integer with least significant digits
C     cba. Then
C
C     a = 0: hard failure  a = 1: soft failure
C     b = 0: silent exit   b = 1: noisy exit
C
C     except that hard failure now always implies a noisy exit.
C
C     S.Hammarling, M.P.Hooper and J.J.du Croz, NAG Central Office.
C
C     .. Scalar Arguments ..
      INTEGER                 IERROR, IFAIL, NREC
      CHARACTER*(*)           SRNAME
C     .. Array Arguments ..
      CHARACTER*(*)           REC(*)
C     .. Local Scalars ..
      INTEGER                 I, NERR
      CHARACTER*72            MESS
C     .. External Subroutines ..
      EXTERNAL                P01ABZ, X04AAF, X04BAF
C     .. Intrinsic Functions ..
      INTRINSIC               ABS, MOD
C     .. Executable Statements ..
      IF (IERROR.NE.0) THEN
C        Abnormal exit from calling routine
         IF (IFAIL.EQ.-1 .OR. IFAIL.EQ.0 .OR. IFAIL.EQ.-13 .OR.
     *       (IFAIL.GT.0 .AND. MOD(IFAIL/10,10).NE.0)) THEN
C           Noisy exit
            CALL X04AAF(0,NERR)
            DO 20 I = 1, NREC
               CALL X04BAF(NERR,REC(I))
   20       CONTINUE
            IF (IFAIL.NE.-13) THEN
               WRITE (MESS,FMT=99999) SRNAME, IERROR
               CALL X04BAF(NERR,MESS)
               IF (ABS(MOD(IFAIL,10)).NE.1) THEN
C                 Hard failure
                  CALL X04BAF(NERR,
     *                     ' ** NAG hard failure - execution terminated'
     *                        )
                  CALL P01ABZ
               ELSE
C                 Soft failure
                  CALL X04BAF(NERR,
     *                        ' ** NAG soft failure - control returned')
               END IF
            END IF
         END IF
      END IF
      P01ABF = IERROR
      RETURN
C
99999 FORMAT (' ** ABNORMAL EXIT from NAG Library routine ',A,': IFAIL',
     *  ' =',I6)
      END
      SUBROUTINE P01ABZ
C     MARK 11.5(F77) RELEASE. NAG COPYRIGHT 1986.
C
C     Terminates execution when a hard failure occurs.
C
C     ******************** IMPLEMENTATION NOTE ********************
C     The following STOP statement may be replaced by a call to an
C     implementation-dependent routine to display a message and/or
C     to abort the program.
C     *************************************************************
C     .. Executable Statements ..
      STOP
      END
      DOUBLE PRECISION FUNCTION X02AJF()
C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
C
C     RETURNS  (1/2)*B**(1-P)  IF ROUNDS IS .TRUE.
C     RETURNS  B**(1-P)  OTHERWISE
C
C     .. Executable Statements ..
      X02AJF =   0.11102230246251567625d-015
      RETURN
      END
      DOUBLE PRECISION FUNCTION X02AKF()
C     MARK 12 RELEASE. NAG COPYRIGHT 1986.
C
C     RETURNS  B**(EMIN-1)  (THE SMALLEST POSITIVE MODEL NUMBER)
C
      DOUBLE PRECISION SMALL
C     .. Data Statements ..
      DATA  SMALL /'10000000000000'X/	 
C     .. Executable Statements ..
      X02AKF = SMALL
      RETURN
      END
      SUBROUTINE X04AAF(I,NERR)
C     MARK 7 RELEASE. NAG COPYRIGHT 1978
C     MARK 7C REVISED IER-190 (MAY 1979)
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C     IF I = 0, SETS NERR TO CURRENT ERROR MESSAGE UNIT NUMBER
C     (STORED IN NERR1).
C     IF I = 1, CHANGES CURRENT ERROR MESSAGE UNIT NUMBER TO
C     VALUE SPECIFIED BY NERR.
C
C     .. Scalar Arguments ..
      INTEGER           I, NERR
C     .. Local Scalars ..
      INTEGER           NERR1
C     .. Save statement ..
      SAVE              NERR1
C     .. Data statements ..
      DATA              NERR1/0/
C     .. Executable Statements ..
      IF (I.EQ.0) NERR = NERR1
      IF (I.EQ.1) NERR1 = NERR
      RETURN
      END
      SUBROUTINE X04BAF(NOUT,REC)
C     MARK 11.5(F77) RELEASE. NAG COPYRIGHT 1986.
C
C     X04BAF writes the contents of REC to the unit defined by NOUT.
C
C     Trailing blanks are not output, except that if REC is entirely
C     blank, a single blank character is output.
C     If NOUT.lt.0, i.e. if NOUT is not a valid Fortran unit identifier,
C     then no output occurs.
C
C     .. Scalar Arguments ..
      INTEGER           NOUT
      CHARACTER*(*)     REC
C     .. Local Scalars ..
      INTEGER           I
C     .. Intrinsic Functions ..
      INTRINSIC         LEN
C     .. Executable Statements ..
      IF (NOUT.GE.0) THEN
C        Remove trailing blanks
         DO 20 I = LEN(REC), 2, -1
            IF (REC(I:I).NE.' ') GO TO 40
   20    CONTINUE
C        Write record to external file
   40    WRITE (NOUT,FMT=99999) REC(1:I)
      END IF
      RETURN
C
99999 FORMAT (A)
      END
#endif
c
      subroutine dum2
c     dummy routine for cray compilation
      end
c
