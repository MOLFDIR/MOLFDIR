C
C     ================================
      SUBROUTINE HMAT (ID, JD, HMR, HMI)
C     ================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INCLUDE 'param.inc'
      INCLUDE 'common.inc'
      INCLUDE 'general.inc'
C
#if defined (CRAY) 
      IAND (I, J) = AND (I, J)
      IOR (I, J) = OR (I, J)
      INOT (I) = COMPL (I)
      IXOR (I, J) = IAND (IOR (I, J), INOT (IAND (I, J)))
      ISHIFT (I, J) = SHIFT (I, J)
#endif
#if defined (IBM) || defined (SGI)
      INOT (I) = NOT (I)
      IXOR (I, J) = IAND (IOR (I, J), INOT (IAND (I, J)))
      ISHIFT (I, J) = ISHFT (I, J)
#endif
#if defined (HP9) || defined (LINUX) || defined (DEC) || defined (SUN)
      IXOR (I, J) = IAND (IOR (I, J), NOT (IAND (I, J)))
      ISHIFT (I, J) = ISHFT (I, J)
#endif
C
      HMR = 0.0D0
      HMI = 0.0D0
C
      IDV = IXOR (ID, JD)
      IT = NBITS (IDV)
C
      IF (IT .EQ. 0) THEN
C
C     ======================
C     DETERMINANTS ARE EQUAL
C     ======================
C
      DO 30 I = 1, NORB
      IF (IAND (ID, ISHIFT (1, I - 1)) .NE. 0) THEN
        HMR = HMR + ONER (I, I)
        DO 20 J = I + 1, NORB
          IF (IAND (ID, ISHIFT (1, J - 1)) .NE. 0) THEN
C---------------------
C H = H + (I, I, J, J)
C---------------------
            INDR = INDTWR(I,I,J,J)
            INDI = INDTWI(I,I,J,J)
            SR = SIGN(1,INDR)
            SI = SIGN(1,INDI)
            INDR = ABS(INDR)
            INDI = ABS(INDI)
            HMR = HMR + SR * TWOINR (INDR)
            HMI = HMI + SI * TWOINI (INDI)
            IF (BREIT) THEN
               INDR = INDGAR(I,I,J,J)
               INDI = INDGAI(I,I,J,J)
               SR = SIGN(1,INDR)
               SI = SIGN(1,INDI)
               INDR = ABS(INDR)
               INDI = ABS(INDI)
               HMR = HMR + SR * TWOINR (INDR)
               HMI = HMI + SI * TWOINI (INDI)
            ENDIF
C---------------------
C H = H - (I, J, J, I)
C---------------------
            INDR = INDTWR(I,J,J,I)
            INDI = INDTWI(I,J,J,I)
            SR = SIGN(1,INDR)
            SI = SIGN(1,INDI)
            INDR = ABS(INDR)
            INDI = ABS(INDI)
            HMR = HMR - SR * TWOINR (INDR)
            HMI = HMI - SI * TWOINI (INDI)
            IF (BREIT) THEN
               INDR = INDGAR(I,J,J,I)
               INDI = INDGAI(I,J,J,I)
               SR = SIGN(1,INDR)
               SI = SIGN(1,INDI)
               INDR = ABS(INDR)
               INDI = ABS(INDI)
               HMR = HMR - SR * TWOINR (INDR)
               HMI = HMI - SI * TWOINI (INDI)
            ENDIF
          END IF
 20       CONTINUE
        END IF
 30     CONTINUE
      RETURN
      END IF
C
      IF(IT.EQ.2) THEN
C
C     ===================================
C     DETERMINANTS DIFFER BY ONE FUNCTION
C     ===================================
C
      IA = IAND (IDV, ID)
      IB = IAND (IDV, JD)
      DO 35 I = 1, NORB
        IF (IAND (IA, ISHIFT (1, I - 1)) .NE. 0) K = I
        IF (IAND (IB, ISHIFT (1, I - 1)) .NE. 0) L = I
35      CONTINUE
C
      HMR = HMR + ONER (K, L)
      HMI = HMI + ONEI (K, L)
C
      DO 40 I = 1, NORB
        IF ((I .EQ. K) .OR. (I .EQ. L)) GO TO 40
        IF (IAND (ID, ISHIFT (1, I - 1)) .NE. 0) THEN
C---------------------
C H = H + (I, I, K, L)
C---------------------
            INDR = INDTWR(I,I,K,L)
            INDI = INDTWI(I,I,K,L)
            SR = SIGN(1,INDR)
            SI = SIGN(1,INDI)
            INDR = ABS(INDR)
            INDI = ABS(INDI)
            HMR = HMR + SR * TWOINR (INDR)
            HMI = HMI + SI * TWOINI (INDI)
            IF (BREIT) THEN
               INDR = INDGAR(I,I,K,L)
               INDI = INDGAI(I,I,K,L)
               SR = SIGN(1,INDR)
               SI = SIGN(1,INDI)
               INDR = ABS(INDR)
               INDI = ABS(INDI)
               HMR = HMR + SR * TWOINR (INDR)
               HMI = HMI + SI * TWOINI (INDI)
            ENDIF
C---------------------
C H = H - (I, L, K, I)
C---------------------
            INDR = INDTWR(I,L,K,I)
            INDI = INDTWI(I,L,K,I)
            SR = SIGN(1,INDR)
            SI = SIGN(1,INDI)
            INDR = ABS(INDR)
            INDI = ABS(INDI)
            HMR = HMR - SR * TWOINR (INDR)
            HMI = HMI - SI * TWOINI (INDI)
            IF (BREIT) THEN
               INDR = INDGAR(I,L,K,I)
               INDI = INDGAI(I,L,K,I)
               SR = SIGN(1,INDR)
               SI = SIGN(1,INDI)
               INDR = ABS(INDR)
               INDI = ABS(INDI)
               HMR = HMR - SR * TWOINR (INDR)
               HMI = HMI - SI * TWOINI (INDI)
            ENDIF
        END IF
 40     CONTINUE
      PH = PHASE (K, ID) * PHASE (L, JD)
      HMR = PH * HMR
      HMI = PH * HMI
      RETURN
      END IF
C
      IF(IT.EQ.4) THEN
C
C     ====================================
C     DETERMINANTS DIFFER BY TWO FUNCTIONS
C     ====================================
C
      IA = IAND (IDV, ID)
      IB = IAND (IDV, JD)
      L = 0
      N = 0
      DO 50 I = 1, NORB
        IF (IAND (IA, ISHIFT (1, I - 1)) .NE. 0) THEN
          K = L
          L = I
        END IF
        IF (IAND (IB, ISHIFT (1, I - 1)) .NE. 0) THEN
          M = N
          N = I
        END IF
 50     CONTINUE
C
C---------------------
C H = H + (K, M, L, N)
C---------------------
      INDR = INDTWR(K,M,L,N)
      INDI = INDTWI(K,M,L,N)
      SR = SIGN(1,INDR)
      SI = SIGN(1,INDI)
      INDR = ABS(INDR)
      INDI = ABS(INDI)
      HMR = HMR + SR * TWOINR (INDR)
      HMI = HMI + SI * TWOINI (INDI)
      IF (BREIT) THEN
         INDR = INDGAR(K,M,L,N)
         INDI = INDGAI(K,M,L,N)
         SR = SIGN(1,INDR)
         SI = SIGN(1,INDI)
         INDR = ABS(INDR)
         INDI = ABS(INDI)
         HMR = HMR + SR * TWOINR (INDR)
         HMI = HMI + SI * TWOINI (INDI)
      ENDIF
C---------------------
C H = H - (K, N, L, M)
C---------------------
      INDR = INDTWR(K,N,L,M)
      INDI = INDTWI(K,N,L,M)
      SR = SIGN(1,INDR)
      SI = SIGN(1,INDI)
      INDR = ABS(INDR)
      INDI = ABS(INDI)
      HMR = HMR - SR * TWOINR (INDR)
      HMI = HMI - SI * TWOINI (INDI)
      IF (BREIT) THEN
         INDR = INDGAR(K,N,L,M)
         INDI = INDGAI(K,N,L,M)
         SR = SIGN(1,INDR)
         SI = SIGN(1,INDI)
         INDR = ABS(INDR)
         INDI = ABS(INDI)
         HMR = HMR - SR * TWOINR (INDR)
         HMI = HMI - SI * TWOINI (INDI)
      ENDIF
      PH = PHASE (K, ID) * PHASE (L, ID) *
     +     PHASE (M, JD) * PHASE (N, JD)
      HMR = HMR * PH
      HMI = HMI * PH
      RETURN
      END IF
C
C     =============================================
C     DETERMINANTS DIFFER BY MORE THEN TWO FUNCTIONS
C     ==============================================
C
      HMR = 0.0D0
      HMI = 0.0D0
      RETURN
      END
