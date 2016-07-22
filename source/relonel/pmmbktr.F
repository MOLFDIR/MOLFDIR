C
      SUBROUTINE PMMBKTR(N,UR,UI,M,D,HR,HI)
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     =======================================================================
C     PERFORM BACKTRANSFORMATION [U+][D][U]=[H]
C     N---ACTUAL DIMENSION OF THE MATRICES
C     M---FIRST DIMENSION OF UR AND UI AS DECLARED IN THE CALLING PROGRAM
C     D---REAL DIAGONAL MATRIX
C     U---SQUARE TRANSFORMATION MATRIX   UR-REAL UI-IMAGINARY PART
C     H---LOWER TRIANGLE OF COMPLEX*16 HERMITIAN MATRIX (HR,HI)
C     ========================================================================
C
      DIMENSION UR(M,N),UI(M,N),HR(N*(N+1)/2),HI(N*(N+1)/2),D(N)
C
      L=0
      DO 30 I=1,N
      DO 20 K=1,I
      SUMR=0.0
      SUMI=0.0
      L=L+1
      DO 10 J=1,N
      DD=D(J)
      SUMR=SUMR+UR(I,J)*DD*UR(K,J)+UI(I,J)*DD*UI(K,J)
      SUMI=SUMI-UR(I,J)*DD*UI(K,J)+UI(I,J)*DD*UR(K,J)
   10 CONTINUE
      HR(L)=SUMR
      HI(L)=SUMI
   20 CONTINUE
   30 CONTINUE
      RETURN
      END
