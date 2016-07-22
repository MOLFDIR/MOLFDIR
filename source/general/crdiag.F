C
      SUBROUTINE CRDIAG (NM,N,AR,AI,W,ZR,ZI,FV1,FV2,FM1,IERR)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     DIAGONALIZE COMPLEX,HERMITIAN MATRIX BY CALLING APPROPRIATE LIBRARY
C     Checks for reality of matrix and calls real diagonalizer if possible.
C
C     1) CALL EISPACK LIBRARY
C     2) CALL NAG LIBRARY
C
C     AR, AI  : MATRIX TO BE DIAGONALIZED ON INPUT; EIGENVECTORS ON OUTPUT
C     ZR, ZI  : WORKING SPACE
C     W       : EIGENVALUES
C     FV1,FM1 : WORKING SPACE
C
      COMMON/DIAGSTAT/IDIAGR,IDIAGC
C
      DIMENSION AR(NM,NM),AI(NM,NM),W(NM),ZR(NM,NM),ZI(NM,NM)
      DIMENSION FV1(NM),FM1(NM)
C
#if defined (CRAY)
      PARAMETER (ILIB = 1)
#else
      PARAMETER (ILIB = 2)
#endif
      DO I = 1, N
      DO J = 1, N
#if defined (BIT64)
         IF (ABS(AI(J,I)).GT.1.D-12) GOTO 11
#else
         IF (DABS(AI(J,I)).GT.1.D-12) GOTO 11
#endif
      ENDDO
      ENDDO
      IDIAGR = IDIAGR + 1
      CALL RS (NM,N,AR,W,1,AI,FV1,FM1,IERR)
      DO I = 1, N
      DO J = 1, N
         AR(J,I) = AI(J,I)
         AI(J,I) = 0.D0
      ENDDO
      ENDDO
      RETURN
   11 CONTINUE
      IDIAGC = IDIAGC + 1
      IF (ILIB.EQ.1) THEN
         CALL CH (NM,N,AR,AI,W,1,ZR,ZI,FV1,FV2,FM1,IERR)
         DO 10 I = 1, N
         DO 10 J = 1, N
         AR(I,J) = ZR(I,J)
         AI(I,J) = ZI(I,J)
   10    CONTINUE
      ELSEIF (ILIB.EQ.2) THEN
C        TOL = X02ADF ()
C        IN MARK 14 NAG TOL IS NO LONGER REALLY USED
         TOL = 0.D0
         EPS = X02AJF ()
         CALL F01BCF (N,TOL,AR,NM,AI,NM,W,FV1,ZR,ZI)
         CALL F02AYF (N,EPS,W,FV1,AR,NM,AI,NM,IERR)
      ENDIF
      RETURN
      END
