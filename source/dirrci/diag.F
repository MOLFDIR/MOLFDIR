C
      SUBROUTINE DIAG
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     ----------------------
C     DIAGONALIZES CI MATRIX
C     ----------------------
C
      INCLUDE 'param.inc'
      INCLUDE 'general.inc'
      INCLUDE 'iterat.inc'
C
      DIMENSION WORKR(N2,N2),WORKI(N2,N2)
      DIMENSION WORK2(N2),WORK3(N2),WORK4(2,N2)
C
      DO  1 I = 1, N2
      DO  1 J = 1, N2
         EVECR (I,J) = 0.0D0
         IF (.NOT.REALAR) EVECI(I,J) = 0.0D0
    1 CONTINUE
C        
      DO 10 I = 1, NVEC
      DO 10 J = 1, NVEC
   10    EVECR (I,J) = CMTR (I,J)
C
      IF (REALAR) THEN
         CALL RS (N2,NVEC,EVECR,EIGEN,1,EVECI,WORK2,WORK3,IERR)
         DO I = 1, NVEC
         DO J = 1, NVEC
            EVECR(J,I) = EVECI(J,I)
         ENDDO
         ENDDO
C     ----
      ELSE
C     ----
         DO 11 I = 1, NVEC
         DO 11 J = 1, NVEC
   11       EVECI (I,J) = CMTI (I,J)
         CALL CH (N2,NVEC,EVECR,EVECI,EIGEN,1,WORKR,WORKI,WORK2,
     $            WORK3,WORK4,IFAIL)
         DO 20 I = 1, NVEC
         DO 20 J = 1, NVEC
         EVECR(I,J) = WORKR(I,J)
         EVECI(I,J) = WORKI(I,J)
   20    CONTINUE
         IF (IFAIL.NE.0) STOP'**ERROR-CANNOT DIAGONALISE CMTR**'
C     -----
      ENDIF
C     -----
      RETURN
      END
