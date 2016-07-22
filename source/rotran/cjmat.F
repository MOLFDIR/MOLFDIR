      SUBROUTINE CJMAT(NDIM1,A,LDA)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     A =  A(Conjg. Transp.)
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C     Tue Oct  4 10:07:46 PDT 1994
C
C---------------Calling variables--------------------------------------
C
      INTEGER NDIM1,LDA
      COMPLEX*16 A(LDA,LDA)
C
C---------------Common Blocks--------------------------------------
C
C---------------Local variables--------------------------------------
C
      INTEGER I,J
      COMPLEX*16 SWAP
C
C---------------Executable code--------------------------------------
C
      DO I = 1, NDIM1
         DO J = 1, I
            SWAP = A(I,J)
#if defined (BIT64)
            A(I,J) = CONJG(A(J,I))
            A(J,I) = CONJG(SWAP)
#else
            A(I,J) = DCONJG(A(J,I))
            A(J,I) = DCONJG(SWAP)
#endif
         ENDDO
      ENDDO
C          
      RETURN
      END
