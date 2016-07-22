      SUBROUTINE CJVEC(NDIM1,A)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     A =  A(Conjg.)
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      INTEGER NDIM1
      COMPLEX*16 A(NDIM1)
C
C---------------Common Blocks--------------------------------------
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      DO I = 1, NDIM1
#if defined (BIT64)
         A(I) = CONJG(A(I))
#else
         A(I) = DCONJG(A(I))
#endif
      ENDDO
C          
      RETURN
      END
