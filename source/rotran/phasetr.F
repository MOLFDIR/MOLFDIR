      SUBROUTINE PHASETR(BACK,NSP,I,J,A,LDA)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Transforms integrals back to original phase choice of spinors
C     If (Back) : backtransformation (from Kramers choice to original)
C     Else :  forward transformation (vice versa)
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
      LOGICAL BACK
      INTEGER NSP,I,J,LDA
      COMPLEX*16 A(LDA,LDA)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "vecs.inc"
C
C---------------Local variables--------------------------------------
C
      COMPLEX*16 PIJ,PIJK,PIJKL
C
C---------------Executable code--------------------------------------
C
#if defined (BIT64)
      PIJ = CONJG(RPHASE(I)) * RPHASE(J)
#else
      PIJ = DCONJG(RPHASE(I)) * RPHASE(J)
#endif
      DO K = 1, NSP
#if defined (BIT64)
         PIJK = PIJ * CONJG(RPHASE(K))
#else
         PIJK = PIJ * DCONJG(RPHASE(K))
#endif
         DO L = 1, NSP
            PIJKL = PIJK * RPHASE(L)
            IF (BACK) A(K,L) = A(K,L) * PIJKL
#if defined (BIT64)
            IF (.NOT.BACK) A(K,L) = A(K,L) * CONJG(PIJKL)
#else
            IF (.NOT.BACK) A(K,L) = A(K,L) * DCONJG(PIJKL)
#endif
         ENDDO
      ENDDO
C          
      RETURN
      END
