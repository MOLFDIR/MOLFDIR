      SUBROUTINE IZERO(N,IA,INCA)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C---------------Description--------------------------------------------
C
C     Zeroes integer vector
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C     Tue Sep 13 10:16:46 PDT 1994
C
C---------------Calling variables--------------------------------------
C
      INTEGER IA(*),N,INCA
C
C---------------Common Blocks--------------------------------------
C
C---------------Local variables--------------------------------------
C
      INTEGER I,II
C
C---------------Executable code--------------------------------------
C
      II = 1
      DO I = 1, N
         IA(II) = 0
         II = II + INCA
      ENDDO
C
      RETURN
      END

