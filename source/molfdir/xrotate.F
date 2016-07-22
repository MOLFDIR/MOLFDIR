      SUBROUTINE XROTATE(ISYMOP,ICOMOP,N7,U,CSFA,CSFB,IND,NBAS,CFA,CFB)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C---------------Description--------------------------------------------
C
C     Rotates symmetry function with operation ISYMOP. 
C     Function should be in CSFA and CSFB, rotated function is written
C     in CFA and CFB.
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C     December 1994.
C
C---------------Calling variables--------------------------------------
C
      COMPLEX*16 U(2,2,48)
      INTEGER ISYMOP,IND,ICOMOP(N7,48)
      COMPLEX*16 CSFA(*),CSFB(*),CFA(*),CFB(*)
C
C---------------Common Blocks--------------------------------------
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      DO I = 1, NBAS
       CFA(I) = (0.0,0.0)
       CFB(I) = (0.0,0.0)
      ENDDO
C
      DO I = 1, NBAS
         IT=ICOMOP(I+IND-1,ISYMOP)
         ITA=IABS(IT)
         ISN=ISIGN(1,IT)
         ITA=ITA-IND+1
         CFA(ITA)=CFA(ITA)+ISN*(CSFA(I)*U(1,1,ISYMOP)+
     $                          CSFB(I)*U(1,2,ISYMOP))
         CFB(ITA)=CFB(ITA)+ISN*(CSFA(I)*U(2,1,ISYMOP)+
     $                          CSFB(I)*U(2,2,ISYMOP))
      ENDDO
C
      RETURN
      END
