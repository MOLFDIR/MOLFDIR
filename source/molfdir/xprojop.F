      SUBROUTINE XPROJOP(IREP,NSYMOP,ICOMOP,N7,PROJOP,U,
     &                   CSFA,CSFB,IND,NBAS,CFA,CFB)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C---------------Description--------------------------------------------
C
C     Projects out (unnormalized) symmetry function for representation 
C     irep. Unprojected function should be in CSFA and CSFB, projected 
C     function is written in CFA and CFB.
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
      COMPLEX*16 PROJOP(48,16),U(2,2,48)
      INTEGER IND,ICOMOP(N7,48)
      COMPLEX*16 CSFA(*),CSFB(*),CFA(*),CFB(*)
C
C---------------Common Blocks--------------------------------------
C
C---------------Local variables--------------------------------------
C
      COMPLEX*16 SNA,SNB
C
C---------------Executable code--------------------------------------
C
      DO I = 1, NBAS
       CFA(I) = (0.0,0.0)
       CFB(I) = (0.0,0.0)
      ENDDO
C
      DO I = 1, NBAS
       IF = I + IND - 1
       DO IOP=1,NSYMOP
         IT=ICOMOP(IF,IOP)
         SNA = CSFA(I)*ISIGN(1,IT)
         SNB = CSFB(I)*ISIGN(1,IT)
         ITA=IABS(IT)
         ITA=ITA-IND+1
#if defined (BIT64)
         CFA(ITA)=CFA(ITA)+CONJG(PROJOP(IOP,IREP))*SNA*U(1,1,IOP)
     &                    +CONJG(PROJOP(IOP,IREP))*SNB*U(1,2,IOP)
         CFB(ITA)=CFB(ITA)+CONJG(PROJOP(IOP,IREP))*SNA*U(2,1,IOP)
     &                    +CONJG(PROJOP(IOP,IREP))*SNB*U(2,2,IOP)
#else
         CFA(ITA)=CFA(ITA)+DCONJG(PROJOP(IOP,IREP))*SNA*U(1,1,IOP)
     &                    +DCONJG(PROJOP(IOP,IREP))*SNB*U(1,2,IOP)
         CFB(ITA)=CFB(ITA)+DCONJG(PROJOP(IOP,IREP))*SNA*U(2,1,IOP)
     &                    +DCONJG(PROJOP(IOP,IREP))*SNB*U(2,2,IOP)
#endif
       ENDDO
      ENDDO
C
      RETURN
      END
