      SUBROUTINE SYMTRIZ (NSYMRP,MDINT,LBRCSK,NRJ,INDRJ,JNDRJ,INDKLP,
     &                    INDKL,RJ,RL,REDME)
C
C     Symmetrizes skeleton integrals by transforming to and from
C     reduced matrix elements.
C     Uses ordened time-reversal unique list of skeleton integrals.
C     Produces ordened tr and permutation unique list of MS integrals
C     Written august 1994 by Luuk Visscher
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE "inpt.inc"
      INCLUDE "vecs.inc"
      INCLUDE "redme.inc"
      INCLUDE "sym.inc"
C
      INTEGER LBRCSK(NSP,*),INDRJ(NRJ),INDKLP(NSYMRP,NSYMRP,2),
     &        INDKL(*),JNDRJ(*),NSYMRP,MDINT,NRJ
      COMPLEX*16 A,B,RJ(NRJ),RL(NSP*NSP),REDME(*)
      LOGICAL ANYSKEL,GETSKEL
C
C     -----------------------------------
C     Make index array for gather/scatter 
C     -----------------------------------
C
      CALL SKELIND(NSP,NSYMRP,MOFSET,INDKLP,INDKL)
C
C     ------------------------------------
C     Loop over (multi-dimensional) irreps
C     ------------------------------------
      DO IRA = 1, NREP
         DO IRB = 1, IRA
C     ------------------------------------
C     Loop over spinors in this irrep pair
C     ------------------------------------
            DO MOI = 1, NFIE(IRA)
               DO MOJ = 1, NFIE(IRB)
C     -------------------------------------
C     Zero the reduced matrix element array
C     -------------------------------------
                  CALL CZERO (NRME(IRA,IRB),REDME)
C     ---------------------------------
C     Loop over rows in this irrep pair
C     ---------------------------------
                  DO IA = IOFFRP(IRA), IOFFRP(IRA+1)-1
                     I = MOFSET(IA) + MOI
                     DO IB = IOFFRP(IRB), IOFFRP(IRB+1)-1 
                        IF (MMATA(IA,IB).EQ.0) GOTO 10
                        J = MOFSET(IB) + MOJ
C     --------------------------------
C     Read batch of skeleton integrals
C     --------------------------------
                        ANYSKEL = GETSKEL(BREIT,NKR,NSP,I,J,RL,
     &                                    RJ,NRJ,INDRJ,LBRCSK)
C     --------------------------------------------
C     Add contributions to reduced matrix elements
C     --------------------------------------------
                        DO IRME = 1, MMATA(IA,IB)
                           NMAT = MMATPA(IRME,IA,IB)
                           ITAU = ITAUA(NMAT)
                           INDEX = INDTAU(ITAU)
                           A = AMAT(NMAT)
                           IK = KA(NMAT)
                           IL = LA(NMAT)
                           NKL = NOC(IK)*NOC(IL)
                           KLP = INDKLP(IK,IL,1)
#if defined (BIT64)
                           CALL CGTHR(NKL,RL,RJ,INDKL(KLP))
                           CALL CAXPY(NKL,A,RJ,1,REDME(INDEX),1)
#else
                           CALL ZGTHR(NKL,RL,RJ,INDKL(KLP))
                           CALL ZAXPY(NKL,A,RJ,1,REDME(INDEX),1)
#endif
                        ENDDO
C     -----------------------------------------------------
C     All contributions from this row combination are added
C     ------------------------------------------------------
   10                   CONTINUE
                     ENDDO
                  ENDDO
C     ----------------------------------------------
C     The reduced matrix elements are complete
C     Backtransformation to get completed integrals
C     Loop over rows of the irrep-pair
C     ----------------------------------------------
                  DO IA = IOFFRP(IRA), IOFFRP(IRA+1)-1
                     I = MOFSET(IA) + MOI
                     IKR = IKRMO(I)
                     DO IB = IOFFRP(IRB), IOFFRP(IRB+1)-1 
                        J = MOFSET(IB) + MOJ
                        JKR = IKRMO(J)
                        IF (MMATB(IA,IB).EQ.0) GOTO 20
                        IF (ABS(IKR).LT.ABS(JKR)) GOTO 20
C     ----------------------------
C     Zero this block of integrals
C     ----------------------------
                        CALL CZERO (NSP*NSP,RL)
C     ---------------------------
C     Loop over all contributions
C     ---------------------------
                        DO IRME = 1, MMATB(IA,IB)
                           NMAT = MMATPB(IRME,IA,IB)
                           ITAU = ITAUB(NMAT)
                           INDEX = INDTAU(ITAU)
                           B = BMAT(NMAT)
                           IK = KB(NMAT)
                           IL = LB(NMAT)
                           NKL = NOC(IK)*NOC(IL)
                           KLP = INDKLP(IK,IL,1)
                           CALL CZERO (NKL,RJ)
#if defined (BIT64)
                           CALL CAXPY(NKL,B,REDME(INDEX),1,RJ,1)
#else
                           CALL ZAXPY(NKL,B,REDME(INDEX),1,RJ,1)
#endif
                           DO IKL = 1, NKL
                            RL(INDKL(KLP+IKL-1)) = RL(INDKL(KLP+IKL-1)) 
     &                                           + RJ(IKL)
                           ENDDO
                        ENDDO
C     -----------------------------------------------------------
C     All contributions are added : write out completed integrals
C     -----------------------------------------------------------
                        CALL PHASETR (.FALSE.,NSP,I,J,RL,NSP)
                        CALL WRITWOE (IPRNT,NKR,MDINT,IKR,JKR,INDRJ,
     &                                JNDRJ,RL,NSP,RJ)
   20                   CONTINUE
                     ENDDO
                  ENDDO
C     ----------------------------------------
C     End of loops over rows in the irrep pair
C     ----------------------------------------
               ENDDO
            ENDDO
C     -------------------------------------------
C     End of loops over spinors in the irrep pair
C     -------------------------------------------
         ENDDO
      ENDDO
C
      RETURN
      END
