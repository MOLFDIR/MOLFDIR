      SUBROUTINE TRFDA2(NRJ,NSKSIZ,NSKBUF,JI,IA,IB,ISUM,LBRCIJ,
     &          INDRJ,NBCK,LBCK,RJ,RJB,RJC,RK,RLR,RL,CBCK)
C ****************************************************************
C ** PURPOSE:
C ** THIS ROUTINE PERFORMS THE SECOND HALF TRANSFORMATION STEP
C ** (pq|ij) -> (kl|ij)
C ** It is important to note that this step is incomplete since it is 
C ** performed on each node with an incomplete (pq|ij) list
C ** for a specific ij the master has to add the missing contributions
C ** when collecting all the ij data...
C ****************************************************************

C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      PARAMETER (THRSH=1.0D-16)
C
      INCLUDE "inpt.inc"
      INCLUDE "vecs.inc"
      INCLUDE "thres.inc"
      INCLUDE "molf.inc"
      INCLUDE "redme.inc"
      INCLUDE "files.inc"
      INCLUDE "mpidef.inc"
      INCLUDE "mpif.h"
C
      COMPLEX*16 A0,A1

#if defined (BIT64)
      PARAMETER (A0=(0.0,0.0),A1=(1.0,0.0))
#else
      PARAMETER (A0=(0.D0,0.D0),A1=(1.D0,0.D0))
#endif

C
      INTEGER NRJ,NSKSIZ,NSKBUF,JI,IA,IB,ISUM,MRJ,I,J
      INTEGER LBRCIJ(0:*),INDRJ(NRJ,2)
      INTEGER NBCK(NSKBUF),LBCK(NSKSIZ,NSKBUF)
      COMPLEX*16 RJ(NRJ),RJB(NRJ),RJC(NRJ)
      COMPLEX*16 RK(NSC,NSP),RLR(NSP,NSP),RL(NSP,NSP)
      COMPLEX*16 CBCK(NSKSIZ,NSKBUF),CSBDUM(2)

      LOGICAL ANYHTI
      LOGICAL GETHTI
      LOGICAL ANYHTBI
      LOGICAL GETHTBI

C ** clear array from residues of the last JI transformation
C ** in case that there is no JI !

      CALL CZERO(NSP*NSP,RL)
      ISUM=0

C ** do transformation for the specific JI pair

      IF (.NOT.BREIT) THEN
C
C.........COULOMB TYPE
C
      ANYHTI=GETHTI(JI,RJ,NRJ,MRJ,INDRJ,LBRCIJ)
      IF (.NOT.ANYHTI) GOTO 100
      ISUM=1
      CALL MATMUL3(RJ,MRJ,INDRJ,NRJ,QA,N22,RK,NSC,NSP,NSC)
#if defined (BIT64)
      CALL CGEMM ('C','N',NSP,NSP,NSC,A1,QA,N22,RK,NSC,A0,RLR,NSP)
#else
      CALL ZGEMM ('C','N',NSP,NSP,NSC,A1,QA,N22,RK,NSC,A0,RLR,NSP)
#endif
      CALL MSUMTIM(RL,NSP,RLR,NSP,NSP,1)
C
            ELSE
C
C.........BREIT TYPE 
C
      ANYHTBI=GETHTBI(JI,RJ,RJB,RJC,NRJ,MRJ,INDRJ,LBRCIJ)
      IF (.NOT.ANYHTBI) GOTO 100
      ISUM=1
      CALL MATMUL3(RJ,MRJ,INDRJ,NRJ,QA,N22,RK,NSC,NSP,NSC)
#if defined (BIT64)
      CALL CGEMM ('C','N',NSP,NSP,NSC,A1,QA,N22,RK,NSC,A0,RLR,NSP)
#else
      CALL ZGEMM ('C','N',NSP,NSP,NSC,A1,QA,N22,RK,NSC,A0,RLR,NSP)
#endif
      CALL MSUMTIM(RL,NSP,RLR,NSP,NSP,-1)
      CALL MATMUL7(RJB,MRJ,INDRJ,NRJ,QB,N22,RK,NSC,NSP,NSC)
#if defined (BIT64)
      CALL CGEMM ('C','N',NSP,NSP,NSC,A1,QA,N22,RK,NSC,A1,RL,NSP)
#else
      CALL ZGEMM ('C','N',NSP,NSP,NSC,A1,QA,N22,RK,NSC,A1,RL,NSP)
#endif
      CALL MATMUL7(RJC,MRJ,INDRJ,NRJ,QA,N22,RK,NSC,NSP,NSC)
#if defined (BIT64)
      CALL CGEMM ('C','N',NSP,NSP,NSC,A1,QB,N22,RK,NSC,A1,RL,NSP)
#else
      CALL ZGEMM ('C','N',NSP,NSP,NSC,A1,QB,N22,RK,NSC,A1,RL,NSP)
#endif
C
            ENDIF
C
C ** Zero out integrals that should be zero due to molecular symmetry
C
      DO IC = 1, NSYMRP
         DO ID = 1, NSYMRP
            IF (ZEROINT(ID,IC,IB,IA)) THEN
               DO K = MOFSET(IC)+1, MOFSET(IC+1)
                  DO L = MOFSET(ID)+1, MOFSET(ID+1)
                     RL(L,K) = A0
                  ENDDO
               ENDDO
            ENDIF
         ENDDO
      ENDDO
C
C ** in RL(*,*) now the incomplete 2nd HT integrals for a specific JI

      IF (IPRNT.GT.10) THEN
      print*, "MS integrals before symmetrization"
      do kx = 1, nsp
      do lx = 1, nsp
#if defined (BIT64)
         if (cabs(rl(lx,kx)).gt.throut) 
#else
         if (cdabs(rl(lx,kx)).gt.throut) 
#endif
     &   write (*,'(4i4,2f20.16)') lx,kx,i,j,rl(lx,kx)
      continue
      enddo
      enddo
      ENDIF

 100  RETURN
      END
