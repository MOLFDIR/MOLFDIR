      SUBROUTINE TRFDA1(NG,NIJSIZ,NIJBUF,IPQL,NB,LBRCIJ,INDG,
     +                  NBCK,LBCK,G,HA,RI,RIA,RIB,CBCK)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE "vecs.inc"
      INCLUDE "inpt.inc"
      INCLUDE "molf.inc"
C
      COMPLEX*16 A0,A1
#if defined (BIT64)
      PARAMETER (A0=(0.0,0.0),A1=(1.0,0.0))
#else
      PARAMETER (A0=(0.D0,0.D0),A1=(1.D0,0.D0))
#endif
C
      INTEGER NG,NIJSIZ,NIJBUF,IPQL,NB,LBRCIJ(0:*),INDG(NG,2)
      INTEGER NBCK(*),LBCK(NIJSIZ,NIJBUF)
      REAL*8 G(NG)
      COMPLEX*16 HA(NSC,NSP),RI(NSP,NSP),RIA(NSP,NSP),RIB(NSP,NSP)
      COMPLEX*16 CBCK(NIJSIZ,NIJBUF)
C

C ******* UNPACK THE PQ LABEL (is actually the second index pair !)

      IP=0.5D0+0.5D0*DSQRT(DBLE(8*IPQL-7))
      IQ=IPQL-IP*(IP-1)/2

          IF (.NOT.BREIT) THEN

C.......Coulomb part : Transform only with QA; BB Contribution by TR symmetry

             CALL MATMUL1(G,NB,INDG,NG,QA,N22,HA,NSC,1,NSC,NSP)
#if defined (BIT64)
             CALL CGEMM ('C','N',NSP,NSP,NSC,A1,QA(1,1),N22,
#else
             CALL ZGEMM ('C','N',NSP,NSP,NSC,A1,QA(1,1),N22,
#endif
     &                   HA(1,1),NSC,A0,RI,NSP)
             CALL MTIMHER(RIA,NSP,RI,NSP,NSP,1)
             CALL TROUT(NSP,NKR,RIA,NSP,IP,IQ,NIJSIZ,NIJBUF,
     &                  NBCK,LBCK,LBRCIJ,CBCK)

          ELSE

C.......Gaunt part : Only AA and BA part; BB, AB Contribution by TR symmetry

             CALL MATMUL1(G,NB,INDG,NG,QA,N22,HA,NSC,1,NSC,NSP)
C            ... (--|AA) - (--|BB) ...
#if defined (BIT64)
             CALL CGEMM ('C','N',NSP,NSP,NSC,A1,QA(1,1),N22,
#else
             CALL ZGEMM ('C','N',NSP,NSP,NSC,A1,QA(1,1),N22,
#endif
     &                   HA(1,1),NSC,A0,RI,NSP)
             CALL MTIMHER(RIA,NSP,RI,NSP,NSP,-1)
C            ... (--|BA) AND (--|AB) ...
#if defined (BIT64)
             CALL CGEMM ('C','N',NSP,NSP,NSC,A1,QB(1,1),N22,
#else
             CALL ZGEMM ('C','N',NSP,NSP,NSC,A1,QB(1,1),N22,
#endif
     &                   HA(1,1),NSC,A0,RI,NSP)
             CALL MTIMHR2(RIB,NSP,RI,NSP,NSP)
C
             CALL TRBOUT(NSP,NKR,RIA,RIB,NSP,IP,IQ,NIJSIZ,NIJBUF,
     &                   NBCK,LBCK,LBRCIJ,CBCK)
          ENDIF
      RETURN
      END
