C
C     ========================================
      SUBROUTINE TRMAT(TAIR,TAII,TBIR,TBII,IM)
C     ========================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INCLUDE 'paracom.inc'
C
      REAL*8 TAIR(N3,N3),TAII(N3,N3),TBIR(N3,N3),TBII(N3,N3)
C
      NBAS=NEQBAS(IM)
      NBS=INDSYM(IM+1)-INDSYM(IM)
      K=IMSET(IM)
      DO 10 J=1,NBS
        DO 20 I=1,NBAS
          TAIR(I,J)=TRANAR(K+(J-1)*NBAS+I)
          TAII(I,J)=TRANAI(K+(J-1)*NBAS+I)
          TBIR(I,J)=TRANBR(K+(J-1)*NBAS+I)
          TBII(I,J)=TRANBI(K+(J-1)*NBAS+I)
 20       CONTINUE
 10     CONTINUE
      RETURN
      END
