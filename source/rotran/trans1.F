      SUBROUTINE TRANS1(IPASS,IPQMIN,IPQMAX,INDPQ,NG,NIJSIZ,NIJBUF,
     &                  LBRCPQ,LBRCIJ,INDG,NBCK,LBCK,
     &                  G,HA,RI,RIA,RIB,CBCK)
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
      INTEGER IPASS,IPQMIN(*),IPQMAX(*),INDPQ(*)
      INTEGER NG,INDG(NG,2)
      INTEGER NIJSIZ,NIJBUF,NBCK(NIJBUF),LBCK(NIJSIZ,NIJBUF)
      REAL*8 G(NG)
      COMPLEX*16 HA(NSC,NSP),RI(NSP,NSP),RIA(NSP,NSP),RIB(NSP,NSP)
      COMPLEX*16 CBCK(NIJSIZ,NIJBUF)
      LOGICAL ANYPQ
      LOGICAL GETSTI
C
      CALL IZERO (NIJBUF,NBCK,1)
      IF (BREIT) CALL IZERO (2*NIJBUF,NBCK,1)
C
      MINPQ=IPQMIN(IPASS)
      MAXPQ=IPQMAX(IPASS)
      INPQMIN = INDPQ(MINPQ)
      IPQ=MINPQ-1
#if defined (BIT64)
      IPS=0.50+0.50* SQRT(REAL(8*MINPQ-7))
#else
      IPS=0.5D0+0.5D0*DSQRT(DBLE(8*MINPQ-7))
#endif
      IQS=MINPQ-IPS*(IPS-1)/2
#if defined (BIT64)
      IPE=0.50+0.50* SQRT(REAL(8*MAXPQ-7))
#else
      IPE=0.5D0+0.5D0*DSQRT(DBLE(8*MAXPQ-7))
#endif
      IQE=MAXPQ-IPE*(IPE-1)/2
C
C     ------------------------
      DO 10 IP=IPS,IPE
        IQF=1
        IF (IP.EQ.IPS) IQF=IQS
        IQL=IP
        IF (IP.EQ.IPE) IQL=IQE
        DO 15 IQ=IQF,IQL
C     ------------------------
C
          IPQ=IPQ+1
          INPQ=INDPQ(IPQ)
          IF (INPQ.EQ.0) GOTO 15
          ANYPQ=GETSTI(INPQ-INPQMIN+1,G,NB,INDG,NG,LBRCPQ)
          IF (.NOT.ANYPQ) GOTO 15
C
C.......Since IP.LE.IR we can restrict the range somewhat
C
c         NSCR = NSC - IP + 1
          NSCR = NSC
C
          IF (.NOT.BREIT) THEN
C.......Coulomb part : Transform only with QA; BB Contribution by TR symmetry
             CALL MATMUL1(G,NB,INDG,NG,QA,N22,HA,NSC,1,NSC,NSP)
#if defined (BIT64)
             CALL CGEMM ('C','N',NSP,NSP,NSCR,A1,QA(1,1),N22,
#else
             CALL ZGEMM ('C','N',NSP,NSP,NSCR,A1,QA(1,1),N22,
#endif
     &                   HA(1,1),NSC,A0,RI,NSP)
             CALL MTIMHER(RIA,NSP,RI,NSP,NSP,1)
             CALL TROUT(NSP,NKR,RIA,NSP,IP,IQ,NIJSIZ,NIJBUF,NBCK,LBCK,
     &                  LBRCIJ,CBCK)
          ELSE
C.......Gaunt part : Only AA and BA part; BB, AB Contribution by TR symmetry
             CALL MATMUL1(G,NB,INDG,NG,QA,N22,HA,NSC,1,NSC,NSP)
C            ... (--|AA) - (--|BB) ...
#if defined (BIT64)
             CALL CGEMM ('C','N',NSP,NSP,NSCR,A1,QA(1,1),N22,
#else
             CALL ZGEMM ('C','N',NSP,NSP,NSCR,A1,QA(1,1),N22,
#endif
     &                   HA(1,1),NSC,A0,RI,NSP)
             CALL MTIMHER(RIA,NSP,RI,NSP,NSP,-1)
C            ... (--|BA) AND (--|AB) ...
#if defined (BIT64)
             CALL CGEMM ('C','N',NSP,NSP,NSCR,A1,QB(1,1),N22,
#else
             CALL ZGEMM ('C','N',NSP,NSP,NSCR,A1,QB(1,1),N22,
#endif
     &                   HA(1,1),NSC,A0,RI,NSP)
             CALL MTIMHR2(RIB,NSP,RI,NSP,NSP)
C
             CALL TRBOUT(NSP,NKR,RIA,RIB,NSP,IP,IQ,NIJSIZ,NIJBUF,
     &                   NBCK,LBCK,LBRCIJ,CBCK)
          ENDIF
 15       CONTINUE
 10     CONTINUE
      RETURN
      END
