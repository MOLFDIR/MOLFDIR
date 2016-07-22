C
C     =========================================================
      SUBROUTINE MFDFMCP (FAAR, FBAR, FAAI, FBAI, FOCKR, FOCKI)
C     =========================================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'param.inc'
      INCLUDE 'common.inc'
C
      REAL*8 FAAR(N21),FBAR(N21),FAAI(N21),FBAI(N21)
C
      DIMENSION TAIR(N3,N3),TBIR(N3,N3),TAKR(N3,N3),TBKR(N3,N3)
      DIMENSION TAII(N3,N3),TBII(N3,N3),TAKI(N3,N3),TBKI(N3,N3)
      REAL*8 TAR(N3,N3),TAI(N3,N3),TBR(N3,N3),TBI(N3,N3)
      DIMENSION FCKAAR(N3,N3),FCKABR(N3,N3),FCKBAR(N3,N3),FCKBBR(N3,N3)
      DIMENSION FCKAAI(N3,N3),FCKABI(N3,N3),FCKBAI(N3,N3),FCKBBI(N3,N3)
      DIMENSION FOCKR(N21*2),FOCKI(N21*2)
      DIMENSION NIDSRP(17)
C
      DO 310 I=1,N21*2
        FOCKR(I)=0.0D0
        FOCKI(I)=0.0D0
 310    CONTINUE
C
C     -----------------------------------------
C     GET STARTING ADRESSES FOR SYMMETRY BLOCKS
C     -----------------------------------------
C
#if defined (BIT64)
      DAMP = REAL(NSYMOP)
#else
      DAMP = DBLE(NSYMOP)
#endif
C
      NIDSRP (1) = 0
      DO 10 IRP = 1 ,NSYMRP
        NIDSRP( IRP + 1) = NIDSRP( IRP ) + NBSIM(IRP) * NBSIM(IRP)
   10 CONTINUE
C
      IF (NIDSRP(NSYMRP+1).GT.2*N21) 
     +   CALL Q2JOB(3, 'MFDFMCP','N21',NIDSRP(NSYMRP+1)/2)
C
      NEQNEQ = NEQUIV
      IF (TWOC) NEQNEQ = NQ1
      DO 230 IM=1,NEQNEQ
      ICTI=KCOMB(IM)
      KCTI=INDCOM(ICTI)
      MCTI=KCTI+1
      NCTI=INDCOM(ICTI+1)
      NBASI=NCTI-KCTI
      NIM=NEM(IM)
      MAA=INDSYM(IM)
      MAB=INDSYM(IM+1)
      NBS=MAB-MAA
      MAA=MAA+1
      CALL TRMAT(TAIR,TAII,TBIR,TBII,IM)
      DO 220 KM=1,IM
      ICTK=KCOMB(KM)
      KCTK=INDCOM(ICTK)
      MCTK=KCTK+1
      NCTK=INDCOM(ICTK+1)
      NBASK=NCTK-KCTK
      NKM=NEM(KM)
      MBA=INDSYM(KM)
      MBB=INDSYM(KM+1)
      MBS=MBB-MBA
      MBA=MBA+1
      CALL TRMAT(TAKR,TAKI,TBKR,TBKI,KM)
C
C     -------------------------------------------------
C     CLEAR THE ARRAY FCKXX TO HOLD THE (IM,KM) - BLOCK
C     -------------------------------------------------
C
      DO 50 J=1,N3
        DO 40 I=1,N3
          FCKAAR(I,J) = 0.0D0
          FCKABR(I,J) = 0.0D0
          FCKBAR(I,J) = 0.0D0
          FCKBBR(I,J) = 0.0D0
          FCKAAI(I,J) = 0.0D0
          FCKABI(I,J) = 0.0D0
          FCKBAI(I,J) = 0.0D0
          FCKBBI(I,J) = 0.0D0
   40     CONTINUE
   50   CONTINUE
C
      DO 150 I=MCTI,NCTI
        IIT=I-MCTI+1
        II=IIT+NIM
        II=II*(II-1)/2
C
        DO 140 K=MCTK,NCTK
          IKT=K-MCTK+1
          KK=IKT+NKM
C
C     ----------------------------------------------------------------
C     GET ELEMENT (I,K) FROM BLOCK (IM,KM) OF THE SKELETON FOCK-MATRIX
C     ----------------------------------------------------------------
C
          IF (IM.GT.KM. OR.I.GT.K) THEN
            FCKAAR(IIT,IKT)= FAAR(II+KK)
            FCKABR(IIT,IKT)=-FBAR(II+KK)
            FCKBAR(IIT,IKT)= FBAR(II+KK)
            FCKBBR(IIT,IKT)= FAAR(II+KK)
            FCKAAI(IIT,IKT)= FAAI(II+KK)
            FCKABI(IIT,IKT)= FBAI(II+KK)
            FCKBAI(IIT,IKT)= FBAI(II+KK)
            FCKBBI(IIT,IKT)=-FAAI(II+KK)
          ELSE IF (I.LT.K) THEN
            II=KK-K+I
            KK=KK*(KK-1)/2
            FCKAAR(IIT,IKT)= FAAR(II+KK)
            FCKABR(IIT,IKT)= FBAR(II+KK)
            FCKBAR(IIT,IKT)=-FBAR(II+KK)
            FCKBBR(IIT,IKT)= FAAR(II+KK)
            FCKAAI(IIT,IKT)=-FAAI(II+KK)
            FCKABI(IIT,IKT)=-FBAI(II+KK)
            FCKBAI(IIT,IKT)=-FBAI(II+KK)
            FCKBBI(IIT,IKT)= FAAI(II+KK)
          ELSE
            FCKAAR(IIT,IKT)= FAAR(II+KK)
            FCKBBR(IIT,IKT)= FAAR(II+KK)
          ENDIF
 140      CONTINUE
 150    CONTINUE
C
C     --------------------------------------------------
C     PERFORM TRANSFORMATION TO SYMMETRY REPRESENTATIONS
C     --------------------------------------------------
C
      DO 210 I=1,NBS
        KSI=KSYMRP(MAA+I-1)
        LSI=LSYMRP(MAA+I-1)
        IF (KSI.EQ.0) GOTO 210
        IF (KM.EQ.IM) MBS=I
        DO 200 K=1,MBS
          LSK=LSYMRP(MBA+K-1)
          IF (KSYMRP(MBA+K-1).NE.KSI) GOTO 200
          SR = 0.0D0
          SI = 0.0D0
          DO 185 IP=1,NBASI
            TAR(IP,K)=0.0D0
            TAI(IP,K)=0.0D0
            TBR(IP,K)=0.0D0
            TBI(IP,K)=0.0D0
 185      CONTINUE
          DO 190 IQ=1,NBASK
            DO 180 IP=1,NBASI
              TAR(IP,K)=TAR(IP,K)+( FCKAAR(IP,IQ)*TAKR(IQ,K)
     +                             -FCKAAI(IP,IQ)*TAKI(IQ,K)
     +                             +FCKABR(IP,IQ)*TBKR(IQ,K)
     +                             -FCKABI(IP,IQ)*TBKI(IQ,K))
              TAI(IP,K)=TAI(IP,K)+( FCKAAR(IP,IQ)*TAKI(IQ,K)
     +                             +FCKAAI(IP,IQ)*TAKR(IQ,K)
     +                             +FCKABR(IP,IQ)*TBKI(IQ,K)
     +                             +FCKABI(IP,IQ)*TBKR(IQ,K))
              TBR(IP,K)=TBR(IP,K)+( FCKBAR(IP,IQ)*TAKR(IQ,K)
     +                             -FCKBAI(IP,IQ)*TAKI(IQ,K)
     +                             +FCKBBR(IP,IQ)*TBKR(IQ,K)
     +                             -FCKBBI(IP,IQ)*TBKI(IQ,K))
              TBI(IP,K)=TBI(IP,K)+( FCKBAR(IP,IQ)*TAKI(IQ,K)
     +                             +FCKBAI(IP,IQ)*TAKR(IQ,K)
     +                             +FCKBBR(IP,IQ)*TBKI(IQ,K)
     +                             +FCKBBI(IP,IQ)*TBKR(IQ,K))
 180          CONTINUE
 190        CONTINUE
          DO 195 IP=1,NBASI
            SR=SR+TAIR(IP,I)*TAR(IP,K)
     +           +TAII(IP,I)*TAI(IP,K)
     +           +TBIR(IP,I)*TBR(IP,K)
     +           +TBII(IP,I)*TBI(IP,K)
            SI=SI+TAIR(IP,I)*TAI(IP,K)
     +           -TAII(IP,I)*TAR(IP,K)
     +           +TBIR(IP,I)*TBI(IP,K)
     +           -TBII(IP,I)*TBR(IP,K)
 195        CONTINUE
          NIDIRP = NIDSRP( KSI )
          IK = (LSI-1) * NBSIM (KSI) + LSK
          FOCKR (NIDIRP + IK) = SR * DAMP
          FOCKI (NIDIRP + IK) = SI * DAMP
  200     CONTINUE
  210   CONTINUE
  220 CONTINUE
  230 CONTINUE
C
C     ----------------------------------
C     AVERAGE DEGENERATE REPRESENTATIONS
C     ----------------------------------
C
      DO 500 IRP=1,NSYMRP
        IF (IAVIRP(IRP).EQ.IRP) GOTO 500
        IND=NIDSRP(IAVIRP(IRP))
        IND1=NIDSRP(IRP)
        DO 510 I=1,NIDSRP(IRP+1)-NIDSRP(IRP)
          FOCKR(IND+I)=FOCKR(IND+I)+FOCKR(IND1+I)
          FOCKI(IND+I)=FOCKI(IND+I)+FOCKI(IND1+I)
 510      CONTINUE
 500    CONTINUE
C
      DO 520 IRP=1,NAVIRP
        IIRP=IIRPAV(IRP,1)
        DO 530 I=NIDSRP(IIRP)+1,NIDSRP(IIRP+1)
          FOCKR(I)=FOCKR(I)*AVIRP(IRP)
          FOCKI(I)=FOCKI(I)*AVIRP(IRP)
 530      CONTINUE
        IND1=NIDSRP(IIRP)
        DO 540 JRP=IIRPAV(IRP,1)+1,IIRPAV(IRP,2)
          IND2=NIDSRP(JRP)
          DO 550 I=1,NIDSRP(IIRP+1)-NIDSRP(IIRP)
            FOCKR(IND2+I)=FOCKR(IND1+I)
            FOCKI(IND2+I)=FOCKI(IND1+I)
 550        CONTINUE
 540      CONTINUE
 520    CONTINUE
C
      RETURN
      END
