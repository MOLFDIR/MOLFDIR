C
C     =======================================
      SUBROUTINE MFDSEIV(IRP,NBAS,NH,HR,HI,E)
C     =======================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     SELECT EIGENVECTORS, CHECK CONVERGENCY, EXTRAPOLATE
C
      INCLUDE 'paracom.inc'
      INCLUDE 'general.inc'
C
      DIMENSION HR(NH,NL),HI(NH,NL),E(NL)
      COMPLEX*16   V1(N22*2),V2(N22*2),V0,W,X
      COMPLEX*16   ALPHA,BETHA,GAMMA,TAU
      CHARACTER TEXT1*18,TEXT2*9,TEXT3*12,TEXT4*20,COMMENT*57
C
      COMPLEX*16 SJ(15)
      COMPLEX*16 ZCMPLX
      REAL*8 FR(N20,15),FI(N20,15)
C
      DATA ICYCRT/3/
      DATA TEXT1,TEXT2,TEXT3,TEXT4/'NORMAL CONVERGENCE',
     $'CONVERGED','DAMPING USED','AITKEN EXTRAPOLATION'/
C
      COMPLEX*16 YYY,ZCONJG,ZCABS
C
#if defined (BIT64)
      ZABS(XXX)=ABS(XXX)
      ZCABS(YYY)=CABS(YYY)
      ZCONJG(YYY)=CONJG(YYY)
      ZREAL(YYY)=REAL(YYY)
      ZIMAG(YYY)=AIMAG(YYY)
      ZSQRT(XXX)=SQRT(XXX)
      ZCMPLX(XXA,XXB)=CMPLX(XXA,XXB)
#else
      ZABS(XXX)=DABS(XXX)
      ZCABS(YYY)=CDABS(YYY)
      ZCONJG(YYY)=DCONJG(YYY)
      ZREAL(YYY)=DREAL(YYY)
      ZIMAG(YYY)=DIMAG(YYY)
      ZSQRT(XXX)=DSQRT(XXX)
      ZCMPLX(XXA,XXB)=DCMPLX(XXA,XXB)
#endif

      IF (PRTOD.AND.(IAVIRP(IRP).EQ.IRP).AND.NOC(IRP).GT.0)
     &   WRITE(*,1000)
      NCLIRP=NOC(IRP)
      NBS=NBSIM2(IRP)
      IVT0=INDNEV(IRP)
      IVT=IVT0
      VDIF=0.0D0
      ENDIF=0.0D0
C
C     -------------------
      DO 150 IEV=1,NCLIRP
C     -------------------
C
      IVT=IVT+1
C
      IF (KCYCLS .GT. 0) THEN
          READ (MFDVECB,REC=IVT+NOCT+2) E1I,E2I,E1O,E2O,
     $                                  (V1(I),V2(I),I=1,NBAS)
      ELSE
          E1I=0.0D0
          E2I=0.0D0
          E1O=0.0D0
          E2O=0.0D0
          DO 2 I=1,NBAS
          V1(I)=(0.0D0,0.0D0)
    2     CONTINUE
      ENDIF
      COMMENT=TEXT1
      ICOMT=1
C
C     ----------------
C     NORMAL SELECTION
C     ----------------
C
      NEIV(IVT)=IEV
C
C     ----------------------
C     CHECK FOR DEGENERACIES
C     ----------------------
C
      KEV=IEV
      IF (KCYCLS.EQ.0.OR.KEV.EQ.NCLIRP) GOTO 20
      IF (N20.LT.NBAS) CALL Q2JOB(3,'MFDSEIV','N20',NBAS)
C
      DO 4 J=1,NCLIRP
        IDEGF=J
        IF (ABS(E(J)-E(KEV)).LT.DEGEN) GOTO 5
    4   CONTINUE
 5    IDEGL=IDEGF
      DO 510 J=IDEGF+1,NCLIRP
        IF (ABS(E(J)-E(KEV)).GT.DEGEN) GOTO 520
        IDEGL=J
 510    CONTINUE
 520  IF ((IDEGF.NE.KEV).OR.(IDEGL.EQ.IDEGF)) GOTO 20
C
C     --------------------------------------------------------------
C     CONSTRUCT FROM NEW SET OF VECTORS VECTORS WITH MAXIMUM OVERLAP
C     WITH THE VECTOR FROM THE PREVIOUS ITERATION
C     --------------------------------------------------------------
C
      NDEGEN=IDEGL-IDEGF+1
      SUMT=0.0D0
      IF (NDEGEN.GT.15) CALL Q2JOB(6,
     &   'MFDSEIV: SJ smaller then the hard coded 15',' ',1)
C
      DO 530 MO=1,NDEGEN
        READ(MFDVECB,REC=INDNEV(IRP)+NOCT+2+IDEGF+MO-1)
     +       E1I,E2I,E1O,E2O,(V1(I),V2(I),I=1,NBAS)
        T=0.0D0
        DO 534 I=1,NBAS
          FR(I,MO)=0.0D0
          FI(I,MO)=0.0D0
 534      CONTINUE
        DO 7 J=1,NDEGEN
          SJ(J)=(0.0D0,0.0D0)
          DO 6 I=1,NBAS
            V0 = ZCMPLX(HR(I,J+IDEGF-1),HI(I,J+IDEGF-1))
            SJ(J)=SJ(J)+ZCONJG(V0)*V1(I)
 6          CONTINUE
          T=T+ZREAL(ZCONJG(SJ(J))*SJ(J))
 7        CONTINUE
        T=1.0D0/ZSQRT(T)
        SUMT=SUMT+1.0D0/T
        DO 8 J=1,NDEGEN
          SR=ZREAL(SJ(J))
          SI=ZIMAG(SJ(J))
          DO 500 I=1,NBAS
            FR(I,MO)=FR(I,MO)+HR(I,J+IDEGF-1)*SR-HI(I,J+IDEGF-1)*SI
            FI(I,MO)=FI(I,MO)+HR(I,J+IDEGF-1)*SI+HI(I,J+IDEGF-1)*SR
 500        CONTINUE
 8        CONTINUE
 530    CONTINUE
      DO 540 MO=1,NDEGEN
        DO 550 I=1,NBAS
          HR(I,MO+IDEGF-1)=FR(I,MO)*T
          HI(I,MO+IDEGF-1)=FI(I,MO)*T
 550      CONTINUE
 540    CONTINUE
C
C     ---------------------------------------------
C     ORTHONORMALIZE THE SET OF PROJECTED FUNCTIONS
C     ---------------------------------------------
C
      DO 560 MEV=IDEGF,IDEGL
        DO 570 NEV=IDEGF,MEV
          TAU=(0.,0.)
          DO 580 L=1,NBAS
            W = ZCMPLX(HR(L,MEV),HI(L,MEV))
            X = ZCMPLX(HR(L,NEV),HI(L,NEV))
            TAU=TAU+ZCONJG(X)*W
 580        CONTINUE
          IF (MEV.NE.NEV) THEN
            DO 590 L=1,NBAS
              W = ZCMPLX(HR(L,MEV),HI(L,MEV))
              X = ZCMPLX(HR(L,NEV),HI(L,NEV))
              W=W-TAU*X
              HR(L,MEV)=ZREAL(W)
              HI(L,MEV)=ZIMAG(W)
 590          CONTINUE
          ELSE
            T=1.0D0/ZSQRT(ZREAL(TAU))
            DO 595 L=1,NBAS
              HR(L,MEV)=HR(L,MEV)*T
              HI(L,MEV)=HI(L,MEV)*T
 595          CONTINUE
          ENDIF
 570      CONTINUE
 560    CONTINUE
      READ(MFDVECB,REC=INDNEV(IRP)+NOCT+2+IDEGF)
     +     E1I,E2I,E1O,E2O,(V1(I),V2(I),I=1,NBAS)
      WRITE(COMMENT,1050) NDEGEN,SUMT/NDEGEN
 1050 FORMAT(I1,' VECS,NORM=',F5.3)
      ICOMT=ICOMT+17
C
   20 CONTINUE
      MEV=NEIV(IVT)
      E0O=E(MEV)
      E0I=E0O
C
C     --------------------------------
C     CHECK ORBITAL ENERGY CONVERGENCE
C     --------------------------------
C
      DIFE0=E0O-E1O
      DIFE1=E2I-E1I
      EIDIF=ZABS(DIFE0)
      IF (EIDIF.GT.ENDIF) ENDIF=EIDIF
      IF (ENDIF.GT.EMDIF) EMDIF=ENDIF
C
C     ---------------------------------------------------
C     GET DAMPINGS FACTOR FROM ORBITAL ENERGY CONVERGENCE
C     ---------------------------------------------------
C
      IF (ZABS(DIFE0+DIFE1).LT.1.D-7) THEN
          DAMP=0.0D0
      ELSE
          DAMP=DIFE0/(DIFE0+DIFE1)
      ENDIF
      IF (DAMP .LT. 0.1D0 .OR. DAMP .GT. .95D0 ) DAMP=0.0D0
C
C     --------------------------------
C     CHECK VECTOR ELEMENT CONVERGENCE
C     --------------------------------
C
      UDIF=0.0D0
      DO 25 I=1,NBAS
        V0=ZCMPLX(HR(I,MEV),HI(I,MEV))
        TDIF=ZABS(ZREAL(ZCABS(V0)-ZCABS(V1(I))))
        IF (TDIF.GT.UDIF) UDIF=TDIF
   25   CONTINUE
      IF (UDIF.GT.VDIF) VDIF=UDIF
      IF (UDIF.LT.CONVCRT) THEN
          WRITE(COMMENT(ICOMT:),1030) TEXT2
          ICOMT=ICOMT+10
          GOTO 80
      ENDIF
      IF ((ICYCLS.LT.ICYCRT) .OR. (KCYCLS .EQ. 0)) GOTO 80
C
C     -------------------------
C     CHECK ORBITAL CONVERGENCE
C     -------------------------
C
      ALPHA=(0.,0.)
      BETHA=(0.,0.)
      GAMMA=(0.,0.)
      DO 30 I=1,NBAS
        V0 = DCMPLX(HR(I,MEV),HI(I,MEV))
        ALPHA=ALPHA+ZCONJG(V1(I))*V2(I)
        BETHA=BETHA+ZCONJG(V0   )*V2(I)
        GAMMA=GAMMA+ZCONJG(V0   )*V1(I)
   30   CONTINUE
      DIF0SQ=1.0D0-ZABS(ZREAL(ALPHA))
      DIF1SQ=1.0D0-ZABS(ZREAL(GAMMA))
      IF (DIF1SQ.LT.CONVCRT*CONVCRT*0.5D0) GOTO 80
C
      IF (KCYCLS.LT.2)     GOTO 80
      CONVK =DIF0SQ/DIF1SQ
      GAMMA=ZCABS(GAMMA)/GAMMA
      ALPHA=GAMMA*ZCABS(ALPHA)/ALPHA
   80 CONTINUE
C
C     -------------------------------------------
C     ORTHONORMALIZE THE NEW EXTRAPOLATED VECTORS
C     -------------------------------------------
C
      IF (.NOT.XORTHO) GOTO 593
      DO 120 KEV=1,IEV
        NEV=NEIV(IVT0+KEV)
        TAU=(0.D0,0.D0)
        DO 90 L=1,NBAS
          W = ZCMPLX(HR(L,MEV),HI(L,MEV))
          X = ZCMPLX(HR(L,NEV),HI(L,NEV))
          TAU=TAU+ZCONJG(X)*W
   90     CONTINUE
        IF (IEV.NE.KEV) THEN
          DO 100 L=1,NBAS
            W = ZCMPLX(HR(L,MEV),HI(L,MEV))
            X = ZCMPLX(HR(L,NEV),HI(L,NEV))
            W=W-TAU*X
            HR(L,MEV)=ZREAL(W)
            HI(L,MEV)=ZIMAG(W)
  100       CONTINUE
        ELSE
          T=1.0D0/ZSQRT(ZREAL(TAU))
          DO 110 L=1,NBAS
            HR(L,MEV)=HR(L,MEV)*T
            HI(L,MEV)=HI(L,MEV)*T
  110       CONTINUE
        ENDIF
  120   CONTINUE
 593  WRITE (MFDVECB,REC=IVT+NOCT+2) E0I,E1I,E0O,E1O,
     $                        (HR(I,MEV),HI(I,MEV),V1(I),I=1,NBAS)
      IF (IAVIRP(IRP).EQ.IRP) THEN
      IF (PRTOD) WRITE (*,1010) INDSRP(IRP),MEV,E0O,EIDIF,UDIF,COMMENT
      ENDIF
  150 CONTINUE
      IF (VDIF.GT.WDIF) WDIF=VDIF
      IF (IAVIRP(IRP).EQ.IRP.AND.NOC(IRP).GT.0) THEN
      IF (PRTRD) WRITE (*,1020) IRP,ENDIF,VDIF
      ENDIF
C
C     ------------------------------------------------------------------
C     ORTHONORMALIZE THE VIRTUALS FOR LATER USE IN CI
C     NO NEED TO WORK WITH NEIV BECAUSE THE VIRTUALS ARE ALWAYS THE LAST
C     ------------------------------------------------------------------
C
      DO 250 IEV = NCLIRP + 1, NBLR(IRP)
      DO 220 KEV = 1, IEV
        TAU=(0.D0,0.D0)
        DO 190 L=1,NBAS
          W = ZCMPLX(HR(L,IEV),HI(L,IEV))
          X = ZCMPLX(HR(L,KEV),HI(L,KEV))
          TAU=TAU+ZCONJG(X)*W
  190     CONTINUE
        IF (IEV.NE.KEV) THEN
          DO 200 L=1,NBAS
            W = ZCMPLX(HR(L,IEV),HI(L,IEV))
            X = ZCMPLX(HR(L,KEV),HI(L,KEV))
            W=W-TAU*X
            HR(L,IEV)=ZREAL(W)
            HI(L,IEV)=ZIMAG(W)
  200       CONTINUE
        ELSE
          T=1.0D0/ZSQRT(ZREAL(TAU))
          DO 210 L=1,NBAS
            HR(L,IEV)=HR(L,IEV)*T
            HI(L,IEV)=HI(L,IEV)*T
  210       CONTINUE
        ENDIF
  220   CONTINUE
  250 CONTINUE
C
      RETURN
 1000 FORMAT(//1X,'REPR. ORBITAL ENERGY',
     $       3X,'DIFF(ENERGY)   DIFF(VECTOR)',5X,' COMMENTS'/)
 1010 FORMAT(I3,I5,F15.6,3X,G14.9,2X,G14.9,2X,A)
 1020 FORMAT(/' MAXIMA FOR REPR.',I3,' ENERGY DIFF. ',
     $        G14.9,' VECTOR DIFF. ',G14.9)

 1030 FORMAT(1X,A,I4)
 1040 FORMAT(1X,A,F5.2)
      END
