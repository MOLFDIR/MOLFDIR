C
C     ============================================
      SUBROUTINE MFDONEE(L1,SS,TT,VV,PP,L2,INDINT,
     &                   NBAS,SOCINT)
C     ============================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     CALCULATE ALL TYPES OF ONE ELECTRON INTEGRALS FOR DIRAC OPERATOR
C
#if defined (BIT64)
      PARAMETER (R1=1.0E0)
      PARAMETER (R5=2.23606797749979E0)
      PARAMETER (R3=1.73205080756887719E0)
      PARAMETER (R35=5.91607978309961613E0)
      PARAMETER (R7=2.64575131106459072E0)
#else
      PARAMETER (R1=1.0D0)
      PARAMETER (R5=2.23606797749979D0)
      PARAMETER (R3=1.73205080756887719D0)
      PARAMETER (R35=5.91607978309961613D0)
      PARAMETER (R7=2.64575131106459072D0)
#endif
C
      INCLUDE 'paracom.inc'
#if defined (PARALLEL)
      INCLUDE 'mpidef.inc'
#else
      PARAMETER (mpirnk = 0)
#endif
C
      NAMELIST/AREP/NUNIQATOM
C
      COMMON/INTI/IM,KM,LM,NM,I,K,L,N
      COMMON/INTR/SINT,TINT,VINT
      CHARACTER*12 MFDAREP
      REAL*8 ECPINT(MXBA*(MXBA+1)/2),MNORM(MAXFUN),MNORMC(MAXFUN)
      REAL*8 RNM(MXBA),TRIN(MXBA*(MXBA+1)/2)
      INTEGER NSUMTRI(0:MXBA),NCUMBAS(MXBA),ICENTER(MAXCOM)
      INTEGER IFUNTYPE(MAXCOM),NSUMCOM(MAXUC),NUNIQBS(MAXUC)
      INTEGER NBSEAA(0:4,MAXATOM+1),NCUMBSEA(MAXATOM+1)
      INTEGER NBSEA(0:4),NBS(0:4),MANG(MAXFUN),MFUN(MAXFUN)
      INTEGER MAP(MXBA),MOLTOARG(2,MXBA*(MXBA+1)/2),NBAS
      LOGICAL ZERO,NULL(3),SPHERIC
      COMPLEX*16 S,T2,H,HD,HS,SSUM,TSUM,HSUM,CSUM
      COMPLEX*16 CSUMX,CSUMY,CSUMZ
      COMPLEX*16 PXSUM,PYSUM,PZSUM,ZSOCSUM,SOCINT(NBAS,NBAS)
      DIMENSION SGN(48),INUCL(48),INDEX(48),PINT(3),INDL(3)
      DIMENSION PP(L1*3),SS(L1),TT(L1),VV(L1),INDINT(L2)
      DIMENSION S(N2),T2(N2),H(N2),HD(N2),HS(N2)
      REAL*8 SSA(NBFST*NBFST), SSB(NBFST*NBFST)
      REAL*8 TTA(NBFST*NBFST), TTB(NBFST*NBFST)
      REAL*8 PPA(NBFST*NBFST,3), PPB(NBFST*NBFST,3)
      EQUIVALENCE (B1,S),(B2,T2,HD),(B3,H,HS)
      EQUIVALENCE (INDEX,INDXX),(INUCL,INDXX(1,2))
      EQUIVALENCE (ZERO,NULL),(PINT,SINT)
C
      DATA NBSEA /1,3,6,10,15/
      DATA MANG/0,
     &          1, 1, 1,
     &          2, 2, 2, 2, 2, 2,
     &          3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
     &          4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4/
      DATA MFUN/1,
     &          1, 2, 3,
     &          1, 2, 3, 4, 5, 6,
     &          1, 2, 3, 4, 5, 6, 7, 8, 9,10,
     &          1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15/
      DATA MNORM/r1,
     &           r1,r1,r1,
     &           r1,r1,r1,r1,r1,r1,
     &           r5,r5,r5,r1,r1,r1,r1,r1,r1,r1,
     &           r35,r35,r35,r5,r5,r5,r5,r5,r5,r3,r3,r3,r1,r1,r1/
      DATA MNORMC/r1,
     &            r1,r1,r1,
     &            r1,r1,r1,r1,r1,r1,
     &            r1,r1,r1,r1,r1,r1,r1,r1,r1,r1,
     &            r1,r1,r1,r1,r1,r1,r1,r1,r1,r1,r1,r1,r1,r1,r1/
C
C     --------------------------------------------------------------
C     IF AREP CALCULATION THEN GET INTEGRALS AND DATA
C     --------------------------------------------------------------
C
      IF (NAREP.GT.0) THEN
         IF (mpirnk .EQ. 0) THEN
            WRITE (MFDAREP,'(A7)') 'MFDAREP'
         ELSEIF (mpirnk .LT. 10) THEN
            WRITE (MFDAREP,'(A7,A1,I1)') 'MFDAREP','.',mpirnk
         ELSEIF (mpirnk .LT. 100) THEN
            WRITE (MFDAREP,'(A7,A1,I2)') 'MFDAREP','.',mpirnk
         ELSE
            WRITE (MFDAREP,'(A7,A1,I3)') 'MFDAREP','.',mpirnk
         ENDIF
      OPEN(19,FILE=MFDAREP,FORM='UNFORMATTED',STATUS='UNKNOWN',
     $     ERR=5556)
      READ(19) SPHERIC
      NCOM=1
      IUNIQCOM=0
 666  READ(19,END=1200) IUC, IC,ICENTER(NCOM),IFUNTYPE(NCOM)
      IF (IUC.NE.IUNIQCOM) THEN
         IF (IUNIQCOM.EQ.0) THEN
            IUNIQCOM=IUC
            ISUMCOM=1
         ELSE
            if (iUniqCom.le.MaxUC) then
               nSumCom(iUniqCom)=iSumCom
               iUniqCom=iUC
               iSumCom=1
            else
               if (mpirnk.eq.0) 
     &         print *, ' ERR : Expand MaxUC with', iUniqCom
               call q2job(6,'MFDONEE: AREP error',' ',0)
            endif
         endif
      else
         iSumCom=iSumCom+1
      endif
      nCom=nCom+1
      goto 666
 1200 nUniqCom=iUniqCom
      nTotCom=nCom-1
      nSumCom(iUniqCom)=iSumCom
      iTotCom=0
      do i=1,nUniqCom
         iTotCom=iTotCom+nSumCom(i)
      enddo
      if(iTotCom.ne.nTotCom)then
         if (mpirnk.eq.0)
     &   print *, ' ERR : iTotCom.ne.nTotCom '
         call q2job(6,'MFDONEE: AREP error',' ',0)
      endif
      close(19)
      if(nTotCom.gt.MaxCom)then
         if (mpirnk.eq.0)
     &   print *, ' ERR : Expand MaxCom with', MaxCom
         call q2job(6,'MFDONEE: AREP error',' ',0)
      endif
      nUniqAtom=1
      read (5,AREP) 
      iAtom=0
      nCumBsEA(iAtom+1)=0
      do 7771 i=1,nUniqAtom
         read (5,*) nEqAtom, MaxAng
         read (5,*) (nBs(ii), ii=0,MaxAng)
         do 7772 j=1,nEqAtom
         iAtom=iAtom+1
         nBsEAt=0
         do 7773 k=0,MaxAng
            nBsEAA(k,iAtom)=nBs(k)
            nBsEAt=nBsEAt+nBs(k)*nBsEA(k)
 7773    continue
         nCumBsEA(iAtom+1)=nCumBsEA(iAtom)+nBsEAt
 7772    continue
 7771 continue
      nTotBs=nCumBsEA(iAtom+1)
      if(nTotBs.gt.Mxba)then
         if (mpirnk.eq.0)
     &   print *, ' ERR: Expand MXBA with', nTotBs
         call q2job(6,'MFDONEE: AREP error',' ',0)
      endif
      ithCom=0
      do 7775 i=1,nUniqCom
         iUniqBs=0
         do 7776 j=1,nSumCom(i)
            ithCom=ithCom+1
            iAtom=iCenter(ithCom)
            iAng=MAng(iFunType(ithCom))
            nBsCom=nBsEAA(iAng,iAtom)
            if (iUniqBs.eq.0) then
               iUniqBs=nBsCom
               nUniqBs(i)=iUniqBs
            else
               if (iUniqBs.eq.nBsCom) then
                  nUniqBs(i)=nBsCom
               else
                  if (mpirnk.eq.0)
     &            print *, ' ERR : iUniqBs.eq.nBsCom '
                  call q2job(6,'MFDONEE: AREP error',' ',0)
               endif
            endif
 7776    continue
 7775 continue
C
C     Match Basis Indices of MOLFDIR with ARGOS
C
      iMolBs=0
      ithCom=0
      do 7777 i=1,nUniqCom
      do 7778 j=1,nUniqBs(i)
         ithSubCom=ithCom
         do 7779 k=1,nSumCom(i)
            iMolBs=iMolBs+1
            ithSubCom=ithSubCom+1
            iAtom=iCenter(ithSubCom)
            iAng=MAng(iFunType(ithSubCom))
            iFun=MFun(iFunType(ithSubCom))
            iArgBs=nCumBsEA(iAtom)
            do 7780 ii=0,iAng-1
               iArgBs=iArgBs+nBsEAA(ii,iAtom)*nBsEA(ii)
 7780       continue
            iArgBs=iArgBs+nBsEA(iAng)*(j-1)+iFun
            map(iMolBs)=iArgBs
            if(spheric) then
            rnm(iMolBs)=MNorm(iFunType(ithSubCom))
            else
            rnm(iMolBs)=MNormC(iFunType(ithSubCom))
            endif
 7779    continue
 7778 continue
      ithCom=ithCom+nSumCom(i)
 7777 continue
      open(17,access='sequential',form='unformatted',
     &     file='arepints',status='old',err=5557)
      read(17,ERR=5557) nnbft
      nnTotBs=nTotBs*(nTotBs+1)/2
      if(nnbft.eq.nnTotBs)then
         CALL RED(nnbft,ECPINT,17)
         close(17)
      else
         if (mpirnk.eq.0)
     &   print *, ' ERR : nnbft.ne.nnTotBs ',nnbft,nnTotBs
         call q2job(6,'MFDONEE: AREP error',' ',0)
      endif
      ncumbas(1)=0
      do 7799 i=2,NEQ1
         ncumbas(i)=ncumbas(i-1)
     &             +INDCOM(KCOMB(i-1)+1)-INDCOM(KCOMB(i-1))
 7799 continue
      ENDIF
C
C     --------------------------------------------------------------
C     CALCULATE INTEGRALS OVER LARGE-LARGE COMPONENT BASISFUNCTIONS
C     --------------------------------------------------------------
C     OVERLAP (S), NUCLEAR ATTRACTION ENERGY (V/H), KINETIC ENERGY (T)
C     --------------------------------------------------------------
C
      IMKM = 0
      DO 311 IM=1,NEQ1
      IMDIF = INDCOM(KCOMB(IM)+1)-INDCOM(KCOMB(IM))
      DO 312 KM=1,IM
      IMKM=IMKM+1
      KMDIF = INDCOM(KCOMB(KM)+1)-INDCOM(KCOMB(KM))
      ICOUNT = IMDIF * KMDIF
      INDINT(IMKM+1)=INDINT(IMKM)+ICOUNT
  312 CONTINUE
  311 CONTINUE
C
      CALL AZERO(VV,INDINT(IMKM+1))
      ICTI = KCOMB(1)
      MCTI = INDCOM(ICTI) + 1
      ICTI = KCOMB(NEQ1)
      NCTI = INDCOM(ICTI + 1)
      MCTK = MCTI
      DO 100 I = MCTI, NCTI
      IM = KCBINV(I,1)
      IN = KCBINV(I,2)
      ICTI = KCOMB(IM)
      MCTIH = INDCOM(ICTI) + 1
      IMAX = INDCOM(ICTI+1) - INDCOM(ICTI)
      NCTK = INDCOM(KCOMB(IN)+1)
      DO 110 K = MCTK, NCTK
      KM = KCBINV(K,1)
      KCTK = KCOMB(KM)
      MCTKH = INDCOM(KCTK) + 1
      ZERO=.FALSE.
      IND=1
      SGN(1)=1.0D0
      INDEX(1)= (K - MCTKH) * IMAX + I - MCTIH + 1
      DO 50 M=1,NSYMOP
      SN=1.0D0
      IT=ICOMOP(I,M)
      KT=ICOMOP(K,M)
      IF (IT.LT.0) THEN
          IT=-IT
          SN=-SN
      ENDIF
      IF (KT.LT.0) THEN
          KT=-KT
          SN=-SN
      ENDIF
      IF (IT-I) 100,10,20
   10 IF (KT-K) 110,20,20
   20 NN = (KT - MCTKH) * IMAX + IT - MCTIH + 1
      DO 30 N=1,IND
      IF (INDEX(N).EQ.NN) GOTO 40
   30 CONTINUE
      IND=IND+1
      INDEX(IND)=NN
      SGN(IND)=SN
      GOTO 50
   40 IF (SGN(N).NE.SN) ZERO=.TRUE.
   50 CONTINUE
      CALL AZERO(SSA,NBFST*NBFST)
      CALL AZERO(TTA,NBFST*NBFST)
      IF (.NOT. ZERO) THEN
      IJC = 0
C
C === COMPUTE ALL THE PRIMITIVE INTEGRALS BELONGING TO IJ
C
      IF ((INDBAS(IM+1)-INDBAS(IM)+1).GT.NBFST) CALL Q2JOB(5,'MFDONEE',
     &   'NBFST',(INDBAS(IM+1)-INDBAS(IM)+1))
      IF ((INDBAS(KM+1)-INDBAS(KM)+1).GT.NBFST) CALL Q2JOB(5,'MFDONEE',
     &   'NBFST',(INDBAS(KM+1)-INDBAS(KM)+1))
      DO 120 IC = INDBAS(IM) + 1, INDBAS(IM + 1)
      DO 130 JC = INDBAS(KM) + 1, INDBAS(KM + 1)
      IJC = IJC + 1
      SINT = 0.0D0
      TINT = 0.0D0
      CALL MFDSTIN(IC,JC)
      SSA(IJC) = SINT
      TTA(IJC) = TINT
  130 CONTINUE
  120 CONTINUE
      ENDIF
C
C === TRANSFORM THE INTEGRALS TO THE CONTRACTED BASIS
C
      CALL AZERO(SSB,NBFST*NBFST)
      CALL AZERO(TTB,NBFST*NBFST)
      IJCC = 0
      DO 140 JCC = KCBINV(K,1), KCBINV(K,2)
      DO 150 ICC = KCBINV(I,1), KCBINV(I,2)
      IJCC = IJCC + 1
      IJC = 0
      DO 160 IC = INDBAS(ICC) + 1, INDBAS(ICC + 1)
      DO 170 JC = INDBAS(JCC) + 1, INDBAS(JCC + 1)
      IJC = IJC + 1
      SSB(IJCC) = SSB(IJCC) + COEFF(IC) * COEFF(JC) * SSA(IJC)
      TTB(IJCC) = TTB(IJCC) + COEFF(IC) * COEFF(JC) * TTA(IJC)
  170 CONTINUE
  160 CONTINUE
  150 CONTINUE
  140 CONTINUE
C
C === PROCEED AS USUAL (WELL ALMOST)
C
      IJCC = 0
      DO 180 JCC = KCBINV(K,1), KCBINV(K,2)
      DO 190 ICC = KCBINV(I,1), KCBINV(I,2)
      IJCC = IJCC + 1
      IF (JCC .GT. ICC) GOTO 190
      IMKM = (ICC * (ICC - 1)) / 2 + JCC
      DO 60 N=1,IND
      NN = INDEX(N) + INDINT(IMKM)
      SS(NN)=SGN(N)*SSB(IJCC)
      TT(NN)=SGN(N)*TTB(IJCC)
      IF (NAREP.GT.0) THEN
         jth=(INDEX(N)-1)/IMAX+1
         ith=INDEX(N)-(jth-1)*IMAX
         ii=ncumbas(ICC)+ith
         jj=ncumbas(JCC)+jth
         invert=0
         if(map(jj).gt.map(ii))then
            iitmp=ii
            ii=jj
            jj=iitmp
            invert=1
         endif
         idxint=map(ii)*(map(ii)-1)/2+map(jj)
         trin(NN)=rnm(ii)*rnm(jj)
         moltoarg(1,NN)=idxint
         moltoarg(2,NN)=invert
      ENDIF
   60 CONTINUE
  190 CONTINUE
  180 CONTINUE
C
      DO 195 L=1,NNUCL
      ZERO=.FALSE.
      IND=1
      SGN(1)=ZNUC(L)
      INDEX(1) = (K - MCTKH) * IMAX + I - MCTIH + 1
      INUCL(1)=L
      DO 220 M=1,NSYMOP
      SN=ZNUC(L)
      IT=ICOMOP(I,M)
      KT=ICOMOP(K,M)
      LT=NUCOP(L,M)
      IF (IT.LT.0) THEN
          IT=-IT
          SN=-SN
      ENDIF
      IF (KT.LT.0) THEN
          KT=-KT
          SN=-SN
      ENDIF
      IF (IT-I) 100,70,90
   70 IF (KT-K) 110,80,90
   80 IF (LT-L) 195,90,90
   90 NN = (KT - MCTKH) * IMAX + IT - MCTIH + 1
      DO 230 N=1,IND
      IF (INDEX(N).EQ.NN.AND.INUCL(N).EQ.LT) GOTO 210
  230 CONTINUE
      IND=IND+1
      INDEX(IND)=NN
      INUCL(IND)=LT
      SGN(IND)=SN
      GOTO 220
  210 IF (SGN(N).NE.SN) ZERO=.TRUE.
  220 CONTINUE
      IF (ZERO) GOTO 195
      IJC = 0
      DO 240 IC = INDBAS(IM) + 1, INDBAS(IM + 1)
      DO 250 JC = INDBAS(KM) + 1, INDBAS(KM + 1)
      IJC = IJC + 1
      VINT = 0D0
      IF (GNUC) THEN
         CALL MFDGINT(IC,JC)
      ELSE
         CALL MFDAINT(IC,JC)
      ENDIF
      SSA(IJC) = VINT
  250 CONTINUE
  240 CONTINUE
      CALL AZERO(SSB,NBFST*NBFST)
      IJCC = 0
      DO 260 JCC = KCBINV(K,1), KCBINV(K,2)
      DO 270 ICC = KCBINV(I,1), KCBINV(I,2)
      IJCC = IJCC + 1
      IJC = 0
      DO 280 IC = INDBAS(ICC) + 1, INDBAS(ICC + 1)
      DO 290 JC = INDBAS(JCC) + 1, INDBAS(JCC + 1)
      IJC = IJC + 1
      SSB(IJCC) = SSB(IJCC) + COEFF(IC) * COEFF(JC) * SSA(IJC)
  290 CONTINUE
  280 CONTINUE
  270 CONTINUE
  260 CONTINUE
      IJCC = 0
      DO 300 JCC = KCBINV(K,1), KCBINV(K,2)
      DO 310 ICC = KCBINV(I,1), KCBINV(I,2)
      IJCC = IJCC + 1
      IF (JCC .GT. ICC) GOTO 310
      IMKM = (ICC * (ICC - 1)) / 2 + JCC
      DO 200 N=1,IND
      NN = INDEX(N) + INDINT(IMKM)
      VV(NN)=VV(NN)-SGN(N)*SSB(IJCC)
  200 CONTINUE
  310 CONTINUE
  300 CONTINUE
  195 CONTINUE
  110 CONTINUE
  100 CONTINUE
C
      IF (NAREP.GT.0) THEN
         do 726 i=1,L1
            VV(i)=VV(i)+ecpint(moltoarg(1,i))*trin(i)
 726     continue
         do 678 i=0,nbas
            nsumtri(i)=i*(i+1)/2
 678     continue
      ENDIF
C
      IND=1
      NSCF = 0
      DO 320 N=1,NSYMRP
      IND=IND+NBSYM(N-1)
      KND=IND+NBSYM1(N)-1
      DO 330 I=IND,KND
      IT=KSYMF(I)
      IM=INDB(IT)
      IF(IM.GT.NEQ1) GOTO 330
      ITA=INDORB(IT)+1
      ITB=INDORB(IT+1)
      IT=KCOMB(IM)
      IMAX=INDCOM(IT+1)-INDCOM(IT)
      DO 340 K=IND,I
      KT=KSYMF(K)
      KM=INDB(KT)
C     IF(KM.GT.IM) GOTO 340
      KTA=INDORB(KT)+1
      KTB=INDORB(KT+1)
      IF (NSCF.GE.N2) THEN
          WRITE (10) T2,S,H
          NSCF=0
      ENDIF
      NSCF=NSCF+1
#if defined (BIT64)
      SSUM=(0.0,0.0)
      TSUM=(0.0,0.0)
      HSUM=(0.0,0.0)
      ZSOCSUM=(0.0,0.0)
#else
      SSUM=(0.D0,0.D0)
      TSUM=(0.D0,0.D0)
      HSUM=(0.D0,0.D0)
      ZSOCSUM=(0.D0,0.D0)
#endif
      ICT=IM*(IM-1)/2 + KM
      ICT=INDINT(ICT)
      DO 350 L=ITA,ITB
      IT=ITRAN(L)
      DO 360 M=KTA,KTB
      KT=ITRAN(M)
      NN=ICT+(KT-1)*IMAX+IT
#if defined (BIT64)
      CSUM=CONJG(CTRANA(L))*CTRANA(M)+CONJG(CTRANB(L))*CTRANB(M)
#else
      CSUM=DCONJG(CTRANA(L))*CTRANA(M)+DCONJG(CTRANB(L))*CTRANB(M)
#endif
      IF (NAREP.GT.0) THEN 
         IF (NAREP.LT.2) goto 8900
         idx0=moltoarg(1,NN)
         do 789 mc=1,nbas
            if(idx0.le.nsumtri(mc))then
               idrow=mc
               idcol=idx0-nsumtri(mc-1)
               goto 6666
            endif
 789     continue
 6666    continue
         comnorm=trin(NN)
c case <a|l.s|a>
         if(moltoarg(2,NN).eq.0)then
         zsocsum=zsocsum
#if defined (BIT64)
     &          +CONJG(CTRANA(L))*CTRANA(M)
#else
     &          +DCONJG(CTRANA(L))*CTRANA(M)
#endif
     &          *socint(idrow,idcol)*comnorm
      elseif(moltoarg(2,NN).eq.1)then
         zsocsum=zsocsum
#if defined (BIT64)
     &          +CONJG(CTRANA(L))*CTRANA(M)
     &          *CONJG(socint(idrow,idcol))*comnorm
#else
     &          +DCONJG(CTRANA(L))*CTRANA(M)
     &          *DCONJG(socint(idrow,idcol))*comnorm
#endif
         endif
c case <a|l.s|b>
         if(moltoarg(2,NN).eq.0)then
         zsocsum=zsocsum
#if defined (BIT64)
     &          +CONJG(CTRANA(L))*CTRANB(M)
#else
     &          +DCONJG(CTRANA(L))*CTRANB(M)
#endif
     &          *socint(idcol,idrow)*comnorm
      elseif(moltoarg(2,NN).eq.1)then
         zsocsum=zsocsum
#if defined (BIT64)
     &          +CONJG(CTRANA(L))*CTRANB(M)
     &          *socint(idcol,idrow)*(-1.0e0)*comnorm
#else
     &          +DCONJG(CTRANA(L))*CTRANB(M)
     &          *socint(idcol,idrow)*(-1.0d0)*comnorm
#endif
         endif
c case <b|l.s|a>
         if(moltoarg(2,NN).eq.0)then
         zsocsum=zsocsum
#if defined (BIT64)
     &          +CONJG(CTRANB(L))*CTRANA(M)
     &          *(-1.0e0)*CONJG(socint(idcol,idrow))*comnorm
#else
     &          +DCONJG(CTRANB(L))*CTRANA(M)
     &          *(-1.0d0)*DCONJG(socint(idcol,idrow))*comnorm
#endif
      elseif(moltoarg(2,NN).eq.1)then
         zsocsum=zsocsum
#if defined (BIT64)
     &          +CONJG(CTRANB(L))*CTRANA(M)
     &          *CONJG(socint(idcol,idrow))*comnorm
#else
     &          +DCONJG(CTRANB(L))*CTRANA(M)
     &          *DCONJG(socint(idcol,idrow))*comnorm
#endif
         endif
c case <b|l.s|b>
         if(moltoarg(2,NN).eq.0)then
         zsocsum=zsocsum
#if defined (BIT64)
     &          +CONJG(CTRANB(L))*CTRANB(M)
     &          *CONJG(socint(idrow,idcol))*comnorm
#else
     &          +DCONJG(CTRANB(L))*CTRANB(M)
     &          *DCONJG(socint(idrow,idcol))*comnorm
#endif
      elseif(moltoarg(2,NN).eq.1)then
         zsocsum=zsocsum
#if defined (BIT64)
     &          +CONJG(CTRANB(L))*CTRANB(M)
#else
     &          +DCONJG(CTRANB(L))*CTRANB(M)
#endif
     &          *socint(idrow,idcol)*comnorm
         endif
 8900    continue
      ENDIF
      SSUM=SSUM+CSUM*SS(NN)
      TSUM=TSUM+CSUM*TT(NN)
      HSUM=HSUM+CSUM*VV(NN)
  360 CONTINUE
  350 CONTINUE
#if defined (BIT64)
      IF (CABS(SSUM).LT.ENORM) SSUM=(0.0,0.0)
      IF (CABS(TSUM).LT.ENORM) TSUM=(0.0,0.0)
      IF (CABS(HSUM).LT.ENORM) HSUM=(0.0,0.0)
      IF (CABS(zsocsum).LT.ENORM) zsocsum=(0.0,0.0)
#else
      IF (CDABS(SSUM).LT.ENORM) SSUM=(0.0D0,0.0D0)
      IF (CDABS(TSUM).LT.ENORM) TSUM=(0.0D0,0.0D0)
      IF (CDABS(HSUM).LT.ENORM) HSUM=(0.0D0,0.0D0)
      IF (CDABS(zsocsum).LT.ENORM) zsocsum=(0.d0,0.d0)
#endif
      S(NSCF)=SSUM
      T2(NSCF)=TSUM
      IF (NAREP.GT.0) THEN
         H(NSCF)=HSUM+ZSOCSUM
      ELSE
         H(NSCF)=HSUM
      ENDIF
  340 CONTINUE
  330 CONTINUE
  320 CONTINUE
      WRITE (10) T2,S,H
      NSCF=0
cC
cC     Write scalar overlap integrals as lower triangle in combined index
cC     Use VV as scratch array
cC
c      IK = 0
c      DO IM=1,NEQ1
c         IMDIF = INDCOM(KCOMB(IM)+1)-INDCOM(KCOMB(IM))
c         DO I = 1, IMDIF
c            DO KM=1,IM
c               IMKM =( IM-1)*IM/2 + KM
c               KMDIF = INDCOM(KCOMB(KM)+1)-INDCOM(KCOMB(KM))
c               KMMAX = KMDIF
c               IF (IM.EQ.KM) KMMAX = I
c               DO K = 1, KMMAX
c                  IK = IK + 1
c                  VV(IK) = SS(INDINT(IMKM)+(I-1)*KMDIF+K)
c               ENDDO
c            ENDDO
c         ENDDO
c      ENDDO
c      print*, "write ",ik,"LL integrals"
c      WRITE (15) (VV(ISL),ISL=1,IK)
C
C     --------------------------------------------------------------
C     CALCULATE INTEGRALS OVER SMALL-SMALL COMPONENT BASISFUNCTIONS
C     --------------------------------------------------------------
C     OVERLAP (S), NUCLEAR ATTRACTION ENERGY (V/H)
C     --------------------------------------------------------------
C
      IF (.NOT. TWOC) THEN
      IMKM = 0
      DO 711 IM=NEQ1+1,NEQUIV
      IMDIF = INDCOM(KCOMB(IM)+1)-INDCOM(KCOMB(IM))
      DO 712 KM=NEQ1+1,IM
      IMKM=IMKM+1
      KMDIF = INDCOM(KCOMB(KM)+1)-INDCOM(KCOMB(KM))
      ICOUNT = IMDIF * KMDIF
      INDINT(IMKM+1)=INDINT(IMKM)+ICOUNT
  712 CONTINUE
  711 CONTINUE
C
      CALL AZERO(VV,INDINT(IMKM+1))
      ICTI = KCOMB(NEQ1+1)
      MCTI = INDCOM(ICTI) + 1
      ICTI = KCOMB(NEQUIV)
      NCTI = INDCOM(ICTI + 1)
      MCTK = MCTI
      DO 500 I = MCTI, NCTI
      IM = KCBINV(I,1)
      IN = KCBINV(I,2)
      ICTI = KCOMB(IM)
      MCTIH = INDCOM(ICTI) + 1
      IMAX = INDCOM(ICTI+1) - INDCOM(ICTI)
      NCTK = INDCOM(KCOMB(IN)+1)
      DO 510 K = MCTK, NCTK
      KM = KCBINV(K,1)
      KCTK = KCOMB(KM)
      MCTKH = INDCOM(KCTK) + 1
      ZERO=.FALSE.
      IND=1
      SGN(1)=1.0D0
      INDEX(1)= (K - MCTKH) * IMAX + I - MCTIH + 1
      DO 450 M=1,NSYMOP
      SN=1.0D0
      IT=ICOMOP(I,M)
      KT=ICOMOP(K,M)
      IF (IT.LT.0) THEN
          IT=-IT
          SN=-SN
      ENDIF
      IF (KT.LT.0) THEN
          KT=-KT
          SN=-SN
      ENDIF
      IF (IT-I) 500,410,420
  410 IF (KT-K) 510,420,420
  420 NN = (KT - MCTKH) * IMAX + IT - MCTIH + 1
      DO 430 N=1,IND
      IF (INDEX(N).EQ.NN) GOTO 440
  430 CONTINUE
      IND=IND+1
      INDEX(IND)=NN
      SGN(IND)=SN
      GOTO 450
  440 IF (SGN(N).NE.SN) ZERO=.TRUE.
  450 CONTINUE
      CALL AZERO(SSA,NBFST*NBFST)
      IF (.NOT. ZERO) THEN
      IJC = 0
C
C === COMPUTE ALL THE PRIMITIVE INTEGRALS BELONGING TO IJ
C
      IF ((INDBAS(IM+1)-INDBAS(IM)+1).GT.NBFST) CALL Q2JOB(5,'MFDONEE',
     &   'NBFST',(INDBAS(IM+1)-INDBAS(IM)+1))
      IF ((INDBAS(KM+1)-INDBAS(KM)+1).GT.NBFST) CALL Q2JOB(5,'MFDONEE',
     &   'NBFST',(INDBAS(KM+1)-INDBAS(KM)+1))
      DO 520 IC = INDBAS(IM) + 1, INDBAS(IM + 1)
      DO 530 JC = INDBAS(KM) + 1, INDBAS(KM + 1)
      IJC = IJC + 1
      SINT = 0.0D0
      CALL MFDSTIN(IC,JC)
      SSA(IJC) = SINT
  530 CONTINUE
  520 CONTINUE
      ENDIF
C
C === TRANSFORM THE INTEGRALS TO THE CONTRACTED BASIS
C
      CALL AZERO(SSB,NBFST*NBFST)
      IJCC = 0
      DO 540 JCC = KCBINV(K,1), KCBINV(K,2)
      DO 550 ICC = KCBINV(I,1), KCBINV(I,2)
      IJCC = IJCC + 1
      IJC = 0
      DO 560 IC = INDBAS(ICC) + 1, INDBAS(ICC + 1)
      DO 570 JC = INDBAS(JCC) + 1, INDBAS(JCC + 1)
      IJC = IJC + 1
      SSB(IJCC) = SSB(IJCC) + COEFF(IC) * COEFF(JC) * SSA(IJC)
  570 CONTINUE
  560 CONTINUE
  550 CONTINUE
  540 CONTINUE
C
C === PROCEED AS USUAL (WELL ALMOST)
C
      IJCC = 0
      DO 580 JCC = KCBINV(K,1), KCBINV(K,2)
      DO 590 ICC = KCBINV(I,1), KCBINV(I,2)
      IJCC = IJCC + 1
      IF (JCC .GT. ICC) GOTO 590
      IMKM = ((ICC - NEQ1) * (ICC - 1 - NEQ1)) / 2 + JCC - NEQ1
      DO 460 N=1,IND
      NN = INDEX(N) + INDINT(IMKM)
      SS(NN)=SGN(N)*SSB(IJCC)
  460 CONTINUE
  590 CONTINUE
  580 CONTINUE
C
      DO 595 L=1,NNUCL
      ZERO=.FALSE.
      IND=1
      SGN(1)=ZNUC(L)
      INDEX(1) = (K - MCTKH) * IMAX + I - MCTIH + 1
      INUCL(1)=L
      DO 620 M=1,NSYMOP
      SN=ZNUC(L)
      IT=ICOMOP(I,M)
      KT=ICOMOP(K,M)
      LT=NUCOP(L,M)
      IF (IT.LT.0) THEN
          IT=-IT
          SN=-SN
      ENDIF
      IF (KT.LT.0) THEN
          KT=-KT
          SN=-SN
      ENDIF
      IF (IT-I) 500,470,490
  470 IF (KT-K) 510,480,490
  480 IF (LT-L) 595,490,490
  490 NN = (KT -MCTKH) * IMAX + IT - MCTIH + 1
      DO 630 N=1,IND
      IF (INDEX(N).EQ.NN.AND.INUCL(N).EQ.LT) GOTO 610
  630 CONTINUE
      IND=IND+1
      INDEX(IND)=NN
      INUCL(IND)=LT
      SGN(IND)=SN
      GOTO 620
  610 IF (SGN(N).NE.SN) ZERO=.TRUE.
  620 CONTINUE
      IF (ZERO) GOTO 595
      IJC = 0
      DO 640 IC = INDBAS(IM) + 1, INDBAS(IM + 1)
      DO 650 JC = INDBAS(KM) + 1, INDBAS(KM + 1)
      IJC = IJC + 1
      VINT = 0.0D0
      IF (GNUC) THEN
         CALL MFDGINT(IC,JC)
      ELSE
         CALL MFDAINT(IC,JC)
      ENDIF
      SSA(IJC) = VINT
  650 CONTINUE
  640 CONTINUE
      CALL AZERO(SSB,NBFST*NBFST)
      IJCC = 0
      DO 660 JCC = KCBINV(K,1), KCBINV(K,2)
      DO 670 ICC = KCBINV(I,1), KCBINV(I,2)
      IJCC = IJCC + 1
      IJC = 0
      DO 680 IC = INDBAS(ICC) + 1, INDBAS(ICC + 1)
      DO 690 JC = INDBAS(JCC) + 1, INDBAS(JCC + 1)
      IJC = IJC + 1
      SSB(IJCC) = SSB(IJCC) + COEFF(IC) * COEFF(JC) * SSA(IJC)
  690 CONTINUE
  680 CONTINUE
  670 CONTINUE
  660 CONTINUE
      IJCC = 0
      DO 700 JCC = KCBINV(K,1), KCBINV(K,2)
      DO 710 ICC = KCBINV(I,1), KCBINV(I,2)
      IJCC = IJCC + 1
      IF (JCC .GT. ICC) GOTO 710
      IMKM = ((ICC - NEQ1) * (ICC - 1 - NEQ1)) / 2 + JCC - NEQ1
      DO 600 N=1,IND
      NN = INDEX(N) + INDINT(IMKM)
      VV(NN)=VV(NN)-SGN(N)*SSB(IJCC)
  600 CONTINUE
  710 CONTINUE
  700 CONTINUE
  595 CONTINUE
  510 CONTINUE
  500 CONTINUE
C
      IND=1
      KND = 0
      NSCF = 0
      DO 720 N=1,NSYMRP
      IND=KND+NBSYM1(N)+1
      KND=KND+NBSYM(N)
      DO 730 I=IND,KND
      IT=KSYMF(I)
      IM=INDB(IT)
      IF(IM.LE.NEQ1) GOTO 730
      ITA=INDORB(IT)+1
      ITB=INDORB(IT+1)
      IT=KCOMB(IM)
      IMAX=INDCOM(IT+1)-INDCOM(IT)
      DO 740 K=IND,I
      KT=KSYMF(K)
      KM=INDB(KT)
      IF(KM.LE.NEQ1) GOTO 740
      KTA=INDORB(KT)+1
      KTB=INDORB(KT+1)
      IF (NSCF.GE.N2) THEN
          WRITE (12) S,H
          NSCF=0
      ENDIF
      NSCF=NSCF+1
#if defined (BIT64)
      SSUM=(0.0,0.0)    
      HSUM=(0.0,0.0)    
#else
      SSUM=(0.0D0,0.0D0)
      HSUM=(0.0D0,0.0D0)
#endif
      ICT=(IM-NEQ1)*(IM-NEQ1-1)/2 + (KM-NEQ1)
      ICT=INDINT(ICT)
      DO 750 L=ITA,ITB
      IT=ITRAN(L)
      DO 760 M=KTA,KTB
      KT=ITRAN(M)
      NN=ICT+(KT-1)*IMAX+IT
#if defined (BIT64)
      CSUM=CONJG(CTRANA(L))*CTRANA(M)+CONJG(CTRANB(L))*CTRANB(M)
#else
      CSUM=DCONJG(CTRANA(L))*CTRANA(M)+DCONJG(CTRANB(L))*CTRANB(M)
#endif
      SSUM=SSUM+CSUM*SS(NN)
      HSUM=HSUM+CSUM*VV(NN)
  760 CONTINUE
  750 CONTINUE
#if defined (BIT64)
      IF (CABS(SSUM).LT.ENORM) SSUM=(0.0,0.0)
      IF (CABS(HSUM).LT.ENORM) HSUM=(0.0,0.0)
#else
      IF (CDABS(SSUM).LT.ENORM) SSUM=(0.0D0,0.0D0)
      IF (CDABS(HSUM).LT.ENORM) HSUM=(0.0D0,0.0D0)
#endif
      S(NSCF)=SSUM
      H(NSCF)=HSUM
  740 CONTINUE
  730 CONTINUE
  720 CONTINUE
      ENDIF
      WRITE (12) S,H
      NSCF=0
cC
cC     Write scalar overlap integrals as lower triangle in combined index
cC     Use VV as scratch array
cC
c      IK = 0
c      DO IM=1,NEQUIV-NEQ1
c         IMDIF = INDCOM(KCOMB(NEQ1+IM)+1)-INDCOM(KCOMB(NEQ1+IM))
c         DO I = 1, IMDIF
c            DO KM=1,IM
c               IMKM =( IM-1)*IM/2 + KM
c               KMDIF = INDCOM(KCOMB(NEQ1+KM)+1)-INDCOM(KCOMB(NEQ1+KM))
c               KMMAX = KMDIF
c               IF (IM.EQ.KM) KMMAX = I
c               DO K = 1, KMMAX
c                  IK = IK + 1
c                  VV(IK) = SS(INDINT(IMKM)+(I-1)*KMDIF+K)
c               ENDDO
c            ENDDO
c         ENDDO
c      ENDDO
c      print*, "write ",ik,"SS integrals"
c      WRITE (15) (VV(ISS),ISS=1,IK)
C
C     --------------------------------------------------------------
C     CALCULATE INTEGRALS OVER SMALL-LARGE COMPONENT BASISFUNCTIONS
C     --------------------------------------------------------------
C     IMPULS INTEGRALS (PX,PY,PZ/H)
C     --------------------------------------------------------------
C
      IF (.NOT. TWOC) THEN
      IMKM = 0
      DO 811 IM=NEQ1+1,NEQUIV
      IMDIF = INDCOM(KCOMB(IM)+1)-INDCOM(KCOMB(IM))
      DO 812 KM=1,NEQ1
      IMKM=IMKM+1
      KMDIF = INDCOM(KCOMB(KM)+1)-INDCOM(KCOMB(KM))
      ICOUNT = IMDIF * KMDIF
      INDINT(IMKM+1)=INDINT(IMKM)+ICOUNT
  812 CONTINUE
  811 CONTINUE
      ICTI = KCOMB(NEQ1+1)
      MCTI = INDCOM(ICTI) + 1
      ICTI = KCOMB(NEQUIV)
      NCTI = INDCOM(ICTI + 1)
      ICTI = KCOMB(1)
      MCTK = INDCOM(ICTI) + 1
      ICTI = KCOMB(NEQ1)
      NCTK = INDCOM(ICTI + 1)
      DO 800 I = MCTI, NCTI
      IM = KCBINV(I,1)
      ICTI = KCOMB(IM)
      MCTIH = INDCOM(ICTI) + 1
      IMAX = INDCOM(ICTI+1) - INDCOM(ICTI)
      DO 810 K = MCTK, NCTK
      KM = KCBINV(K,1)
      KCTK = KCOMB(KM)
      MCTKH = INDCOM(KCTK) + 1
      INDL(1)=0
      INDL(2)=0
      INDL(3)=0
      DO 820 L=1,3
      IND=1
      NULL(L)=.FALSE.
      PINT(L)=0.0D0
      ISN(1,L)=1
      INDXX(1,L)=(L-1)*L1+(K-MCTKH)*IMAX+I-MCTIH+1
      DO 830 M=1,NSYMOP
      IS=1
      IT=ICOMOP(I,M)
      KT=ICOMOP(K,M)
      LT=ITYPOP(L,M)
      IF (IT.LT.0) THEN
          IT=-IT
          IS=-IS
      ENDIF
      IF (KT.LT.0) THEN
          KT=-KT
          IS=-IS
      ENDIF
      IF (LT.LT.0) THEN
          LT=-LT
          IS=-IS
      ENDIF
      IF (IT-I) 800,840,860
  840 IF (KT-K) 810,850,860
  850 IF (LT-L) 820,860,860
  860 NN=(LT-1)*L1+(KT-MCTKH)*IMAX+IT-MCTIH+1
      DO 870 N=1,IND
      IF (INDXX(N,L).EQ.NN) GOTO 880
  870 CONTINUE
      IND=IND+1
      INDXX(IND,L)=NN
      ISN  (IND,L)=IS
      GOTO 830
  880 IF (ISN(N,L).NE.IS) NULL(L)=.TRUE.
  830 CONTINUE
      INDL(L)=IND
  820 CONTINUE
      MCOUNT=MCOUNT+INDL(1)+INDL(2)+INDL(3)
      CALL AZERO(PPA,3*NBFST*NBFST)
      IF(.NOT.(NULL(1).AND.NULL(2).AND.NULL(3))) THEN
      IJC = 0
C
C === COMPUTE ALL THE PRIMITIVE INTEGRALS BELONGING TO IJ
C
      IF ((INDBAS(IM+1)-INDBAS(IM)+1).GT.NBFST) CALL Q2JOB(5,'MFDONEE',
     &   'NBFST',(INDBAS(IM+1)-INDBAS(IM)+1))
      IF ((INDBAS(KM+1)-INDBAS(KM)+1).GT.NBFST) CALL Q2JOB(5,'MFDONEE',
     &   'NBFST',(INDBAS(KM+1)-INDBAS(KM)+1))
      DO 890 IC = INDBAS(IM) + 1, INDBAS(IM + 1)
      DO 900 JC = INDBAS(KM) + 1, INDBAS(KM + 1)
      IJC = IJC + 1
      PINT(1) = 0.0D0
      PINT(2) = 0.0D0
      PINT(3) = 0.0D0
      CALL MFDPINT(IC,JC)
      DO 910 L = 1, 3
      PPA(IJC,L) = PINT(L)
  910 CONTINUE
  900 CONTINUE
  890 CONTINUE
      ENDIF
C
C === TRANSFORM THE INTEGRALS TO THE CONTRACTED BASIS
C
      CALL AZERO(PPB,3*NBFST*NBFST)
      DO 960 L = 1, 3
      IJCC = 0
      DO 920 JCC = KCBINV(K,1), KCBINV(K,2)
      DO 930 ICC = KCBINV(I,1), KCBINV(I,2)
      IJCC = IJCC + 1
      IJC = 0
      DO 940 IC = INDBAS(ICC) + 1, INDBAS(ICC + 1)
      DO 950 JC = INDBAS(JCC) + 1, INDBAS(JCC + 1)
      IJC = IJC + 1
      PPB(IJCC,L) = PPB(IJCC,L) + COEFF(IC) * COEFF(JC) * PPA(IJC,L)
  950 CONTINUE
  940 CONTINUE
  930 CONTINUE
  920 CONTINUE
  960 CONTINUE
C
C === PROCEED AS USUAL (WELL ALMOST)
C
      IJCC = 0
      DO 970 JCC = KCBINV(K,1), KCBINV(K,2)
      DO 980 ICC = KCBINV(I,1), KCBINV(I,2)
      IJCC = IJCC + 1
      IF (JCC .GT. ICC) GOTO 980
      IMKM = ((ICC - NEQ1 - 1) * NEQ1) + JCC
      DO 1000 L = 1, 3
      DO 990 N=1,INDL(L)
      NN = INDXX(N,L) + INDINT(IMKM)
#if defined (BIT64)
      PP(NN)=REAL(ISN(N,L))*PPB(IJCC,L)
#else
      PP(NN)=DBLE(ISN(N,L))*PPB(IJCC,L)
#endif
  990 CONTINUE
 1000 CONTINUE
  980 CONTINUE
  970 CONTINUE
C
  810 CONTINUE
  800 CONTINUE
C
      IND=1
      NSCF = 0
      NB =0
      NB1=0
      KND=0
      MND=0
      DO 1100 N=1,NSYMRP
      MND=MND+NB-NB1
      NB =NBSYM (N)
      NB1=NBSYM1(N)
      IND=KND+NB1+1
      KND=KND+NB
      LND=MND+1
      MND=MND+NB1
      DO 1110 I=IND,KND
      IT=KSYMF(I)
      IM=INDB(IT)
      IF(IM.LE.NEQ1) GOTO 1110
      ITA=INDORB(IT)+1
      ITB=INDORB(IT+1)
      IT=KCOMB(IM)
      IMAX=INDCOM(IT+1)-INDCOM(IT)
      DO 1120 K=LND,MND
      KT=KSYMF(K)
      KM=INDB(KT)
      IF(KM.GT.NEQ1) GOTO 1120
      KTA=INDORB(KT)+1
      KTB=INDORB(KT+1)
      IF (NSCF.GE.N2) THEN
          WRITE (11) H
          NSCF=0
      ENDIF
      NSCF=NSCF+1
#if defined (BIT64)
      PXSUM=(0.0,0.0)
      PYSUM=(0.0,0.0)
      PZSUM=(0.0,0.0)
#else
      PXSUM=(0.0D0,0.0D0)
      PYSUM=(0.0D0,0.0D0)
      PZSUM=(0.0D0,0.0D0)
#endif
      ICT=(IM-NEQ1-1)*NEQ1 + KM
      ICT=INDINT(ICT)
      DO 1130 L=ITA,ITB
      IT=ITRAN(L)
      DO 1140 M=KTA,KTB
      KT=ITRAN(M)
      NN=ICT+(KT-1)*IMAX+IT
#if defined (BIT64)
      CSUMX=CONJG(CTRANA(L))*CTRANB(M)+CONJG(CTRANB(L))*CTRANA(M)
      CSUMY=CONJG(CTRANA(L))*CTRANB(M)-CONJG(CTRANB(L))*CTRANA(M)
      CSUMZ=CONJG(CTRANA(L))*CTRANA(M)-CONJG(CTRANB(L))*CTRANB(M)
      CSUMY=CMPLX(0.0,-1.0)*CSUMY
#else
      CSUMX=DCONJG(CTRANA(L))*CTRANB(M)+DCONJG(CTRANB(L))*CTRANA(M)
      CSUMY=DCONJG(CTRANA(L))*CTRANB(M)-DCONJG(CTRANB(L))*CTRANA(M)
      CSUMZ=DCONJG(CTRANA(L))*CTRANA(M)-DCONJG(CTRANB(L))*CTRANB(M)
      CSUMY=DCMPLX(0.D0,-1.D0)*CSUMY
#endif
      PXSUM=PXSUM+CSUMX*PP(NN)
      PYSUM=PYSUM+CSUMY*PP(NN+L1)
      PZSUM=PZSUM+CSUMZ*PP(NN+L1+L1)
 1140 CONTINUE
 1130 CONTINUE
#if defined (BIT64)
      CSUM=CMPLX(0.0,-1.0)*(PXSUM+PYSUM+PZSUM)
      IF (CABS(CSUM).LT.ENORM) CSUM=(0.0,0.0)
#else
      CSUM=DCMPLX(0.D0,-1.D0)*(PXSUM+PYSUM+PZSUM)
      IF (CDABS(CSUM).LT.ENORM) CSUM=(0.0D0,0.0D0)
#endif
      H(NSCF)=CSUM
 1120 CONTINUE
 1110 CONTINUE
 1100 CONTINUE
      ENDIF
      WRITE (11) H
      RETURN
 5556 CALL Q2JOB(2,'FILE MFDAREP CANNOT BE ACCESSED',' ',0)
 5557 CALL Q2JOB(2,'FILE arepints CANNOT BE ACCESSED',' ',0)
      END

      SUBROUTINE RED(N,A,NU)
      INTEGER N,NU
      REAL*8 A(N)
      READ(NU) A
      RETURN
      END
