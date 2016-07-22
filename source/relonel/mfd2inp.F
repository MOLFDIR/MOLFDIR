C
C     ==================
      SUBROUTINE MFD2INP
C     ==================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     READ INPUT DATA FROM MOLFDIR OUTPUT
C
      INCLUDE 'paracom.inc'
#if defined (PARALLEL)
      INCLUDE 'mpidef.inc'
#else
      PARAMETER (mpirnk = 0)
#endif
C
      DIMENSION IPARAM(8)
      EQUIVALENCE(INDBAS,IPARAM)
      CHARACTER*8  TIMEX,DATEX*10,VERS*10
C
      NAMELIST/RELONEL/DNORM,ENORM,OVLAPL,OVLAPS,GNUC,IEIGEN,IKINBL,
     $                 NAREP
      NAMELIST/GENERAL/TWOC
C
      DNORM = 32.0D0
      ENORM = 1.0D-14
      OVLAPL = 1.D-10
      OVLAPS = 1.D-10
      GNUC = .TRUE.
      IEIGEN = 1
      IKINBL = 1
      NAREP = 0
C
      OPEN (5, FILE='MOLFDIR.INP', FORM='FORMATTED')
C
      CALL LOCATE('RELONEL',7,ITEST)
      IF (ITEST.GT.0) READ (5,RELONEL)
      IF (NAREP.GT.0) GNUC=.TRUE.
      TWOC = .FALSE.
      CALL LOCATE('GENERAL',7,ITEST)
      IF (ITEST.GT.0) READ (5,GENERAL)
C
      CLOSE(5)
C
      TIMEX='00:00:00'
      CALL DAYTIME(DATEX,TIMEX)
      if (mpirnk.eq.0) WRITE(*,1000) VERSION,DATEX,TIMEX,TWOC,GNUC,
     &DNORM,ENORM,OVLAPL,OVLAPS,IEIGEN,IKINBL,NAREP
      READ (3)     T,D,VERS,NAME
      READ (3)     (IPARAM(I),I=1,8)
C
C     ------------------------------------------
C     CHECK THE PARAMETERS FOR INCOMPATIBILITIES
C     ------------------------------------------
C
      IF (IPARAM(1).GT.N14) CALL Q2JOB(5,'P.MFD2INP','N14',IPARAM(1))
      IF (IPARAM(2).GT.N1 ) CALL Q2JOB(5,'P.MFD2INP','N1' ,IPARAM(2))
      IF (IPARAM(3).GT.N3 ) CALL Q2JOB(5,'P.MFD2INP','N3' ,IPARAM(3))
      IF (IPARAM(4).GT.N5 ) CALL Q2JOB(5,'P.MFD2INP','N5' ,IPARAM(4))
      IF (IPARAM(5).GT.N7 ) CALL Q2JOB(5,'P.MFD2INP','N7' ,IPARAM(5))
      IF (IPARAM(6).GT.N10) CALL Q2JOB(5,'P.MFD2INP','N10',IPARAM(6))
      IF (IPARAM(7).GT.N12) CALL Q2JOB(5,'P.MFD2INP','N12',IPARAM(7))
      WRITE(14)     T,D,VERS,NAME,TIMEX ,DATEX ,VERSION
      READ (3)      NEQ1,NEQUIV,IDUM,NEQCOM,NGAUS,NCOMB,NSYMOP,NNUCL,
     $              NSYMF,
     $              NCOEFF,NSYMRP,NWRDS1,NWRDS2,NWRDS3,NBT,NST,POTNUC
      IF (NGAUS.GT.N15) CALL Q2JOB(5,'P.MFD2INP','N15',NGAUS)
      WRITE(14)     NEQ1,NEQUIV,IDUM,NEQCOM,NGAUS,NCOMB,NSYMOP,NNUCL,
     $              NSYMF,
     $              NCOEFF,NSYMRP,NWRDS1,NWRDS2,NWRDS3,NBT,NST,POTNUC
      READ (3)      (ZNUC(I),XCORD(I),YCORD(I),ZCORD(I),RNUC(I),
     $              I=1,NNUCL)
      WRITE(14)     (ZNUC(I),XCORD(I),YCORD(I),ZCORD(I),RNUC(I),
     $              I=1,NNUCL)
      READ (3)      ((NUCOP(I,J),I=1,NNUCL),(ITYPOP(I,J),I=1,3),
     $              J=1,NSYMOP)
      WRITE(14)     ((NUCOP(I,J),I=1,NNUCL),(ITYPOP(I,J),I=1,3),
     $              J=1,NSYMOP)
      READ (3)      (KCOMB(I),INDBAS(I),INDSYM(I),I=1,NEQUIV)
      WRITE(14)     (KCOMB(I),INDBAS(I),INDSYM(I),I=1,NEQUIV)
      READ (3)      (ALPHA(I),COEFF(I),I=1,NGAUS)
      WRITE(14)     (ALPHA(I),COEFF(I),I=1,NGAUS)
      READ (3)      (ICENT(I),ITYPE(I),(ICOMOP(I,J),J=1,NSYMOP),
     $              I=1,NCOMB),(INDCOM(I),I=1,NEQCOM)
      WRITE(14)     (ICENT(I),ITYPE(I),(ICOMOP(I,J),J=1,NSYMOP),
     $              I=1,NCOMB),(INDCOM(I),I=1,NEQCOM)
      READ (3)      (INDORB(I),INDB(I),KSYMRP(I),LSYMRP(I),KSYMF(I),
     $              I=1,NSYMF)
      WRITE(14)     (INDORB(I),INDB(I),KSYMRP(I),LSYMRP(I),KSYMF(I),
     $              I=1,NSYMF)
      READ (3)      (ITRAN(I),CTRANA(I),CTRANB(I),I=1,NCOEFF)
      WRITE(14)     (ITRAN(I),CTRANA(I),CTRANB(I),I=1,NCOEFF)
      READ (3)      (NBSYM(I),NBSYM1(I),NBSYM2(I),I=1,NSYMRP)
      WRITE(14)     (NBSYM(I),NBSYM1(I),NBSYM2(I),I=1,NSYMRP)
      READ (3)      LCHAIN,(REPN(I),I=1,NSYMRP)
      WRITE(14)     LCHAIN,(REPN(I),I=1,NSYMRP)
      INDCOM(NEQCOM+1)=NCOMB
      INDBAS(NEQUIV+1)=NGAUS
      INDSYM(NEQUIV+1)=NSYMF
      INDORB(NSYMF+1)=NCOEFF
C
C     -----------------------------------------------------------------
C     MAKE POINTER ARRAY (INVERSE OF INDCOM)
C     FROM FUNCTIONTYPE IXX TO EXPONENTS IM
C     USAGE : ESTABLISH COMPATIBILITY OLD AND NEW LOOPSTRUCTURE MFDTWOE
C     -----------------------------------------------------------------
      IX=0
      IMP=1
      DO 12 IMZ=1,NEQUIV
      IXN=INDCOM(KCOMB(IMZ))
      IF (IXN.NE.IX) THEN
         DO 11 IXX=IX+1,IXN
         KCBINV(IXX,1)=IMP
         KCBINV(IXX,2)=IMZ-1
   11    CONTINUE
         IX=IXN
         IMP=IMZ
      ENDIF
   12 CONTINUE
      IXN=INDCOM(KCOMB(NEQUIV)+1)
      DO 13 IXX=IX+1,IXN
      KCBINV(IXX,1)=IMP
      KCBINV(IXX,2)=NEQUIV
   13 CONTINUE
C
      NBSYM(0) = 0
      INDTWO(1)=0
      DO 50 I=1,NSYMRP
      INDTWO(I+1)=INDTWO(I)+(NBSYM(I)+1)*NBSYM(I)/2
   50 CONTINUE
      INTONE=INDTWO(NSYMRP+1)
      NT=0
      NB=0
      DO 70 I=1,NSYMF
      NA=NB+1
      NB=INDORB(I+1)
      NN=0
      DO 60 N=NA,NB
      NN=NN+1
      NT=NT+1
      ITRAN(NT)=NN
   60 CONTINUE
      INDORB(I+1)=NT
   70 CONTINUE
      DO 90 I=1,3
      DO 80 K=1,NSYMOP
      IT=ITYPOP(I,K)
      IS=IABS(IT)
      IT=(IS-1)*IT/IS
      ITYPOP(I,K)=IT
   80 CONTINUE
   90 CONTINUE
      CLOSE(3)
 1000 FORMAT(//1X,32('*')/1X,'* INTEGRAL PROGRAM "RELONEL"',T33,'*'/
     $      1X,'* VERSION ',A,T33,'*'/1X,'* DATE ',A,T33,'*'/
     $      1X,'* TIME ',A,T33,'*'/1X,32('*')///
     $      1X,'NAMELIST GENERAL',T30,'TWOC :',T45,L1//
     $      1X,'NAMELIST RELONEL',
     $      T30,'GNUC :',T45,L1/T30,'DNORM :',T45,G14.6/
     $      T30,'ENORM :',T45,G14.6/T30,'OVLAPL :',T45,G14.6/
     $      T30,'OVLAPS :',T45,G14.6/T30,'IEIGEN :',T45,I4/
     $      T30,'IKINBL :',T45,I4/
     $      T30,'NAREP  :',T45,I2////)
      RETURN
      END
