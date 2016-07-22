C
C     =================
      SUBROUTINE MFDONE
C     =================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     ALLOCATE FIELD-LENGH FOR THE ONE ELECTRON INTEGRALS
C
      INCLUDE 'paracom.inc'
#if defined (PARALLEL)
      INCLUDE 'mpidef.inc'
#else
      PARAMETER (mpirnk = 0)
#endif
C
      COMPLEX*16 SOCINT(MXBA*MXBA)
C
      NEQ2=NEQUIV-NEQ1
      L1=MAX0(NWRDS1,NWRDS2,NWRDS3)
      L2=MAX0(NEQ1*(NEQ1+1)/2,NEQ2*(NEQ2+1)/2,NEQ1*NEQ2)+1
      NWRDS=3*L1
      IF(N16.LE.NWRDS) CALL Q2JOB(3,'P.MFDONE_FIRST CALL',
     +                            'N16',NWRDS+1)
      IAR=1
      IBR=IAR+L1
      ICR=IBR+L1
      IF(N16I.LE.L2)CALL Q2JOB(3,'P.MFDONE_2E N16I',
     $    'N16',L2+1)
      IF (NAREP.GT.0) THEN
         OPEN(18,FILE='socints',ACCESS='SEQUENTIAL',FORM='UNFORMATTED',
     $           STATUS='UNKNOWN',ERR=5555)
         READ(18,ERR=5555) NBAS,LENSOC
         IF (NBAS.LT.MXBA) THEN
            CALL RED(LENSOC,SOCINT,18)
            CLOSE(18)
         ELSE
            if (mpirnk.eq.0) call q2job(3,'MFDONE: MXBA','MXB',NBAS)
         ENDIF
      ENDIF
      CALL MFDONEE(L1,G(IAR),G(IBR),G(ICR),
     +                G(IAR),L2,INTG(1),
     +                NBAS,SOCINT)
      IG=1
      IH=1
      DO 10 I=1,NSYMRP
      IF(NBSYM1(I).GT.NBSYM1(IG)) IG=I
      IF(NBSYM2(I).GT.NBSYM2(IH)) IH=I
   10 CONTINUE
      NB1=NBSYM1(IG)
      NB2=NBSYM2(IH)
      NB=MAX0(NB1,NB2)
      NWLLH=NB1*(NB1+1)/2
      NWSSH=NB2*(NB2+1)/2
      NWLS =NB1*NB2
      NWRDS2=MAX0(NB1*NB1,NWLS)
      NWRDS3=MAX0(NWLLH,NWSSH)
      IAR=1
      IBR=IAR+NWLLH
      ICR=IBR+NWLS
      IDR=ICR+NWLLH
      IER=ICR+MAX0(NWLLH+NWRDS2,NWSSH)
      IFR=IER+NWRDS3
      IG =IFR+NWRDS3
      IH =IG +NB
      NWRDS=IH-IAR+NB
      IAI=IAR+NWRDS
      IBI=IBR+NWRDS
      ICI=ICR+NWRDS
      IDI=IDR+NWRDS
      IEI=IER+NWRDS
      IFI=IFR+NWRDS
      NWRDS=2*(NWRDS-NB)
      IF(N16.LE.NWRDS) CALL Q2JOB(3,'P.MFDONE_SECND CALL',
     +                            'N16',NWRDS+1)
      NWRDS1=(IER-ICR)
      CALL MFDPPT(NWLLH ,G(IAR),G(IAI),
     $            NWLS  ,G(IBR),G(IBI),
     $            NWRDS1,G(ICR),G(ICI),
     $            NWRDS2,G(IDR),G(IDI),
     $            NWRDS3,G(IER),G(IEI),
     $            NWRDS3,G(IFR),G(IFI),
     $            NB +1 ,G(IER),G(IEI),
     $            NB    ,G(IG ),G(IH ))
      RETURN
 5555 CALL Q2JOB(2,'FILE socints CANNOT BE ACCESSED',' ',0)
      END
