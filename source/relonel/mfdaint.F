C
C     ==================
      SUBROUTINE MFDAINT(II,JJ)
C     ==================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     CALCULATE NUCLEAR ATTRACTION INTEGRALS
C
      INCLUDE 'paracom.inc'
C
      COMMON/INTI/IM,JM,KM,LM,I,J,K,L
      COMMON/INTR/SINT,TINT,AINT
      DIMENSION G2(11,3),F(13)
      EQUIVALENCE (F0,F(1))
      ITYP=ITYPE(I)
      JTYP=ITYPE(J)
      NL1=NR(ITYP,1)
      NL2=NR(JTYP,1)
      NM1=NR(ITYP,2)
      NM2=NR(JTYP,2)
      NN1=NR(ITYP,3)
      NN2=NR(JTYP,3)
      ICNT=ICENT(I)
      JCNT=ICENT(J)
      KCNT = K
      MX=NL1+NL2+1
      MY=NM1+NM2+1
      MZ=NN1+NN2+1
      AR(1)=XCORD(ICNT)
      AR(2)=YCORD(ICNT)
      AR(3)=ZCORD(ICNT)
      BR(1)=XCORD(JCNT)
      BR(2)=YCORD(JCNT)
      BR(3)=ZCORD(JCNT)
      CR(1)=XCORD(KCNT)
      CR(2)=YCORD(KCNT)
      CR(3)=ZCORD(KCNT)
      DSAB=(AR(1)-BR(1))**2+(AR(2)-BR(2))**2+(AR(3)-BR(3))**2
      A=ALPHA(II)
      B=ALPHA(JJ)
      T1=A+B
      T0=1./T1
      DISTAB=A*B*T0*DSAB
      P1=(A*AR(1)+B*BR(1))*T0
      P2=(A*AR(2)+B*BR(2))*T0
      P3=(A*AR(3)+B*BR(3))*T0
      IF(DISTAB.GT.DNORM) GO TO 1635
#if defined (BIT64)
      SOO=TWOPI*T0     *EXP(-DISTAB)
#else
      SOO=TWOPI*T0     *DEXP(-DISTAB)
#endif
      PCX=P1-CR(1)
      PCY=P2-CR(2)
      PCZ=P3-CR(3)
      PCSQ=PCX*PCX+PCY*PCY+PCZ*PCZ
      ARG=T1*PCSQ
      Y=0.0
      IF(ARG.GT.100.0) GO TO 10
#if defined (BIT64)
      Y=EXP(-ARG)
#else
      Y=DEXP(-ARG)
#endif
  10  CONTINUE
      F12=FMCH(12,ARG,Y)
      ARG=2.*ARG
      F11=(ARG*F12+Y)*X23
      F10=(ARG*F11+Y)*X21
      F9=(ARG*F10+Y)*X19
      F8=(ARG*F9+Y)*X17
      F7=(ARG*F8+Y)*X15
      F6=(ARG*F7+Y)*X13
      F5=(ARG*F6+Y)*X11
      F4=(ARG*F5+Y)*X9
      F3=(ARG*F4+Y)*X7
      F2=(ARG*F3+Y)*X5
      F1=(ARG*F2+Y)*X3
      F0= ARG*F1+Y
      PAX=P1-AR(1)
      PBX=P1-BR(1)
      CALL GFUNCT(NL1,NL2,PAX,PBX,PCX,T0,G2,1)
      PAY=P2-AR(2)
      PBY=P2-BR(2)
      CALL GFUNCT(NM1,NM2,PAY,PBY,PCY,T0,G2,2)
      PAZ=P3-AR(3)
      PBZ=P3-BR(3)
      CALL GFUNCT(NN1,NN2,PAZ,PBZ,PCZ,T0,G2,3)
      RAWINT=0.
      DO 506 IX=1,MX
      DO 507 JY=1,MY
      DO 508 KZ=1,MZ
      MXYZ=IX+JY+KZ-2
  508 RAWINT=RAWINT+G2(IX,1)*G2(JY,2)*G2(KZ,3)*F(MXYZ)
  507 CONTINUE
  506 CONTINUE
      AINT=RAWINT*SOO
 1635 CONTINUE
      RETURN
      END
