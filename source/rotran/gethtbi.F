      LOGICAL FUNCTION GETHTBI(JI,RJA,RJB,RJC,NRJ,NINT,
     &                        INDRJ,LBRCIJ)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE "files.inc"
C
      REAL*8 RJ(16384)
      COMPLEX*16 RJA(*),RJB(*),RJC(*)
      INTEGER JI,NRJ,NINT
      INTEGER INDRJ(NRJ,2),LBRCIJ(0:*),IND(16384)
C
      COMPLEX*16 A0,A1
#if defined (BIT64)
      PARAMETER (A0=(0.0,0.0),A1=(1.0,0.0))
      DATA MASK /Z"0000000000001FFF"/
      DATA MASK2/Z"0000000000000001"/
#else
      PARAMETER (A0=(0.D0,0.D0),A1=(1.D0,0.D0))
      DATA MASK /'00001FFF'X/
      DATA MASK2/'00000001'X/
#endif
C
      SAVE MASK,MASK2
C
#if defined (CRAY)
      ISHIFT (I, J) = SHIFT (I, J)
#else
      ISHIFT (I, J) = ISHFT (I, J)
#endif
      NINT=0
      IREC=LBRCIJ(JI)
C
 10   IF (IREC.EQ.0) GOTO 20
        READ(INTTWO,REC=IREC) NTL,NTI,IREC,
     &                  (IND(II),II=1,NTL),
     &                  (RJ(II),II=1,NTI)
C
      DO K=NINT+1,NINT+NTL
         RJA(K) = A0
         RJB(K) = A0
         RJC(K) = A0
      ENDDO
C
      II = 0
      DO 30 K=1,NTL
C
        NINT=NINT+1
C
#if defined (BIT64)
        INDRJ(NINT,1)=AND(ISHIFT(IND(K),51),MASK)
        INDRJ(NINT,2)=AND(IND(K),MASK)
#else
        INDRJ(NINT,1)=IAND(ISHIFT(IND(K),-13),MASK)
        INDRJ(NINT,2)=IAND(IND(K),MASK)
#endif
C
#if defined (BIT64)
        ITYP=AND(ISHIFT(IND(K),38),MASK2)
#else
        ITYP=IAND(ISHIFT(IND(K),-26),MASK2)
#endif
        IF (ITYP.NE.0) THEN
           II = II + 1
#if defined (BIT64)
           RJA(NINT)=CMPLX(RJ(II),0.0)
#else
           RJA(NINT)=DCMPLX(RJ(II),0.D0)
#endif
        ENDIF
C
#if defined (BIT64)
        ITYP=AND(ISHIFT(IND(K),37),MASK2)
#else
        ITYP=IAND(ISHIFT(IND(K),-27),MASK2)
#endif
        IF (ITYP.NE.0) THEN
           II = II + 1
#if defined (BIT64)
           RJA(NINT)=RJA(NINT)+CMPLX(0.0,RJ(II))
#else
           RJA(NINT)=RJA(NINT)+DCMPLX(0.D0,RJ(II))
#endif
        ENDIF
C
#if defined (BIT64)
        ITYP=AND(ISHIFT(IND(K),36),MASK2)
#else
        ITYP=IAND(ISHIFT(IND(K),-28),MASK2)
#endif
        IF (ITYP.NE.0) THEN
           II = II + 1
#if defined (BIT64)
           RJB(NINT)=CMPLX(RJ(II),0.0)
#else
           RJB(NINT)=DCMPLX(RJ(II),0.D0)
#endif
        ENDIF
C
#if defined (BIT64)
        ITYP=AND(ISHIFT(IND(K),35),MASK2)
#else
        ITYP=IAND(ISHIFT(IND(K),-29),MASK2)
#endif
        IF (ITYP.NE.0) THEN
           II = II + 1
#if defined (BIT64)
           RJB(NINT)=RJB(NINT)+CMPLX(0.0,RJ(II))
#else
           RJB(NINT)=RJB(NINT)+DCMPLX(0.D0,RJ(II))
#endif
        ENDIF
C
#if defined (BIT64)
        ITYP=AND(ISHIFT(IND(K),34),MASK2)
#else
        ITYP=IAND(ISHIFT(IND(K),-30),MASK2)
#endif
        IF (ITYP.NE.0) THEN
           II = II + 1
#if defined (BIT64)
           RJC(NINT)=CMPLX(RJ(II),0.0)
#else
           RJC(NINT)=DCMPLX(RJ(II),0.D0)
#endif
        ENDIF
C
#if defined (BIT64)
        ITYP=AND(ISHIFT(IND(K),33),MASK2)
#else
        ITYP=IAND(ISHIFT(IND(K),-31),MASK2)
#endif
        IF (ITYP.NE.0) THEN
           II = II + 1
#if defined (BIT64)
           RJC(NINT)=RJC(NINT)+CMPLX(0.0,RJ(II))
#else
           RJC(NINT)=RJC(NINT)+DCMPLX(0.D0,RJ(II))
#endif
        ENDIF
C       
 30     CONTINUE
C
      GOTO 10
C
 20   IF (NINT.EQ.0) THEN
        GETHTBI=.FALSE.
      ELSE
        GETHTBI=.TRUE.
      ENDIF
C
      RETURN
      END
