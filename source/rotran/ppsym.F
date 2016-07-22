      SUBROUTINE PPSYM(INTSKEL,MDINT,ISKEL,IS,JS,KS,LS,NKR,INDKL,INDK,
     &                 INDL,RKL,NSKSIZ,LBRCSK,LBCK,CBCK,CBCK2,NC2,
     &                 JKRIND,ICIND,IPRNT,THROUT,NFINAL)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INTEGER INDKL(*),INDK(*),INDL(*)
      INTEGER LBRCSK(0:NKR),LBCK(2,NSKSIZ),JKRIND(NKR),ICIND(NC2)
      COMPLEX*16 CBCK(NSKSIZ),CBCK2(NKR*NKR,NC2),RKL(*)
C
C     Restore particle-particle symmetry, 
C     write completed integrals to file.
C
      INTSKEL1 = INTSKEL+ISKEL
      INTSKEL2 = INTSKEL+ISKEL+4
C
      REWIND (INTSKEL1)
C
C Loop over canonical blocks ikr, jkr ikr => jkr
C First read the integrals from scratch file intskel1
C
      CALL CZERO (NC2*NKR*NKR,CBCK2,1)
      CALL IZERO (NKR,JKRIND,1)
      CALL IZERO (NC2,ICIND,1)
C
      IC2 = 1
 1    CONTINUE
         READ (INTSKEL1) IKR,JKR,NZ,
     &            (INDKL(INZ),INZ=1,NZ),
     &            (RKL(INZ),INZ=1,NZ)
         IF (IC2.EQ.1) IKRC = IKR
         DO 101 INZ = 1, NZ
            CBCK2(INDKL(INZ),IC2) = RKL(INZ)
 101     CONTINUE
         IF (IC2.LT.NC2.AND.IKRC.EQ.IKR) THEN
            JKRIND(JKR) = IC2
            ICIND(IC2) = JKR
         ELSE
C
C The buffer space is full : add on the particle-particle interchanged
C integrals that are on the direct access file intskel2
C
            IF (IKR.NE.IKRC) THEN
               JKRN = JKR
               IC2 = IC2 - 1
            ELSE
               JKRIND(JKR) = IC2
               ICIND(IC2) = JKR
            ENDIF
            LREC = LBRCSK(IKRC)
  10        IF (LREC.EQ.0) GOTO 20
               READ(INTSKEL2,REC=LREC) NB,LREC,
     +        ((LBCK(IX1,IX2),IX1=1,2),IX2=1,NB),
     +        (CBCK(IX),IX=1,NB)
            DO 201 IX = 1, NB
               IF (JKRIND(LBCK(1,IX)).NE.0) THEN
                  JC2 = JKRIND(LBCK(1,IX))
                  CBCK2(LBCK(2,IX),JC2) = CBCK2(LBCK(2,IX),JC2)
     &                                  + CBCK(IX)
                ENDIF
 201        CONTINUE
            GOTO 10
  20        CONTINUE
C
C Write these buffers to file, and clean the work space
C
            DO 301 JC2 = 1, IC2
               JKRA = ICIND(JC2)
               NZ = 0
               IX = 0
               DO 302 KKR = 1, NKR
               DO 303 LKR = 1, NKR
                  IX = IX + 1
#if defined (BIT64)
                  IF ( CABS(CBCK2(IX,JC2)).GT.THROUT) THEN
#else
                  IF (CDABS(CBCK2(IX,JC2)).GT.THROUT) THEN
#endif
                     NZ = NZ + 1
                     INDK(NZ) = KKR * KS
                     INDL(NZ) = LKR * LS
                     RKL(NZ) = CBCK2(IX,JC2)
                     IF (IPRNT.GE.2) WRITE(6,1000) 
     &                  ISKEL,IKRC,JKRA,KKR,LKR,RKL(NZ)
                  ENDIF
 303           CONTINUE
 302           CONTINUE
               NFINAL = NFINAL + NZ
               WRITE (MDINT) IKRC*IS,JKRA*JS,NZ,
     &              (INDK(INZ),INDL(INZ),INZ=1,NZ),
     &              (RKL(INZ),INZ=1,NZ)
 301        CONTINUE
            IF (IKR.NE.IKRC) THEN
#if defined (BIT64)
               CALL CCOPY (NKR*NKR,CBCK2(1,IC2+1),1,CBCK2,1)
#else
               CALL ZCOPY (NKR*NKR,CBCK2(1,IC2+1),1,CBCK2,1)
#endif
               CALL CZERO (IC2*NKR*NKR,CBCK2(1,2),1)
               CALL IZERO (NKR,JKRIND,1)
               CALL IZERO (NC2,ICIND,1)
               IC2 = 1
               ICIND(IC2) = JKRN
               JKRIND(JKRN) = IC2
               IKRC = IKR
            ELSE
               CALL CZERO (IC2*NKR*NKR,CBCK2,1)
               CALL IZERO (NKR,JKRIND,1)
               CALL IZERO (NC2,ICIND,1)
               IC2 = 0
            ENDIF
         ENDIF
         IC2 = IC2 + 1
         IF (IKR.NE.0) GOTO 1
C
 1000 FORMAT (5I4,2X,2F20.16)
      RETURN
      END
