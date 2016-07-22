      SUBROUTINE TRBOUT(NSP,NKR,HA,HB,MH,IP,IQ,NPQSIZ,NPQBUF,NBCK,LBCK,
     &                  LBRCIJ,RBCK)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     ROUTINE SIMILAR TO TROUT ; BUT 3 BUCKETS OF BREIT INTEGRALS
C
      INTEGER NBCK(2,*),LBCK(NPQSIZ,NPQBUF),LBRCIJ(0:*)
      COMPLEX*16 HA(MH,*),HB(MH,*)
      REAL*8 RBCK(6*NPQSIZ,NPQBUF)
C
      INCLUDE "vecs.inc"
      INCLUDE "files.inc"
C
      PARAMETER (THRSH=1.0D-16)
C
#if defined (CRAY)
      ISHIFT (I, J) = SHIFT (I, J)
#else
      ISHIFT (I, J) = ISHFT (I, J)
#endif
#if defined (BIT64)
      IRECL = 8 + 2*8 + NPQSIZ*8 + NPQSIZ*8
#else
      IRECL = 4 + 2*4 + NPQSIZ*4 + NPQSIZ*8
#endif
C
#if defined (BIT64)
      IPQ = ISHIFT(IP,13)
      IPQ = OR(IQ,IPQ)
#else
      IPQ = ISHIFT(IP,13)
      IPQ = IOR(IQ,IPQ)
#endif
      JI = 0
      DO 30 IBAR=1,2
      DO 20 IK=1,NKR
        IF (IBAR.EQ.1) I = KRA(IK,1)
        IF (IBAR.EQ.2) I = KRB(IK,1)
        DO 10 JK=IK,NKR
          J = KRB(JK,1)
          JI = JI + 1
#if defined (BIT64)
          RMAGNB = CABS(HA(J,I)) + CABS(HB(J,I)) + CABS(HB(I,J))
#else
          RMAGNB = CDABS(HA(J,I)) + CDABS(HB(J,I)) + CDABS(HB(I,J))
#endif
          IF (RMAGNB.LT.THRSH) GOTO 10
          NBL=NBCK(1,JI)+1
C
C--In most cases the integrals are real or imaginary and not all
C--classes are non-zero. Store only the non-zero numbers
C
          NBI=NBCK(2,JI)
          ITYP = 0
#if defined (BIT64)
          RMAG = REAL(HA(J,I))
#else
          RMAG = DBLE(HA(J,I))
#endif
          IF (ABS(RMAG).GT.THRSH) THEN
             NBI = NBI + 1
             RBCK(NBI,JI) = RMAG
             ITYP = ITYP + 1
          ENDIF
#if defined (BIT64)
          RMAG = AIMAG(HA(J,I))
#else
          RMAG = DIMAG(HA(J,I))
#endif
          IF (ABS(RMAG).GT.THRSH) THEN
             NBI = NBI + 1
             RBCK(NBI,JI) = RMAG
             ITYP = ITYP + 2
          ENDIF
#if defined (BIT64)
          RMAG = REAL(HB(J,I))
#else
          RMAG = DBLE(HB(J,I))
#endif
          IF (ABS(RMAG).GT.THRSH) THEN
             NBI = NBI + 1
             RBCK(NBI,JI) = RMAG
             ITYP = ITYP + 4
          ENDIF
#if defined (BIT64)
          RMAG = AIMAG(HB(J,I))
#else
          RMAG = DIMAG(HB(J,I))
#endif
          IF (ABS(RMAG).GT.THRSH) THEN
             NBI = NBI + 1
             RBCK(NBI,JI) = RMAG
             ITYP = ITYP + 8
          ENDIF
#if defined (BIT64)
          RMAG = REAL(HB(I,J))
#else
          RMAG = DBLE(HB(I,J))
#endif
          IF (ABS(RMAG).GT.THRSH) THEN
             NBI = NBI + 1
             RBCK(NBI,JI) = RMAG
             ITYP = ITYP + 16
          ENDIF
#if defined (BIT64)
          RMAG = - AIMAG(HB(I,J))
#else
          RMAG = - DIMAG(HB(I,J))
#endif
          IF (ABS(RMAG).GT.THRSH) THEN
             NBI = NBI + 1
             RBCK(NBI,JI) = RMAG
             ITYP = ITYP + 32
          ENDIF
#if defined (BIT64)
          ITYP = ISHIFT(ITYP,26)
          ITYP = OR(ITYP,IPQ)
#else
          ITYP = ISHIFT(ITYP,26)
          ITYP = IOR(ITYP,IPQ)
#endif
          LBCK(NBL,JI)=ITYP
C---Check whether we need to flush, there should be room left for
C---6 additional reals and one integer
#if defined (BIT64)
          ICURRS = 8*(NBL+1)+8*(NBI+6)+3*8
#else
          ICURRS = 4*(NBL+1)+8*(NBI+6)+3*4
#endif
          IF (ICURRS.GT.IRECL) THEN
            LREC=LBRCIJ(0)+1
            WRITE(INTTWO,REC=LREC) NBL,NBI,LBRCIJ(JI),
     +                             (LBCK(IX,JI),IX=1,NBL),
     +                             (RBCK(IX,JI),IX=1,NBI)
            LBRCIJ(0)=LREC
            LBRCIJ(JI)=LREC
            NHALFT = NHALFT + NBL
            NBL=0
            NBI=0
          ENDIF
          NBCK(1,JI)=NBL
          NBCK(2,JI)=NBI
 10       CONTINUE
 20     CONTINUE
 30     CONTINUE
      RETURN
      END
