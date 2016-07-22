C
C     ============================
      SUBROUTINE KRAMERS (NSP,NSC)
C     ============================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE "vecs.inc"
#if defined (PARALLEL)
      INCLUDE "mpidef.inc"
#else
      PARAMETER (mpirnk=0)
#endif
C
C     MAKE LIST OF TIME REVERSED FUNCTIONS
C
      COMPLEX*16 PHASE,QR
      PARAMETER (PRECIS=1.D-10)
C
      QRABS =0.D0
      Difmax = 0.D0
C
      DO 10 IMO = 1, NSP
         ITRMO(IMO) = 0
   10    CONTINUE
C
      DO 400 IMO = 1, NSP
         IF (ITRMO(IMO).NE.0) GOTO 400
C        Find the largest element
         RMOMAX = 0.D0
         DO 20 I = 1, NSC
#if defined (BIT64)
            RMO = CONJG(QA(I,IMO))*QA(I,IMO) 
     &          + CONJG(QB(I,IMO))*QB(I,IMO)
            IF (SQRT(RMO).GT.RMOMAX) THEN
#else
            RMO = DCONJG(QA(I,IMO))*QA(I,IMO) 
     &          + DCONJG(QB(I,IMO))*QB(I,IMO)
            IF (DSQRT(RMO).GT.RMOMAX) THEN
#endif
               MOMAX = I
#if defined (BIT64)
               RMOMAX = SQRT(RMO)
               RMOA = CABS(QA(I,IMO))
               RMOB = CABS(QB(I,IMO))
#else
               RMOMAX = DSQRT(RMO)
               RMOA = CDABS(QA(I,IMO))
               RMOB = CDABS(QB(I,IMO))
#endif
            ENDIF
   20       CONTINUE
C        -------------------------------------------
C        STORE THE PHASE FOR THE BACK TRANSFORMATION
C        -------------------------------------------
         RPHASE(IMO) = 1.0D0
C
         DO 300 JMO = 1, NSP
            IF (ITRMO(JMO).NE.0) GOTO 300
#if defined (BIT64)
            SMOA = CABS(QA(MOMAX,JMO))
            SMOB = CABS(QB(MOMAX,JMO))
            DIF = (ABS(RMOA-SMOB) + ABS(RMOB-SMOA)) / RMOMAX
#else
            SMOA = CDABS(QA(MOMAX,JMO))
            SMOB = CDABS(QB(MOMAX,JMO))
            DIF = (DABS(RMOA-SMOB) + DABS(RMOB-SMOA)) / RMOMAX
#endif
C           -------------------------------------
C           TIMEREVERSAL : T QA(IMO) = QB+(JMO)
C                          T QB(IMO) = - QA+(JMO)
C           -------------------------------------
            IF (DIF.GT.PRECIS) GOTO 300
            IF (RMOA.GT.RMOB) THEN
#if defined (BIT64)
               PHASE = CONJG(QA(MOMAX,IMO))/QB(MOMAX,JMO)
#else
               PHASE = DCONJG(QA(MOMAX,IMO))/QB(MOMAX,JMO)
#endif
            ELSE
#if defined (BIT64)
               PHASE = - CONJG(QB(MOMAX,IMO))/QA(MOMAX,JMO)
#else
               PHASE = - DCONJG(QB(MOMAX,IMO))/QA(MOMAX,JMO)
#endif
            ENDIF
            DO 200 I = 1, NSC
               QR = QB(I,JMO) * PHASE
#if defined (BIT64)
               DIF = CABS(CONJG(QA(I,IMO))-QR)
#else
               DIF = CDABS(DCONJG(QA(I,IMO))-QR)
#endif
               IF (DIF.GT.PRECIS) GOTO 300
#if defined (BIT64)
               If (Difmax.LT.Dif) QRABS = CAbs(QR)
#else
               If (Difmax.LT.Dif) QRABS = CDAbs(QR)
#endif
               If (Difmax.LT.Dif) Difmax = Dif
               QR = QA(I,JMO) * PHASE
#if defined (BIT64)
               DIF = CABS(CONJG(QB(I,IMO))+QR)
#else
               DIF = CDABS(DCONJG(QB(I,IMO))+QR)
#endif
               IF (DIF.GT.PRECIS) GOTO 300
#if defined (BIT64)
               If (Difmax.LT.Dif) QRABS = CAbs(QR)
#else
               If (Difmax.LT.Dif) QRABS = CDAbs(QR)
#endif
               If (Difmax.LT.Dif) Difmax = Dif
  200          CONTINUE
            ITRMO(IMO) = JMO
            ITRMO(JMO) = - IMO
C           -------------------------------------------
C           STORE THE PHASE FOR THE BACK TRANSFORMATION
C           -------------------------------------------
#if defined (BIT64)
            RPHASE(JMO) = CONJG(PHASE)
            IF (CABS(PHASE-CMPLX(1.0,0.00)).GT.PRECIS) THEN
#else
            RPHASE(JMO) = DCONJG(PHASE)
            IF (CDABS(PHASE-DCMPLX(1.D0,0.0D0)).GT.PRECIS) THEN
#endif
             if (mpirnk.eq.0) WRITE (6,1020) JMO,PHASE
             CALL Q2JOB(6,'VECTORS SHOULD BE TRUE KRAMERS PAIRS',' ',1)
            ENDIF
            GOTO 400
  300       CONTINUE
  400    CONTINUE
C
      NOKR = 0
      DO 700 JMO = 1, NSP
         IF (ITRMO(JMO).GT.0) GOTO 700
         IF (ITRMO(JMO).EQ.0) THEN
             if (mpirnk.eq.0) WRITE (6,1000) JMO
             NOKR = NOKR + 1
         ENDIF
         IMO = - ITRMO(JMO)
         DO 600 I = 1, NSC
#if defined (BIT64)
            QA(I,JMO) = - CONJG(QB(I,IMO))
            QB(I,JMO) = CONJG(QA(I,IMO))
#else
            QA(I,JMO) = - DCONJG(QB(I,IMO))
            QB(I,JMO) = DCONJG(QA(I,IMO))
#endif
  600       CONTINUE
  700    CONTINUE
         if (mpirnk.eq.0) Write (6,1030) Difmax,qrabs
         IF (NOKR.NE.0) THEN
            if (mpirnk.eq.0) WRITE (6,1010) NOKR
            CALL Q2JOB (6,'MOS WITHOUT KRAMERS PARTNER FOUND',' ',0)
         ENDIF
C
 1000 FORMAT (' MO',I4,' has no Kramers partner')
 1010 FORMAT (/I4,' MOS without partner found : 4-index transformation ',
     &'not possible'/' Use proper symmetry or rotate vectors such that',
     &' they form pairs.')
 1020 Format (' MO',I4,' is rotated by ',2f12.6)
 1030 Format (/' Maximum deviation from Kramers symmetry :',E10.3/
     &         ' Absolute value of the coefficient :      ',E10.3)
      RETURN
      END
