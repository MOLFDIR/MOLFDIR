C
C     ======================================================
      SUBROUTINE CONCOP (FR, FI, QR, QI, IREC, NBASL, NBASS,
     +                   SLR, SLI, SSR, SSI, BR, BI, ALPHA,IRP,ISHELL)
C     ======================================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     CONSTRUCT F = ALPHA * S * D * Q(O) + H.C.
C     F WILL BE STORED ROWWISE AS A SQUARE MATRIX
C     THE DENSITY MATRICES ARE READ FROM MFDENSM
C     THIS SUBROUTINE SHOULD BE CALLED FOR EACH OCC. IRP (ONCE OR TWICE)
C
C     IREC IS A POINTER TO THE RECS OF MFDOEMR AND MFDOEMI;
C     MFDENSM SHOULD BE OPEN
C     REWIND MFDENSM BEFORE THE LOOP OVER IRP
C
      INCLUDE 'paracom.inc'
      INCLUDE 'general.inc'
C
      REAL*8 FR (*), FI (*), QR (*), QI(*)
      REAL*8 SLR (MS,MS), SLI (MS,MS), SSR (MS,MS), SSI(MS,MS)
      REAL*8 BR (NBM,NBM), BI (NBM,NBM)
      REAL*8 ALPHA
      INTEGER IREC, NBASL, NBASS, IRP, ISHELL
C
      NBAS = NBASL + NBASS
      NBASH = NBAS * (NBAS + 1) / 2
C
C     -----------------------------------------------------------
C     READ DENSITY MATRIX (LOWER TRIANGLE OF COMPLETE SYM. BASIS)
C     -----------------------------------------------------------
C
      CALL RSDENS(IRECDC,FR,FI,NBASH,IRP,ISHELL)
C
C     -----------------------------------------------
C     READ LARGE AND SMALL COMPONENT OVERLAP MATRICES
C     -----------------------------------------------
C
      READ (MFDOEMR, REC = IREC + 2)
     +     ((SLR (I, J), J = 1, I), I = 1, NBASL)
      READ (MFDOEMI, REC = IREC + 2)
     +     ((SLI (I, J), J = 1, I), I = 1, NBASL)
      CALL MHERM (SLR, SLI, MS, NBASL)
C
      READ (MFDOEMR, REC = IREC + 3)
     +     ((SSR (I, J), J = 1, I), I = 1, NBASS)
      READ (MFDOEMI, REC = IREC + 3)
     +     ((SSI (I, J), J = 1, I), I = 1, NBASS)
      CALL MHERM (SSR, SSI, MS, NBASS)
C
C     ---------
C     B = S * F
C     ---------
C
      DO 10 I = 1, NBASL
        DO 20 J = 1, NBASL
          BR(I,J) = 0.0D0
          BI(I,J) = 0.0D0
          DO 30 K = 1, NBASL
            IF (K .GE. J) THEN
              BR(I,J) = BR(I,J)
     +                + SLR(I,K) * FR(K*(K-1)/2+J)
     +                - SLI(I,K) * FI(K*(K-1)/2+J)
              BI(I,J) = BI(I,J)
     +                + SLR(I,K) * FI(K*(K-1)/2+J)
     +                + SLI(I,K) * FR(K*(K-1)/2+J)
            ELSE
              BR(I,J) = BR(I,J)
     +                + SLR(I,K) * FR(J*(J-1)/2+K)
     +                + SLI(I,K) * FI(J*(J-1)/2+K)
              BI(I,J) = BI(I,J)
     +                - SLR(I,K) * FI(J*(J-1)/2+K)
     +                + SLI(I,K) * FR(J*(J-1)/2+K)
            ENDIF
 30       CONTINUE
 20     CONTINUE
 10   CONTINUE
C
      DO 40 I = NBASL + 1, NBAS
        DO 50 J = 1, NBASL
          BR(I,J) = 0.0D0
          BI(I,J) = 0.0D0
          DO 60 K = NBASL + 1, NBAS
            BR(I,J) = BR(I,J)
     +              + SSR(I-NBASL,K-NBASL) * FR(K*(K-1)/2+J)
     +              - SSI(I-NBASL,K-NBASL) * FI(K*(K-1)/2+J)
            BI(I,J) = BI(I,J)
     +              + SSR(I-NBASL,K-NBASL) * FI(K*(K-1)/2+J)
     +              + SSI(I-NBASL,K-NBASL) * FR(K*(K-1)/2+J)
 60       CONTINUE
 50     CONTINUE
 40   CONTINUE
C
      DO 70 I = NBASL + 1, NBAS
        DO 80 J = NBASL + 1, NBAS
          BR(I,J) = 0.0D0
          BI(I,J) = 0.0D0
          DO 90 K = NBASL + 1, NBAS
            IF (K .GE. J) THEN
              BR(I,J) = BR(I,J)
     +                + SSR(I-NBASL,K-NBASL) * FR(K*(K-1)/2+J)
     +                - SSI(I-NBASL,K-NBASL) * FI(K*(K-1)/2+J)
              BI(I,J) = BI(I,J)
     +                + SSR(I-NBASL,K-NBASL) * FI(K*(K-1)/2+J)
     +                + SSI(I-NBASL,K-NBASL) * FR(K*(K-1)/2+J)
            ELSE
              BR(I,J) = BR(I,J)
     +                + SSR(I-NBASL,K-NBASL) * FR(J*(J-1)/2+K)
     +                + SSI(I-NBASL,K-NBASL) * FI(J*(J-1)/2+K)
              BI(I,J) = BI(I,J)
     +                - SSR(I-NBASL,K-NBASL) * FI(J*(J-1)/2+K)
     +                + SSI(I-NBASL,K-NBASL) * FR(J*(J-1)/2+K)
            ENDIF
 90       CONTINUE
 80     CONTINUE
 70   CONTINUE
C
      DO 100 I = 1, NBASL
        DO 110 J = NBASL + 1, NBAS
          BR(I,J) = 0.0D0
          BI(I,J) = 0.0D0
          DO 120 K = 1, NBASL
            BR(I,J) = BR(I,J)
     +              + SLR(I,K) * FR(J*(J-1)/2+K)
     +              + SLI(I,K) * FI(J*(J-1)/2+K)
            BI(I,J) = BI(I,J)
     +              - SLR(I,K) * FI(J*(J-1)/2+K)
     +              + SLI(I,K) * FR(J*(J-1)/2+K)
 120      CONTINUE
 110    CONTINUE
 100  CONTINUE
C
C     ---------
C     F = B * Q
C     ---------
C
      DO 130 I = 1, NBAS
        DO 140 J = 1, NBAS
          FR ((I-1)*NBAS+J) = 0.0D0
          FI ((I-1)*NBAS+J) = 0.0D0
          DO 150 K = 1, NBAS
            FR((I-1)*NBAS+J) = FR((I-1)*NBAS+J)
     +                       + BR(I,K) * QR((K-1)*NBAS+J)
     +                       - BI(I,K) * QI((K-1)*NBAS+J)
            FI((I-1)*NBAS+J) = FI((I-1)*NBAS+J)
     +                       + BR(I,K) * QI((K-1)*NBAS+J)
     +                       + BI(I,K) * QR((K-1)*NBAS+J)
 150      CONTINUE
 140    CONTINUE
 130  CONTINUE
C
C     --------------------
C     F = ALPHA * (F + F#)
C     --------------------
C
      DO 160 I = 1, NBAS
        DO 170 J = 1, I
          FR((I-1)*NBAS+J) = ALPHA *
     +                       (FR((I-1)*NBAS+J) + FR ((J-1)*NBAS+I))
          FI((I-1)*NBAS+J) = ALPHA *
     +                       (FI((I-1)*NBAS+J) - FI ((J-1)*NBAS+I))
 170    CONTINUE
 160  CONTINUE
C
      DO 180 I = 1, NBAS
        DO 190 J = I + 1, NBAS
          FR((I-1)*NBAS+J) = FR((J-1)*NBAS+I)
          FI((I-1)*NBAS+J) = - FI((J-1)*NBAS+I)
 190    CONTINUE
 180  CONTINUE
      RETURN
      END
