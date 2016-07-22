C
C     ==================
      SUBROUTINE TWOSTRT(VECR,VECI,SSSR,SSSI,SUMR,SUMI)
C     ==================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     GENERATE FOUR COMPONENT START VECTORS FROM TWO COMPONENT VECTORS
C
      INCLUDE 'paracom.inc'
      INCLUDE 'general.inc'
      INCLUDE 'memory.inc'
C
      CHARACTER*80 TITLE
      EQUIVALENCE (TITLE,FORMAT)
C
      REAL*8 VECR (*), VECI (*)
      REAL*8 SSSR (*), SSSI (*)
      REAL*8 SUMR (*), SUMI (*)
      REAL*8 D (N22*2), E (N22*2), TEMPE(N22*2)
      REAL*8 VMOR (NNT), VMOI (NNT), VR (NNT), VI (NNT)
C
      MVBREC = 2
C
      PRINT*
      PRINT*, ' GENERATE FOUR-COMPONENT START FROM TWOC-VECTORS'
      PRINT*
C
      DO 10 IRP = 1, NSYMRP
        MVBOLD = MVBREC
        IREC = (IRP - 1) * NREC
        NBASL = NBSIM1 (IRP)
        NBASS = NBSIM2 (IRP)
        NBAS = NBASL + NBASS
        IF (NBAS*NBAS.GT.LENGTH) STOP 'ARRAYS TO SMALL IN TWOSTRT'  
C
C       =========================
C       GET V(NUC, SS) AND S (SS)
C       =========================
C
        DO 20 MO = 1, NOC (IRP)
          MVBREC = MVBREC + 1
          READ (MFDVECB,REC=MVBREC) IDUM,JDUM,LV,DUM,
     +         (VMOR (L), VMOI (L), L = 1, LV)
C
C         =====================================
C         CALCULATE (EPS(MO) + 2MC2 - VNUC(SS))
C         =====================================
C
          READ (MFDOEMR, REC = IREC + 3)
     +       ((SSSR (I+(J-1)*NBASS), J = 1, I), I = 1, NBASS)
          READ (MFDOEMI, REC = IREC + 3)
     +       ((SSSI (I+(J-1)*NBASS), J = 1, I), I = 1, NBASS)
          READ (MFDOEMR, REC = IREC + 5)
     +       ((SUMR (I+(J-1)*NBASS), J = 1, I), I = 1, NBASS)
          READ (MFDOEMI, REC = IREC + 5)
     +       ((SUMI (I+(J-1)*NBASS), J = 1, I), I = 1, NBASS)
          DO 30 I = 1, NBASS
            DO 40 J = 1, I
              SUMR (I+(J-1)*NBASS) = SSSR(I+(J-1)*NBASS)*
     +                      (SFRAC * ALLEW (IRP, MO) + TWOCSQ) -
     +                       SFRAC * SUMR (I+(J-1)*NBASS)
              SUMI (I+(J-1)*NBASS) = SSSI (I+(J-1)*NBASS) *
     +                      (SFRAC * ALLEW (IRP, MO) + TWOCSQ) -
     +                       SFRAC * SUMI (I+(J-1)*NBASS)
              SUMR (J+(I-1)*NBASS) =   SUMR (I+(J-1)*NBASS)
              SUMI (J+(I-1)*NBASS) = - SUMI (I+(J-1)*NBASS)
 40           CONTINUE
 30         CONTINUE
C
C         ==================================
C         CALCULATE THE INVERSE OF THE THING
C         ==================================
C
          CALL CRDIAG (NBASS,NBASS,SUMR,SUMI,D,SSSR,SSSI,TEMPE,TEMPE,E,
     +                 IFAIL)
          DO 50 I = 1, NBASS
            IF(D(I).LT.1.0E-13) STOP'1-EIGENVALUE TOO SMALL'
            D (I) = 1.0D0 / D (I)
 50         CONTINUE
          CALL OBKTR (NBASS, NBASS, SUMR, SUMI, D, NBASS, SSSR, SSSI)
C
C         =============================
C         CALCULATE C[@.P] * VEC(LARGE)
C         =============================
C
          READ (MFDOEMR, REC = IREC + 6)
     +         (( SUMR (I+(J-1)*NBASS), I = 1, NBASS), J = 1, NBASL)
          READ (MFDOEMI, REC = IREC + 6)
     +         (( SUMI (I+(J-1)*NBASS), I = 1, NBASS), J = 1, NBASL)
          CALL PMM MM (NBASS, NBASS, SUMR, SUMI,
     +                 NBASL, NNT, VMOR, VMOI,
     +                 1, NNT, VR, VI, 1)
C
C
C         ============================================
C         CALCULATE THAT INVERSE * C[@.P] * VEC(LARGE)
C         ============================================
C
          CALL OHMV (NBASS, NBASS, SSSR, SSSI, VR, VI,
     +         VMOR (NBASL + 1), VMOI (NBASL + 1))
          WRITE (MFDVECB,REC=MVBREC) IDUM,JDUM,NBAS,DUM,
     +         (VMOR (L), VMOI (L), L = 1, NBAS)
 20       CONTINUE
C
C       ==================================
C       ORTHO-NORMALIZE THE SET OF VECTORS
C       ==================================
C
      READ (MFDOEMR, REC = IREC + 3)
     +   ((SSSR (I+(J-1)*NNS), J = 1, I), I = 1, NBASS)
      READ (MFDOEMI, REC = IREC + 3)
     +   ((SSSI (I+(J-1)*NNS), J = 1, I), I = 1, NBASS)
      DO 31 I = 1, NBASS
         DO 41 J = 1, I
            SSSR (J+(I-1)*NNS) = SSSR (I+(J-1)*NNS)
            SSSI (J+(I-1)*NNS) = - SSSI (I+(J-1)*NNS)
  41     CONTINUE
  31  CONTINUE
      MVBREC = MVBOLD
      DO 100 MO = 1, NOC (IRP)
        MVBREC = MVBREC + 1
        READ (MFDVECB,REC=MVBREC) IDUM,JDUM,LV,DUM,
     +       (VECR (L+(MO-1)*NBAS), VECI (L+(MO-1)*NBAS), L = 1, LV)
 100    CONTINUE
      DO 60 MO = 1, NOC (IRP)
        DO 70 MO1 = 1, MO - 1
C
          CALL OHMV (NBASS, NBASS, SSSR, SSSI,
     +         VECR (NBASL+1+(MO-1)*NBAS), VECI (NBASL+1+(MO-1)*NBAS),
     +         VR, VI)
C
          RESR = XDOT (VECR(NBASL+1+(MO1-1)*NBAS), VR, NBASS) +
     +           XDOT (VECI(NBASL+1+(MO1-1)*NBAS), VI, NBASS)
          RESI = XDOT (VECR(NBASL+1+(MO1-1)*NBAS), VI, NBASS) -
     +           XDOT (VECI(NBASL+1+(MO1-1)*NBAS), VR, NBASS)
C
            DO 80 I = 1, NBAS
              VECR(I+(MO-1)*NBAS) = VECR(I+(MO-1)*NBAS) - 
     +                              RESR * VECR(I+(MO1-1)*NBAS)
     +                            + RESI * VECI(I+(MO1-1)*NBAS)
              VECI(I+(MO-1)*NBAS) = VECI(I+(MO-1)*NBAS) - 
     +                              RESR * VECI(I+(MO1-1)*NBAS)
     +                            - RESI * VECR(I+(MO1-1)*NBAS)
 80           CONTINUE
 70         CONTINUE
C
          CALL OHMV (NBASS, NBASS, SSSR, SSSI,
     +         VECR (NBASL+1+(MO-1)*NBAS), VECI (NBASL+1+(MO-1)*NBAS),
     +         VR, VI)
C
          RESR = XDOT (VECR(NBASL+1+(MO-1)*NBAS), VR, NBASS) +
     +           XDOT (VECI(NBASL+1+(MO-1)*NBAS), VI, NBASS) + 1.0D0
          RESI = XDOT (VECR(NBASL+1+(MO-1)*NBAS), VI, NBASS) -
     +           XDOT (VECI(NBASL+1+(MO-1)*NBAS), VR, NBASS)
C
#if defined (BIT64)
          RNORM = 1.0 / SQRT (RESR)
#else
          RNORM = 1.0D0 / DSQRT (RESR)
#endif
          DO 120 I = 1, NBAS
            VECR (I+(MO-1)*NBAS) = VECR (I+(MO-1)*NBAS) * RNORM
            VECI (I+(MO-1)*NBAS) = VECI (I+(MO-1)*NBAS) * RNORM
 120        CONTINUE
 60       CONTINUE
        MVBREC = MVBOLD
        DO 110 MO = 1, NOC (IRP)
          MVBREC = MVBREC + 1
          WRITE (MFDVECB,REC=MVBREC) IDUM,JDUM,LV,DUM,
     +          (VECR (L+(MO-1)*NBAS), VECI (L+(MO-1)*NBAS), L = 1, LV)
 110      CONTINUE
 10   CONTINUE
C
      RETURN
      END
