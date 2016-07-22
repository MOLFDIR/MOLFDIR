C
C     ==========================================
      SUBROUTINE DENSN (DAAR,DAAI,DBAR,DBAI,OSH)
C     ==========================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     GENERATE DENSITY MATRIX IN NON-SYMMETRY ADDAPTED BASIS;
C     IF OSH THEN OPEN SHELL DM ELSE CLOSED SHELL DM.
C
      INCLUDE 'param.inc'
      INCLUDE 'common.inc'
C
      REAL*8 DAAR(N21), DBAR(N21)
      REAL*8 DAAI(N21), DBAI(N21)
C
      LOGICAL OSH
C
      REAL*8 CTRANAR(N10), CTRANAI(N10), CTRANBR(N10), CTRANBI(N10)
      DIMENSION VMOR(N22*2), VMOI(N22*2)
      DIMENSION VAR(N22) , VBR( N22 )
      DIMENSION VAI(N22) , VBI( N22 )
      EQUIVALENCE (KMO,NV),(NOCT,NOCC)
      EQUIVALENCE (TITLE,FORMAT)
      CHARACTER*80 TITLE
C
      DIMENSION SLLR (NNL, NNL)
      DIMENSION SLLI (NNL, NNL)
      DIMENSION WORKR (1, NNL), WORKI (1, NNL)
      IF (NNL .LT. NL) CALL Q2JOB(3, 'DENSN','NNL',NL)
C
      LENREC=8 * MAX0(4*(NL+NS)+4,80)
      OPEN(MFDVECB,FILE=FNAM(2),ACCESS='DIRECT',RECL=LENREC)
      MVBREC=2
C
C     ---------------------------------------------------------------
C     GET THE TRANSFORMATION COEFFICIENTS FOR THE BACK-TRANSFORMATION
C     ---------------------------------------------------------------
C
C
      REWIND MFDSYMC
      ITB = INDORB(NSYMF+1)
      IF (ITB.GT.N10) CALL Q2JOB(3,'P.DENSN','N10',ITB)
      DO 75 IT = 1 ,NSYMF
        ITA = INDORB (IT ) + 1
        ITB = INDORB ( IT + 1 )
        READ (MFDSYMC,END=10030) (CTRANAR(I) , CTRANAI(I),
     +                            CTRANBR(I) , CTRANBI(I), I=ITA,ITB)
   75 CONTINUE
C
      IF (TWOC) THEN
        NBT = NBTOT1 * (NBTOT1 + 1) / 2
      ELSE
        NBT = NBTOT*(NBTOT+1)/2
      ENDIF
C
      DO 142 II = 1, NBT
        DAAR(II) = 0.0D0
        DAAI(II) = 0.0D0
        DBAR(II) = 0.0D0
        DBAI(II) = 0.0D0
 142  CONTINUE
      DBBR1=0.0D0
      DBBI1=0.0D0
      DABR1=0.0D0
      DABI1=0.0D0
      DBBR2=0.0D0
      DBBI2=0.0D0
      DABR2=0.0D0
      DABI2=0.0D0
C
      INBB = 0
C
C     -----------------------
      DO 140 IRP = 1 , NSYMRP
C     -----------------------
C
      NB1 = NBSYM1 (IRP)
C
      IF (TWOC) THEN
        IREC = (IRP - 1) * NREC
        READ (MFDOEMR, REC = IREC + 2)
     +       ((SLLR (I, J), J = 1, I), I = 1, NB1)
        READ (MFDOEMI, REC = IREC + 2)
     +       ((SLLI (I, J), J = 1, I), I = 1, NB1)
        DO 500 I = 1, NB1
          DO 510 J = 1, I
            SLLR (J, I) = SLLR (I, J)
            SLLI (J, I) = - SLLI (I, J)
 510      CONTINUE
 500    CONTINUE
      ENDIF
C
      INBA = INBB + 1
      INBB = INBB + NBSYM ( IRP )
C
      IF (OSH) THEN
        NORP = NOP (IRP)
        MVBREC = MVBREC + NCL (IRP)
        FRAC = OCOPN (IRP)
      ELSE
        FRAC = 1.0D0
        NORP = NCL (IRP)
      ENDIF
C
C     -----------------------------------------------
C     SCALE FRAC FOR AVERAGING OF DAA/DBB AND DAB/DBA
C     -----------------------------------------------
C
      FRAC=FRAC*0.5D0
C
C     --------------------
      DO 130 MO = 1 , NORP
C     --------------------
C
      MVBREC = MVBREC + 1
C
C     ----------------------------------------
C     READ IN SYMMETRY ADAPTED SOLUTION VECTOR
C     ----------------------------------------
C
      READ (MFDVECB, REC = MVBREC) IDUM,IREPS,LV,DUM,
     +     (VMOR(I),VMOI(I),I=1,LV)
C
C     ---------------------------
C     IF TWOC RENORMALIZE VECTORS
C     ---------------------------
C
      IF (TWOC) THEN
        IF (LV .NE. NB1) CALL Q2JOB(6,'LV <> NB1 IN DENS1',' ',1)
C
        CALL TDVT (VMOR, VMOI, N22*2, SLLR, SLLI, NNL, XR, XI,
     +             1, WORKR, WORKI, 1, 1, NB1)
C
# if defined (BIT64)
        IF (ABS (XI) .GT. 1.E-13) 
#else
        IF (DABS (XI) .GT. 1.D-13)
#endif
     &     CALL Q2JOB(6,'TJA, XI <> 0 IN DENS 1',' ',1)
C
#if defined (BIT64)
        XR = 1. / SQRT (XR)
#else
        XR = 1.0D0 / DSQRT (XR)
#endif
C
        DO 540 I = 1, NB1
          VMOR (I) = VMOR (I) * XR
          VMOI (I) = VMOI (I) * XR
 540    CONTINUE
      ENDIF
C
      NBTOTT = NBTOT
      IF (TWOC) NBTOTT = NBTOT1
C
C     ----------------------
C     INITIALIZE LONG VECTOR
C     ----------------------
C
      DO 80 I = 1,NBTOTT
        VAR(I) = 0.0D0
        VAI(I) = 0.0D0
        VBR(I) = 0.0D0
   80   VBI(I) = 0.0D0
C
      INBBB = INBB
      IF (TWOC) INBBB = INBB - NBSYM2 (IRP)
C
C     ------------------------
C     TRANSFORM TO LONG VECTOR
C     ------------------------
C
      DO 100 I = INBA, INBBB
      IT = KSYMF (I)
      IM = INDB ( IT )
      INDV = NEM ( IM )
      IND = INDORB ( IT ) + 1
      KND = INDORB ( IT + 1 )
      II = I - INBA + 1
C
      DO 90 J = IND, KND
      JJ = J - IND + 1
        VAR( INDV + JJ ) = VAR( INDV + JJ ) + CTRANAR( J ) * VMOR ( II )
     +                                      - CTRANAI( J ) * VMOI ( II )
        VBR( INDV + JJ ) = VBR( INDV + JJ ) + CTRANBR( J ) * VMOR ( II )
     +                                      - CTRANBI( J ) * VMOI ( II )
        VAI( INDV + JJ ) = VAI( INDV + JJ ) + CTRANAR( J ) * VMOI ( II )
     +                                      + CTRANAI( J ) * VMOR ( II )
        VBI( INDV + JJ ) = VBI( INDV + JJ ) + CTRANBR( J ) * VMOI ( II )
     +                                      + CTRANBI( J ) * VMOR ( II )
   90   CONTINUE
  100 CONTINUE
C
C     -------------------------------------------------
C     CALCULATE CONTRIBUTION OF V TO THE DENSITY MATRIX
C     -------------------------------------------------
C
      II = 0
      NBTOTT = NBTOT
      IF (TWOC) NBTOTT = NBTOT1
C
      DO 120 I = 1 , NBTOTT
        II = II + I - 1
        DO 110 J = 1 , I
          DAAR (II + J) = DAAR (II + J) + VAR (I) * VAR (J) * FRAC
     +                                  + VAI (I) * VAI (J) * FRAC
     +                                  + VBR (I) * VBR (J) * FRAC
     +                                  + VBI (I) * VBI (J) * FRAC
          DBAR (II + J) = DBAR (II + J) + VBR (I) * VAR (J) * FRAC
     +                                  + VBI (I) * VAI (J) * FRAC
     +                                  - VAR (I) * VBR (J) * FRAC
     +                                  - VAI (I) * VBI (J) * FRAC
          DAAI (II + J) = DAAI (II + J) - VAR (I) * VAI (J) * FRAC
     +                                  + VAI (I) * VAR (J) * FRAC
     +                                  + VBR (I) * VBI (J) * FRAC
     +                                  - VBI (I) * VBR (J) * FRAC
          DBAI (II + J) = DBAI (II + J) - VBR (I) * VAI (J) * FRAC
     +                                  + VBI (I) * VAR (J) * FRAC
     +                                  - VAR (I) * VBI (J) * FRAC
     +                                  + VAI (I) * VBR (J) * FRAC
          DBBR1=DBBR1+VAR (I) * VAR (J)+ VAI (I) * VAI (J)
          DBBR2=DBBR2+VBR (I) * VBR (J)+ VBI (I) * VBI (J)
          DABR1=DABR1+VBR (I) * VAR (J)+ VBI (I) * VAI (J)
          DABR2=DABR2-VAR (I) * VBR (J)- VAI (I) * VBI (J)
          DBBI1=DBBI1-VAR (I) * VAI (J)+ VAI (I) * VAR (J)
          DBBI2=DBBI2+VBR (I) * VBI (J)- VBI (I) * VBR (J)
          DABI1=DABI1-VBR (I) * VAI (J)+ VBI (I) * VAR (J)
          DABI2=DABI2-VAR (I) * VBI (J)+ VAI (I) * VBR (J)
  110     CONTINUE
  120   CONTINUE
  130 CONTINUE
C
      IF (.NOT. OSH) MVBREC = MVBREC + NOP (IRP)
  140 CONTINUE
C
      CLOSE (MFDVECB)
#if defined (BIT64)
      DBBR= ABS(DBBR1-DBBR2)
      DBBI= ABS(DBBI1-DBBI2)
      DABR= ABS(DABR1-DABR2)
      DABI= ABS(DABI1-DABI2)
#else
      DBBR=DABS(DBBR1-DBBR2)
      DBBI=DABS(DBBI1-DBBI2)
      DABR=DABS(DABR1-DABR2)
      DABI=DABS(DABI1-DABI2)
#endif
      IF (DBBR.GT.1.0D-10) PRINT*,'WARNING DBBR>1D-10, ',DBBR
      IF (DBBI.GT.1.0D-10) PRINT*,'WARNING DBBI>1D-10, ',DBBI
      IF (DABR.GT.1.0D-10) PRINT*,'WARNING DABR>1D-10, ',DABR
      IF (DABI.GT.1.0D-10) PRINT*,'WARNING DABI>1D-10, ',DABI
      RETURN
10030 CALL Q2JOB(1,'NEXT VECTOR (FROM MFDSYMC)',' ',0)
      END
