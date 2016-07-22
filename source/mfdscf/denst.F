C
C     =========================================================
      SUBROUTINE DENST (DAAR, DBAR, DAAI, DBAI, DENSR, DENSI,IOSH)
C     =========================================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'paracom.inc'
      INCLUDE 'general.inc'
C
      REAL*8 DAAR(*),DBAR(*),DAAI(*),DBAI(*)
      DIMENSION DENSR(*),DENSI(*)
C
      REAL*8 CTRANAR(N10),CTRANAI(N10),CTRANBR(N10),CTRANBI(N10)
C
C === WE START WITH READING IN THE TRANSFORMATION MATRICES
C
      REWIND MFDSYMC
      ITB = INDORB(NSYMF+1)
      IF (ITB.GT.N10) CALL Q2JOB (3,'P.DENST','N10',ITB)
      DO 75 IT = 1 ,NSYMF
        ITA = INDORB (IT ) + 1
        ITB = INDORB ( IT + 1 )
        READ (MFDSYMC,END=10030) (CTRANAR(I) , CTRANAI(I),
     +                            CTRANBR(I) , CTRANBI(I), I=ITA,ITB)
   75 CONTINUE
C
C === THE DENSITY MATRIX IN THE SYMMETRY ADAPTED BASIS IS STORED IN THE
C === ARRAY DENSR AND DENSI AS THE CONSECUTIVE LOWER TRIANGLE'S OF THE
C === SYMMETRY BLOCKS OF THE MATRIX.
C === WE WILL TRANSFORM THE DENSITY MATRIX PER SYMMETRY BLOCK
C
      FRAC = 0.5D0
C
      IF (TWOC) THEN
        NBTT = NBTOT1 * (NBTOT1 + 1) / 2
      ELSE
        NBTT = NBTOT*(NBTOT+1)/2
      ENDIF
C
      DO 100 II = 1, NBTT
        DAAR(II) = 0.0D0
        DAAI(II) = 0.0D0
        DBAR(II) = 0.0D0
        DBAI(II) = 0.0D0
 100  CONTINUE
C
      ICTD = 0
      IMAX = 0
      DO 200 IRP = 1, NSYMRP
      NB = NBSIM(IRP)
      IMIN = IMAX + 1
      IMAX = IMAX + NB
      NBT = NB * (NB + 1) / 2
      IF (IOSH .GT. 1) THEN
      IF (NOP(IRP,IOSH-1) .EQ. 0) GOTO 210
      ELSE IF (IOSH .EQ. 1) THEN
      IF (NCL(IRP) .EQ. 0) GOTO 210
      ENDIF
C === LOOP OVER THE SYMMETRY FUNCTIONS
      DO 300 I = IMIN, IMAX
C === COMPUTE THE ADDRESSES OF THE COEFFICIENTS IN CTRAN
C === AND THE ADDRESS IN DAAR WHERE THE RESULT SHOULD GO
      IT = KSYMF(I)
      INDF = INDORB(IT) + 1
      INDL = INDORB(IT + 1)
      IM = INDB(IT)
      INDV = NEM(IM)
C === COMPUTE THE BOUNDARIES FOR THE TWO INNERMOST LOOPS
      LMIN = INDV + 1
      LMAX = LMIN + INDL - INDF
      DO 310 J = IMIN, IMAX
C === COMPUTE THE ADDRESSES OF THE COEFFICIENTS IN CTRAN
C === AND THE ADDRESS IN DAAR WHERE THE RESULT SHOULD GO
      JT = KSYMF(J)
      JNDF = INDORB(JT) + 1
      JNDL = INDORB(JT + 1)
      JM = INDB(JT)
      JNDV = NEM(JM)
C === COMPUTE THE BOUNDARIES FOR THE TWO INNERMOST LOOPS
      KMIN = JNDV + 1
      KMAXE = KMIN + JNDL - JNDF
C === LOOP OVER THE CONTRACTED GAUSSIANS
      IF (J .LE. I) THEN
      IR = I - IMIN + 1
      IJ = J - IMIN + 1 + (IR * (IR - 1) / 2) + ICTD
      DO 320 L = LMIN, LMAX
      LI = INDF - LMIN + L
      KMAX = MIN(L,KMAXE)
      LK = (L * (L - 1) / 2) + KMIN - 1
      DO 330 K = KMIN, KMAX
      LK = LK + 1
      KJ = JNDF - KMIN + K
      DAAR(LK) = DAAR(LK) + CTRANAR(LI) * CTRANAR(KJ) * DENSR(IJ) * FRAC
     +                    + CTRANAR(LI) * CTRANAI(KJ) * DENSI(IJ) * FRAC
     +                    - CTRANAI(LI) * CTRANAR(KJ) * DENSI(IJ) * FRAC
     +                    + CTRANAI(LI) * CTRANAI(KJ) * DENSR(IJ) * FRAC
     &                    + CTRANBR(LI) * CTRANBR(KJ) * DENSR(IJ) * FRAC
     +                    + CTRANBR(LI) * CTRANBI(KJ) * DENSI(IJ) * FRAC
     +                    - CTRANBI(LI) * CTRANBR(KJ) * DENSI(IJ) * FRAC
     +                    + CTRANBI(LI) * CTRANBI(KJ) * DENSR(IJ) * FRAC
      DAAI(LK) = DAAI(LK) + CTRANAI(LI) * CTRANAR(KJ) * DENSR(IJ) * FRAC
     +                    + CTRANAI(LI) * CTRANAI(KJ) * DENSI(IJ) * FRAC
     +                    + CTRANAR(LI) * CTRANAR(KJ) * DENSI(IJ) * FRAC
     +                    - CTRANAR(LI) * CTRANAI(KJ) * DENSR(IJ) * FRAC
     &                    - CTRANBI(LI) * CTRANBR(KJ) * DENSR(IJ) * FRAC
     +                    - CTRANBI(LI) * CTRANBI(KJ) * DENSI(IJ) * FRAC
     +                    - CTRANBR(LI) * CTRANBR(KJ) * DENSI(IJ) * FRAC
     +                    + CTRANBR(LI) * CTRANBI(KJ) * DENSR(IJ) * FRAC
      DBAR(LK) = DBAR(LK) + CTRANBR(LI) * CTRANAR(KJ) * DENSR(IJ) * FRAC
     +                    + CTRANBR(LI) * CTRANAI(KJ) * DENSI(IJ) * FRAC
     +                    - CTRANBI(LI) * CTRANAR(KJ) * DENSI(IJ) * FRAC
     +                    + CTRANBI(LI) * CTRANAI(KJ) * DENSR(IJ) * FRAC
     &                    - CTRANAR(LI) * CTRANBR(KJ) * DENSR(IJ) * FRAC
     +                    - CTRANAR(LI) * CTRANBI(KJ) * DENSI(IJ) * FRAC
     +                    + CTRANAI(LI) * CTRANBR(KJ) * DENSI(IJ) * FRAC
     +                    - CTRANAI(LI) * CTRANBI(KJ) * DENSR(IJ) * FRAC
      DBAI(LK) = DBAI(LK) + CTRANBI(LI) * CTRANAR(KJ) * DENSR(IJ) * FRAC
     +                    + CTRANBI(LI) * CTRANAI(KJ) * DENSI(IJ) * FRAC
     +                    + CTRANBR(LI) * CTRANAR(KJ) * DENSI(IJ) * FRAC
     +                    - CTRANBR(LI) * CTRANAI(KJ) * DENSR(IJ) * FRAC
     &                    + CTRANAI(LI) * CTRANBR(KJ) * DENSR(IJ) * FRAC
     +                    + CTRANAI(LI) * CTRANBI(KJ) * DENSI(IJ) * FRAC
     +                    + CTRANAR(LI) * CTRANBR(KJ) * DENSI(IJ) * FRAC
     +                    - CTRANAR(LI) * CTRANBI(KJ) * DENSR(IJ) * FRAC
  330 CONTINUE
  320 CONTINUE
      ELSE
      JR = J - IMIN + 1
      IJ = I - IMIN + 1 + (JR * (JR - 1) / 2) + ICTD
      DO 340 L = LMIN, LMAX
      LI = INDF - LMIN + L
      KMAX = MIN(L,KMAXE)
      LK = (L * (L - 1) / 2) + KMIN - 1
      DO 350 K = KMIN, KMAX
      LK = LK + 1
      KJ = JNDF - KMIN + K
      DAAR(LK) = DAAR(LK) + CTRANAR(LI) * CTRANAR(KJ) * DENSR(IJ) * FRAC
     +                    - CTRANAR(LI) * CTRANAI(KJ) * DENSI(IJ) * FRAC
     +                    + CTRANAI(LI) * CTRANAR(KJ) * DENSI(IJ) * FRAC
     +                    + CTRANAI(LI) * CTRANAI(KJ) * DENSR(IJ) * FRAC
     &                    + CTRANBR(LI) * CTRANBR(KJ) * DENSR(IJ) * FRAC
     +                    - CTRANBR(LI) * CTRANBI(KJ) * DENSI(IJ) * FRAC
     +                    + CTRANBI(LI) * CTRANBR(KJ) * DENSI(IJ) * FRAC
     +                    + CTRANBI(LI) * CTRANBI(KJ) * DENSR(IJ) * FRAC
      DAAI(LK) = DAAI(LK) + CTRANAI(LI) * CTRANAR(KJ) * DENSR(IJ) * FRAC
     +                    - CTRANAI(LI) * CTRANAI(KJ) * DENSI(IJ) * FRAC
     +                    - CTRANAR(LI) * CTRANAR(KJ) * DENSI(IJ) * FRAC
     +                    - CTRANAR(LI) * CTRANAI(KJ) * DENSR(IJ) * FRAC
     &                    - CTRANBI(LI) * CTRANBR(KJ) * DENSR(IJ) * FRAC
     +                    + CTRANBI(LI) * CTRANBI(KJ) * DENSI(IJ) * FRAC
     +                    + CTRANBR(LI) * CTRANBR(KJ) * DENSI(IJ) * FRAC
     +                    + CTRANBR(LI) * CTRANBI(KJ) * DENSR(IJ) * FRAC
      DBAR(LK) = DBAR(LK) + CTRANBR(LI) * CTRANAR(KJ) * DENSR(IJ) * FRAC
     +                    - CTRANBR(LI) * CTRANAI(KJ) * DENSI(IJ) * FRAC
     +                    + CTRANBI(LI) * CTRANAR(KJ) * DENSI(IJ) * FRAC
     +                    + CTRANBI(LI) * CTRANAI(KJ) * DENSR(IJ) * FRAC
     &                    - CTRANAR(LI) * CTRANBR(KJ) * DENSR(IJ) * FRAC
     +                    + CTRANAR(LI) * CTRANBI(KJ) * DENSI(IJ) * FRAC
     +                    - CTRANAI(LI) * CTRANBR(KJ) * DENSI(IJ) * FRAC
     +                    - CTRANAI(LI) * CTRANBI(KJ) * DENSR(IJ) * FRAC
      DBAI(LK) = DBAI(LK) + CTRANBI(LI) * CTRANAR(KJ) * DENSR(IJ) * FRAC
     +                    - CTRANBI(LI) * CTRANAI(KJ) * DENSI(IJ) * FRAC
     +                    - CTRANBR(LI) * CTRANAR(KJ) * DENSI(IJ) * FRAC
     +                    - CTRANBR(LI) * CTRANAI(KJ) * DENSR(IJ) * FRAC
     &                    + CTRANAI(LI) * CTRANBR(KJ) * DENSR(IJ) * FRAC
     +                    - CTRANAI(LI) * CTRANBI(KJ) * DENSI(IJ) * FRAC
     +                    - CTRANAR(LI) * CTRANBR(KJ) * DENSI(IJ) * FRAC
     +                    - CTRANAR(LI) * CTRANBI(KJ) * DENSR(IJ) * FRAC
  350 CONTINUE
  340 CONTINUE
      ENDIF
  310 CONTINUE
  300 CONTINUE
      ICTD = ICTD + NBT
C === IF TWOC WE MUST SKIP THE FUNCTIONS BELONGING TO THE SMALL COMPONENT
  210 CONTINUE
      IF (TWOC) IMAX = IMAX + NBSYM2(IRP)
  200 CONTINUE
C
      RETURN
C
10030 CALL Q2JOB(6,'DENST: Error reading file MFDSYMC',' ',1)
C
      END
