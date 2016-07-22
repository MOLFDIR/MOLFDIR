C
C     ==================
      SUBROUTINE DENSMAT
C     ==================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     =========================================================
C     TRANSFORM THE MO'S BACK TO THE NON SYMMETRY ADAPTED BASIS
C     AND GET DENSITYMATRIX IN THE NON-ADAPTED BASIS
C     =========================================================
C
C
      INCLUDE "param.inc"
      INCLUDE "comblock.inc"
C
      DIMENSION VMOR (N22 * 2), VMOI (N22 * 2)
      DIMENSION VAR (N22), VAI (N22), VBR (N22), VBI (N22)
      CHARACTER FORMAT*80,CARD*80
C
      PRINT*
      PRINT*, " Orbitals included in the (partial) density plot"
      PRINT*
      OPEN (MFDVECA, FILE = FILENAM, FORM = 'FORMATTED',ERR=10020)
      REWIND MFDVECA
      READ (MFDVECA, *,END=10020)
      READ (MFDVECA, *,END=10020)
      READ (MFDVECA, *,END=10020)
      READ (MFDVECA, *,END=10020)
      READ(MFDVECA,1000,END=10020) CARD
 1000 FORMAT(A)
      READ(CARD,1001,ERR=10020) FORMAT
 1001 FORMAT(20A)
      IF (FORMAT.EQ.' ') FORMAT='(6F22.16)'
      READ (MFDVECA, *,END=10020)
      READ (MFDVECA, *,END=10020)
      READ (MFDVECA, *,END=10020)
C
      IF (TWOC) THEN
      NBT = NBTOT1 * (NBTOT1 + 1) / 2
      ELSE
      NBT = NBTOT*(NBTOT+1)/2
      ENDIF
C
      DO 111 I = 1, NBT
      DR(I) = 0.0D0
      DI(I) = 0.0D0
 111  CONTINUE
C
      INBB = 0
C
C     -----------------------
      DO 140 IRP = 1 , NSYMRP
      IF (IPRTNR(IRP).NE.1) GOTO 140
C     -----------------------
C
      NB1 = NBSYM1 (IRP)
      LV = NBSYM (IRP)
      IF (TWOC) LV = NBSYM1 (IRP)
      INBA = INBB + 1
      INBB = INBB + NBSYM (IRP)
C
      NV=NVAL(IRP)+NSKIP1(IRP)+NSKIP2(IRP)
      IF (NV.GT.0) THEN
         READ (MFDVECA,1000,END=10020,ERR=10020) CARD
         READ(CARD,1002) IDUMMY,NSPINOR,IDUMMY
      ENDIF
 1002 FORMAT(I4,26X,2I4)
      IF (NSPINOR.LT.NVAL(IRP)+NSKIP1(IRP)) THEN
         STOP 'I find less spinors then you want to analyze !'
      ENDIF
      NSKIP2(IRP)=NSPINOR-NSKIP1(IRP)-NVAL(IRP)
C
C     -------------------------
      DO 135 MO = 1, NSKIP1(IRP)
C     -------------------------
C
        READ(MFDVECA,1000,END=10020,ERR=10020) CARD
        READ (MFDVECA,FORMAT,END=10020,ERR=10020)
     +       (VMOR (I), VMOI (I), I = 1, LV)
 135    CONTINUE
C
C     --------------------------
      DO 130 MO = 1 , NVAL (IRP)
C     --------------------------
C
C     ----------------------------------------------------------------
C     READ IN SYMMETRY ADAPTED SOLUTION VECTOR, INITIALIZE LONG VECTOR
C     ----------------------------------------------------------------
C
      READ(MFDVECA,1211,END=10020,ERR=10020)
     +       NO, EPS, OCCUP, THECOP
 1211 FORMAT(4X,I2,4X,G20.10,5X,G15.10,8X,G20.10)
      IF (OCCUP.EQ.0.D0) THEN
         OCCUP = 1.D0
         PRINT*, ' Eps =',EPS,', Occ 0.0 set TO 1.0'
      ELSE
         PRINT*,' Eps = ',EPS,', Occ = ', OCCUP
      ENDIF
      READ (MFDVECA,FORMAT,END=10020,ERR=10020)
     +     (VMOR (I), VMOI (I), I = 1, LV)
C
      DO 129 JRP = IRP, IRP + IRPDIM(IRP) - 1
      JNBA = INBA + (JRP-IRP) * NBSYM(IRP)
      JNBB = INBB + (JRP-IRP) * NBSYM(IRP)
      NBTOTT = NBTOT
      IF (TWOC) NBTOTT = NBTOT1
      DO 80 I = 1,NBTOTT
      VAR(I) = 0.0D0
      VAI(I) = 0.0D0
      VBR(I) = 0.0D0
   80 VBI(I) = 0.0D0
      JNBBB = JNBB
      IF (TWOC) JNBBB = JNBB - NBSYM2 (JRP)
      DO 100 I = JNBA,JNBBB
      IT = KSYMF (I)
      IM = INDB ( IT )
      INDV = NEM ( IM )
      IND = INDORB ( IT ) + 1
      KND = INDORB ( IT + 1 )
      II = I - JNBA + 1
      DO 90 J = IND, KND
      JJ = J - IND + 1
      VAR(INDV+JJ)=VAR(INDV+JJ)+ CTRANAR(J)*VMOR(II)
     +                         - CTRANAI(J)*VMOI(II)
      VBR(INDV+JJ)=VBR(INDV+JJ)+ CTRANBR(J)*VMOR(II)
     +                         - CTRANBI(J)*VMOI(II)
      VAI(INDV+JJ)=VAI(INDV+JJ)+ CTRANAR(J)*VMOI(II)
     +                         + CTRANAI(J)*VMOR(II)
      VBI(INDV+JJ)=VBI(INDV+JJ)+ CTRANBR(J)*VMOI(II)
     +                         + CTRANBI(J)*VMOR(II)
   90 CONTINUE
  100 CONTINUE
C
C     -------------------------------------------------
C     CALCULATE CONTRIBUTION OF V TO THE DENSITY MATRIX
C     -------------------------------------------------
C
      II = 0
      NBTOTT = NBTOT
      IF (TWOC) NBTOTT = NBTOT1
      DO 120 I = 1 , NBTOTT
      II = II + I - 1
      DO 110 J = 1 , I
      DR (II + J) = DR (II + J) +(VAR (I) * VAR (J)
     +                          + VAI (I) * VAI (J)) * OCCUP
      DR (II + J) = DR (II + J) +(VBR (I) * VBR (J)
     +                          + VBI (I) * VBI (J)) * OCCUP
      DI (II + J) = DI (II + J) +(-VAR (I) * VAI (J)
     +                          + VAI (I) * VAR (J)) * OCCUP
      DI (II + J) = DI (II + J) +(-VBR (I) * VBI (J)
     +                          + VBI (I) * VBR (J)) * OCCUP
  110 CONTINUE
  120 CONTINUE
  129 CONTINUE
  130 CONTINUE
C
C     --------------------------
      DO 139 MO = 1, NSKIP2(IRP)
C     --------------------------
C
        READ(MFDVECA,1000,END=10020,ERR=10020) CARD
        READ (MFDVECA,FORMAT,END=10020,ERR=10020)
     +     (VMOR (I), VMOI (I), I = 1, LV)
  139 CONTINUE
      INBA = INBA + (IRPDIM(IRP)-1) * NBSYM(IRP)
      INBB = INBB + (IRPDIM(IRP)-1) * NBSYM(IRP)
C
  140 CONTINUE
      CLOSE(MFDVECA)
C
      RETURN
10020 CALL Q2JOB(1,'END OR ERROR ON VECTOR FILE',' ',0)
      END
