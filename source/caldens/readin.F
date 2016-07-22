C
C     =================
      SUBROUTINE READIN
C     =================
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INCLUDE "param.inc"
      INCLUDE "comblock.inc"
C
      REAL*8 RNUC(N1)
C
      INTEGER NCL(16),NOP(16),NOP2(16),NOPEN,TOTAL
C
      NAMELIST/OCCUP/TOTAL,NOPEN
      NAMELIST/EDITDENS/NMO
      NAMELIST/GRID/NDIM,X0,Y0,Z0
      NAMELIST/GENERAL/TWOC
      NAMELIST/NATORB/USENAT,FILENAM
      NAMELIST/CALDENS/FORMOUT,LOGSCL
C
      WRITE (6, *) ' ======================================='
      WRITE (6, *) ' =   CALDENS V 3.2  (01 - 04 - 1998)   ='
      WRITE (6, *) ' ======================================='
C
C     VERSION 1.1: PRINT GRID ON OUTPUT
C                  READ OCCUPATON FROM MFDVECA
C                  HANDLE FINITE NUCLEUS DATA ON MFD1OUT
C                  PARAMETERS INCREASED
C                  NOTE: PARAMETERS ARE <<NOT>> CHECKED !!!
C                                                           OV, 8/2/91
C
C     VERSION 1.2: BUG FIXES IN READING MFDVECA AND DEFINITION OF GRID
C                                                           OV, 18/2/91
C     VERSION 1.3: CHECKS N15, DIMENSION INCREASED
C                                                           LV, 2/3/1992
C     VERSION 1.4: OUTPUT FORMAT ADAPTED
C                                                           LV, 5/3/1992
C     VERSION 1.5: PRODUCES AVS DATA FILE
C                  READS FROM MFDVECA
C                                                           LV, 15/12/1992
C     Version 1.6: Bug introduced in 1.5 repared            LV, 3/3/1993
C
C     Version 1.7: SX2 CALLS REMOVED. PROGRAM ONLY WORKS FOR HP, CONVEX,
C                  CYBER AND CRAY                          BDJ, 15/7/1993
C
C     Version 1.8: Can also generate basisset from natural orbitals
C                                                          BDJ, 3/10/1993
C
C     Version 1.9: Removed CNVE statements : Runs on IBM RS6000
C                                                          LV, 4/11/1994
C
C     Version 2.0: Split in sub routines                  BDJ, 4/26/1995
C
C     Version 3.0: Call AVS routine directly              LV, 11/24/1995
C
C     Version 3.1: AVS calls are taken out, all densities are printed
C                  and special AVS files will be generated
C   
C     Version 3.2: Changed input in order to compile under Fortran 90
C                                                          BDJ, 4/1/1998
C
C     ------------------------------------
C     READ INFO FROM MOLFDIR (SYM + BASIS)
C     ------------------------------------
C
      OPEN(MFD1OUT, STATUS = 'OLD', FORM = 'UNFORMATTED',
     +     FILE = 'MFD1OUT', ERR=20000)
      REWIND MFD1OUT
      READ (MFD1OUT)
      READ (MFD1OUT)
      READ (MFD1OUT) NQ1,NEQUIV,IDUM,NEQCOM,NGAUS,NCOMB,NSYMOP,NNUCL,
     +               NSYMF,
     +               NCOEF,NSYMRP
      IF (NGAUS.GT.N15) CALL Q2JOB (5,'READIN','N15',NGAUS)
      READ (MFD1OUT) (ZNUC(I),(COAT(I,J),J=1,3),RNUC(I),I=1,NNUCL)
      READ (MFD1OUT)
      READ (MFD1OUT) (KCOMB(I),INDBAS(I),INDSYM(I),I=1,NEQUIV)
      READ (MFD1OUT) (ALPHA(I),COEFF(I),I=1,NGAUS)
      READ (MFD1OUT) (ICENT(I),ITYPE(I),(IDUM,J=1,NSYMOP),I=1,NCOMB),
     +               (INDCOM(I),I=1, NEQCOM)
      READ (MFD1OUT) (INDORB(I),INDB(I),KSYMRP(I),LSYMRP(I),KSYMF(I),
     +               I=1,NSYMF)
      READ (MFD1OUT) (ITRAN,CTRANAR(I),CTRANAI(I),
     +                CTRANBR(I),CTRANBI(I),I=1,NCOEF)
      READ (MFD1OUT) (NBSYM(I),NBSYM1(I),NBSYM2(I),I=1,NSYMRP)
      CLOSE (MFD1OUT)
C
      INDCOM(NEQCOM+1)=NCOMB
      INDBAS(NEQUIV+1)=NGAUS
      INDORB(NSYMF+1)=NCOEF
      INDSYM(NEQUIV+1)=NSYMF
C
C     -------------------------
C     READ SYMMETRY INFORMATION
C     -------------------------
      OPEN(MFDSYMC, STATUS = 'OLD', FORM = 'UNFORMATTED',
     +     FILE = 'MFDSYMC', ERR=20001)
      REWIND MFDSYMC
      DO 5 I=1,NSYMF+5
        READ(MFDSYMC)
   5    CONTINUE
C
      READ(MFDSYMC) (IRPDIM(I),I=1,NSYMRP),(ITHIRP(I),I=1,NSYMRP),
     +                                   (IPRTNR(I),I=1,NSYMRP)
      CLOSE (MFDSYMC)
C
C
C     ---------------
C     READ USER INPUT
C     ---------------
C
      OPEN (5, FILE='MOLFDIR.INP', FORM='FORMATTED')
      WRITE (6, *)
      WRITE (6, *) ' ====== READ USER INPUT ======'
      NA (1) = 1
      NA (2) = 1
      NA (3) = 1
      REWIND(5)
      TOTAL=0
      NOPEN=0
      DO I = 1, NSYMRP
         NCL(I) = 0
         NOP(I) = 0
         NOP2(I) = 0
      ENDDO
      CALL LOCATE('OCCUP',5,ITEST)
      IF (ITEST.GT.0) THEN
         READ(5,OCCUP,ERR=20010)
      ELSE
         GOTO 20010
      ENDIF
      READ(5,1000) (NCL(IRP),IRP=1,NSYMRP)
      IF (NOPEN.GE.1) READ(5,1000) (NOP(IRP),IRP=1,NSYMRP)
      IF (NOPEN.GE.2) READ(5,1000) (NOP2(IRP),IRP=1,NSYMRP)
      DO 120 IRP=1,NSYMRP
        NSKIP1(IRP)=0
        NVAL(IRP)=NCL(IRP)+NOP(IRP)+NOP2(IRP)
 120    CONTINUE
      NMO=0
      CALL LOCATE('EDITDENS',8,ITEST)
      IF (ITEST.GT.0) THEN
         READ(5,EDITDENS,ERR=110)
      ELSE
         GOTO 110
      ENDIF
      READ(5,1000) (NSKIP1(IRP),IRP=1,NSYMRP)
      READ(5,1000) (NVAL(IRP),IRP=1,NSYMRP)
 110  DO 100 IRP=1,NSYMRP
        NOCC(IRP)=NCL(IRP)+NOP(IRP)+NOP2(IRP)
        NSKIP2(IRP)=NBSYM1(IRP)-NSKIP1(IRP)-NVAL(IRP)
 100    CONTINUE
C
      LOGSCL = .FALSE.
      FORMOUT = .FALSE.
      CALL LOCATE('CALDENS',7,ITEST)
      IF (ITEST.GT.0) READ (5,CALDENS)
      WRITE (6,*) 'Formatted output  ',FORMOUT
      WRITE (6,*) 'Logarithmic scale ',LOGSCL
      NDIM=3
      X0=0.0D0
      Y0=0.0D0
      Z0=0.0D0
      NDIM = 3
      X0 = 0.0
      Y0 = 0.0
      Z0 = 0.0
      CALL LOCATE('GRID',4,ITEST)
      IF (ITEST.GT.0) THEN
         READ (5,GRID,ERR=150)
      ELSE
         GOTO 150
      ENDIF
      DO 10 IDIM = 1, NDIM
      READ (5,*,END=20020) RX (IDIM), RY (IDIM), RZ (IDIM), AF (IDIM),
     +            AL (IDIM), NA (IDIM)
#if defined (BIT64)
      RNORM = SQRT (RX (IDIM) ** 2 + RY (IDIM) ** 2 + RZ (IDIM) ** 2)
#else
      RNORM = DSQRT (RX (IDIM) ** 2 + RY (IDIM) ** 2 + RZ (IDIM) ** 2)
#endif
      RX (IDIM) = RX (IDIM) / RNORM
      RY (IDIM) = RY (IDIM) / RNORM
      RZ (IDIM) = RZ (IDIM) / RNORM
 10   CONTINUE
      GOTO 160
C--> Default grid is 50x50x50, origin (0,0,0), extending 2 bohr 
C    beyond nuclear framework
  150 CONTINUE
      RX(1) = 1.0
      RX(2) = 0.0
      RX(3) = 0.0
      RY(1) = 0.0
      RY(2) = 1.0
      RY(3) = 0.0
      RZ(1) = 0.0
      RZ(2) = 0.0
      RZ(3) = 1.0
      NA(1) = 50
      NA(2) = 50
      NA(3) = 50
      DO J = 1, 3
      AF(J) = 1000.
      AL(J) = -1000.
      DO I = 1, NNUCL
      IF (COAT(I,J).LT.AF(J)) AF(J) = COAT(I,J)
      IF (COAT(I,J).GT.AL(J)) AL(J) = COAT(I,J)
      ENDDO
      AF(J) = AF(J) - 2.0
      AL(J) = AL(J) + 2.0
      ENDDO
 160  WRITE (6, 1020) NDIM, X0, Y0, Z0
      WRITE (6, 1030)
      DO IDIM = 1, NDIM
      WRITE (6, 1031) RX (IDIM), RY (IDIM), RZ (IDIM), AF (IDIM),
     +             AL (IDIM), NA (IDIM)
      ENDDO
      TWOC=.FALSE.
      CALL LOCATE('GENERAL',7,ITEST)
      IF (ITEST.GT.0) READ (5,GENERAL)
      WRITE (6, 1010) TWOC
      USENAT=.FALSE.
      CALL LOCATE('NATORB',6,ITEST)
      IF (ITEST.GT.0) READ (5,NATORB)
      IF (.NOT.USENAT) FILENAM='MFDVECA   '
 140  IF (USENAT) WRITE (6,*) ' USING NATURAL ORBITALS '
      WRITE (6, *)
      WRITE (6, *) ' ====== END OF USER INPUT ======'
      WRITE (6, *)
C
C     -------------------------------
C     CALCULATE NEM, NBTOT AND NBTOT1
C     -------------------------------
C
      NBTOT = 0
      DO 20 IM = 1, NEQUIV
      IT = KCOMB (IM)
      IND = INDCOM (IT)
      KND = INDCOM (IT + 1)
      NBAS = KND - IND
      NEM (IM) = NBTOT
      NBTOT = NBTOT + NBAS
      IF (IM .EQ. NQ1) NBTOT1 = NBTOT
 20   CONTINUE
      NEM (NEQUIV + 1) = NBTOT
C
 1000 FORMAT (20I4)
 1010 FORMAT (3X, L1)
 1020 FORMAT (/" Dimension of grid ",I4/" Origin of grid ",3F10.4)
 1030 FORMAT (/" Grid vectors")
 1031 FORMAT (5F12.8,I10)
      RETURN
20000 STOP ' ERROR WHILE OPENING FILE MFD1OUT'
20001 STOP ' ERROR WHILE OPENING FILE MFDSYMC'
20010 CALL Q2JOB(1,'DIRECTIVE OCCUP NOT FOUND',' ',0)
20020 CALL Q2JOB(1,'ERROR IN GRID DEFINITION',' ',0)
      END
