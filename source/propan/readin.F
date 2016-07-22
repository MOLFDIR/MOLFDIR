C
C     =================
      SUBROUTINE READIN
C     =================
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INCLUDE 'paracom.inc'
C
      INTEGER NCL(16),NOP(16),TOTAL
      DIMENSION ILAB(24)
      LOGICAL USENAT 
C
      NAMELIST/OCCUP/TOTAL,NOPEN
      NAMELIST/EDITPOP/NMO
      NAMELIST/GRLABEL/NLABEL
      NAMELIST/PROPAN/PRTNET,PRTOP,PRTRP,PRTTP
      NAMELIST/GENERAL/TWOC
      NAMELIST/NATORB/USENAT,FILENAM
C
      WRITE (6, *) ' ======================================='
      WRITE (6, *) ' =   Popan Version 1.5                 ='
      WRITE (6,*)  ' =   Written by Luuk Visscher          ='
      WRITE (6, *) ' ======================================='
C
C     VERSION 1.1: POPULATION ANALYSIS
C                                                           LV, 25/3/1992
C     VERSION 1.2: READS VECTORS FROM MFDVECA, NAMELIST CHANGED TO PROPAN
C                                                           LV, 17/9/1992
C     VERSION 1.3: IMPLEMENTED READING FROM FILE WITH NATURAL ORBITALS
C                                                          BDJ, 31/9/1993
C     VERSION 1.4: Cleaned up old CNVE statements : Runs on IBM RS6000
C                                                          LV, 1994-04-11
C     VERSION 1.4.1 : Renamed EDIT namelist
C                                                          LV, 1994-09-26
C     VERSION 1.5   : Updated readin for use in Fortran 90
C                                                         BDJ, 1998-04-01
C
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
      READ (MFD1OUT)
      READ (MFD1OUT)
      READ (MFD1OUT) (KCOMB(I),INDBAS(I),INDSYM(I),I=1,NEQUIV)
      READ (MFD1OUT)
      READ (MFD1OUT) (ICENT(I),ITYPE(I),(IDUM,J=1,NSYMOP),I=1,NCOMB),
     +               (INDCOM(I),I=1, NEQCOM)
      READ (MFD1OUT) (INDORB(I),INDB(I),KSYMRP(I),LSYMRP(I),KSYMF(I),
     +               I=1,NSYMF)
      READ (MFD1OUT)
      READ (MFD1OUT) (NBSYM(I),NBSYM1(I),NBSYM2(I),I=1,NSYMRP)
      CLOSE (MFD1OUT)
C
      INDCOM(NEQCOM+1)=NCOMB
      INDBAS(NEQUIV+1)=NGAUS
      INDORB(NSYMF+1)=NCOEF
      INDSYM(NEQUIV+1)=NSYMF
C
      OPEN(MFDSYMC, STATUS = 'OLD', FORM = 'UNFORMATTED',
     +     FILE = 'MFDSYMC', ERR=30000)
      REWIND MFDSYMC
      DO 10 I=1,NSYMF+5
        READ(MFDSYMC)
  10    CONTINUE
C
      READ(MFDSYMC) (IDIM(I),I=1,NSYMRP),(ITHIRP(I),I=1,NSYMRP),
     +                                   (IPRTNR(I),I=1,NSYMRP)
      NREP = 0
      DO 11 I = 1, NSYMRP
      NREP = MAX0(ITHIRP(I),NREP)
  11  CONTINUE
C
C
C     ---------------
C     READ USER INPUT
C     ---------------
C
      OPEN (5, FILE='MOLFDIR.INP', FORM='FORMATTED')
      TOTAL=0
      NOPEN=0
      CALL LOCATE('OCCUP',5,ITEST)
      IF (ITEST.GT.0) THEN
         READ(5,OCCUP,ERR=20010)
      ELSE
         GOTO 20010
      ENDIF
      READ(5,1000) (NCL(IRP),IRP=1,NSYMRP)
      DO IRP=1,NSYMRP
        NSKIP1(IRP)=0
        NVAL(IRP)=NCL(IRP)
      ENDDO
      DO IOPEN = 1, NOPEN
         READ(5,1000) (NOP(IRP),IRP=1,NSYMRP)
         READ(5,1001) RDUM
         DO IRP=1,NSYMRP
           NVAL(IRP)=NVAL(IRP)+NOP(IRP)
         ENDDO
      ENDDO
      NMO=0
      CALL LOCATE('EDITPOP',7,ITEST)
      IF (ITEST.GT.0) THEN
         READ(5,EDITPOP,ERR=110)
      ELSE
         GOTO 110
      ENDIF
      READ(5,1000) (NSKIP1(IRP),IRP=1,NSYMRP)
      READ(5,1000) (NVAL(IRP),IRP=1,NSYMRP)
 110  DO 100 IRP=1,NSYMRP
        NOCC(IRP)=NCL(IRP)+NOP(IRP)
        NSKIP2(IRP)=NBSYM1(IRP)-NVAL(IRP)-NSKIP1(IRP)
 100    CONTINUE
C
      TWOC=.FALSE.
      CALL LOCATE('GENERAL',7,ITEST)
      IF (ITEST.GT.0) READ (5,GENERAL)
C
      PRTOP = .TRUE.
      PRTRP = .TRUE.
      PRTTP = .TRUE.
      PRTNET = .FALSE.
      CALL LOCATE('PROPAN',6,ITEST)
      IF (ITEST.GT.0) READ (5,PROPAN)
      USENAT=.FALSE.
      CALL LOCATE('NATORB',6,ITEST)
      IF (ITEST.GT.0) READ (5,NATORB)
      IF (.NOT.USENAT) FILENAM='MFDVECA   '
      IF (USENAT) THEN     
	 WRITE (6,*)    
	 WRITE (6,*) 'USING NATURAL ORBITALS'
	 WRITE (6,*)
	 WRITE (6,*) 'FILE IS: ',FILENAM
      ENDIF
C
      NLABEL = 0
      CALL LOCATE('GRLABEL',7,ITEST)
      IF (ITEST.GT.0) READ (5,GRLABEL)
      DO 200 ILABEL = 1, NLABEL
         READ (5,1010) GRLAB(ILABEL),(ILAB(I),I=1,24)
         DO 190 I = 1, 24
            IF (ILAB(I).LE.0.OR.ILAB(I).GT.KCOMB(NEQUIV)) GOTO 190
            INDG(ILAB(I)) = ILABEL
  190       CONTINUE
  200    CONTINUE
      DO 220 I = 1, KCOMB(NEQUIV)
         IF (INDG(I).EQ.0) THEN
            NLABEL = NLABEL + 1
            INDG(I) = NLABEL
            WRITE (GRLAB(NLABEL),1020) I
         ENDIF
  220    CONTINUE
C
 1000 FORMAT (20I4)
 1001 FORMAT (G12.8)
 1010 FORMAT (A6,24I3)
 1020 FORMAT ('GR(',I2,')')
      RETURN
20000 STOP ' ERROR WHILE OPENING FILE MFD1OUT'
30000 STOP ' ERROR WHILE OPENING FILE MFDSYMC'
20010 CALL Q2JOB(1,'DIRECTIVE OCCUP NOT FOUND',' ',0)
20020 CALL Q2JOB(2,'NAMELIST EDITPOP',' ',0)
20030 CALL Q2JOB(2,'NAMELIST GENERAL',' ',0)
20040 CALL Q2JOB(2,'NAMELIST PROPAN',' ',0)
20050 CALL Q2JOB(2,'NAMELIST GRLABEL',' ',0)
      END
