C
      SUBROUTINE LEADING(IVWU)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'param.inc'
      INCLUDE 'general.inc'
      INCLUDE 'symm.inc'
      INCLUDE 'files.inc'
      INCLUDE 'iterat.inc'
C
      PARAMETER (NDREC=1023)
C
      DIMENSION IVWU(16,0:NORB,0:NELEC)
      DIMENSION IDREC(N8),IRAS1(N3),IRAS2E(N3),IRAS2H(N3),IRAS3(N3)
      DIMENSION COR(NDREC),COI(NDREC),IDETER(NDREC)
      LOGICAL GOVER
C
      WRITE(6,*)
      WRITE(6,*)
      WRITE(6,'(A35,F6.4)') 'Determinants with coefficient .GT. ',COMIN
#if defined (BIT64)
      OPEN (MRCFINV,FILE=FNAM(5),ACCESS='DIRECT',RECL=8)
#else
      OPEN (MRCFINV,FILE=FNAM(5),ACCESS='DIRECT',RECL=4)
#endif
      READ (MRCFINV,REC=1,ERR=10000) LENREC
      CLOSE (MRCFINV)
      OPEN (MRCFINV,FILE=FNAM(5),ACCESS='DIRECT',RECL=LENREC)
      READ (MRCFINV,REC=1,ERR=10000) LENREC,NPDET,NPROOT,
     &                     (EIGEN1(IROOT),IROOT=1,NPROOT),
     &                     (IDREC(IROOT),IROOT=1,NPROOT)
      IF (NPDET.NE.NDET.OR.NPROOT.LT.NROOTS) GOTO 10000
      DO 7120 IROOT = 1,NROOTS
      WRITE(6,*)
      WRITE(6,'(A24,I4,A18,F16.8)') "Determinant(s) of root: ",
     $ IROOT," with eigenvalue: ",EIGEN1(IROOT)
      WRITE(6,*)
      IREC = IDREC(IROOT)
 7110 IF (REALAR) THEN
         READ (MRCFINV,REC=IREC,ERR=10000) INREC, NONZERO,
     &        (IDETER(IDNZ),IDNZ=1,NONZERO),
     &        (COR(IDNZ),IDNZ=1,NONZERO)
      ELSE
         READ (MRCFINV,REC=IREC,ERR=10000) INREC, NONZERO,
     &        (IDETER(IDNZ),IDNZ=1,NONZERO),
     &        (COR(IDNZ),COI(IDNZ),IDNZ=1,NONZERO)
      ENDIF
      DO 7111 IDNZ = 1, NONZERO
         COEFF = COR(IDNZ)**2
         IF (.NOT.REALAR) COEFF = COEFF + COI(IDNZ)**2
         IF (COEFF.GT.COMIN) THEN
            WRITE(6,'(A12,I8,A23,F16.8)') "Determinant ",IDETER(IDNZ),
     &       " with square of coeff. ",(COEFF)
            IA=0
            IB=0
            IC=0
            IG=0
            LORB=NORB
            IEL=NELEC
            IGAM=IREP
 7108       IF (LORB.EQ.0) GOTO 7109
            GOVER=IVWU(IGAM,LORB-1,IEL).GE.IDETER(IDNZ)
            IF (GOVER) THEN
               IF (LORB.LE.NORBR(1)) THEN
                  IA=IA+1
                  IRAS1(IA)=LORB
               ELSEIF (LORB.LE.(NORBR(1)+NORBR(2))) THEN
                  IB=IB+1
                  IRAS2H(IB)=LORB
               ENDIF
            ELSE
               IDETER(IDNZ)=IDETER(IDNZ)-IVWU(IGAM,LORB-1,IEL)
               IGAM=MULTBI(IRPAMO(LORB),IGAM)
               IEL=IEL-1
               IF (LORB.GT.(NORBR(1)+NORBR(2))) THEN
                  IG=IG+1
                  IRAS3(IG)=LORB
               ELSEIF (LORB.GT.NORBR(1)) THEN
                  IC=IC+1
                  IRAS2E(IC)=LORB
               ENDIF
            ENDIF
            LORB=LORB-1
            GOTO 7108
 7109       WRITE(6,12010) "Holes in RAS1: ",
     &           (IRAS1(I),REPNA(IRPAMO(IRAS1(I))),I=IA,1,-1)
            IF (IC.LE.IB) THEN
               WRITE(6,12010) "Elec. in RAS2: ",
     &              (IRAS2E(I),REPNA(IRPAMO(IRAS2E(I))),I=IC,1,-1)
            ELSE
               WRITE(6,12010) "Holes in RAS2: ",
     &              (IRAS2H(I),REPNA(IRPAMO(IRAS2H(I))),I=IB,1,-1)
            ENDIF
            WRITE(6,12010) "Elec. in RAS3: ",
     &           (IRAS3(I),REPNA(IRPAMO(IRAS3(I))),I=IG,1,-1)
         ENDIF
 7111 CONTINUE
      IF (INREC.NE.0) THEN
         IREC = INREC
         GOTO 7110
      ENDIF
 7120 CONTINUE
      CLOSE (MRCFINV,STATUS='KEEP')
C
12010 FORMAT(A15,6(I4,"(",A4,")"))
      RETURN
10000 CALL Q2JOB (2,'ERROR READING MRCFINV',' ',0)
      RETURN
      END
