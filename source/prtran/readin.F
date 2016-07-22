C
C     =================
      SUBROUTINE READIN
C     =================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE "molf.inc"
      INCLUDE "sym.inc"
      INCLUDE "inpt.inc"
      INCLUDE "vecs.inc"
      INTEGER TOTAL,NCL(16),NOP(16),NOP2(16)
      LOGICAL DEFRAS,TOBE
      NAMELIST/OCCUP/TOTAL,NOPEN
      NAMELIST/PRTRAN/IPRNT
      NAMELIST/EDIT/NMO,DEFRAS
      NAMELIST/GENERAL/TWOC
C
C     -------------------------------
C     DETERMINE IPQMAX, MOFSET AND NSP
C     -------------------------------
C
      OPEN (5, FILE='MOLFDIR.INP', FORM='FORMATTED')
      TOTAL=0
      NOPEN=0
      DO IRP=1,NSYMRP
         NOP(IRP)=0
      ENDDO
      CALL LOCATE('OCCUP',5,ITEST)
      IF (ITEST.GT.0) THEN
         READ(5,OCCUP,ERR=3000)
      ELSE
         GOTO 3000
      ENDIF
      READ(5,1000) (NCL(IRP),IRP=1,NSYMRP)
      DO IOPEN = 1, NOPEN
         READ(5,1000) (NOP2(IRP),IRP=1,NSYMRP)
         READ(5,*) DUM
         DO IRP = 1, NSYMRP
            NOP(IRP) = NOP(IRP) + NOP2(IRP)
         ENDDO
      ENDDO
C
      INQUIRE (FILE='MFDVECA',EXIST=TOBE)
      IF (.NOT.TOBE) STOP 'MFDVECA NOT FOUND'
      DO 526 IRP=1,NSYMRP
        NSKIP1(IRP)=0
        NOC(IRP)=NCL(IRP)+NOP(IRP)
 526    CONTINUE
C
      NMO=0
      DEFRAS=.FALSE.
      CALL LOCATE('EDIT',4,ITEST)
      IF (ITEST.GT.0) THEN
         READ(5,EDIT,ERR=525)
      ELSE
         GOTO 525
      ENDIF
      READ(5,1000) (NSKIP1(IRP),IRP=1,NSYMRP)
      READ(5,1000) (NOC(IRP),IRP=1,NSYMRP)
 525  DO 510 IRP=1,NSYMRP
        IF (NOC(IRP).GT.N40) CALL Q2JOB(3,'READIN','N40',NOC(IRP))
        NSKIP2(IRP)=NBSYM1(IRP)-NOC(IRP)-NSKIP1(IRP)
        IF (NSKIP2(IRP).LT.0) STOP 'ERROR IN $OCCUP / $EDIT '
 510  CONTINUE
      WRITE(6,1020) ' Frozen spinors :',(NSKIP1(IRP),IRP=1,NSYMRP)
      WRITE(6,1020) ' Active spinors :',(NOC(IRP),IRP=1,NSYMRP)
      WRITE(6,1020) ' Deleted spinors :',(NSKIP2(IRP),IRP=1,NSYMRP)
      TWOC=.FALSE.
      CALL LOCATE('GENERAL',7,ITEST)
      IF (ITEST.GT.0) READ(5,GENERAL)
      IPRNT=0
      CALL LOCATE('PRTRAN',6,ITEST)
      IF (ITEST.GT.0) READ(5,PRTRAN)
      WRITE(6,1030) IPRNT,TWOC
C
      NSP=0
      MOFSET(1)=0
      DO 10 IRP=1,NSYMRP
        MOFSET(IRP+1)=MOFSET(IRP)+NOC(IRP)
        NSP=NSP+NOC(IRP)
 10     CONTINUE
      IF (NSP.GT.N28) STOP 'INCREASE N28'
C
      IFIRST=1
      ID=0
      DO 20 IRP=1,NSYMRP
        IFRSTPT(IRP)=IFIRST
        ID=ID+1
        IF (ID.LT.IDIM(IRP)) GOTO 20
        IFIRST=IFIRST+NOC(IRP)*IDIM(IRP)
        NFIE(ITHIRP(IRP))=NOC(IRP)
        ID=0
 20     CONTINUE
C
      NSC1=NEM(NEQ1+1)
      IF (TWOC.OR.MFIL.EQ.1) THEN
        NSC=NSC1
      ELSE
        NSC=NEM(NEQUIV+1)
      ENDIF
      IPQMAX=NSC*(NSC+1)/2
C
C     -------------------------
C     MAKE REORDER ARRAY IMORAS
C     -------------------------
C
      IF (DEFRAS) THEN
         STOP 'USER DEFINITION OF RAS SPACES NOT YET PROGRAMMED'
      ELSE
C
         KMO = 0
C
C        ---------------------------------------------
C        RAS1 SPACE : NOT FROZEN CLOSED SHELL ORBITALS
C        ---------------------------------------------
         DO 45 IRP = 1, NSYMRP
         DO 44 IMO = 1, NCL(IRP) - NSKIP1(IRP)
         KMO = KMO + 1
         IMORAS(IMO,IRP) = KMO
         IMOREP(KMO) = IRP
   44    CONTINUE
   45    CONTINUE
         NORB1 = KMO
C
C        ---------------------------------
C        RAS2 SPACE : OPEN SHELLS ORBITALS
C        ---------------------------------
C
         DO 47 IRP = 1, NSYMRP
         DO 46 IMO =NCL(IRP)-NSKIP1(IRP)+1,NCL(IRP)-NSKIP1(IRP)+NOP(IRP)
         KMO = KMO + 1
         IMORAS(IMO,IRP) = KMO
         IMOREP(KMO) = IRP
   46    CONTINUE
   47    CONTINUE
         NORB2 = KMO -NORB1
C
C        --------------------------------
C        RAS3 SPACE : UNOCCUPIED ORBITALS
C        --------------------------------
C
         DO 49 IRP = 1, NSYMRP
         DO 48 IMO = NCL(IRP)-NSKIP1(IRP)+NOP(IRP)+1, NOC(IRP)
         KMO = KMO + 1
         IMORAS(IMO,IRP) = KMO
         IMOREP(KMO) = IRP
   48    CONTINUE
   49    CONTINUE
         NORB3 = KMO - NORB1 - NORB2
      ENDIF
C
      RETURN
 1000 FORMAT(BZ,16I4)
 1020 FORMAT(A20,16I3)
 1030 FORMAT(//' Selected Options :'/' Print Level :',T35,I5
     &/' TwoComponent Formalism :',T39,L1)
 3000 CALL Q2JOB(1,'VECTORS DIRECTIVE EXPECTED',' ',0)
      END
