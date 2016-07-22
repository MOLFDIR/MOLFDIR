C     =================
      SUBROUTINE GENIND
C     =================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     ANALYSE ATOMIC VECTORS TO DETERMINE L-TYPES
C
      INCLUDE 'paracom.inc'
C
      DIMENSION INDCS(16),INDCF(16),ISRPS(16)
      DATA INDREPS /16*0/
C
      IND = 1
   4  READ (2,1200,END=8,ERR=8) JREP,NSPINOR,NBAS
      INDREPS(JREP)=IND
   5  JREP=JREP+1
      IF ((IPRTNR(JREP).EQ.1).OR.(JREP.GT.NSYMRP)) GOTO 6
      INDREPS(JREP)=IND  
      GOTO 5
   6  DO 7 J=1,NSPINOR
         READ (2,1201) ORBENE(IND)
	 READ (2,FORMAT) (RDUM,K=1,2*NBAS)
         IND=IND+1
   7  CONTINUE
      GOTO 4
   8  CONTINUE
C
C     ------------------------------------
C     DETERMINE CONTENTS OF ATOMIC VECTORS
C     ------------------------------------
      NRP = 0
      IMS = INDB (1)
      IX  = INDCOM(KCOMB(IMS)) + 1
      ITYPX  = ITYPE (IX)
      DO 100 ISYMF = 1,NSYMF
      IM     = INDB (ISYMF)
      ISYMRP = KSYMRP (ISYMF)
      INDC   = LSYMRP (ISYMF)
      IXN    = INDCOM(KCOMB(IM)) + 1
      ITYPXN  = ITYPE (IXN)
      IF (ITYPXN.EQ.ITYPX) THEN
         DO 10 I=1,NRP
         IF (ISYMRP.EQ.ISRPS(I)) GOTO 20
   10    CONTINUE
         NRP = NRP + 1
         INDCS(NRP) = INDC
         INDCF(NRP) = INDC
         ISRPS(NRP) = ISYMRP
         GOTO 30
   20    INDCF(I) = INDC
   30    CONTINUE
      ELSE
         DO 60 I = 1, NRP
         JSYMRP = ISRPS (I)
         IF  ( INDCF (I) .LE. NBSYM1 (JSYMRP) ) THEN
             IOM = 1
         ELSE
             IOM = 2
         ENDIF
         MCONT = IM - IMS
         NCONT = INDCF (I) - INDCS (I) + 1
         NTP = NT (ITYPX)
         IMSCOF (IOM,NTP) = IMS
         IMFCOF (IOM,NTP) = IM - 1
         NUMCOF (IOM,NTP) = MCONT
         DO 50 J = 1, NCONT / MCONT
         NTYPES (JSYMRP) = NTYPES (JSYMRP) + 1
         NTYP = NTYPES (JSYMRP)
         INDCOF (JSYMRP,NTYP) = INDCS (I) - 2 + J
         NDUMCF (JSYMRP,NTYP) = NCONT / MCONT -1
         ITPCOF (JSYMRP,NTYP) = NTP
         IOMCOF (JSYMRP,NTYP) = IOM
   50    CONTINUE
   60    CONTINUE
         NRP = 1
         IMS = IM
         INDCS(NRP) = INDC
         INDCF(NRP) = INDC
         ISRPS(NRP) = ISYMRP
         ITYPX = ITYPXN
      ENDIF
  100 CONTINUE
      DO 160 I=1,NRP
      JSYMRP = ISRPS (I)
      IF ( INDCF (I) .LE. NBSYM1 (JSYMRP) ) THEN
          IOM = 1
      ELSE
          IOM = 2
      ENDIF
      MCONT = IM + 1 - IMS
      NCONT = INDCF (I) - INDCS (I) + 1
      NTP = NT (ITYPX)
      IMSCOF (IOM,NTP) = IMS
      IMFCOF (IOM,NTP) = IM
      NUMCOF (IOM,NTP) = MCONT
      DO 150 J = 1, NCONT / MCONT
      NTYPES (JSYMRP) = NTYPES (JSYMRP) + 1
      NTYP = NTYPES (JSYMRP)
      INDCOF (JSYMRP,NTYP) = INDCS (I) - 2 + J
      NDUMCF (JSYMRP,NTYP) = NCONT / MCONT - 1
      ITPCOF (JSYMRP,NTYP) = NTP
      IOMCOF (JSYMRP,NTYP) = IOM
  150 CONTINUE
  160 CONTINUE
      IRP = 0
      DO 180 I=1,NSYMRP
      IF (INDREPS(I).NE.0) THEN
         IRP = IRP + 1
         IREPS (IRP) = I
         DO 170 J = 1, NTYPES(I)
  170    IF (IOMCOF(I,J).EQ.1) NLARGE (I) = J
      ELSE
         NLARGE (I) = 0
      ENDIF
  180 CONTINUE
      RETURN
 1200 FORMAT(I4,27X,2I4)
 1201 FORMAT(10X,G20.10,48X)
      END
