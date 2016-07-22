C     =========================
      SUBROUTINE USRINP (CONTN)
C     =========================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     READ USER INPUT
C
      INCLUDE 'paracom.inc'
C
      CHARACTER*2 DEFSM,SMPRT,LTYP*1,LLTYP*1
      DIMENSION LTYP(0:N5),TRSHLD(3)
      LOGICAL CONTN
      DATA LTYP /'S','P','D','F','G','H',' '/
      NAMELIST /GENBAS/ NAMES,NMO,DEFSM,IPROF,IPRINT,TRSHLD
      CONTN = .TRUE.
C
      REWIND (5)
      NAMES(2) = 'BASIS_FILE'
      IPRINT = 0
      IPROF = 0
      IF (TWOC) THEN
         DEFSM = 'XX'
      ELSE
         DEFSM = 'KA'
      ENDIF
      TRSHLD (1) = 1.D-5
      TRSHLD (2) = 1.D-7
      TRSHLD (3) = 1.D-7
      CALL LOCATE('GENBAS',6,ITEST)
      IF (ITEST.GT.0) THEN
         READ (5,GENBAS,ERR=10000)
      ELSE
         GOTO 10000
      ENDIF
C
      IF (NMO.GT.N16) CALL Q2JOB (3,'USRINP','N16',NMO)
      WRITE (*,1000) NAMES (1)
      IF (IPROF.GE.1) IPRINT = MAX0(IPRINT,2)
      WRITE (*,1010) NMO,DEFSM,IPROF,IPRINT,(TRSHLD(IX),IX=1,3)
      DO 50 I = 1 , NMO
      SMPRT = DEFSM
      READ (5,1120) MOS(I,1),LLTYP,SMPRT,(TRSH(I,IX),IX=1,3)
      IF (SMPRT.EQ.'  ') SMPRT = DEFSM
      IF (TRSH(I,1).EQ.0.0) TRSH (I,1) = TRSHLD (1)
      IF (TRSH(I,2).EQ.0.0) TRSH (I,2) = TRSHLD (2)
      IF (TRSH(I,3).EQ.0.0) TRSH (I,3) = TRSHLD (3)
      DO 40 ITP = 0,N4
      IF (LLTYP.EQ.LTYP(ITP)) GOTO 45
   40 CONTINUE
      CALL Q2JOB (2,'UNKNOWN LTYP SPECIFIED '//LLTYP,' ',0)
   45 CONTINUE
      MOS(I,2) = ITP
      DO 46 INIT = 3,5
   46 MOS (I,INIT) = -1
      ISMPRT = INDEX (SMPRT,'A')
      JSMPRT = INDEX (SMPRT,'B')
      KSMPRT = INDEX (SMPRT,'C')
      IF (((ISMPRT+JSMPRT+KSMPRT).GT.0).AND.TWOC) CALL Q2JOB
     & (2,'TWOC OPTION INCOMPATIBLE WITH SMPRT = '//SMPRT,' ',0)
      IF (ISMPRT.GT.0.OR.JSMPRT.GT.0) MOS(I,3) = ITP - 1
      IF (ISMPRT.GT.0.OR.KSMPRT.GT.0) MOS(I,4) = ITP + 1
      ISMPRT = INDEX (SMPRT,'K')
      JSMPRT = INDEX (SMPRT,'L')
      KSMPRT = INDEX (SMPRT,'M')
      IF (ISMPRT.GT.0) MOS(I,5) = 1
      IF (JSMPRT.GT.0) MOS(I,5) = 2
      IF (KSMPRT.GT.0) MOS(I,5) = 0
   50 CONTINUE
C     ------------------------
C     ADD EXTRA BASISFUNCTIONS
C     ------------------------
      IN = NMO
      DO IOM = 1, 2
      DO ITP = 0, N4
      NWPRIM (IOM,ITP) = 0
      NWCONT (IOM,ITP) = 0
      ENDDO
      ENDDO
  100 READ (5,1130,END=999,ERR=999) IOM,LLTYP,IP,IC,SMPRT,TRSH1,TRSH3
      DO 101 ITP = 0,N4
      IF (LLTYP.EQ.LTYP(ITP)) GOTO 102
  101 CONTINUE
      IF (IOM.EQ.0.AND.IP.EQ.0.AND.IC.EQ.0) GOTO 999
      CALL Q2JOB (2,'UNKNOWN LTYP SPECIFIED '//LLTYP,' ',0)
  102 CONTINUE
      IF (IOM.EQ.0) IOM = 1
      IF (SMPRT.EQ.'  ') SMPRT = DEFSM
      ISMPRT = INDEX (SMPRT,'K')
      JSMPRT = INDEX (SMPRT,'L')
      KSMPRT = INDEX (SMPRT,'M')
      IF (TRSH1.EQ.0.0) THEN
                              TRSH1 = 1.D-5
                              TRSH2 = 1.D-7
      ENDIF
      IF (TRSH3.EQ.0.0)       TRSH3 = 1.D-7
      IPP = NWPRIM (IOM,ITP)
      IPC = NWCONT (IOM,ITP)
      NWPRIM (IOM,ITP) = IPP + IP
      NWCONT (IOM,ITP) = IPC + IC
      READ (5,*) (EN(IX,ITP,IOM),IX=IPP+1,IPP+IP)
      IF (IP.NE.IC) THEN
      READ (5,*) ((CN(JX,IX,ITP,IOM),JX=IPP+1,IPP+IP),IX=IPC+1,IPC+IC)
      ELSE
         DO 120 I = 1, IC
         DO 119 J = 1, IP
         CN(IPP+J,IPC+I,ITP,IOM) = 0.0D0
  119    CONTINUE
         CN(IPP+I,IPC+I,ITP,IOM) = 1.0D0
  120    CONTINUE
      ENDIF
C     Zero out non-diagonal blocks
      DO I = 1, IPC
      DO J = IPP+1, IPP+IP
      CN(J,I,ITP,IOM) = 0.D0
      ENDDO
      ENDDO
      DO I = IPC+1, IPC+IC
      DO J = 1, IPP
      CN(J,I,ITP,IOM) = 0.D0
      ENDDO
      ENDDO
      DO 140 I = 1, IC
      IN = IN + 1
      INDNW (IPC+I,IOM,ITP) = IN
      TRSH (IN,1) = TRSH1
      TRSH (IN,2) = TRSH2
      TRSH (IN,3) = TRSH3
      MOS(IN,1) = IPC+I
      MOS(IN,2) = ITP
      DO 130 INIT = 3,5
  130 MOS (IN,INIT) = -1
      IF (ISMPRT.GT.0) MOS(IN,5) = 1
      IF (JSMPRT.GT.0) MOS(IN,5) = 2
      IF (KSMPRT.GT.0) MOS(IN,5) = 0
  140 CONTINUE
      GOTO 100
  999 RETURN    
 1000 FORMAT (//3X,A)
 1010 FORMAT (/3X,'NUMBER OF MO S :',T20,I5,/3X,'DEFSM :',T23,A2,
     &/3X,'IPROF :',T20,I5,/3X,'IPRINT :',T20,I5,
     &/3X,'TRESHOLD (LARGE) :',T35,G10.4,
     &/3X,'TRESHOLD (SMALL) :',T35,G10.4,
     &/3X,'TRESHOLD (KIN. BALANCE) :',T35,G10.4)
 1100 FORMAT (A)
 1110 FORMAT (I4,2X,A2,2I4,3G10.4)
 1120 FORMAT (I4,1X,A1,2X,A2,3G10.4)
 1130 FORMAT (I4,1X,A1,2I4,2X,A2,2G10.4)
10000 CONTN = .FALSE.
      RETURN
      END
