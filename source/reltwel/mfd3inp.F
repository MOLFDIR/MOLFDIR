C
C     ==================
      SUBROUTINE MFD3INP
C     ==================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     READ INPUT DATA
C
      INCLUDE 'paracom.inc'
#if defined (PARALLEL)
      INCLUDE 'mpidef.inc'
#else
      PARAMETER (mpirnk = 0)
#endif
C
      DIMENSION IPARAM(8)
      EQUIVALENCE (INDBAS,IPARAM)
      CHARACTER*8 TIMEX,DATEX*10,VERS*10, MFD1OUT*12
      NAMELIST/RELTWEL/ICR,ICMX,DNORM,ENORM,COUNT,ONECEN,ICORE
      NAMELIST/GENERAL/TWOC
C
      CALL DAYTIME(DATEX,TIMEX)
C
C     -------------------------------------------------
C     READ RESTART-PARAMETERS AND TRUNCATION-PARAMETERS
C     -------------------------------------------------
C
      OPEN (5, FILE='MOLFDIR.INP', FORM='FORMATTED')
      ICR = 1
      ICMX = 4
      ICORE = 0
      DNORM = 32.D0
      DO I=1,4
         ENORM(I) = 1.D-12
      ENDDO
      COUNT = .FALSE.
      CALL LOCATE('RELTWEL',7,ITEST)
      IF (ITEST.GT.0) READ (5,RELTWEL)
      TWOC = .FALSE.
      CALL LOCATE('GENERAL',7,ITEST)
      IF (ITEST.GT.0) READ (5,GENERAL)
      IF (TWOC) ICMX = 1
      if (mpirnk.eq.0) then
      WRITE (*,1000) VERSION,DATEX,TIMEX
      WRITE (*,1010) TWOC
      WRITE (*,1020) COUNT,DNORM
      IF (ICMX.GT.0.AND.ICR.LT.2) WRITE (*,1021) ENORM(1)
      IF (ICMX.GT.1.AND.ICR.LT.3) WRITE (*,1022) ENORM(2)
      IF (ICMX.GT.2.AND.ICR.LT.4) WRITE (*,1023) ENORM(3)
      IF (ICMX.GT.3)              WRITE (*,1024) ENORM(4)
      WRITE (*,1025) ICR,ICMX,ICORE
      endif
      CLOSE(5)
C
C     ----------------------
C     READ DATA FROM MOLFDIR
C     ----------------------
C
      IF (mpirnk .EQ. 0) THEN
         WRITE (MFD1OUT,'(A7)') 'MFD1OUT'
      ELSEIF (mpirnk .LT. 10) THEN
         WRITE (MFD1OUT,'(A7,A1,I1)') 'MFD1OUT','.',mpirnk
      ELSEIF (mpirnk .LT. 100) THEN
         WRITE (MFD1OUT,'(A7,A1,I2)') 'MFD1OUT','.',mpirnk
      ELSE
         WRITE (MFD1OUT,'(A7,A1,I3)') 'MFD1OUT','.',mpirnk
      ENDIF

      OPEN (3, FILE = MFD1OUT, STATUS = 'OLD',
     +      FORM = 'UNFORMATTED', ERR = 20000)
      REWIND 3
      READ (3)     T,D,VERS,NAME
      READ (3)     (IPARAM(I),I=1,8)
C
C     --------------------------------------
C     CHECK PARAMETERS FOR INCOMPATIBILITIES
C     --------------------------------------
C
      IF (IPARAM(1).GT.N14) CALL Q2JOB(5,'P.MFDINP3','N14',IPARAM(1))
      IF (IPARAM(2).GT.N1 ) CALL Q2JOB(5,'P.MFDINP3','N1',IPARAM(2))
      IF (IPARAM(3).GT.N3 ) CALL Q2JOB(5,'P.MFDINP3','N3',IPARAM(3))
      IF (IPARAM(4).GT.N5 ) CALL Q2JOB(5,'P.MFDINP3','N5',IPARAM(4))
      IF (IPARAM(5).GT.N7 ) CALL Q2JOB(5,'P.MFDINP3','N7',IPARAM(5))
      IF (IPARAM(7).GT.N12) CALL Q2JOB(5,'P.MFDINP3','N12',IPARAM(7))
      READ (3)      NEQ1,NEQUIV,NEQCO1,NEQCOM,NGAUS,NCOMB,NSYMOP,NNUCL,
     $              NSYMF,NCOEFF,NSYMRP,NWRDS1,NWRDS2,NWRDS3
      IF (NGAUS.GT.N15) CALL Q2JOB(5,'P.MFDINP3','N15',NGAUS)
      READ (3)      (DUMMY,XCORD(I),YCORD(I),ZCORD(I),DUMMY,I=1,NNUCL)
      READ (3)      DUMMY
      READ (3)      (KCOMB(I),INDBAS(I),INDSYM(I),I=1,NEQUIV),
     &              (NCORE(I,1),NCORE(I,2),I=1,NEQUIV)
      READ (3)      (ALPHA(I),COEFF(I),I=1,NGAUS)
      READ (3)      (ICENT(I),ITYPE(I),(ICOMOP(I,J),J=1,NSYMOP),
     $              I=1,NCOMB),(INDCOM(I),I=1,NEQCOM)
      READ (3)      DUMMY
      READ (3)      DUMMY
      READ (3)      (NBSYM(I),NBSYM1(I),NBSYM2(I),I=1,NSYMRP)
      INDCOM(NEQCOM+1)=NCOMB
      INDBAS(NEQUIV+1)=NGAUS
C
C     -----------------------------------------------------------------
C     MAKE POINTER ARRAY (INVERSE OF INDCOM)
C     FROM FUNCTIONTYPE IXX TO EXPONENTS IM
C     USAGE : ESTABLISH COMPATIBILITY OLD AND NEW LOOPSTRUCTURE MFDTWOE
C     -----------------------------------------------------------------
      IX=0
      IMP=1
      DO 12 IMZ=1,NEQUIV
      IXN=INDCOM(KCOMB(IMZ))
      IF (IXN.NE.IX) THEN
         DO 11 IXX=IX+1,IXN
         KCBINV(IXX,1)=IMP
         KCBINV(IXX,2)=IMZ-1
   11    CONTINUE
         IX=IXN
         IMP=IMZ
      ENDIF
   12 CONTINUE
      IXN=INDCOM(KCOMB(NEQUIV)+1)
      DO 13 IXX=IX+1,IXN
      KCBINV(IXX,1)=IMP
      KCBINV(IXX,2)=NEQUIV
   13 CONTINUE
C
C     Calculate NBTOT, NEM
C
      NBTOT=0
      DO 20 IM=1,NEQUIV
      IT=KCOMB(IM)
      IND=INDCOM(IT)
      KND=INDCOM(IT+1)
      NBAS=KND-IND
      NEM(IM)=NBTOT 
      NBTOT=NBTOT+NBAS
   20 CONTINUE
C
      IF (TWOC) ICMX = 1
      IR=1
      KR=1
      LR=1
      NR=1
      IBLK=0
      RETURN
 1000 FORMAT(//1X,32('*')/1X,'* INTEGRAL PROGRAM "RELTWEL"',T33,'*'/
     $      1X,'* VERSION ',A,T33,'*'/1X,'* DATE ',A,T33,'*'/
     $      1X,'* TIME ',A,T33,'*'/1X,32('*')///)
 1010 FORMAT(/1X,'NAMELIST GENERAL',T30,'TWOC :',T45,L1/)
 1020 FORMAT(/1X,'NAMELIST RELTWEL',
     $      T30,'COUNT :',T45,L1/T30,'DNORM :',T45,G14.6/)
 1021 FORMAT(T30,'ENORM LL:',T45,G14.6)
 1022 FORMAT(T30,'ENORM SL:',T45,G14.6)
 1023 FORMAT(T30,'ENORM SS:',T45,G14.6)
 1024 FORMAT(T30,'ENORM BR:',T45,G14.6)
 1025 FORMAT(T30,'ICR :',T45,I4/
     $      T30,'ICMX :',T45,I4/
     $      T30,'ICORE :',T45,I4////)
10000 CALL Q2JOB(1,'RESTART DATA OR EMPTY CARD',' ',0)
20000 CALL Q2JOB(2,'FILE MFD1OUT (UN3) CANNOT BE ACCESSED',' ',0)
      END
