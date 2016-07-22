C
      SUBROUTINE SHUFFLE (RKLR,RKLI,INDK,INDL,TIR,TII)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'param.inc'
      INCLUDE 'files.inc'
      INCLUDE 'general.inc'
      INCLUDE 'symm.inc'
      INCLUDE 'mempoint.inc'
C
C     ---------------------------------------------------------
C     CANONICAL INTEGRALS ARE GENERATED FROM THE ORIGINAL LIST.
C     DIAGONAL INTEGRALS (IJ=KL) NEED TO BE DIVIDED BY 2.
C     shift if necessary if spinors are deleted
C     ---------------------------------------------------------
C
      REAL*8 RKLR(*),RKLI(*),TIR(*),TII(*)
      INTEGER INDK(*),INDL(*)
      DIMENSION KR(-N3:N3)
C
      CHARACTER*10 DATEX,DATEXB,TIMEX*8,TIMEXB*8
C
      REALAR = .TRUE.
      TRESINT = 1.D-12
      NINT = 0
      KLS = 0
      KLF = M2
      MM = M1 * M2
      OPEN(MDCINT,FILE=FNAM(2),FORM='UNFORMATTED')
      Read (mdcint,err=10000,end=10000) datex,timex,nkr,
     & (kr(i),kr(-i),i=1,nkr)
      IF (2*NKR.NE.NORB+NDELETE) 
     &   STOP 'MRCONEE AND MDCINT ARE INCOMPATIBLE'
      IF (BREIT) THEN
      OPEN(MDBINT,FILE=FNAM(3),FORM='UNFORMATTED')
      Read (mdbint,err=10001,end=10001) datexb,timexb,nkrb
      IF (NKR.NE.NKRB) STOP 'MDCINT AND MDBINT ARE INCOMPATIBLE'
      ENDIF
      OPEN (MRCTWOE,FILE=FNAM(8),FORM='UNFORMATTED')
   10 REWIND (MDCINT)
      READ (MDCINT)
      IF (BREIT) REWIND (MDBINT)
      IF (BREIT) READ (MDBINT)
      MINT = 0
      MINTB = 0
      MAX = MM
      IF (BREIT) MAX = MAX * 2
      DO 20 IJKL = 1, MAX
      TIR (IJKL) = 0.D0
      TII (IJKL) = 0.D0
   20 CONTINUE
C
C     -------------------------
C     READ IN COULOMB INTEGRALS
C     -------------------------
C
   30 read (mdcint,ERR=10010,END=10010) ikr,jkr,nz,
     &             (indk(inz),indl(inz),inz=1,nz),
     &             (rklr(inz),rkli(inz),inz=1,nz)
      DO INZ = 1, NZ
         IF (ABS(RKLI(INZ)).GT.TRESINT) REALAR = .FALSE.
      ENDDO
      MINT = MINT + NZ
      If (ikr.EQ.0) GoTo 40 ! End of File
      i = indmo(kr(ikr))
      j = indmo(kr(jkr))
      itr = indmo(kr(-ikr))
      jtr = indmo(kr(-jkr))
      SignIJ = SIGN(1,ikr) * SIGN(1,jkr)
      DO INZ = 1, NZ
         kkr = indk(inz)
         lkr = indl(inz)
         k = indmo(kr(kkr))
         l = indmo(kr(lkr))
         ktr = indmo(kr(-kkr))
         ltr = indmo(kr(-lkr))
         SignKL = SIGN(1,kkr) * SIGN(1,lkr)
C--> Original integral
         IF (I.NE.0.AND.J.NE.0.AND.K.NE.0.AND.L.NE.0) THEN
         TWOR = RKLR(INZ)
         TWOI = RKLI(INZ)
         IJ = (I-1) * NORB + J
         KL = (K-1) * NORB + L
         JI = (J-1) * NORB + I
         LK = (L-1) * NORB + K
         IF (KL.GT.KLS.AND.KL.LE.KLF.AND.IJ.GE.KL) THEN
            TIR(IJ+(KL-KLS-1)*M1) = TWOR
            TII(IJ+(KL-KLS-1)*M1) = TWOI
         ENDIF
         IF (LK.GT.KLS.AND.LK.LE.KLF.AND.JI.GE.LK) THEN
            TIR(JI+(LK-KLS-1)*M1) = TWOR
            TII(JI+(LK-KLS-1)*M1) = - TWOI
         ENDIF
         IF (IJ.GT.KLS.AND.IJ.LE.KLF.AND.KL.GE.IJ) THEN
            TIR(KL+(IJ-KLS-1)*M1) = TWOR
            TII(KL+(IJ-KLS-1)*M1) = TWOI
         ENDIF
         IF (JI.GT.KLS.AND.JI.LE.KLF.AND.LK.GE.JI) THEN
            TIR(LK+(JI-KLS-1)*M1) = TWOR
            TII(LK+(JI-KLS-1)*M1) = - TWOI
         ENDIF
         ENDIF ! check on deleted spinors
C--> Time reversal left
         IF (ITR.NE.0.AND.JTR.NE.0.AND.K.NE.0.AND.L.NE.0) THEN
         TWOR = RKLR(INZ) * SignIJ
         TWOI = RKLI(INZ) * SignIJ
         IJ = (JTR-1) * NORB + ITR
         KL = (K-1) * NORB + L
         JI = (ITR-1) * NORB + JTR
         LK = (L-1) * NORB + K
         IF (KL.GT.KLS.AND.KL.LE.KLF.AND.IJ.GE.KL) THEN
            TIR(IJ+(KL-KLS-1)*M1) = TWOR
            TII(IJ+(KL-KLS-1)*M1) = TWOI
         ENDIF
         IF (LK.GT.KLS.AND.LK.LE.KLF.AND.JI.GE.LK) THEN
            TIR(JI+(LK-KLS-1)*M1) = TWOR
            TII(JI+(LK-KLS-1)*M1) = - TWOI
         ENDIF
         IF (IJ.GT.KLS.AND.IJ.LE.KLF.AND.KL.GE.IJ) THEN
            TIR(KL+(IJ-KLS-1)*M1) = TWOR
            TII(KL+(IJ-KLS-1)*M1) = TWOI
         ENDIF
         IF (JI.GT.KLS.AND.JI.LE.KLF.AND.LK.GE.JI) THEN
            TIR(LK+(JI-KLS-1)*M1) = TWOR
            TII(LK+(JI-KLS-1)*M1) = - TWOI
         ENDIF
         ENDIF ! check on deleted spinors
C--> Time reversal right
         IF (I.NE.0.AND.J.NE.0.AND.KTR.NE.0.AND.LTR.NE.0) THEN
         TWOR = RKLR(INZ) * SignKL
         TWOI = RKLI(INZ) * SignKL
         IJ = (I-1) * NORB + J
         KL = (LTR-1) * NORB + KTR
         JI = (J-1) * NORB + I
         LK = (KTR-1) * NORB + LTR
         IF (KL.GT.KLS.AND.KL.LE.KLF.AND.IJ.GE.KL) THEN
            TIR(IJ+(KL-KLS-1)*M1) = TWOR
            TII(IJ+(KL-KLS-1)*M1) = TWOI
         ENDIF
         IF (LK.GT.KLS.AND.LK.LE.KLF.AND.JI.GE.LK) THEN
            TIR(JI+(LK-KLS-1)*M1) = TWOR
            TII(JI+(LK-KLS-1)*M1) = - TWOI
         ENDIF
         IF (IJ.GT.KLS.AND.IJ.LE.KLF.AND.KL.GE.IJ) THEN
            TIR(KL+(IJ-KLS-1)*M1) = TWOR
            TII(KL+(IJ-KLS-1)*M1) = TWOI
         ENDIF
         IF (JI.GT.KLS.AND.JI.LE.KLF.AND.LK.GE.JI) THEN
            TIR(LK+(JI-KLS-1)*M1) = TWOR
            TII(LK+(JI-KLS-1)*M1) = - TWOI
         ENDIF
         ENDIF ! check on deleted spinors
C--> Time reversal both
         IF (ITR.NE.0.AND.JTR.NE.0.AND.KTR.NE.0.AND.LTR.NE.0) THEN
         TWOR = RKLR(INZ) * SignIJ * SignKL
         TWOI = RKLI(INZ) * SignIJ * SignKL
         IJ = (JTR-1) * NORB + ITR
         KL = (LTR-1) * NORB + KTR
         JI = (ITR-1) * NORB + JTR
         LK = (KTR-1) * NORB + LTR
         IF (KL.GT.KLS.AND.KL.LE.KLF.AND.IJ.GE.KL) THEN
            TIR(IJ+(KL-KLS-1)*M1) = TWOR
            TII(IJ+(KL-KLS-1)*M1) = TWOI
         ENDIF
         IF (LK.GT.KLS.AND.LK.LE.KLF.AND.JI.GE.LK) THEN
            TIR(JI+(LK-KLS-1)*M1) = TWOR
            TII(JI+(LK-KLS-1)*M1) = - TWOI
         ENDIF
         IF (IJ.GT.KLS.AND.IJ.LE.KLF.AND.KL.GE.IJ) THEN
            TIR(KL+(IJ-KLS-1)*M1) = TWOR
            TII(KL+(IJ-KLS-1)*M1) = TWOI
         ENDIF
         IF (JI.GT.KLS.AND.JI.LE.KLF.AND.LK.GE.JI) THEN
            TIR(LK+(JI-KLS-1)*M1) = TWOR
            TII(LK+(JI-KLS-1)*M1) = - TWOI
         ENDIF
         ENDIF ! check on deleted spinors
      ENDDO
C
      GOTO 30
C
   40 IF (MINT.EQ.0) WRITE (6,1010) 'MDCINT'
C
      IF (.NOT.BREIT) GOTO 61
C
C     -----------------------
C     READ IN BREIT INTEGRALS
C     -----------------------
C
   50 read (mdbint,ERR=10011,END=10011) ikr,jkr,nz,
     &             (indk(inz),indl(inz),inz=1,nz),
     &             (rklr(inz),rkli(inz),inz=1,nz)
      DO INZ = 1, NZ
         IF (ABS(RKLI(INZ)).GT.TRESINT) REALAR = .FALSE.
      ENDDO
      MINTB = MINTB + NZ
      If (ikr.EQ.0) GoTo 60 ! End of File
      i = indmo(kr(ikr))
      j = indmo(kr(jkr))
      itr = indmo(kr(-ikr))
      jtr = indmo(kr(-jkr))
      SignIJ = - SIGN(1,ikr) * SIGN(1,jkr)
      DO INZ = 1, NZ
         kkr = indk(inz)
         lkr = indl(inz)
         k = indmo(kr(kkr))
         l = indmo(kr(lkr))
         ktr = indmo(kr(-kkr))
         ltr = indmo(kr(-lkr))
         SignKL = - SIGN(1,kkr) * SIGN(1,lkr)
C--> Original integral
         IF (I.NE.0.AND.J.NE.0.AND.K.NE.0.AND.L.NE.0) THEN
         TWOR = RKLR(INZ)
         TWOI = RKLI(INZ)
         IJ = (I-1) * NORB + J
         KL = (K-1) * NORB + L
         JI = (J-1) * NORB + I
         LK = (L-1) * NORB + K
         IF (KL.GT.KLS.AND.KL.LE.KLF.AND.IJ.GE.KL) THEN
            TIR(IJ+(KL-KLS-1)*M1+MM) = TWOR
            TII(IJ+(KL-KLS-1)*M1+MM) = TWOI
         ENDIF
         IF (LK.GT.KLS.AND.LK.LE.KLF.AND.JI.GE.LK) THEN
            TIR(JI+(LK-KLS-1)*M1+MM) = TWOR
            TII(JI+(LK-KLS-1)*M1+MM) = - TWOI
         ENDIF
         IF (IJ.GT.KLS.AND.IJ.LE.KLF.AND.KL.GE.IJ) THEN
            TIR(KL+(IJ-KLS-1)*M1+MM) = TWOR
            TII(KL+(IJ-KLS-1)*M1+MM) = TWOI
         ENDIF
         IF (JI.GT.KLS.AND.JI.LE.KLF.AND.LK.GE.JI) THEN
            TIR(LK+(JI-KLS-1)*M1+MM) = TWOR
            TII(LK+(JI-KLS-1)*M1+MM) = - TWOI
         ENDIF
         ENDIF ! check on deleted spinors
C--> Time reversal left
         IF (ITR.NE.0.AND.JTR.NE.0.AND.K.NE.0.AND.L.NE.0) THEN
         TWOR = RKLR(INZ) * SignIJ
         TWOI = RKLI(INZ) * SignIJ
         IJ = (JTR-1) * NORB + ITR
         KL = (K-1) * NORB + L
         JI = (ITR-1) * NORB + JTR
         LK = (L-1) * NORB + K
         IF (KL.GT.KLS.AND.KL.LE.KLF.AND.IJ.GE.KL) THEN
            TIR(IJ+(KL-KLS-1)*M1+MM) = TWOR
            TII(IJ+(KL-KLS-1)*M1+MM) = TWOI
         ENDIF
         IF (LK.GT.KLS.AND.LK.LE.KLF.AND.JI.GE.LK) THEN
            TIR(JI+(LK-KLS-1)*M1+MM) = TWOR
            TII(JI+(LK-KLS-1)*M1+MM) = - TWOI
         ENDIF
         IF (IJ.GT.KLS.AND.IJ.LE.KLF.AND.KL.GE.IJ) THEN
            TIR(KL+(IJ-KLS-1)*M1+MM) = TWOR
            TII(KL+(IJ-KLS-1)*M1+MM) = TWOI
         ENDIF
         IF (JI.GT.KLS.AND.JI.LE.KLF.AND.LK.GE.JI) THEN
            TIR(LK+(JI-KLS-1)*M1+MM) = TWOR
            TII(LK+(JI-KLS-1)*M1+MM) = - TWOI
         ENDIF
         ENDIF ! check on deleted spinors
C--> Time reversal right
         IF (I.NE.0.AND.J.NE.0.AND.KTR.NE.0.AND.LTR.NE.0) THEN
         TWOR = RKLR(INZ) * SignKL
         TWOI = RKLI(INZ) * SignKL
         IJ = (I-1) * NORB + J
         KL = (LTR-1) * NORB + KTR
         JI = (J-1) * NORB + I
         LK = (KTR-1) * NORB + LTR
         IF (KL.GT.KLS.AND.KL.LE.KLF.AND.IJ.GE.KL) THEN
            TIR(IJ+(KL-KLS-1)*M1+MM) = TWOR
            TII(IJ+(KL-KLS-1)*M1+MM) = TWOI
         ENDIF
         IF (LK.GT.KLS.AND.LK.LE.KLF.AND.JI.GE.LK) THEN
            TIR(JI+(LK-KLS-1)*M1+MM) = TWOR
            TII(JI+(LK-KLS-1)*M1+MM) = - TWOI
         ENDIF
         IF (IJ.GT.KLS.AND.IJ.LE.KLF.AND.KL.GE.IJ) THEN
            TIR(KL+(IJ-KLS-1)*M1+MM) = TWOR
            TII(KL+(IJ-KLS-1)*M1+MM) = TWOI
         ENDIF
         IF (JI.GT.KLS.AND.JI.LE.KLF.AND.LK.GE.JI) THEN
            TIR(LK+(JI-KLS-1)*M1+MM) = TWOR
            TII(LK+(JI-KLS-1)*M1+MM) = - TWOI
         ENDIF
         ENDIF ! check on deleted spinors
C--> Time reversal both
         IF (ITR.NE.0.AND.JTR.NE.0.AND.KTR.NE.0.AND.LTR.NE.0) THEN
         TWOR = RKLR(INZ) * SignIJ * SignKL
         TWOI = RKLI(INZ) * SignIJ * SignKL
         IJ = (JTR-1) * NORB + ITR
         KL = (LTR-1) * NORB + KTR
         JI = (ITR-1) * NORB + JTR
         LK = (KTR-1) * NORB + LTR
         IF (KL.GT.KLS.AND.KL.LE.KLF.AND.IJ.GE.KL) THEN
            TIR(IJ+(KL-KLS-1)*M1+MM) = TWOR
            TII(IJ+(KL-KLS-1)*M1+MM) = TWOI
         ENDIF
         IF (LK.GT.KLS.AND.LK.LE.KLF.AND.JI.GE.LK) THEN
            TIR(JI+(LK-KLS-1)*M1+MM) = TWOR
            TII(JI+(LK-KLS-1)*M1+MM) = - TWOI
         ENDIF
         IF (IJ.GT.KLS.AND.IJ.LE.KLF.AND.KL.GE.IJ) THEN
            TIR(KL+(IJ-KLS-1)*M1+MM) = TWOR
            TII(KL+(IJ-KLS-1)*M1+MM) = TWOI
         ENDIF
         IF (JI.GT.KLS.AND.JI.LE.KLF.AND.LK.GE.JI) THEN
            TIR(LK+(JI-KLS-1)*M1+MM) = TWOR
            TII(LK+(JI-KLS-1)*M1+MM) = - TWOI
         ENDIF
         ENDIF ! check on deleted spinors
      ENDDO
C
      GOTO 50
C
   60 IF (MINTB.EQ.0) WRITE (6,1010) 'MDBINT'
C
C     -----------------------------------------
C     ADD BREIT INTEGRALS AND COULOMB INTEGRALS
C     -----------------------------------------
C
      DO IJKL = 1, (KLF-KLS) * M1
         TIR(IJKL) = TIR(IJKL) + TIR(IJKL+MM)
         TII(IJKL) = TII(IJKL) + TII(IJKL+MM)
      ENDDO
C
   61 CONTINUE
C
C     ----------------------------------
C     WRITE 2-ELECTRON INTEGRALS TO FILE
C     ----------------------------------
C
      IJKL = 0
      DO 200 KL = KLS + 1, KLF
      TIR (KL+(KL-KLS-1)*M1) = 0.5D0 * TIR(KL+(KL-KLS-1)*M1)
      TII (KL+(KL-KLS-1)*M1) = 0.5D0 * TII(KL+(KL-KLS-1)*M1)
      NKL = 0
      DO 100 IJ = 1, M1
         IJKL = IJKL + 1
         IF (ABS(TIR(IJKL))+ABS(TII(IJKL)).GT.TRESINT) THEN
            NKL = NKL + 1
            INDK(NKL) = IJ
            RKLR(NKL) = TIR(IJKL)
            RKLI(NKL) = TII(IJKL)
          ENDIF
  100     CONTINUE
      NINT = NINT + NKL
      IF (REALAR) THEN
         WRITE (MRCTWOE) KL,NKL,(INDK(IX),IX=1,NKL),(RKLR(IX),IX=1,NKL)
      ELSE
         WRITE (MRCTWOE) KL,NKL,(INDK(IX),IX=1,NKL),(RKLR(IX),IX=1,NKL),
     &                   (RKLI(IX),IX=1,NKL)
      ENDIF
  200 CONTINUE
      KLS = KLF
      KLF = MIN0 (M1, KLF+M2)
      IF (KLS.NE.M1) GOTO 10
      IF (.NOT.BREIT) THEN
         WRITE (6,1000) MINT,DATEX,TIMEX,NINT
      ELSE
         WRITE(6,1001) MINT,DATEX,TIMEX,MINTB,DATEXB,TIMEXB,NINT
      ENDIF
      IF (NINT.GT.0) THEN
         WRITE (MRCTWOE) -NINT,0
      ELSE
         WRITE (MRCTWOE) -1,0
      ENDIF
      IF (REALAR) THEN
         WRITE (6,1002)
      ELSE
         WRITE (6,1003)
      ENDIF
C
      CLOSE (MDCINT,STATUS='KEEP')
      IF (BREIT) CLOSE (MDBINT,STATUS='KEEP')
      RETURN
 1000 FORMAT (
     &/I8,' Unique Coulomb integrals read from MDCINT (',A10,1X,A8,')'
     &/I8,' Coulomb integrals written to MRCTWOE')
 1001 FORMAT (
     &/I8,' Unique Coulomb integrals read from MDCINT (',A10,1X,A8,')'
     &/I8,' Unique Breit integrals read from MDBINT (',A10,1X,A8,')'
     &/I8,' Combined integrals written to MRCTWOE')
 1002 FORMAT (/' Real arithmetic will be used in EXPAND and TWOLIN')
 1003 FORMAT (/' Complex arithmetic will be used')
 1010 FORMAT (//' CAUTION : No non-zero integrals found on ',A//)
10000 STOP 'ERROR READING HEADER OF MDCINT'
10001 STOP 'ERROR READING HEADER OF MDBINT'
10010 STOP 'ERROR READING INTEGRALS FROM MDCINT'
10011 STOP 'ERROR READING INTEGRALS FROM MDBINT'
      END
