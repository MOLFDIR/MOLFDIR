C
C     ===============
      SUBROUTINE INIT
C     ===============
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE "molf.inc"
      INCLUDE "files.inc"
      INCLUDE "sym.inc"
#if defined (PARALLEL)
      INCLUDE "mpidef.inc"
#else
      PARAMETER (mpirnk=0)
#endif
C
      INTEGER IPARAM(8)
      CHARACTER*8 T,D,NAME*80 
      CHARACTER*12 FILE1,FILE2
C
C
C     ----------------------
C     READ DATA FROM MOLFDIR
C     ----------------------
C
      IF (mpirnk .EQ. 0) THEN
         WRITE (FILE1,'(A7)') FNAME(11)
         WRITE (FILE2,'(A7)') FNAME(10)
      ELSEIF (mpirnk .LT. 10) THEN
         WRITE (FILE1,'(A7,A1,I1)') FNAME(11),'.',mpirnk
         WRITE (FILE2,'(A7,A1,I1)') FNAME(10),'.',mpirnk
      ELSEIF (mpirnk .LT. 100) THEN
         WRITE (FILE1,'(A7,A1,I2)') FNAME(11),'.',mpirnk
         WRITE (FILE2,'(A7,A1,I2)') FNAME(10),'.',mpirnk
      ELSE
         WRITE (FILE1,'(A7,A1,I3)') FNAME(11),'.',mpirnk
         WRITE (FILE2,'(A7,A1,I3)') FNAME(10),'.',mpirnk
      ENDIF
      OPEN(MFD1OUT,FILE=FILE1,STATUS='OLD',
     +     FORM='UNFORMATTED',ERR=20000)
      REWIND MFD1OUT
      READ (MFD1OUT)     T,D,IPARAM(1),NAME
      READ (MFD1OUT)     (IPARAM(I),I=1,8)
C
C     --------------------------------------
C     CHECK PARAMETERS FOR INCOMPATIBILITIES
C     --------------------------------------
C
      IF (IPARAM(1).GT.N14) CALL Q2JOB(5,'INIT','N14',IPARAM(1))
      IF (IPARAM(5).GT.N7 ) CALL Q2JOB(5,'INIT','N7 ',IPARAM(5))
C
      READ (MFD1OUT) NEQ1,NEQUIV,NEQCO1,NEQCOM,NGAUS,NCOMB,NSYMOP,
     +               NNUCL,NSYMF,NCOEFF,NSYMRP,NWRDS1,NWRDS2,NWRDS3
      READ (MFD1OUT) (DUMMY,(RDUM,J=1,3),I=1,NNUCL)
      READ (MFD1OUT) DUMMY
      READ (MFD1OUT) (KCOMB(I),INDBAS(I),INDSYM(I),I=1,NEQUIV)
      READ (MFD1OUT) DUMMY
      READ (MFD1OUT) (ICENT(I),ITYPE(I),
     +               (ICOMOP(I,J),J=1,NSYMOP),I=1,NCOMB),
     +               (INDCOM(I),I=1,NEQCOM)
      READ (MFD1OUT) (INDORB(I),INDB(I),KSYMRP(I),
     +                LSYMRP(I),KSYMF(I),I=1,NSYMF)
      READ (MFD1OUT) DUMMY
      READ (MFD1OUT) (NBSYM(I),NBSYM1(I),NBSYM2(I),I=1,NSYMRP)
      INDCOM(NEQCOM+1)=NCOMB
      INDBAS(NEQUIV+1)=NGAUS
      INDSYM(NEQUIV+1)=NSYMF
      INDORB(NSYMF+1)=NCOEFF
C
C     DETERMINE NEM AND NSC
C
      NSC=0
      DO 20 IM=1,NEQUIV
        IT=KCOMB(IM)
        IND=INDCOM(IT)
        KND=INDCOM(IT+1)
        NBAS=KND-IND
        NEM(IM)=NSC
        NSC=NSC+NBAS
   20   CONTINUE
      NEM(NEQUIV+1)=NSC
C
      OPEN(MFDSYMC,FILE=FILE2,FORM='UNFORMATTED',STATUS='OLD')
C
      REWIND(MFDSYMC)
C.... skip transformation coefficients and abelian subgroup information
      DO 100 I=1,NSYMF+5
        READ(MFDSYMC)
 100    CONTINUE
C
      READ(MFDSYMC) (IDIM(I),I=1,NSYMRP),(ITHIRP(I),I=1,NSYMRP),
     +                                   (IPRTNR(I),I=1,NSYMRP)
      NREP = ITHIRP(NSYMRP)
      JREP = 0
      DO 101 I = 1, NSYMRP
      IREP = ITHIRP(I)
      IF (IREP.NE.JREP) IOFFRP(IREP)=I
      JREP = IREP
 101  CONTINUE
      IOFFRP(NREP+1)=NSYMRP+1
C
 110  READ(MFDSYMC,END=120,ERR=120) IRP,IRPDIM,ISYMOP,
     +    ((REPMAT(I,J,ISYMOP,ITHIRP(IRP)),J=1,IRPDIM),I=1,IRPDIM)
#if defined (BIT64)
      REPCHR(ISYMOP,ITHIRP(IRP)) = (0.0, 0.0)
#else
      REPCHR(ISYMOP,ITHIRP(IRP)) = (0.D0, 0.D0)
#endif
      DO 111 I = 1, IRPDIM
      REPCHR(ISYMOP,ITHIRP(IRP)) = REPCHR(ISYMOP,ITHIRP(IRP)) +
     +                             REPMAT(I,I,ISYMOP,ITHIRP(IRP))
 111  CONTINUE
      GOTO 110
 120  CONTINUE
C
      CLOSE(MFDSYMC)
      RETURN
20000 CALL Q2JOB(1,'INIT: UNABLE TO OPEN MFD1OUT',' ',0)
      END
