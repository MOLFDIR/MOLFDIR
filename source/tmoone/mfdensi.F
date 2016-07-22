C
C     ==================
      SUBROUTINE MFDENSI
C     ==================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     GET DENSITY MATRICES FROM STARTVECTORS (IF PRESENT)
C
      INCLUDE 'param.inc'
      INCLUDE 'common.inc'
#if defined (PARALLEL)
      INCLUDE 'mpidef.inc'
#else
      PARAMETER (mpirnk=0)
#endif
C
      EQUIVALENCE (TITLE,FORMAT)
      CHARACTER*80 TITLE
C
      COMPLEX*16 VMO (N22 * 2)
      LOGICAL JDUM
      CHARACTER*80 CARD, FILE1*12
      INTEGER IDUM(17)
C
C     -----------------------------------
C     READ FORMATTED VECTORS FROM MFDVECA
C     -----------------------------------
C
      IF (mpirnk .EQ. 0) THEN
         WRITE (FILE1,'(A7)') FNAM(4)
      ELSEIF (mpirnk .LT. 10) THEN
         WRITE (FILE1,'(A7,A1,I1)') FNAM(4),'.',mpirnk
      ELSEIF (mpirnk .LT. 100) THEN
         WRITE (FILE1,'(A7,A1,I2)') FNAM(4),'.',mpirnk
      ELSE
         WRITE (FILE1,'(A7,A1,I3)') FNAM(4),'.',mpirnk
      ENDIF

      OPEN(MFDVECA,FILE=FNAM(8),FORM='FORMATTED')
      OPEN(MFDSYMC,FILE=FILE1,FORM='UNFORMATTED',STATUS='OLD',
     +             ERR=10040)
      REWIND MFDVECA
      REWIND MFDSYMC
C
      READ (MFDVECA,1000,END=30,ERR=30) TITLE
      READ (MFDVECA,1005,END=10020,ERR=10020) D0,T0
      READ (MFDVECA,1006,END=10020,ERR=10020) ICYCLS,JDUM,JDUM
      READ (MFDVECA,1007,END=10020,ERR=10020) ETOTAL,EPREV,EMDIF,
     +                                       WDIFP,TOTDIF,DUM
      READ (MFDVECA,1000,END=10020) CARD
      READ (MFDVECA,1008,END=10020,ERR=10020) IDUM(1)
      READ (MFDVECA,1009,END=10020,ERR=10020) (IDUM(I+1),I=1,IDUM(1))
      READ (MFDVECA,1009,END=10020,ERR=10020) (IDUM(I+1),I=1,IDUM(1))
      WDIF = WDIFP
      KCYCLS = 0
C
      WRITE(*,1010) FNAM(8),TITLE,D0,T0,ICYCLS,
     +              ETOTAL,EPREV,EMDIF,WDIFP,TOTDIF
C
      LENREC=8 * MAX0(4*(NL+NS)+4,80)
      OPEN(MFDVECB,FILE=FNAM(2),ACCESS='DIRECT',RECL=LENREC)
C
      WRITE(MFDVECB,REC=1) MAX0(4*(NL+NS)+4,80),D0,T0,ICYCLS,KCYCLS,
     +                     ETOTAL,EPREV,EMDIF,WDIFP,TOTDIF
      WRITE(MFDVECB,REC=2) SCFTXT
      READ (CARD, 1001, ERR=30) FORMAT
      IF (FORMAT.EQ.' ') FORMAT='(6F22.16)'
      MVBREC = 2
C
      DO 20 IRP=1,NSYMRP
        NBSM = NBSIM (IRP)
        IF (IAVIRP(IRP).EQ.IRP) READ (MFDVECA, 1002) IDUMMY,
     +                                       NSPINOR,IDUMMY
        NSKP(IRP)=NSPINOR-NCOR(IRP)-NACT(IRP)
C
        DO 10 MO=1,NCOR(IRP)
          IF (IAVIRP(IRP).EQ.IRP) THEN
             READ (MFDVECA, 2220) ALLEW (IRP, MO)
 2220        FORMAT (10X, G20.10)
             READ (MFDVECA, FORMAT, END = 30, ERR = 30)
     +            (VMO (I), I = 1, NBSM)
          ELSE
              ALLEW (IRP,MO) = ALLEW (IAVIRP(IRP),MO)
              READ (MFDVECB,REC=MVBREC-NCOR(IRP)+1)  JDUM,JDUM,LV,DUM,
     +                                               (VMO(I),I=1,NBSM)
          ENDIF
          MVBREC=MVBREC+1
#if defined (BIT64)
          WRITE(MFDVECB,REC=MVBREC) INDSRP(IRP),IRP,NBSM,0.0,
#else
          WRITE(MFDVECB,REC=MVBREC) INDSRP(IRP),IRP,NBSM,0.0D0,
#endif
     +                              (VMO(I),I=1,NBSM)
   10   CONTINUE
        IF (IAVIRP(IRP).EQ.IRP) THEN
           DO 11 MO = 1, NACT(IRP) + NSKP(IRP)
             READ (MFDVECA, 2220) DUM
             READ (MFDVECA, FORMAT, END = 30, ERR = 30)
     +            (VMO (I), I = 1, NBSM)
   11      CONTINUE
         ENDIF
C
   20 CONTINUE
      CLOSE(MFDVECA)
      CLOSE(MFDVECB)
      CALL MFDENSS
      RETURN
C
   30 CLOSE(MFDVECA)
      CALL Q2JOB(2,'NO CORRECT VECTORS ON MFDVECA',' ',0)
      RETURN
 1000 FORMAT(A)
 1001 FORMAT(20A)
 1002 FORMAT(I4,26X,2I4)
 1005 FORMAT(2A10)
 1006 FORMAT(I4,3X,L1,3X,L1)
 1007 FORMAT(6G20.10)
 1008 FORMAT(I4)
 1009 FORMAT(16I4)
 1010 FORMAT(//' VECTORS TAKEN FROM FILE ',A7//
     $         ' FILE HEADING : ',A/
     $         ' SCF RUN DATE : ',A10/
     $         ' SCF RUN TIME : ',A8//
     $         ' FILE INFORMATION : '/
     $         ' CYCLE COUNT ',T25,I20/
     $         ' ETOTAL',T25,G20.10/
     $         ' EPREV ',T25,G20.10/
     $         ' EMDIF',T25,G20.10/
     $         ' WDIF ',T25,G20.10/
     $         ' TOTDIF',T25,G20.10)
10020 CALL Q2JOB(2,'END OR ERROR ON FILE MFDVECA   ',' ',0)
10040 CALL Q2JOB(2,'MFDENSI:FILE MFDSYMC CANNOT BE ACCESSED',' ',0)
      END
