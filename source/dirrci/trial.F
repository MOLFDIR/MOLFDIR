C
      SUBROUTINE TRIAL (VECTR,VECTI,IDIND,JDET,JELEC,JSIND,
     &                  IVWU,DGEL)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'param.inc'
      INCLUDE 'general.inc'
      INCLUDE 'files.inc'
      INCLUDE 'symm.inc'
      INCLUDE 'iterat.inc'
C
      PARAMETER (NDREC=1023)
#if defined (BIT64)
      PARAMETER (LREC=24*(NDREC+1))
#else
      PARAMETER (LREC=20*(NDREC+1))
#endif
C
      DIMENSION IDIND(*),JDET(*),JELEC(*)
      DIMENSION VECTR(NDET,*),VECTI(NDET,*),DGEL(NDET)
C
      CHARACTER*11 VECFIL
C
      OPEN (MRCFINV,FILE=FNAM(5),ACCESS='DIRECT',RECL=LREC)
C
      IF (ISTART.GT.1) GOTO 300
      VECFIL = FNAM(4)(1:6)//'_'//REPNA(IREP)
      IF (VECFIL(8:8).EQ.'''') VECFIL(8:8) = '1'
      IF (VECFIL(8:8).EQ.'"') VECFIL(8:8) = '2'
      IOS = 0
      OPEN (MRCTRIV,FILE=VECFIL,ACCESS='DIRECT',STATUS='OLD'
#if defined (BIT64)
     &      ,RECL=8,ERR=300,IOSTAT=IOS)
#else
     &      ,RECL=4,ERR=300,IOSTAT=IOS)
#endif
      IF (IOS.NE.0) GOTO 300
      READ (MRCTRIV,REC=1,ERR=300,IOSTAT=IOS) LENREC
      IF (IOS.NE.0) GOTO 300
      CLOSE (MRCTRIV)
      OPEN (MRCTRIV,FILE=VECFIL,ACCESS='DIRECT',RECL=LENREC,ERR=9999)
      READ (MRCTRIV,REC=1,ERR=9999) LENREC,NTRIV
      READ (MRCTRIV,REC=2,ERR=9999) NCOEF,(JDET(J),J=1,NCOEF)
C
C     ----------------------------------------------------------
C     CHECK IF THE TRIAL VECTORS CAN BE USED IN THIS CALCULATION
C     ----------------------------------------------------------
C
      NIDET = NOVERI(NORBR(2),NELEC-NORBR(1))
      IF (NIDET.LT.NCOEF.OR.NDET.LT.NCOEF)
     &CALL Q2JOB (2,'TRIAL VECTORS CAN NOT BE USED',' ',0)
      WRITE (6,1010)
      CALL DETIND (NCOEF,IDIND,JDET,JELEC,JSIND,IVWU)
C
C     ------------------------------------------------------------
C     WRITE REFERENCE DETERMINANTS TO FILE FOR DAVIDSON CORRECTION
C     ------------------------------------------------------------
C
      WRITE(MRCFINV,REC=2) NCOEF,(IDIND(J),J=1,NCOEF)
C
      DO 200 IROOT = 1, NROOTS
	 IVEC = IROOT
         CALL ZZERO (NDET,VECTR(1,IVEC))
         IF (.NOT.REALAR) CALL ZZERO (NDET,VECTI(1,IVEC))
         ISEL = NSEL(IROOT)
         IF (ISEL.GT.NTRIV) CALL Q2JOB
     &      (2,'TRIAL VECTOR NOT ON FILE',' ',0)
         IREC = ISEL + 2
         READ (MRCTRIV,REC=1,ERR=9999) LENREC,NTRIV,(DUM,I=1,ISEL-1),
     &                                 EIGEN1(IROOT)
         EIGEN2(IROOT) = EIGEN1(IROOT)
         IF (REALAR) THEN
            READ (MRCTRIV,REC=IREC,ERR=9999)
     &       (VECTR(IDIND(J),IVEC),VDUM,J=1,NCOEF)
         ELSE
            READ (MRCTRIV,REC=IREC,ERR=9999)
     &       (VECTR(IDIND(J),IVEC),VECTI(IDIND(J),IVEC),J=1,NCOEF)
         ENDIF
         NVEC = NVEC + 1
         CALL ORTHN (VECTR,VECTI)
         WRITE (6,1000) NSEL(IROOT),EIGEN1(IROOT)
  200    CONTINUE
      RETURN
 1000 FORMAT (/' Vector #',I4,' read from MRCTRIV; Eigenvalue ',F20.12)
 1010 FORMAT (/' WARNING !    Trial vector coefficients adapted'/
     &' Be sure that : NORBR(2,OLD)=NORBR(2,NEW) AND ',
     &'NORBR(1,OLD)=0')
C     ---------------------------------------------------------------
C     NO CORRECT FILE FOUND : MAKE START VECTOR BY LOOKING FOR LOWEST
C     DIAGONAL ELEMENTS.
C     ---------------------------------------------------------------
  300 open (83,file='Intermediate_results',form='FORMATTED')
      WRITE (6,1020)
      write (83,1020)
      IF (ISTART.EQ.2) THEN
         DO 320 IROOT = 1, NROOTS
            IVEC = IROOT
#if defined (BIT64)
            INDMIN(IROOT) = ISMIN(NDET,DGEL,1)
#else
            INDMIN(IROOT) = IDMIN(NDET,DGEL,1)
#endif
            EIGEN1(IROOT) = DGEL(INDMIN(IROOT)) + ECORE
            EIGEN2(IROOT) = EIGEN1(IROOT)
            CALL ZZERO (NDET,VECTR(1,IVEC))
            IF (.NOT.REALAR) CALL ZZERO (NDET,VECTI(1,IVEC))
            VECTR(INDMIN(IROOT),IVEC) = DGEL(INDMIN(IROOT))
            DGEL(INDMIN(IROOT)) = 0.D0
            WRITE (6,1030) IROOT,INDMIN(IROOT),EIGEN1(IROOT)
            write (83,1030) IROOT,INDMIN(IROOT),EIGEN1(IROOT)
  320    CONTINUE
C
         DO 340 IROOT = 1, NROOTS
	    IVEC = IROOT
            DGEL(INDMIN(IROOT)) = VECTR(INDMIN(IROOT),IVEC)
            VECTR(INDMIN(IROOT),IVEC) = 1.D0
            CALL PUTVEC (IVEC,VECTR(1,IVEC),VECTI(1,IVEC))
  340    CONTINUE
         WRITE(MRCFINV,REC=2) NROOTS,(INDMIN(J),J=1,NROOTS)
      ELSE
         DO 350 IROOT = 1, NROOTS
            IVEC = IROOT
            INDMIN(IROOT) = IROOT
            CALL ZZERO (NDET,VECTR(1,IVEC))
            IF (.NOT.REALAR) CALL ZZERO (NDET,VECTI(1,IVEC))
            VECTR(IROOT,IVEC) = 1.D0
            CALL PUTVEC (IVEC,VECTR(1,IVEC),VECTI(1,IVEC))
            EIGEN1(IROOT) = DGEL(IROOT) + ECORE
            EIGEN2(IROOT) = EIGEN1(IROOT)
            WRITE (6,1030) IROOT,IROOT,EIGEN1(IROOT)
            WRITE (83,1030) IROOT,IROOT,EIGEN1(IROOT)
  350    CONTINUE
         ISTART=3
         WRITE(MRCFINV,REC=2) NROOTS,(INDMIN(J),J=1,NROOTS)
      ENDIF
      close (83,status='KEEP')
      NVEC = NROOTS
C
      CLOSE(MRCFINV,STATUS='KEEP')
C
 1020 FORMAT
     & (/' Start vectors are single determinants')
 1030 FORMAT (/' Root',i4,' is determinant',i4,' Exp. Value',F20.12)
      RETURN
 9999 CALL Q2JOB (1,'FILE MRCTRIV',' ',0)
      RETURN
      END
