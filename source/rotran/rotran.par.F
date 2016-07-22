      PROGRAM ROTRAN
C     ====================================================== 
C     FIRST VERSION WRITTEN BY OLIVIER VISSER
C     THEORETICAL CHEMISTRY DEPARTMENT,
C     UNIVERSITY OF GRONINGEN (RUG),
C     NIJENBORGH 16, 9747 AG GRONINGEN, THE NETHERLANDS
C     MODIFIED AND OPTIMIZED JUNE-JULY 1994
C     LUUK VISSCHER
C     NASA AMES RESEARCH CENTER
C     MOFFETT FIELD
C     CA 94035-1000
C     PARALLEL VERSION BY M. PERNPOINTNER (OKT 1999)
C     THEORETICAL CHEMISTRY DEPARTMENT, RUG
C     ======================================================
      IMPLICIT INTEGER (A-Z)
      EXTERNAL INITC
C
#if defined (BIT64)
      PARAMETER (NITR=1,NRTC=2)
#else
      PARAMETER (NITR=2,NRTC=2)
#endif
      PARAMETER (NWORK=16 000 000)

C     the buffer size for the records in the SRTPQIL file
      PARAMETER (NBUFSZ=256)
C
C Instead of increasing NWORK one can also reduce NPQBUF
C
      REAL*8 RW(NWORK)
      CHARACTER*10 DATEX,TIMEX*8
      LOGICAL DOBREIT,EX1,EX2
C
      INCLUDE "inpt.inc"
      INCLUDE "molf.inc"
      INCLUDE "redme.inc"
      INCLUDE "files.inc"
      INCLUDE "vecs.inc"
      INCLUDE "mpidef.inc"
      INCLUDE "mpif.h"

      CHARACTER*11 FILNM,FILNO
      INTEGER IERR,IRCV
      INTEGER IDATOP    ! points to the end of the DA file !
      LOGICAL GETPQ,MOREPQ,INTAVL,GETHTI,GETHTBI,ERRFLG
      INTEGER ITCA(0:MAXNOD),NOCO
      INTEGER ISTAT(MPI_STATUS_SIZE)
      DATA ITCA  /MAXNOD*0,0/
      INTEGER ISUM,IDUM,JI3(3)
      REAL*8 TIMES,TIMEE,TIMEFH,TSYST

C ** define local memory calculation function
      IKB(I)=8*I/1024
 
C **  MPI Initialization section

      IERR = 0
      CALL MPI_INIT(IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,mpisiz,IERR)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD,mpirnk,IERR)
      IF (IERR.NE.0) call mpiqt('Error initializing MPI',0)
      IF (mpisiz.LT.2) THEN
         WRITE (*,'(A)') 'Please use serial code for this !'
         call MPI_FINALIZE(IERR)
         stop
      ENDIF
      IF (mpisiz.GT.MAXNOD) THEN
         WRITE (*,'(A)') 'Please enlarge MAXNOD parameter to ',mpisiz
         call MPI_FINALIZE(IERR)
         stop
      ENDIF

C ** say hello !

      CALL DAYTIME (DATEX,TIMEX)
      if (mpirnk.eq.0) WRITE (6, 1000) VERSION, DATEX, TIMEX  
 

C     ================================
C     COMMUNICATION CHECK START
C     ================================

      IF(mpirnk.eq.0) THEN     ! MASTER POLLS THROUGH THE NODES

C ** write title page if you are master

C
        WRITE(*,'(A)') '********************************'
        WRITE(*,'(A)') '****  START   COMMUNICATION ****'
        WRITE(*,'(A)') '*********   CHECK   ************'
        WRITE(*,'(A)') '********************************'
        WRITE(*,'(A)') ' '
        DO I=1,mpisiz-1
          CALL MPI_SEND(mpirnk,1,MPI_INTEGER,i,M_BELL_T,
     +    MPI_COMM_WORLD,IERR)
          if(IERR.ne.0) call mpiqt('MPI_SEND',8)
          CALL MPI_RECV(IRCV,1,MPI_INTEGER,i,M_IMHE_T,
     +    MPI_COMM_WORLD,ISTAT,IERR)
          if(IERR.ne.0) call mpiqt('MPI_RECV',8)
          if(IRCV.ne.i) call mpiqt('MPI information corrupted !',27)
          WRITE(*,'(A,I4,A)') '    ** NODE',i,'  READY.'
        ENDDO
        WRITE(*,'(A)') ' '
        WRITE(*,'(A)') '********************************'
        WRITE(*,'(A)') '****   END    COMMUNICATION ****'
        WRITE(*,'(A)') '*********   CHECK   ************'
        WRITE(*,'(A)') '********************************'
        WRITE(*,'(A)') ' '

      ELSE                 ! EVERY NODE COMMUNICATES ITS NODE # (RANK)

        CALL MPI_RECV(IRCV,1,MPI_INTEGER,0,M_BELL_T,MPI_COMM_WORLD,
     +  ISTAT,IERR)
        if(IERR.ne.0) call mpiqt('MPI_RECV',8)
        if(IRCV.ne.0) call mpiqt('MPI information corrupted !',27)
        CALL MPI_SEND(mpirnk,1,MPI_INTEGER,0,M_IMHE_T,MPI_COMM_WORLD,
     +  IERR)
        if(IERR.ne.0) call MPIQT('MPI_SEND',8)

      ENDIF

C     ===========================================
C     INITIALIZATION SECTION FOR MASTER AND SLAVE
C     ===========================================

      CALL INIT
      CALL READIN 
      CALL GETMOS
      CALL COMSYM

C ** determine for master/slave if we do Breit integrals as well

      IF (BREIT) THEN 
         IF (MFIL.NE.0) THEN
            BREIT = .FALSE.
            DOBREIT = .TRUE.
         ELSE
            BREIT = .TRUE.
            DOBREIT = .FALSE.
         ENDIF
      ELSE
         DOBREIT = .FALSE.
      ENDIF

C
C ** Calculate the basic boundaries for master and slave
C
      NSCTR = NSC*(NSC+1)/2
      NSPTR = NKR*(NKR+1)
      NSPSQ = NSP*NSP
      INTBUF = 12288 ! RELTWEL INTEGRAL BUFFER SIZE
      NPQBUF = 8192
      NPQSIZ = 512
      NIJBUF = NSPTR
C
C     Caution : When changing this parameter (16384), you should also 
C     change it in GETHTBI
C
      NIJSIZ = MIN0(NSCTR,16384)
      NSKBUF = NSPTR
      NSKSIZ = MIN0(NSPSQ,16384)

C ** if master give s brief overview over pointer sizes:

      IF(mpirnk.EQ.0) THEN
        WRITE(*,*) ' '
        WRITE(*,*) ' # OF SCALAR BASIS FUNCTIONS: ',NSC
        WRITE(*,*) ' # OF KRAMERS FUNCTIONS:      ',NKR
        WRITE(*,*) ' # OF SPINOR  FUNCTIONS:      ',NSP
        WRITE(*,*) ' NSCTR,NSPTR,NSPSQ: ',NSCTR,NSPTR,NSPSQ
        WRITE(*,*) ' NIJSIZ,NSKBUF,NSKSIZ: ',NIJSIZ,NSKBUF,NSPSQ
      ENDIF

C     ===============================================
C     END INITIALIZATION SECTION FOR MASTER AND SLAVE
C     ===============================================


C     ===========================================================
C     ENTRY POINT FOR THE TWO DIFFERENT TRANSFORMATION TYPES
C     (COULOMB/BREIT).THESE ARE COMPLETELY INDEPENDENT FROM EACH 
C     OTHER AND ALSO TWO DIFFERENT FILES MCDINT/MDBINT WILL 
C     BE GENERATED AFTER ALL TRANSFORMATIONS.
C     ===========================================================
   1  CONTINUE

      IF (.NOT.BREIT.and.mpirnk.eq.0) WRITE (6,1040) 'COULOMB'
      IF (BREIT.and.mpirnk.eq.0) WRITE (6,1040) 'BREIT'
 
      IF(mpirnk.EQ.0) THEN    ! NOW CONTROL THE TRANSFORMATIONS ON EACH NODE...

C**************************************************************************
C**************************************************************************
C**************                            ********************************
C**************     M A S T E R   C O D E  ********************************
C**************                            ********************************
C**************************************************************************
C**************************************************************************

C ** wait for the nodes until they have checked local files.
C ** browse each node...

      CALL CPUUSED(TIMES)

      ERRFLG=.FALSE.
      DO I=1,mpisiz-1
        CALL  MPI_RECV(IRCV,1,MPI_INTEGER,MPI_ANY_SOURCE,M_ERRS_T,
     +     MPI_COMM_WORLD,ISTAT,IERR)
        IF(IRCV.EQ.-1) THEN
           ERRFLG=.TRUE.
           WRITE(*,'(A,I5)') ' ** PROBLEM ON NODE',
     +         ISTAT(MPI_SOURCE)
           WRITE(*,'(A)') ' ** CHECK LOCAL OUTPUT FILE !'
        ELSE
           WRITE(*,'(A,I5,A)') 'NODE',ISTAT(MPI_SOURCE),
     +                ' FILES OK.'
        ENDIF
      ENDDO


      IXX=0
      IF(ERRFLG) THEN  ! ONE OR MORE NODES HAD SOME TROUBLE. WE STOP !
         WRITE(*,*) ' **'
         WRITE(*,*) ' ** ERROR OCCURRED ON ONE OR MORE NODES'
         WRITE(*,*) ' ** EXECUTION ABORTED ! **'
         WRITE(*,*) ' **'
         IXX=-1
         CALL MPI_BCAST(IXX,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
         CALL MPI_FINALIZE(IERR)
         STOP 1
      ENDIF
      CALL MPI_BCAST(IXX,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)

C ** from here nodes do individual transformations...

C ** provide memory for SKELOUT and SYMTRIZ steps

 7    LBRCSK = 1
      NBCK   = LBRCSK + ((NSPTR+1)+1)/NITR
      LBCK   = NBCK   + (NSKBUF+1)/NITR
      RL     = LBCK   + (NSKBUF*NSKSIZ+1)/NITR
      CBCK   = RL     +  NSPSQ*NRTC
      INDRJ  = CBCK   + NSKBUF*NSKSIZ*NRTC
      JNDRJ  = INDRJ  + (NSPSQ+1)/NITR
      INDKLP = JNDRJ  + (NSPSQ+1)/NITR
      INDKL  = INDKLP + (NSYMRP*NSYMRP*2+1)/NITR
      RJ     = INDKL  + (NSPSQ*2+1)/NITR
      REDME  = RJ     + NSPSQ*NRTC
C     Need extra array for reshuffling of rphase
      WORKRPH = REDME + NRMES*NRTC
      TOP = WORKRPH + NSP*NRTC

C ** check if we have enough memory. otherwise reduce NSKSIZ !

      IF (TOP.GT.NWORK) THEN
         IF (NSKSIZ.LE.128) THEN
            WRITE(*,*) 'OUT OF MEMORY FOR SYMSTEP !'
            WRITE(*,*) 'TOP, NWORK',TOP, NWORK
            WRITE(*,*) ' ** PROGRAM ABORTED **'
            CALL MPI_FINALIZE(IERR)
            STOP 1
         ELSE
            NSKSIZ=NSKSIZ/2
            GOTO 7
         ENDIF
      ENDIF

      NRJ = NSPSQ

C ** print out memory allocation table for control purposes

      if (iprnt.gt.0) then
      WRITE(*,*) ' '
      WRITE(*,*) '    Memory allocation table (MASTER) for SYMSTEP'
      WRITE(*,*) '    ============================================'
      WRITE(*,*) ' '
      WRITE(*,*) '            LBRCSK  =',IKB(NBCK-LBRCSK),' KB.'
      WRITE(*,*) '            NBCK    =',IKB(LBCK-NBCK),' KB.'
      WRITE(*,*) '            LBCK    =',IKB(RL-LBCK),' KB.'
      WRITE(*,*) '            RL      =',IKB(CBCK-RL),' KB.'
      WRITE(*,*) '            CBCK    =',IKB(INDRJ-CBCK),' KB.'
      WRITE(*,*) '            INDRJ   =',IKB(JNDRJ-INDRJ),' KB.'
      WRITE(*,*) '            JNDRJ   =',IKB(INDKLP-JNDRJ),' KB.'
      WRITE(*,*) '            INDKLP  =',IKB(INDKL-INDKLP),' KB.'
      WRITE(*,*) '            INDKL   =',IKB(RJ-INDKL),' KB.'
      WRITE(*,*) '            RJ      =',IKB(REDME-RJ),' KB.'
      WRITE(*,*) '            REDME   =',IKB(WORKRPH-REDME),' KB.'
      WRITE(*,*) '            WORKRPH =',IKB(TOP-WORKRPH),' KB.'
      WRITE(*,*) ' '
      WRITE(*,*) '            TOTAL   =',IKB(TOP-LBRCSK),' KB.'
      WRITE(*,*) '    ============================================'
      endif

C ** synchronize now the 1st HT steps before data collection ...

      ERRFLG=.FALSE.
      DO I=1,mpisiz-1
        CALL MPI_RECV(IRCV,1,MPI_INTEGER,MPI_ANY_SOURCE,
     +   M_JODO_T,MPI_COMM_WORLD,ISTAT,IERR)
       IF(IRCV.EQ.-1) ERRFLG=.TRUE.
      ENDDO
      IF(ERRFLG) THEN
          WRITE(*,*) ' ** DURING 1ST HALF STEP AN **'
          WRITE(*,*) ' ** ERROR OCCURRED ON A NODE. **'
          WRITE(*,*) ' ** EXECUTION TERMINATED **'
          CALL MPI_FINALIZE(IERR)
          STOP 1
      ENDIF

C ** open INTSKEL file accessed in SKELOUT/FLSHSK and clear arrays

#if defined (BIT64)
      IRECL = 8 + 8 + 16*NSKSIZ + 8*NSKSIZ
#else
      IRECL = 8 + 8 + 16*NSKSIZ + 4*NSKSIZ
#endif

      OPEN (INTSKEL,FILE=FNAME(16),ACCESS='DIRECT',RECL=IRECL)
      CALL IZERO (NSKBUF+1,RW(LBRCSK),1)
      CALL IZERO (NSKBUF,RW(NBCK),1)

C ** now master collects incomplete second HT integrals according
C ** to the JI labels

      IDUM=0
      JI = 0
      DO 220 IBAR=1,2
      DO 210 IK=1,NKR
         IF (IBAR.EQ.1) THEN
            I = KRA(IK,1)
            IA = KRA(IK,2)
         ELSE
            I = KRB(IK,1)
            IA = KRB(IK,2)
         ENDIF
         DO 200 JK=IK,NKR
            J = KRB(JK,1)
            IB = KRB(JK,2)
            JI = JI + 1

C ** signal compound index to be processed

       JI3(1)=JI
       JI3(2)=IA
       JI3(3)=IB
       CALL MPI_BCAST(JI3,3,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)

C ** check if there are integrals
       
       ISUM=0
       CALL MPI_REDUCE(IDUM,ISUM,1,MPI_INTEGER,
     + MPI_SUM,0,MPI_COMM_WORLD,IERR)

C ** collect transformed integrals from each node

       CALL CZERO(NSP*NSP,RW(RL))
       CALL CZERO(NSP*NSP,RW(RJ))
       CALL MPI_REDUCE(RW(RJ),RW(RL),NSP*NSP,MPI_DOUBLE_COMPLEX,
     +  MPI_SUM,0,MPI_COMM_WORLD,IERR)

C ** write the specific JI stream to INTSKEL. The buckets NBCK,LBCK,CBCK
C ** are filled and written out in case of completeness !
   
       IF(ISUM.EQ.0) GOTO 200
       CALL SKELOUT(NSP,NKR,J,I,JI,NSKSIZ,NSKBUF,RW(NBCK),
     +  RW(LBCK),RW(LBRCSK),RW(RL),RW(CBCK))

 200  CONTINUE
 210  CONTINUE
 220  CONTINUE

C ** all JI pairs done, flush INTSKEL buffers and signal end to slaves

      JI3(1)=-999
      JI3(2)=0
      JI3(3)=0
      CALL MPI_BCAST(JI3,3,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)

      CALL FLSHSK (IRECL,NSKSIZ,NSKBUF,
     +             RW(NBCK),RW(LBCK),RW(LBRCSK),RW(CBCK))

      WRITE(*,*) ' '
      WRITE(*,*) 'SYMMETRIZING SKELETON MATRIX.'
      WRITE(*,*) ' '

C -------------------------------------------------------------------
C ---- DO SYMMETRIZATION STEP (EXCLUSIVELY EXECUTED ON MASTER NODE !)
C -------------------------------------------------------------------

      CALL CPUUSED(TSYST)
      CALL INITTWOE (IPRNT,BREIT,NKR,NSP,MDINT,RW(WORKRPH))
      CALL SYMTRIZ (NSYMRP,MDINT,RW(LBRCSK),NRJ,RW(INDRJ),RW(JNDRJ),
     &             RW(INDKLP),RW(INDKL),RW(RJ),RW(RL),RW(REDME))
      CLOSE (INTSKEL,STATUS='DELETE')
      CALL EXITTWOE (MDINT)

      WRITE(*,*) ' '
      WRITE(*,'(A)') '  Timing on Master node'
      WRITE(*,'(A)') '  ====================='
      WRITE(*,*) ' '
      CALL CPUUSED(TIMEE)
      WRITE(*,*) 'SYMMETRIZATION STEP:',TIMEE-TSYST,' SEC.'
      WRITE(*,*) ' '
      IF (.NOT.BREIT) THEN
        WRITE(*,*) 'TOTAL MASTER TIME COULOMB :',TIMEE-TIMES,' SEC.'
      ELSE
        WRITE(*,*) 'TOTAL MASTER TIME BREIT   :',TIMEE-TIMES,' SEC.'
      ENDIF
      WRITE(*,*) ' '
      WRITE(*,*) '     ============================================='
      WRITE(*,*) ' '


      ELSE


C**********************************************************
C**********************************************************
C**************                            ****************
C**************     S L A V E     C O D E  ****************
C**************                            ****************
C**********************************************************
C**********************************************************

C -------------------------------------------------
C ---- do all the necessary checks and report  ----
C ---- error status to master. if there is an  ----
C ---- error all slaves stop due to the BCAST  ----
C -------------------------------------------------

      IF (mpirnk .EQ. 0) THEN
         WRITE (FILNM,'(A7)') FNAME(17)
         WRITE (FILNO,'(A7)') FNAME(18)
      ELSEIF (mpirnk .LT. 10) THEN
         WRITE (FILNM,'(A7,A1,I1)') FNAME(17),'.',mpirnk
         WRITE (FILNO,'(A7,A1,I1)') FNAME(18),'.',mpirnk
      ELSEIF (mpirnk .LT. 100) THEN
         WRITE (FILNM,'(A7,A1,I2)') FNAME(17),'.',mpirnk
         WRITE (FILNO,'(A7,A1,I2)') FNAME(18),'.',mpirnk
      ELSE
         WRITE (FILNM,'(A7,A1,I3)') FNAME(17),'.',mpirnk
         WRITE (FILNO,'(A7,A1,I3)') FNAME(18),'.',mpirnk
      ENDIF

      INQUIRE (FILE = FILNM, EXIST = EX1)
      INQUIRE (FILE = FILNO, EXIST = EX2)

      IF( .NOT.(EX1.AND.EX2)) THEN
        IXX=-1
        WRITE(*,*) 'DIRECT ACCESS OR INFO FILE MISSING !'
        GOTO 333
      ENDIF

#if defined (BIT64)
      IRECL=8+8+8+NBUFSZ*8 + NBUFSZ*8
#else
      IRECL=4+4+4+NBUFSZ*8 + NBUFSZ*4
#endif

C     WRITE(*,*) 'NODE',mpirnk,' OPENING ',FILNO
      OPEN(UNIT=SRTPQLN,FILE=FILNO,ACCESS='SEQUENTIAL',
     + STATUS='UNKNOWN',FORM='UNFORMATTED')
      READ(SRTPQLN) LRECCO,IRECX,ILSTA,ILEND,ISSTA,ISEND
      CLOSE(SRTPQLN)

      IF(IRECX.NE.IRECL) THEN
        IXX=-1
        WRITE(*,*) 'NON MATCHING RECORD LENGTH IN DA FILE ON NODE ',
     +  mpirnk
        GOTO 333
      ENDIF

      IF(LRECCO.EQ.0) THEN
        IXX=-1
        WRITE(*,*) 'NO INTEGRALS ON NODE ',mpirnk
        GOTO 333
      ENDIF

C **  if everything is all right, we send this.
      IXX=0

 333  CALL MPI_SEND(IXX,1,MPI_INTEGER,0,M_ERRS_T,MPI_COMM_WORLD,IERR)

C ** wait for confirmation that all nodes do well. Then we go off...

      CALL MPI_BCAST(IXX,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
      IF(IXX.NE.0) THEN
        WRITE(*,*) ' **'
        WRITE(*,*) ' ** EXECUTION TERMINATED ON NODE',mpirnk
        WRITE(*,*) ' **'
        CALL MPI_FINALIZE(IERR)
        STOP 1
      ENDIF

C     WRITE(*,*) 'NODE',mpirnk,' OPENING ',FILNM
      OPEN(UNIT=SRTPQIL,FILE=FILNM,ACCESS='DIRECT',RECL=IRECL,
     + STATUS='UNKNOWN',FORM='UNFORMATTED')

      if (iprnt.gt.0) then
      WRITE(*,*) ' '
      WRITE(*,*) 'PARAMETER OF FILE ',FILNM
      WRITE(*,*) '    # OF RECORDS:  ',LRECCO
      WRITE(*,*) '    RECORD LENGTH: ',IRECL
      WRITE(*,*) '    LARGE START/LARGE END INDEX: ',ILSTA,ILEND
      WRITE(*,*) '    SMALL START/SMALL END INDEX: ',ISSTA,ISEND
      WRITE(*,*) ' '
      endif

C--------------------------------------
C-----  FIRST HALF TRANSFORMATION -----
C--------------------------------------

      CALL CPUUSED(TIMES)

C ** provide the required memory

      LBRCIJ = 1
      IRDCFM = LBRCIJ + ((NSPTR+1)+1)/NITR  ! space for LBRCIJ
      IPNTIN = IRDCFM + (LRECCO+1)/NITR     ! space for record indicator
      IPNTLA = IPNTIN + NSCTR               ! space for pq integrals
      IPRBUF = IPNTLA + 2*(NSCTR+1)/NITR    ! space for sorted/unpckd R/S l.
      IPLBUF = IPRBUF + NBUFSZ              ! space for "cache int. buffer"
      TOP    = IPLBUF + (NBUFSZ+1)/NITR     ! space for "cache label buffer"
      IF (TOP.GT.NWORK) THEN
         PRINT*, "TOP, NWORK",TOP, NWORK
         CALL Q2JOB (4,'DA file handling',' ',0)
      ENDIF
C ** new top entry point is starting address of NBCK !
      NBCK = TOP+1


 110  IF (.NOT.BREIT) THEN
          LBCK = NBCK + (NIJBUF+1)/NITR
          CBCK = LBCK + (NIJBUF*NIJSIZ+1)/NITR
          HA   = CBCK + NIJBUF*NIJSIZ*NRTC
      ELSE
          LBCK = NBCK + 2*(NIJBUF+1)/NITR
          CBCK = LBCK + (NIJBUF*NIJSIZ+1)/NITR
          HA   = CBCK + 6*NIJBUF*NIJSIZ
      ENDIF
      RI = HA + NSC*NSP*NRTC
      RIA = RI + NSPSQ*NRTC
      RIB = RIA + NSPSQ*NRTC
      TOP = RIB + NSPSQ*NRTC
      IF (TOP.GT.NWORK) THEN
         IF (NIJSIZ.LE.128) THEN
            PRINT*, "TOP, NWORK",TOP, NWORK
            CALL Q2JOB (4,'Trans1',' ',0)
         ELSE
            NIJSIZ=NIJSIZ/2
            GOTO 110
         ENDIF
      ENDIF

C ** inform user about  memory demands 

      if (iprnt.gt.0) then
      WRITE(*,*) ' '
      WRITE(*,*) '    Memory allocation table 1. HT'
      WRITE(*,*) '    ============================='
      WRITE(*,*) ' '
      WRITE(*,*) '         LBRCIJ =',IKB(IRDCFM-LBRCIJ),' KB.'
      WRITE(*,*) '         IRDCFM =',IKB(IPNTIN-IRDCFM),' KB.'
      WRITE(*,*) '         IPNTIN =',IKB(IPNTLA-IPNTIN),' KB.'
      WRITE(*,*) '         IPNTLA =',IKB(IPRBUF-IPNTLA),' KB.'
      WRITE(*,*) '         IPRBUF =',IKB(IPLBUF-IPRBUF),' KB.'
      WRITE(*,*) '         IPLBUF =',IKB(NBCK-IPLBUF),' KB.'
      WRITE(*,*) '         NBCK   =',IKB(LBCK-NBCK),' KB.'
      WRITE(*,*) '         LBCK   =',IKB(CBCK-LBCK),' KB.'
      WRITE(*,*) '         CBCK   =',IKB(HA-CBCK),' KB.'
      WRITE(*,*) '         HA     =',IKB(RI-HA),' KB.'
      WRITE(*,*) '         RI     =',IKB(RIA-RI),' KB.'
      WRITE(*,*) '         RIA    =',IKB(RIB-RIA),' KB.'
      WRITE(*,*) '         RIB    =',IKB(TOP-RIB),' KB.'
      WRITE(*,*) ' '
      WRITE(*,*) '   TOTAL MEMORY USED FOR 1. HALF-TRANSFORMATION',
     + IKB(TOP-LBRCIJ),' KB.'
      WRITE(*,*) ' '
      endif

C *** clear the relevant arrays

      CALL SR2INI(NIJBUF,NIJSIZ,RW(LBRCIJ))
      CALL CLRDCF(LRECCO,RW(IRDCFM))
      CALL IZERO (NIJBUF,RW(NBCK),1)
      IF (BREIT) CALL IZERO (2*NIJBUF,RW(NBCK),1)

C ** transform all pq streams available on this node ...

      IDATOP=LRECCO

 150  MOREPQ =  GETPQ(IPQL,NINT,IDATOP,NSCTR,RW(IRDCFM),RW(IPNTIN),
     +     RW(IPNTLA),RW(IPRBUF),RW(IPLBUF))
      CALL TRFDA1(NSCTR,NIJSIZ,NIJBUF,IPQL,NINT,RW(LBRCIJ),RW(IPNTLA),
     +     RW(NBCK),RW(LBCK),RW(IPNTIN),RW(HA),RW(RI),RW(RIA),
     +     RW(RIB),RW(CBCK))

      IF(MOREPQ) GOTO 150
 
C ***  flush remaining buckets of the (PQ|IJ) integrals
C ***  to the INTTWO file. No INTSRT occurs anymore in this process !

      IF (.NOT.BREIT) THEN
        CALL SR2FLSH(NIJBUF,NIJSIZ,RW(LBRCIJ),RW(NBCK),RW(LBCK),
     &     RW(CBCK))
      ELSE
        CALL SR2FLSB(NIJBUF,NIJSIZ,RW(LBRCIJ),RW(NBCK),RW(LBCK),
     &     RW(CBCK))
      ENDIF

C ** now measure time for first half transformation

      CALL CPUUSED(TIMEFH)
      if (iprnt.gt.0) then
      WRITE(*,*) ' '
      IF (.NOT.BREIT) THEN
        WRITE(*,*) 'NODE',mpirnk,' TIME FOR COULOMB 1st HT:',
     + TIMEFH-TIMES,' SEC.'
      ELSE
        WRITE(*,*) 'NODE',mpirnk,' TIME FOR BREIT 1st HT:',
     + TIMEFH-TIMES,' SEC.'
      ENDIF
      endif
      CLOSE(SRTPQIL,STATUS='KEEP')

C ** node specific INTTWO file finished. 
C ** File was reopened by the SR2FLSH/SR2FLSB routines. Is locally open now !
C ** SYNC !   ... and
C ** now we wait for the desired JI index which should be half-transformed.
C ** after this step we immediately communicate the RL(NSP,NSP) to master !

      IXX=0
      CALL MPI_SEND(IXX,1,MPI_INTEGER,0,M_JODO_T,
     +    MPI_COMM_WORLD,IERR)

C ------------------------------------
C ---- SECOND HALF TRANSFORMATION ----
C ------------------------------------

C ** provide required memory for 2nd HT

    2 TOP    = LBRCIJ + ((NSPTR+1)+1)/NITR
      LBRCSK = TOP    + 1
      INDRJ  = LBRCSK + ((NSPTR+1)+1)/NITR
      NBCK   = INDRJ  + (NSCTR*2+1)/NITR
      LBCK   = NBCK   + (NSKBUF+1)/NITR
      INDBUF = LBCK   + (NSKBUF*NSKSIZ+1)/NITR
      RJ     = INDBUF + (NSCTR + 1)/NITR
      RJB    = RJ     + NSCTR*NRTC
      RJC    = RJB    + NSCTR*NRTC
      RK     = RJC    + NSCTR*NRTC
      RLR    = RK     + NSP*NSC*NRTC
      RL     = RLR    + NSPSQ*NRTC
      CBCK   = RL     + NSPSQ*NRTC
      TOP    = CBCK   + NSKBUF*NSKSIZ*NRTC
      IF (TOP.GT.NWORK) THEN
         IF (NSKSIZ.LE.128) THEN
            PRINT*, "TOP, NWORK",TOP, NWORK
            CALL Q2JOB (4,'Trans2',' ',0)
         ELSE
            NSKSIZ=NSKSIZ/2
            GOTO 2
         ENDIF
      ENDIF

C ** inform user about  memory demands 

      if (iprnt.gt.0) then
      WRITE(*,*) ' '
      WRITE(*,*) '    Memory allocation table 2. HT'
      WRITE(*,*) '    ============================='
      WRITE(*,*) ' '
      WRITE(*,*) '       LBRCIJ =',IKB(LBRCSK-LBRCIJ),' KB.'
      WRITE(*,*) '       LBRCSK =',IKB(INDRJ-LBRCSK),' KB.'
      WRITE(*,*) '       INDRJ  =',IKB(NBCK-INDRJ),' KB.'
      WRITE(*,*) '       NBCK   =',IKB(LBCK-NBCK),' KB.'
      WRITE(*,*) '       LBCK   =',IKB(INDBUF-LBCK),' KB.'
      WRITE(*,*) '       INDBUF =',IKB(RJ-INDBUF),' KB.'
      WRITE(*,*) '       RJ     =',IKB(RJB-RJ),' KB.'
      WRITE(*,*) '       RJB    =',IKB(RJC-RJB),' KB.'
      WRITE(*,*) '       RJC    =',IKB(RK-RJC),' KB.'
      WRITE(*,*) '       RK     =',IKB(RLR-RK),' KB.'
      WRITE(*,*) '       RLR    =',IKB(RL-RLR),' KB.'
      WRITE(*,*) '       RL     =',IKB(CBCK-RL),' KB.'
      WRITE(*,*) '       CBCK   =',IKB(TOP-CBCK),' KB.'
      WRITE(*,*) ' '
      WRITE(*,*) ' '
      WRITE(*,*) '   TOTAL MEMORY USED FOR 2. HALF-TRANSFORMATION',
     + IKB(TOP-LBRCIJ),' KB.'
      WRITE(*,*) ' '
      endif

C ** Local memory is set up and we start transformation by getting the
C ** various JI compund indices from the master ...

 250  CALL MPI_BCAST(JI3,3,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
      JI=JI3(1)
      IF(JI.EQ.-999) GOTO 252
      IA=JI3(2)
      IB=JI3(3)

      CALL TRFDA2(NSCTR,NSKSIZ,NSKBUF,JI,IA,IB,ISUM,RW(LBRCIJ),
     &  RW(INDRJ),RW(NBCK),RW(LBCK),RW(RJ),RW(RJB),RW(RJC),RW(RK),
     &           RW(RLR),RW(RL),RW(CBCK))

C ** send information about integrals to master. Transformed integrals
C ** are in the RL resp. RW(RL) area...

      CALL MPI_REDUCE(ISUM,IDUM,1,MPI_INTEGER,
     + MPI_SUM,0,MPI_COMM_WORLD,IERR)
      CALL MPI_REDUCE(RW(RL),RW(RJ),NSP*NSP,MPI_DOUBLE_COMPLEX,
     + MPI_SUM,0,MPI_COMM_WORLD,IERR)
      IF(IERR.NE.0) THEN
        WRITE(*,*) 'MPI ERROR (REDUCE) IN ROTRAN.F !'
        CALL MPI_FINALIZE(IERR)
        STOP 1
      ENDIF

      GOTO 250

C ** 2nd HT finished

 252  CONTINUE      
      CLOSE(INTTWO,STATUS='DELETE')

      WRITE(*,'(A,I5)') ' Total timing on node',mpirnk
      WRITE(*,*) ' ========================='
      WRITE(*,*) ' '
      CALL CPUUSED(TIMEE)
      IF (.NOT.BREIT) THEN
        WRITE(*,*) 'ND',mpirnk,' COULOMB 1st HT:',TIMEFH-TIMES,' SEC.'
        WRITE(*,*) 'ND',mpirnk,' COULOMB 2nd HT:',TIMEE-TIMEFH,' SEC.'
        WRITE(*,*) 'ND',mpirnk,' COULOMB TOTAL:',TIMEE-TIMES,' SEC.'
      ELSE
        WRITE(*,*) 'ND',mpirnk,' BREIT 1st HT:',TIMEFH-TIMES,' SEC.'
        WRITE(*,*) 'ND',mpirnk,' BREIT 2nd HT:',TIMEE-TIMEFH,' SEC.'
        WRITE(*,*) 'ND',mpirnk,' BREIT TOTAL:',TIMEE-TIMES,' SEC.'
      ENDIF

C------------------------------------------------------
C----------- END OF SLAVE CODE ------------------------
C------------------------------------------------------

      ENDIF     !   SLAVE CODE ENDS HERE !!

C -----------------------------------------------
C ---     CONTINUATION OF COMMON CODE
C ---          FROM HERE ! 
C -----------------------------------------------

      IF (DOBREIT) THEN 
         BREIT = .TRUE.
         DOBREIT = .FALSE.
         GOTO 1
      ENDIF

      CALL MPI_FINALIZE(IERR)
      CALL DAYTIME (DATEX,TIMEX)
      if (mpirnk.eq.0) WRITE (6,1020) DATEX,TIMEX

C ** ROTRAN banner

 1000 FORMAT (' ***********************************'/
     +        ' *                                 *'/
     +        ' *  R O T R A N    VS ',A10,   '   *'/
     +        ' *                                 *'/
     +        ' *  First version written by :     *'/
     +        ' *  Olivier Visser                 *'/
     +        ' *  University of Groningen        *'/
     +        ' *                                 *'/
     +        ' *  Extended version written by:   *'/
     +        ' *  Luuk Visscher                  *'/
     +        ' *  NASA Ames Research Center      *'/
     +        ' *                                 *'/
     +        ' *  Parallel version written by:   *'/
     +        ' *  Markus Pernpointner            *'/
     +        ' *  University of Groningen        *'/
     +        ' *                                 *'/
     +        ' ***********************************'///
     +        ' Program start: ',A10,2X,A8/)
 1020 FORMAT (/' Program end  : ',A10,2X,A8)
 1040 FORMAT ( ' '/
     +         ' ---------------------------------------'/
     +         ' ---  STARTING TRANSFORMATION OF -------'/
     +         ' ------ ',A7,'  INTEGRALS -------------'/
     +         ' ---------------------------------------'/
     +         ' '/)

      END
