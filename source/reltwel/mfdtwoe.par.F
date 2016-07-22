C
C     ==================
      SUBROUTINE MFDTWOE
C     ==================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'work.inc'
      INCLUDE 'paracom.inc'
      INCLUDE 'mpif.h'
      INCLUDE 'mpidef.inc'
C
      COMMON/BUFR/GINTEG(N2,14)
#if defined (BIT64)
      COMMON/BUFI/LABEL(N2,14)
#else
      COMMON/BUFI/LABEL(N2,14),LABEL1(N2,14)
#endif
C
#if defined (IBM)
      INTEGER*8 NUMBLL,NUMBSS,NUMBR,NUMBL,NUMBS,NUMBSL,NUMBZ,NUMBE
      INTEGER AND,OR,SHIFT
#endif
      INTEGER NI(3),NK(3),NL(3),NN(3)
      DIMENSION IDEM(8),NTYPE(14),ILST(4)
      LOGICAL DOGAM,DOIK,DOBREI
      LOGICAL IEQK,IEQL,IEQN,KEQL,KEQN,LEQN,YDEM(8),SPECIAL

      INTEGER IPQINV(2,N14),IPQCAR(5),INDINV(2,N7+1)
      INTEGER NBUCK(NPQBUF),LBUCK(NPQSIZ,NPQBUF),LBR(NPQBUF)
      REAL*8  RBUCK(NPQSIZ,NPQBUF)
      INTEGER NUMILL,NUMISL,NUMISS,NUMIBR
      INTEGER NUMOLL,NUMOSL,NUMOSS,NUMOBR
      INTEGER LRECCO,LIX,NIX,LIXS,LIXE
      CHARACTER*11 FILNM,FILNO
      CHARACTER*7  FNAME,RNAME
      CHARACTER*9  INFO(4)
      PARAMETER (FNAME='SRTPQIL')
      PARAMETER (RNAME='SRTPQLN')
      DATA INFO /'  (LL|LL)','  (SS|LL)','  (SS|SS)','  (SL|SL)'/

C------------ MPI VAIRABLES ---
      INTEGER IERR,ISTAT(MPI_STATUS_SIZE)
C------------------------------
#if defined (CRAY)
      ISHIFT (I, J) = SHIFT (I, J)
#else
      ISHIFT (I, J) = ISHFT (I, J)
#endif

C     --------------------------------
C     --- MASTER PRINTS OUT INFORMATION ABOUT 
C     --- TOTAL NUMBER OF INTEGRALS
C     --------------------------------
      IF(mpirnk.EQ.0) THEN
        NUMBL = NEM(NEQ1) + INDCOM(KCOMB(NEQ1)+1)
     &        - INDCOM(KCOMB(NEQ1))
        NUMBS = NEM(NEQUIV) + INDCOM(KCOMB(NEQUIV)+1)
     &        - INDCOM(KCOMB(NEQUIV)) - NUMBL
        NUMBR = NUMBS*NUMBL
        NUMBL = NUMBL*(NUMBL+1)/2
        NUMBS = NUMBS*(NUMBS+1)/2
        NUMBLL= NUMBL*(NUMBL+1)/2
        NUMBSL= NUMBS*NUMBL
        NUMBSS= NUMBS*(NUMBS+1)/2
        NUMBR = NUMBR*(NUMBR+1)/2
        PRINT 850,NUMBLL,NUMBSL,NUMBSS,NUMBR
  850   FORMAT(//' CALCULATING TWO ELECTRON INTEGRALS ',//,11X,
     $       '(LL/LL)',7X,'(SS/LL)',8X,'(SS/SS)',7X,'(SL/SL)',
     $       /,1X,'TOTAL',1X,I11,2X,I12,2X,I13,2X,I12/)
      ENDIF

C     -------------------------------------------
C     --- FROM HERE ALL NODES (MASTER INCLUSIVE) DO THE INITIALIZATION
C     --- OF ALL THE IMPORTANT POINTER ARRAYS
C     -------------------------------------------

      MAXTOP = 0
      NUMILL=0
      NUMISL=0
      NUMISS=0
      NUMIBR=0
      NUMOLL=0
      NUMOSL=0
      NUMOSS=0
      NUMOBR=0

C     ------------------------------------------------------
C     --- DETERMINE NUMBER OF SCALAR BASIS FUNCTIONS NSC ---
C     ------------------------------------------------------
      NSC=0
      DO IM=1,NEQUIV
        IT=KCOMB(IM)
        IND=INDCOM(IT)
        KND=INDCOM(IT+1)
        NBAS=KND-IND
        NSC=NSC+NBAS
      ENDDO
      IF(NSC.GT.N14) CALL  Q2JOB(3,'OVERFLOW IN # OF BASISF.',
     + 'N14',NSC)

C     -------------------------
C     --- COMPUTE IPQINV,INDINV
C     -------------------------
      ICO=0
      DO I=1,NEQCOM
        INDINV(1,I)=ICO+1
        IS=INDCOM(I)+1
        IT=INDCOM(I+1)
        DO II=IS,IT 
          DO III=KCBINV(II,1),KCBINV(II,2)
            ICO=ICO+1
            IF(ICO.GT.N14) CALL Q2JOB(3,'MFDTWOE.F','N14',ICO)
            IPQINV(1,ICO)=IS
            IPQINV(2,ICO)=IT
C           write(*,*) '     ipqinv: ',ico,ipqinv(1,ico),ipqinv(2,ico)
          ENDDO
        ENDDO
        INDINV(2,I)=ICO
      ENDDO


C     -------------------------------------------
C     --- CHECK BOUNDARIES FOR BUCKETS VIA INDINV
C     -------------------------------------------
      IBMAX=0
      DO LIX=1,NEQCOM
        DO NIX=1,LIX
          IPS=INDINV(1,LIX)
          IPE=INDINV(2,LIX)
          IQS=INDINV(1,NIX)
          IQE=INDINV(2,NIX)
          IBAS=IQE-IQS+1
          IU=(IPE-IPS+1)*IBAS + IBAS
          IF(IU.GT.NPQBUF) CALL Q2JOB(3,'NUMBER OF BUCKETS',
     +     'NPQBUF',IU)
          IF(IU.GT.IBMAX) IBMAX=IU
        ENDDO
      ENDDO


      IF(mpirnk.EQ.0) THEN

C ******************************************************************
C *****                                                       ******
C *****    M A S T E R   C O N T R O L     C O D E            ******
C *****                                                       ******
C ******************************************************************

C ** check the allowed ranges for ICR/ICMX

      IF( (ICR.LT.1).OR.(ICMX.GT.4).OR.(ICR.GT.ICMX)) THEN
        WRITE(*,*) ' ** ERROR IN THE ICR/ICMX RANGES **'
        CALL MPIQT(' ',0)
      ENDIF
      DOBREI=.FALSE.
      IF(ICMX.EQ.4) DOBREI=.TRUE.

      WRITE(*,*) 'PROGRAM WILL CALCULATE THE FOLLOWING INTEGRALS:'
      WRITE(*,*) '==============================================='
      WRITE(*,*)
      WRITE(*,*) (INFO(II),II=ICR,ICMX)
      WRITE(*,*)

c ** calculate the index ranges for the distribution
C ** skip, if user wants to calculate breit ints only

      IF(ICR.EQ.4) GOTO 795
      IF(ICMX.LE.2) THEN     ! LL|LL OR SS|LL WANTED !
        LIXS=1
        LIXE=NEQCO1
      ELSEIF (ICR.LE.2) THEN ! SEC. PAIR HAS TO BE LL AND SS !!
        LIXS=1
        LIXE=NEQCOM
      ELSE                   ! SEC. PAIR ONLY HAS TO BE SS !!
        LIXS=NEQCO1+1
        LIXE=NEQCOM
      ENDIF
     
      WRITE(*,*)
      WRITE(*,*) 'MAXIMUM NUMBER OF BUCKETS TO PROVIDE: ',IBMAX
      WRITE(*,*)
      WRITE(*,*) 'DISTRIBUTION LIST (SECOND INDEX PAIR)'
      WRITE(*,*) '====================================='
      WRITE(*,*)

C     ---------------------------------------------
C     --- LOOP OVER THE PQ/RANGES TO BE DISTRIBUTED
C     ---------------------------------------------

      IPQCAR(5)=0

      DO 777 LIX=LIXS,LIXE
        NIXS=1
        IF(LIX.GE.(NEQCO1+1)) NIXS=NEQCO1+1
        DO 777 NIX=NIXS,LIX
C       write(*,*) 'lix,nix:',lix,nix

      IPS=INDINV(1,LIX)
      IPE=INDINV(2,LIX)
      IQS=INDINV(1,NIX)
      IQE=INDINV(2,NIX)
      IPQCAR(1)=IPS
      IPQCAR(2)=IPE
      IPQCAR(3)=IQS
      IPQCAR(4)=IQE

C     --- wait for a node available and send the p/q range to be computed

      CALL MPI_RECV(NODA,1,MPI_INTEGER,MPI_ANY_SOURCE,M_AFDA_T,
     +      MPI_COMM_WORLD,ISTAT,IERR)
      IF(IERR.NE.0) CALL MPIQT('MPI_RECV IN MFDTWOE.F (MASTER)',0)
      IF(NODA.NE.ISTAT(MPI_SOURCE)) CALL MPIQT('NODE CONSISTENCY',0)
      CALL MPI_SEND(IPQCAR,5,MPI_INTEGER,NODA,M_ACDA_T,
     +    MPI_COMM_WORLD,IERR)
      IF(IERR.NE.0) CALL MPIQT('MPI_SEND IN MFDTWOE.F (MASTER)',0)
C     WRITE(*,'(A,4I4,A,I4)') '  MASTER TRANSFERRED: ',IPS,IPE,IQS,IQE,
C    + '   TO NODE ',NODA

 777  CONTINUE

      IF(.NOT.DOBREI) GOTO 796

 795  CONTINUE

C ** yes, we do the breit integrals (either separately or additionally !)

      DO 778 LIX=NEQCO1+1,NEQCOM
        DO 778 NIX=1,NEQCO1

      IPS=INDINV(1,LIX)
      IPE=INDINV(2,LIX)
      IQS=INDINV(1,NIX)
      IQE=INDINV(2,NIX)
      IPQCAR(1)=IPS
      IPQCAR(2)=IPE
      IPQCAR(3)=IQS
      IPQCAR(4)=IQE

C     --- wait for a node available and send the p/q range to be computed

      CALL MPI_RECV(NODA,1,MPI_INTEGER,MPI_ANY_SOURCE,M_AFDA_T,
     +      MPI_COMM_WORLD,ISTAT,IERR)
      IF(IERR.NE.0) CALL MPIQT('MPI_RECV IN MFDTWOE.F (MASTER)',0)
      IF(NODA.NE.ISTAT(MPI_SOURCE)) CALL MPIQT('NODE CONSISTENCY',0)
      CALL MPI_SEND(IPQCAR,5,MPI_INTEGER,NODA,M_ACDA_T,
     +    MPI_COMM_WORLD,IERR)
      IF(IERR.NE.0) CALL MPIQT('MPI_SEND IN MFDTWOE.F (MASTER)',0)
      WRITE(*,'(A,4I4,A,I4)') '  MASTER TRANSFERRED: ',IPS,IPE,IQS,IQE,
     + '   TO NODE ',NODA

 778  CONTINUE

C     --- wait until all nodes have finished their integral calculation
C     --- and send a finished signal

 796  CONTINUE
      IPQCAR(5)=-1
      DO IIX=1,mpisiz-1
        CALL MPI_RECV(NODA,1,MPI_INTEGER,MPI_ANY_SOURCE,M_AFDA_T,
     +      MPI_COMM_WORLD,ISTAT,IERR)
        IF(IERR.NE.0) CALL MPIQT('MPI_RECV IN MFDTWOE.F (MASTER)',0)
        IF(NODA.NE.ISTAT(MPI_SOURCE)) CALL MPIQT('NODE INCONSISTENCY',0)
        CALL MPI_SEND(IPQCAR,5,MPI_INTEGER,NODA,M_ACDA_T,
     +    MPI_COMM_WORLD,IERR)
        IF(IERR.NE.0) CALL MPIQT('MPI_SEND IN MFDTWOE.F (MASTER)',0)
      ENDDO
      WRITE(*,*)
      WRITE(*,*) '================================'
      WRITE(*,*) 'MASTER : ALL PROCESSES FINISHED.'
      WRITE(*,*)

C     --- collect return status from each node 
      
      DO NODA=1,mpisiz-1
        CALL MPI_RECV(IRTS,1,MPI_INTEGER,NODA,M_IRTS_T,MPI_COMM_WORLD,
     +             ISTAT,IERR)
        IF(IERR.NE.0) CALL MPIQT('MPI_RECV IN MFDTWOE.F',0)
        IF(IRTS.NE.0) THEN 
          WRITE(*,*) 'ERROR ON NODE',NODA,' DURING ITEGRAL EVALUATION'
          CALL MPIQT('PROGRAM ABORTED DUE TO ERROR',0)
        ENDIF
      ENDDO

C ** synchronize all nodes and get statistics

      CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
      IF(IERR.NE.0) CALL MPIQT('MPI_BARRIER IN MFDTWOE.F',0)

      WRITE(*,*)
      WRITE(*,*)
      WRITE(*,*) '          Computation statistics of the nodes '
      WRITE(*,*) '          ===================================='
      WRITE(*,*)
      WRITE(*,'(A6,5A12)') 'NODE','(LL|LL)','(SS|LL)','(SS|SS)',
     +                     '(SL|SL)','NODE SUM'
      WRITE(*,*)

      DO NODA=1,mpisiz-1
        CALL MPI_RECV(ILST,4,MPI_INTEGER,NODA,M_ACDA_T,
     +                MPI_COMM_WORLD,ISTAT,IERR)
        IF(IERR.NE.0) CALL MPIQT('MPI_RECV IN MFDTWOE.F',0)
        WRITE(*,'(I6,5I12)') NODA,ILST(1),ILST(2),ILST(3),ILST(4),
     + ILST(1)+ILST(2)+ILST(3)+ILST(4)
        NUMOLL=NUMOLL+ILST(1)
        NUMOSL=NUMOSL+ILST(2)
        NUMOSS=NUMOSS+ILST(3)
        NUMOBR=NUMOBR+ILST(4)
      ENDDO
      WRITE(*,'(2A)') ' -----------------------------------------',
     +'------------------------'
      WRITE(*,'(A6,5I12)') 'TOTAL',NUMOLL,NUMOSL,NUMOSS,NUMOBR,
     + NUMOLL+NUMOSL+NUMOSS+NUMOBR
      WRITE(*,*)
      WRITE(*,*)
C     --- end of master code ---

      ELSE

C ******************************************************************
C *****                                                       ******
C *****  S L A V E   E X E C U T I O N    C O D E             ******
C *****                                                       ******
C ******************************************************************


C     ----------------------------------------
C     --- OPEN THE NODE-SPECIFIC INTEGRAL FILE
C     --- THE FILE REMAINS OPEN UNTIL ALL P/Q-
C     --- RANGES HAVE BEEN PROCESSED !
C     ----------------------------------------
      IF (mpirnk .EQ. 0) THEN
         WRITE (FILNM,'(A7)') FNAME
         WRITE (FILNO,'(A7)') RNAME
      ELSEIF (mpirnk .LT. 10) THEN
         WRITE (FILNM,'(A7,A1,I1)') FNAME,'.',mpirnk
         WRITE (FILNO,'(A7,A1,I1)') RNAME,'.',mpirnk
      ELSEIF (mpirnk .LT. 100) THEN
         WRITE (FILNM,'(A7,A1,I2)') FNAME,'.',mpirnk
         WRITE (FILNO,'(A7,A1,I2)') RNAME,'.',mpirnk
      ELSE
         WRITE (FILNM,'(A7,A1,I3)') FNAME,'.',mpirnk
         WRITE (FILNO,'(A7,A1,I3)') RNAME,'.',mpirnk
      ENDIF
#if defined (BIT64)
      IRECL=8 + 8 + 8 + NPQSIZ*8 + NPQSIZ*8
#else
      IRECL=4 + 4 + 4 + NPQSIZ*8 + NPQSIZ*4
#endif
      INTUN=40+mpirnk
C     OPEN(UNIT=INTUN,FILE=FILNM,ACCESS='SEQUENTIAL',
C    + FORM='UNFORMATTED',STATUS='UNKNOWN',ERR=8888)
      OPEN(UNIT=INTUN,FILE=FILNM,ACCESS='DIRECT',RECL=IRECL)
      LRECCO=0   ! EVERY SLAVE COUNTS THE RECORDS FOR ITS INDIVIDUAL FILE
      IRTS=0     ! SO FAR EVERYTHING IS STILL OK


C     --- signal availability
C     --- 
C     ---  MAIN ENTRY POINT FOR THE INFINITE LOOP
C     --- 

 800  CALL MPI_SEND(mpirnk,1,MPI_INTEGER,0,M_AFDA_T,
     +    MPI_COMM_WORLD,IERR)
      IF(IERR.NE.0) CALL MPIQT('MPI_SEND IN MFDTWOE.F (SLAVE)',0)
C     --- wait until master has ackn. request. get data
      CALL MPI_RECV(IPQCAR,5,MPI_INTEGER,0,M_ACDA_T,
     +      MPI_COMM_WORLD,ISTAT,IERR)
      IF(IERR.NE.0) CALL MPIQT('MPI_RECV IN MFDTWOE.F (SLAVE)',0)
C     --- check if there are any more blocks
      IF(IPQCAR(5).EQ.-1) GOTO 4000

C     --- now data are available. start computation

      IPS=IPQCAR(1)
      IPE=IPQCAR(2)
      IQS=IPQCAR(3)
      IQE=IPQCAR(4)
      ILS=IPQINV(1,IPS)
      ILE=IPQINV(2,IPE)
      INS=IPQINV(1,IQS)
      INE=IPQINV(2,IQE)
      IBAS=IQE-IQS+1
      IPOFF=IPS
      IQOFF=IQS
      IBATN=(IPE-IPS+1)*IBAS
C     WRITE(*,*) ' '
C     WRITE(*,*) 'RANGE OF SECOND INDEX PAIR:',IPS,IPE,IQS,IQE
C     WRITE(*,*) '# OF P/Q BATCHES (MAX) IN THIS RUN:',IBATN


C     ---------------------------------
C     --- CLEAR BUCKETS/RECORD POINTER
C     --- AND RECORD COUNTER ARRAY. THIS
C     --- MEANS: INDEXING ONLY VALID WITHIN ONE P/Q RANGE !!
C     --- ACCESS TO ALL P/QS IN THE DA-FILE HAS TO BE A LITTLE
C     --- BIT DIFFERENT
C     ---------------------------------

      DO I=1,NPQBUF
        NBUCK(I)=0
        LBR(I)=0
      ENDDO

      DO 1330 ID = ICR, ICMX

      IF (ID.EQ.1) THEN
          NEQX=NEQ1
          NEQY=NEQ1
          IKM=1
          LNM=1
      ELSEIF(ID.EQ.2) THEN
          NEQX=NEQUIV
          NEQY=NEQ1
          IKM=NEQ1+1
          LNM=1
          IF (ICR.NE.2) THEN
             CALL CPUUSED(SEC2)
             EXECS=SEC2-SEC1
C            PRINT 860,'(LL/LL)',NUMBZ,NUMBE,EXECS
             NUMILL=NUMILL+NUMBE
             NUMOLL=NUMOLL+NUMBZ
             SEC1=SEC2
          ENDIF
      ELSEIF(ID.EQ.3) THEN
          NEQX=NEQUIV
          NEQY=NEQUIV
          IKM=NEQ1+1
          LNM=NEQ1+1
          IF (ICR.NE.3) THEN
             CALL CPUUSED(SEC2)
             EXECS=SEC2-SEC1
C            PRINT 860,'(SS/LL)',NUMBZ,NUMBE,EXECS
             NUMISL=NUMISL+NUMBE
             NUMOSL=NUMOSL+NUMBZ
             SEC1=SEC2
          ENDIF
      ELSEIF(ID.EQ.4) THEN
          NEQX=NEQUIV
          NEQY=NEQ1
          IKM=NEQ1+1
          LNM=1
          IF (ICR.NE.4) THEN
             CALL CPUUSED(SEC2)
             EXECS=SEC2-SEC1
C            PRINT 860,'(SS/SS)',NUMBZ,NUMBE,EXECS
             NUMISS=NUMISS+NUMBE
             NUMOSS=NUMOSS+NUMBZ
             SEC1=SEC2
          ENDIF
      ENDIF
      NUMBZ = 0
      NUMBE = 0
      NSCF = 0
      ICNTOLD = 0
      KCNTOLD = 0
      LCNTOLD = 0
      NCNTOLD = 0
      ISOLD = 0
      KSOLD = 0
      LSOLD = 0
      NSOLD = 0
C     --------------------------------
C     DETERMINE STARTPOINTS OF LOOPS :
C     --------------------------------
      ICTI=KCOMB(IKM)
      KCTI=INDCOM(ICTI)
      MCTIK=KCTI+1
      ICTL=KCOMB(LNM)
      KCTL=INDCOM(ICTL)
      MCTLN=KCTL+1
      MCTI=MAX0(MCTIK,IR)
      MCTK=MAX0(MCTIK,KR)
      MCTL=MAX0(MCTLN,LR)
      MCTN=MAX0(MCTLN,NR)
      IF (ID.EQ.4) THEN
          MCTK = MAX0(MCTLN,KR)
          MCTL = MAX0(MCTIK,LR)
      ENDIF
      IR=1
      KR=1
      LR=1
      NR=1
C     -------------------------------
C     START LOOPS OVER FUNCTION TYPES
C     -------------------------------
      ICTI=KCOMB(NEQX)
      NCTI=INDCOM(ICTI+1)
      ICTL=KCOMB(NEQY)
      DO 1030 I=MCTI,NCTI
      IM=KCBINV(I,1)
      ICONTR=KCBINV(I,2)-IM+1
      IS=INDBAS(IM)+1
      IF=INDBAS(IM+1)
      IPRIM = IF - IS + 1
      ICPRI = NCORE(IM,1)
      ICCON = NCORE(IM,2)
      ICNT=ICENT(I)
      ITYP = ITYPE(I)
      NI(1) = NX(ITYP)
      NI(2) = NY(ITYP)
      NI(3) = NZ(ITYP)
      NCTK=I
      IF (ID.EQ.4) NCTK = INDCOM(ICTL+1 )
      DO 1020 K=MCTK,NCTK
      KM=KCBINV(K,1)
      KCONTR=KCBINV(K,2)-KM+1
      KS=INDBAS(KM)+1
      KF=INDBAS(KM+1)
      KPRIM = KF - KS + 1
      KCPRI = NCORE(KM,1)
      KCCON = NCORE(KM,2)
      KCNT=ICENT(K)
      KTYP = ITYPE(K)
      NK(1) = NX(KTYP)
      NK(2) = NY(KTYP)
      NK(3) = NZ(KTYP)
      IF ((ID.EQ.2.OR.ID.EQ.3).AND.
     &    (KCNT.NE.ICNT.AND.ONECEN)) GOTO 1020
      IEQK=I.EQ.K
      NCTL=MIN0( INDCOM(ICTL+1) ,I )
      IF (ID.EQ.4) NCTL = I
      DOIK = .TRUE.
      DO 1010 L=MCTL,NCTL
      LM=KCBINV(L,1)
      LCONTR=KCBINV(L,2)-LM+1
      LS=INDBAS(LM)+1
      LF=INDBAS(LM+1)
      LPRIM = LF - LS + 1
      LCPRI = NCORE(LM,1)
      LCCON = NCORE(LM,2)
      LCNT=ICENT(L)
      LTYP = ITYPE(L)
      NL(1) = NX(LTYP)
      NL(2) = NY(LTYP)
      NL(3) = NZ(LTYP)
      IF ((ID.EQ.3.OR.ID.EQ.4).AND.
     &    (LCNT.NE.ICNT.AND.ONECEN)) GOTO 1010
      IEQL=I.EQ.L
      KEQL=K.EQ.L
      NCTN=L
      IF (ID.EQ.4) NCTN = NCTK
      IF (IEQL) NCTN=K
      DO 1000 N=MCTN,NCTN
      NM=KCBINV(N,1)
      NCONTR=KCBINV(N,2)-NM+1
      NS=INDBAS(NM)+1
      NF=INDBAS(NM+1)
      NPRIM = NF - NS + 1
      NCPRI = NCORE(NM,1)
      NCCON = NCORE(NM,2)
      NCNT=ICENT(N)
      NTYP = ITYPE(N)
      NN(1) = NX(NTYP)
      NN(2) = NY(NTYP)
      NN(3) = NZ(NTYP)
C     ----------------------------------------------------------------
C     Calculate sum of l values : in one center case we need only half
C     of the incomplete gamma function values
C     ----------------------------------------------------------------
      MS = 0
      DO KT = 1, 3
      MSKT = NI(KT) + NK(KT) + NL(KT) + NN(KT)
      IF (ICNT.EQ.KCNT.AND.LCNT.EQ.NCNT.AND.ICNT.EQ.LCNT) MSKT = MSKT/2
      MS = MS + MSKT
      ENDDO
      IF ((ID.EQ.3).AND.
     &    (NCNT.NE.ICNT.AND.ONECEN)) GOTO 1000
      IEQN=I.EQ.N
      KEQN=K.EQ.N
      LEQN=L.EQ.N
C     ----------------------------------------------------
C     DETERMINE SYMMETRY EQUIVALENCES AND CHECK FOR ZERO'S
C     ----------------------------------------------------
      DO 20 IX=1,8
   20 IDEM(IX)=0
      SPECIAL=.FALSE.
      IMM=I*(I-1)/2+K
      LMM=L*(L-1)/2+N
      INDEX=(IMM)*(IMM-1)/2+LMM
      DO 980 M=1,NSYMOP
      IT=ICOMOP(I,M)
      KT=ICOMOP(K,M)
      LT=ICOMOP(L,M)
      NT=ICOMOP(N,M)
      SN=1.0
      IF (IT.GT.0) GOTO 880
      IT=-IT
      SN=-SN
  880 IF (KT.GT.0) GOTO 890
      KT=-KT
      SN=-SN
  890 IF (LT.GT.0) GOTO 900
      LT=-LT
      SN=-SN
  900 IF (NT.GT.0) GOTO 910
      NT=-NT
      SN=-SN
  910 IF (I-IT) 1030,920,950
  920 IF (K-KT) 1020,930,950
  930 IF (L-LT) 1010,940,950
  940 IF (N-NT) 1000,955,955
  950 I1=MAX0(IT,KT)
      I2=MIN0(IT,KT)
      I1=I1*(I1-1)/2+I2
      L1=MAX0(LT,NT)
      L2=MIN0(LT,NT)
      L1=L1*(L1-1)/2+L2
      II1=MAX0(I1,L1)
      LL1=MIN0(I1,L1)
      INN=II1*(II1-1)/2+LL1
      GOTO 958
  955 INN=INDEX+NT-N
  958 CONTINUE
      IF (INN.GT.INDEX) GOTO 1000
  970 IF (INN.EQ.INDEX) THEN
      IF (IT.EQ.I.AND.KT.EQ.K.AND.LT.EQ.L.AND.NT.EQ.N) THEN
         IF (SN.LT.0) GOTO 1000
         IDEM(1)=IDEM(1)+1
      ENDIF
      IF (IT.EQ.I.AND.KT.EQ.K.AND.LT.EQ.N.AND.NT.EQ.L) IDEM(2)=IDEM(2)+1
      IF (IT.EQ.K.AND.KT.EQ.I.AND.LT.EQ.L.AND.NT.EQ.N) IDEM(3)=IDEM(3)+1
      IF (IT.EQ.K.AND.KT.EQ.I.AND.LT.EQ.N.AND.NT.EQ.L) IDEM(4)=IDEM(4)+1
      IF (IT.EQ.L.AND.KT.EQ.N.AND.LT.EQ.I.AND.NT.EQ.K) IDEM(5)=IDEM(5)+1
      IF (IT.EQ.L.AND.KT.EQ.N.AND.LT.EQ.K.AND.NT.EQ.I) IDEM(6)=IDEM(6)+1
      IF (IT.EQ.N.AND.KT.EQ.L.AND.LT.EQ.I.AND.NT.EQ.K) IDEM(7)=IDEM(7)+1
      IF (IT.EQ.N.AND.KT.EQ.L.AND.LT.EQ.K.AND.NT.EQ.I) IDEM(8)=IDEM(8)+1
      ENDIF
  980 CONTINUE

C     -----------------------------------------------
C     TAKE CARE OF CASES WHERE IDEM WAS COUNTED TWICE
C     -----------------------------------------------
      DO  981 IX=1,8
  981 YDEM(IX)=IDEM(IX).GT.0
      IF (I EQ K) THEN
         IF (L EQ N) THEN
             IDEM(2)=0
             IDEM(3)=0
             IDEM(4)=0
             IDEM(6)=0
             IDEM(7)=0
             IDEM(8)=0
             IF (I EQ L) THEN
                 IDEM(5)=0
             ENDIF
         ELSE
             IDEM(3)=0
             IDEM(4)=0
             IDEM(6)=0
             IDEM(7)=0
         ENDIF
      ELSE
         IF (L EQ N) THEN
             IDEM(2)=0
             IDEM(4)=0
             IDEM(6)=0
             IDEM(7)=0
         ELSEIF (IEQL.AND.KEQN) THEN
             IDEM(5)=0
             IDEM(6)=0
             IDEM(7)=0
             SPECIAL=.TRUE.
         ELSEIF (IEQN.AND.KEQL) THEN
             SPECIAL=.TRUE.
             IDEM(6)=0
             IDEM(7)=0
             IDEM(8)=0
         ENDIF
      ENDIF

C     --------------------------------------------------
C     Check if symmetry unique indcom quadruple is able
C     to produce basisfunctions within the desired range
C     --------------------------------------------------
      IF( (N.LT.INS).OR.(N.GT.INE) ) GOTO 1000
      IF( (L.LT.ILS).OR.(L.GT.ILE) ) GOTO 1000

C     Setup primitive calculation:
C     Number of primitives included is determined by ICORE and NCORE
C     ICORE = 0 : All integrals calculated
C     ICORE = 1 : Discard (SS|SS) integrals with one or more diffuse exponents
C     ICORE = 2 : Discard (SL|SL) and (SS|LL) ints with one or more diffuse 
      IF (ID.EQ.1.OR.((ID.EQ.2.OR.ID.EQ.4).AND.ICORE.LE.1).OR.
     &    ID.EQ.3.AND.ICORE.EQ.0) THEN
          ICPRIM = IPRIM
          KCPRIM = KPRIM
          LCPRIM = LPRIM
          NCPRIM = NPRIM
          ICCONTR = ICONTR
          KCCONTR = KCONTR
          LCCONTR = LCONTR
          NCCONTR = NCONTR
      ELSE
          ICPRIM = ICPRI
          KCPRIM = KCPRI
          LCPRIM = LCPRI
          NCPRIM = NCPRI
          ICCONTR = ICCON
          KCCONTR = KCCON
          LCCONTR = LCCON
          NCCONTR = NCCON
      ENDIF
      IKPRIM = ICPRIM*KCPRIM
      LNPRIM = LCPRIM*NCPRIM
      IKLNPR = IKPRIM*LNPRIM
      IKLNCON = ICCONTR*KCCONTR*LCCONTR*NCCONTR
      call cpuused(sec1a)
      IF (.NOT.COUNT.AND.IKLNPR.GT.0.AND.IKLNCON.GT.0) THEN
C     -------------------------------------------------
C     COMPUTE THE WHOLE BLOCK OF THIS TYPE OF INTEGRALS
C     SKIP COMPUTATION OF FGAMMA IF POSSIBLE
C     -------------------------------------------------
C
      IF ((ICNTOLD.EQ.ICNT).AND.(KCNTOLD.EQ.KCNT).AND.
     &    (LCNTOLD.EQ.LCNT).AND.(NCNTOLD.EQ.NCNT).AND.
     &    (ISOLD.EQ.IS).AND.(KSOLD.EQ.KS).AND.
     &    (LSOLD.EQ.LS).AND.(NSOLD.EQ.NS).AND.
     &    (MSOLD.GE.MS)) THEN
         DOGAM = .FALSE.
         INDONE = INDONE + 1
      ELSE
         DOGAM = .TRUE.
         ICNTOLD = ICNT
         KCNTOLD = KCNT
         LCNTOLD = LCNT
         NCNTOLD = NCNT
         ISOLD = IS
         KSOLD = KS
         LSOLD = LS
         NSOLD = NS
         MSOLD = MS
         IDONE = IDONE + 1
      ENDIF
C
C     Allocate memory
C
      ITOP = 1
C
C     The 2-index ij quantities (I didn't invent this stupid ikln convention)
      IPOINDA = ITOP                        ! DISTAB
      IPOINGA = IPOINDA + 1    * IKPRIM     ! GAB
      IPOINHL = IPOINGA + 1    * IKPRIM     ! HL
      IPOINT1 = IPOINHL + 13   * IKPRIM     ! T1
      IPOINP  = IPOINT1 + 1    * IKPRIM     ! P
      ITOP    = IPOINP  + 3    * IKPRIM    
C     The 4-index quantities
      IPOINR = ITOP
      IPOINF = IPOINR + 3         * IKLNPR
      ITOP   = IPOINF + (MSOLD+1) * IKLNPR
C     Up to here needs to be saved between calls
C     The 2-index kl quantities
      NW1 = MAX0(IKPRIM,LNPRIM)
      IPOINDC = ITOP                         ! DISTCD
      IPOINGC = IPOINDC + 1      * LNPRIM    ! GCD
      IPOINHM = IPOINGC + 1      * LNPRIM    ! HM
      IPOINT2 = IPOINHM + 13     * LNPRIM    ! T2
      IPOINQ  = IPOINT2 + 1      * LNPRIM    ! Q
      IPOINWX = IPOINQ  + 3      * LNPRIM    ! WX
      IPOINC  = IPOINWX + 1      * LNPRIM    ! C         
      IPOINW1 = IPOINC  + (MS+3) * LNPRIM    ! WORK ARRAY
      ITOP    = IPOINW1 + 21     * NW1
C     The 4-index scratch arrays
      IPOINZ  = ITOP                          ! Z
      IPOINW2 = IPOINZ   + 1      * IKLNPR    ! WORK ARRAY IN FGAMMA
      ITOP    = IPOINW2  + 8      * IKLNPR
C     We can store the final integrals in the scratch arrays
      IPOINRI = IPOINW2           ! RAWINT
      IPOINRU = IPOINRI  + IKLNPR ! RUWINT
C     Check memory
      IF (ITOP.GT.NWORK) THEN
         PRINT*, "ITOP, NWORK in MFDTWOE",ITOP,NWORK
         CALL Q2JOB(6,'NOT ENOUGH MEMORY',' ',0)
      ENDIF
      MAXTOP = MAX0(MAXTOP,ITOP)
C
      CALL SPDFG (ICNT,KCNT,LCNT,NCNT,ICPRIM,KCPRIM,LCPRIM,NCPRIM,
     &            MS,NI,NK,NL,NN,DOIK,DOGAM,
     &            ALPHA(IS),ALPHA(KS),ALPHA(LS),ALPHA(NS),
     &            IKPRIM,LNPRIM,IKLNPR,
     &            WORK(IPOINW1),NW1,21,WORK(IPOINW2),8,
     &            WORK(IPOINDA),WORK(IPOINDC),
     &            WORK(IPOINGA),WORK(IPOINGC),
     &            WORK(IPOINHL),WORK(IPOINHM),
     &            WORK(IPOINT1),WORK(IPOINT2),
     &            WORK(IPOINP),WORK(IPOINQ),
     &            WORK(IPOINR),WORK(IPOINZ),
     &            WORK(IPOINWX),WORK(IPOINF),
     &            WORK(IPOINC),WORK(IPOINRI))
C
C     ----------------------------------------------
C     4-INDEX TRANSFORMATION TO CONTRACTED INTEGRALS
C     ----------------------------------------------
      call cpuused (sec2a)
      secsp = secsp + sec2a - sec1a
      sec1a = sec2a
      CALL FOURIND (ICPRIM,KCPRIM,LCPRIM,NCPRIM,
     &              ICCONTR,KCCONTR,LCCONTR,NCCONTR,
     &              COEFF(IS),COEFF(KS),COEFF(LS),COEFF(NS),
     &              IPRIM,KPRIM,LPRIM,NPRIM,
     &              WORK(IPOINRU),WORK(IPOINRI))
      call cpuused (sec2a)
      sec4i = sec4i + sec2a - sec1a
      sec1a = sec2a
      ENDIF

C     --------------------------------------
C     DISTRIBUTE THE NON-REDUNDANT INTEGRALS
C     --------------------------------------
C     write(*,*) '---> SYM UNI QUAD: ',I,K,L,N
      DO 2030 IM=KCBINV(I,1),KCBINV(I,1)+ICCONTR-1
      IMM = NEM(IM) + I - INDCOM(KCOMB(IM))
      IMIND=IM-KCBINV(I,1)
      IMIND=IMIND*KCCONTR*LCCONTR*NCCONTR+IPOINRI
      KMMAX=KCBINV(K,1)+KCCONTR-1
      IDEMSI=IDEM(1)
      IF (YDEM(3).OR.YDEM(4)) KMMAX=IM
      DO 2020 KM=KCBINV(K,1),KMMAX
      KMM = NEM(KM) + K - INDCOM(KCOMB(KM))
      IMMM=MAX0(IMM,KMM)
      KMMM=MIN0(IMM,KMM)
      KMIND=KM-KCBINV(K,1)
      KMIND=KMIND*LCCONTR*NCCONTR+IMIND
      IDEMSK=IDEMSI
      IF (IM.EQ.KM) IDEMSK=IDEMSK+IDEM(3)
      LMMAX=KCBINV(L,1)+LCCONTR-1
      IF (YDEM(5).OR.YDEM(6)) LMMAX=IM
      DO 2010 LM=KCBINV(L,1),LMMAX
      LMM = NEM(LM) + L - INDCOM(KCOMB(LM))
      LMIND=LM-KCBINV(L,1)
      LMIND=LMIND*NCCONTR+KMIND
      NMMAX=KCBINV(N,1)+NCCONTR-1
      IF (YDEM(2).OR.(YDEM(4).AND.IM.EQ.KM)) NMMAX=LM
      IF ((YDEM(5).AND.IM.EQ.LM).OR.(YDEM(6).AND.IM.EQ.LM.AND.
     &    IM.GE.KM)) NMMAX=MIN0(NMMAX,KM)
      IF (YDEM(6).AND.IM.EQ.LM.AND.IM.LT.KM) NMMAX=MIN0(NMMAX,KM-1)
      IF ((YDEM(7).AND.((KM.GT.LM).OR.(KM.EQ.LM.AND.IM.LE.KM))).OR.
     &    (YDEM(8).AND.KM.GE.LM)) NMMAX=MIN0(NMMAX,IM)
      IF ((KM.LT.LM.AND.(YDEM(7).OR.YDEM(8))).OR.(YDEM(7)
     &   .AND.(KM.EQ.LM.AND.IM.GT.KM))) NMMAX=MIN0(NMMAX,IM-1)
      DO 2000 NM=KCBINV(N,1),NMMAX
      NMM = NEM(NM) + N - INDCOM(KCOMB(NM))
      LMMM=MAX0(LMM,NMM)
      NMMM=MIN0(LMM,NMM)

C     --------           ATTENTION !!!                      --------
C     --- THE INTERCHANGE OF BASIS FUNCTION LABELS HERE TO
C     --- CREATE CANONICAL COMBINATIONS DESTROYS THE RANGE CONVENTION
C     --- USED BY THE INDINV ARRAY IN A WAY THAT ELEMENTS BEING IN A
C     --- SPECIFIC BUCKET ARE NOT ANYMORE AFTER INTERCHANGE --->
C     --- THE RANGE CHECK HAS TO BE PERFORMED BEFORE THE LABEL INTERCHANGE
C     ----------------------------------------------------------------


C     IF (IMMM.GT.LMMM.OR.(IMMM.EQ.LMMM.AND.KMMM.GE.NMMM)) THEN
C        IMMMM = IMMM
C        KMMMM = KMMM
C        LMMMM = LMMM
C        NMMMM = NMMM
C     ELSE
C        IMMMM = LMMM
C        KMMMM = NMMM
C        LMMMM = IMMM
C        NMMMM = KMMM
C     ENDIF

      IMMMM = IMMM
      KMMMM = KMMM
      LMMMM = LMMM
      NMMMM = NMMM

C     -------------------------------------------------------
C     BASIS FUNCTION LABEL CHECK
C     -------------------------------------------------------

      IF( (NMMMM.LT.IQS).OR.(NMMMM.GT.IQE) ) GOTO 2000
      IF( (LMMMM.LT.IPS).OR.(LMMMM.GT.IPE) ) GOTO 2000

      NMIND=NM-KCBINV(N,1)+LMIND
      IDEMSN=IDEMSK
      IF (.NOT.(IM.EQ.KM.AND.LM.EQ.NM.AND.IM.EQ.LM)) THEN
         IF (LM.EQ.NM) IDEMSN=IDEMSN+IDEM(2)
         IF (IM.EQ.KM.AND.LM.EQ.NM) IDEMSN=IDEMSN+IDEM(4)
         IF (IM.EQ.LM.AND.KM.EQ.NM) IDEMSN=IDEMSN+IDEM(5)
         IF (IM.EQ.NM.AND.KM.EQ.LM) IDEMSN=IDEMSN+IDEM(8)
      ELSE
         IDEMSN=IDEMSN+IDEM(2)+IDEM(4)+IDEM(5)+IDEM(6)+IDEM(7)+IDEM(8)
         IF (SPECIAL) IDEMSN=IDEMSN-IDEM(4)
      ENDIF
      IF (.NOT.COUNT) THEN
         VALINT=WORK(NMIND)
      ELSE
         VALINT=0.D0
      ENDIF
      NUMBZ=NUMBZ+1


#if defined (BIT64)
      IF (ABS(VALINT).GT.ENORM(ID)) THEN
#else
      IF (DABS(VALINT).GT.ENORM(ID)) THEN
#endif

C   --- HERE WE HAVE AN INTEGRAL WITHIN THE DESIRED PQ RANGE
C   --- WE FILL THE CORRESPONDING BUCKET. IF THE BUCKET IS FULL
c   --- WE WRITE IT OUT AND STORE THE LINK RECORD NUMBER AS WELL

C       write(*,*) ' ---> ',immmm,kmmmm,lmmmm,nmmmm
        NUMBE =NUMBE + 1
        ITT=(LMMMM-IPOFF)*IBAS + (NMMMM-IQOFF) + 1
C       write(*,*) 'lmmmm,nmmmm,itt',lmmmm,nmmmm,itt
        NTT=NBUCK(ITT)
        NTT=NTT+1
        RBUCK(NTT,ITT)=VALINT/DBLE(IDEMSN)
#if defined (BIT64)
        LBUCK(NTT,ITT)=OR(KMMMM,ISHIFT(IMMMM,16))
#else
        LBUCK(NTT,ITT)=IOR(KMMMM,ISHIFT(IMMMM,16))
#endif
        IF(NTT.EQ.NPQSIZ) THEN
          IPQL=(LMMMM)*(LMMMM-1)/2 + NMMMM
          LREC=LRECCO+1
          WRITE(INTUN,REC=LREC) IPQL, NTT, LBR(ITT),
     +        (RBUCK(KK,ITT),KK=1,NTT),
     +        (LBUCK(KK,ITT),KK=1,NTT)
C         write(*,*) ' -- DA --',lrec,lmmmm,nmmmm,lbr(itt)
C         write(*,*) ' REC# ',LRECCO,' CONTAINS ',NTT, 'I/LS'
          LRECCO=LREC
          LBR(ITT)=LREC
          NTT=0
        ENDIF
        NBUCK(ITT)=NTT
      ENDIF

 2000 CONTINUE
 2010 CONTINUE
 2020 CONTINUE
 2030 CONTINUE

 1000 CONTINUE
      MCTN=MAX0(MCTLN,NR)
 1010 CONTINUE
      MCTL=MAX0(MCTLN,LR)
      IF (ID.EQ.4) MCTL = MAX0(MCTIK,LR)
 1020 CONTINUE
      MCTK=MAX0(MCTIK,KR)
      IF (ID.EQ.4) MCTK = MAX0(MCTLN,KR)
 1030 CONTINUE

 1330 CONTINUE

      IF (ICMX.EQ.1) THEN
C         PRINT 860,'(LL/LL)',NUMBZ,NUMBE,EXECS
          NUMILL=NUMILL+NUMBE
          NUMOLL=NUMOLL+NUMBZ
      ELSEIF (ICMX.EQ.2) THEN
C         PRINT 860,'(SS/LL)',NUMBZ,NUMBE,EXECS
          NUMISL=NUMISL+NUMBE
          NUMOSL=NUMOSL+NUMBZ
      ELSEIF (ICMX.EQ.3) THEN
C         PRINT 860,'(SS/SS)',NUMBZ,NUMBE,EXECS
          NUMISS=NUMISS+NUMBE
          NUMOSS=NUMOSS+NUMBZ
      ELSEIF (ICMX.EQ.4) THEN
C         PRINT 860,'(SL/SL)',NUMBZ,NUMBE,EXECS
          NUMIBR=NUMIBR+NUMBE
          NUMOBR=NUMOBR+NUMBZ
      ENDIF

C 860   FORMAT(/,1X,A7,1X,'INTEGRALS COMPLETED',4X,
C    $      'NON-ZERO(SYMM.)  ABOVE THRESHOLD    CPU TIME'/
C    $      9X,'CALCULATED THIS RUN   ',I11,6X,I11,6X,F10.4)


C     ------------------------------------------------
C     --- ALL R/S CLASSES ARE TREATED. WRITE OUT BUCKETS
C     --- TO DIRECT ACCESS FILE.  
C     ------------------------------------------------

      DO 1500 ITT=1,IBATN
        NTT=NBUCK(ITT)
        IF(NTT.EQ.0) GOTO 1500
        IPP=(ITT-1)/IBAS+IPOFF
        IQQ=MOD((ITT-1),IBAS)+IQOFF
        IPQL=IPP*(IPP-1)/2 + IQQ
        LREC=LRECCO+1
C       write(*,*) 'WRITING OUT: ',ipp,iqq,ntt,lrec
        WRITE(INTUN,ERR=8890,REC=LREC) IPQL,NTT,LBR(ITT),
     +        (RBUCK(JL,ITT),JL=1,NTT),
     +        (LBUCK(JL,ITT),JL=1,NTT)
C       write(*,*) ' -- DA --',lrec,ipp,iqq,lbr(ITT)
C       write(*,*) ' REC# ',LRECCO,' CONTAINS ',NTT, 'I/LS'
        LRECCO=LREC
        LBR(ITT)=LREC
        NBUCK(ITT)=0
 1500 CONTINUE
C     write(*,*) '--- all buckets of a range written ! ---'

C     ------------- wait for next range ! --------------------
      GOTO 800
C     --------------------------------------------------------

C     --- all ranges are done. synchronize and send return status
 4000 CONTINUE
      CALL MPI_SEND(IRTS,1,MPI_INTEGER,0,M_IRTS_T,
     +    MPI_COMM_WORLD,IERR)
      IF(IERR.NE.0) CALL MPIQT('MPI_SEND IN MFDTWOE.F (SLAVE)',0)
      WRITE(*,*)
      WRITE(*,*)
      WRITE(*,*) 'Computation statistics on node ',mpirnk
      WRITE(*,*) '====================================='
      WRITE(*,*)
      WRITE(*,'(A,I11)') '    LL INTEGRALS         ',NUMILL
      WRITE(*,'(A,I11)') '    SL INTEGRALS         ',NUMISL
      WRITE(*,'(A,I11)') '    SS INTEGRALS         ',NUMISS
      WRITE(*,'(A,I11)') '    BR INTEGRALS         ',NUMIBR
      WRITE(*,'(A)') '--------------------------------------'
      WRITE(*,'(A,I11)') '       TOTAL             ',
     +                    NUMILL+NUMISL+NUMISS+NUMIBR
      WRITE(*,*)
      WRITE(*,*) 'FILE PARAMETER: ',LRECCO,IRECL
      WRITE(*,*)
      WRITE(*,'(A)') '--------------------------------------'
      WRITE(*,*)

      CLOSE(INTUN)
C     --- write record length and start/end ranges to file
      IPS=1
      IPE=INDINV(2,NEQCO1)
      IQS=INDINV(1,NEQCO1+1)
      IQE=INDINV(2,NEQCOM)
      OPEN(UNIT=INTUN,FILE=FILNO,ACCESS='SEQUENTIAL',STATUS='UNKNOWN',
     + FORM='UNFORMATTED',ERR=8888)
      WRITE(INTUN,ERR=8890) LRECCO,IRECL,IPS,IPE,IQS,IQE
      CLOSE(INTUN)
      CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
C ** every slave communicates its # of integrals to the master
      ILST(1)=NUMILL
      ILST(2)=NUMISL
      ILST(3)=NUMISS
      ILST(4)=NUMIBR
      CALL MPI_SEND(ILST,4,MPI_INTEGER,0,M_ACDA_T,
     +    MPI_COMM_WORLD,IERR)

C *************************************************************************
C ***                                                                   ***
C ***   END OF SLAVE CODE.                                              ***
C ***                                                                   ***
C *************************************************************************


      ENDIF
      RETURN


C     PRINT 1340
C1340 FORMAT(//' TWO ELECTRON INTEGRALS COMPLETED')
c     print1350, "Time in spdfg",secsp," seconds" 
c     print1350, "Time in 4-ind",sec4i," seconds"
c     print1350, "Time in distr",secdi," seconds"
c1350 format (a,f10.3,a)
c     print*
c     print*, "Maximum memory used :",maxtop," 8-Byte words"

C     *******************************
C     ******* ERROR HANDLING ********
C     *******************************


 8888 WRITE(*,*) 'ERROR OPENING FILE ',FILNM,' ON NODE',mpirnk
      CALL Q2JOB(6,'MFDTWOE.F',' ',0)
 8890 WRITE(*,*) 'WRITE ERROR ON FILE ',FILNM,' ON NODE',mpirnk
      CALL Q2JOB(6,'MFDTWOE.F',' ',0)

      END
