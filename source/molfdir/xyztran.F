C
C     ==================
      SUBROUTINE XYZTRAN
C     ==================
      IMPLICIT REAL*8 (A-H, O-Z)
#if defined (CRAY)
      REAL C45, S45, C120, S120
#endif
C
C     XYZTRAN IS A ROUTINE THAT RETURNS ALL RELEVANT POINTGROUP DATA
C     FOR THE DOUBBLE GROUPS NAMED IN ARRAY 'GRNAME'. THE MAIN PURPOSE
C     HOWEVER IS TO RETURN ALSO THE COEFFICIENTS OF A PROJECTION OPER-
C     ATOR THAT PROJECTS OUT TO A IRREDUCIBLE REPRESENTATION OF THE
C     DOUBBLE GROUP. EACH PROJECTOR IS IN FACT A PRODUCT OF PROJECTORS
C     ONE FOR EACH GROUP IN A SO CALLED GROUP-CHAIN. A GROUP-CHAIN IS
C     A SET OF SUBGROUPS EACH OF WHICH RESOLVES A DEGENERATE REPRESEN-
C     TATION OF THE PREVIOUS GROUP IN THE CHAIN. THE LAST GROUP OF THE
C     CHAIN CONTAINS NO DEGENERATE REPRESENTATIONS.
C     ON ENTRY :
C     GROUPN---SCHOENFLIES SYMBOL FOR POINTGROUP (3-CHARACTER VARIABLE)
C     ON EXIT :
C     NREP-----NAMES OF THE REPRESENTATIONS OF THE GROUP(4-CHARACT.VAR)
C     REPN-----ACCUMULATIVE NAMES OF THE REPRESENTATIONS OF THE GROUP
C              AND SUBGROUPS IN THE SUBGROUP-CHAIN(14-CHARACTER VARIABLE
C     OPERN----SYMBOLS REPRESENTING THE GROUP-OPERATIONS(4-CHARACT.VAR)
C
      INCLUDE 'paracom.inc'
#if defined (PARALLEL)
      INCLUDE 'mpidef.inc'
#else
      PARAMETER (mpirnk = 0)
#endif
C
      COMPLEX*16 TW,TWM,TWI,TWIM,TWW,TWWM,TWWI,TWWIM,TWT,TWTM,TWTI,TWTIM
      COMPLEX*16 ZERO,VR,VRM,TWSQ,TWSQM,AT,ATM
      CHARACTER*3 DEBUG,DBGOPTN
      PARAMETER (DEBUG='ON',DBGOPTN='OFF')
#if defined (BIT64)
      PARAMETER (ZERO=(0.,0.),TW=(2.,0.),VR=(4.,0.),AT=(8.,0.))
      PARAMETER (TWI=(0.,2.),TWW=(-1.0,1.73205080756888))
      PARAMETER (TWWI=(-1.0,-1.73205080756888))
      PARAMETER (TWT=(1.41421356237310,1.41421356237310))
      PARAMETER (TWTI=(1.41421356237310,-1.41421356237310))
      PARAMETER (TWSQ=(2.82842712474619,0.),TWSQM=-TWSQ)
      PARAMETER (TWM=-TW,TWIM=-TWI,TWWM=-TWW,TWWIM=-TWWI,TWTM=-TWT)
      PARAMETER (TWTIM=-TWTI,VRM=-VR,ATM=-AT)
      PARAMETER (PQ=0.70710678118655,QQ=-0.70710678118655)
#else
      PARAMETER (ZERO=(0.D0,0.D0),TW=(2.D0,0.D0))
      PARAMETER (VR=(4.D0,0.D0),AT=(8.D0,0.D0),TWI=(0.D0,2.D0))
      PARAMETER (TWW=(-1.0D0,1.73205080756888D0))
      PARAMETER (TWWI=(-1.0D0,-1.73205080756888D0))
      PARAMETER (TWT=(1.41421356237310D0,1.41421356237310D0))
      PARAMETER (TWTI=(1.41421356237310D0,-1.41421356237310D0))
      PARAMETER (TWSQ=(2.82842712474619D0,0.D0),TWSQM=-TWSQ)
      PARAMETER (TWM=-TW,TWIM=-TWI,TWWM=-TWW,TWWIM=-TWWI,TWTM=-TWT)
      PARAMETER (TWTIM=-TWTI,VRM=-VR,ATM=-AT)
      PARAMETER (PQ=0.70710678118655D0,QQ=-0.70710678118655D0)
#endif
C
      COMMON/GROUPR/PROJOP(48,16),UU(8,48)
      CHARACTER*1 CH*2
      CHARACTER*1 CHX,PLUSMIN(3)
      COMPLEX*16 PROJOP,CHRA,CHARA,CHARB,CSUM
      DATA PLUSMIN /'-', ' ', '+'/
      CHX(I)=PLUSMIN(I+2)
      IUX(M)=M-(M/25)*24
      NOPER(1) = 0
      NOPER(2) = 0
      NOPER(3) = 0
C
C     --------------------------------------------
C     IDENTIFY NUMBER OP GROUPS IN THE GROUP-CHAIN
C     --------------------------------------------
C
      LCHAIN=0
      DO 72 IC=1,3
      DO 72 K=1,NUMGRPS
   72 IF (GROUPN(IC).EQ.GRNAME(K)) LCHAIN=LCHAIN+1
C
C     -------------------------------------------------
C     IDENTIFY THE SPECIFIED GROUP(S)IN THE GROUP-CHAIN
C     -------------------------------------------------
C
      DO 3 IC=1,3
      DO 1  K=1,NUMGRPS
    1 IF (GROUPN(IC).EQ.GRNAME(K)) GOTO 2
      CALL Q2JOB(2,'GROUP NON EXISTING OR NOT IMPLEMENTED : '//
     &GROUPN(IC),' ',0)
C
C     ------------------------------------------------------------------
C     IF FOR A 'DEGENERATE' GROUP NO SUBGROUPS ARE SPECIFIED, TAKE DEFAU
C     ------------------------------------------------------------------
C
    2 IF (IC.GT.1 .AND. K.LT.3) K=ICHAIN(IGRID(IC-1))
      IGRID(IC)=K
      GROUPN(IC)=GRNAME(K)
      IGAB = K
    3 IF (K.LT.14) GOTO 4
C
C     --------------------------------------------------
C     THE GROUPCHAIN HAS NO PROPER END BECAUSE THE FINAL
C     GROUP CONTAINS MULTIDIMENSIONAL REPRESENTATIONS
C     --------------------------------------------------
C
      CALL Q2JOB(2,'ILLEGAL LAST GROUP IN CHAIN :'//GROUPN(3),' ',0)
C
C     ---------------------------------
C     A GROUPCHAIN HAS BEEN ESTABLISHED
C     ---------------------------------
C
    4 ICM=IC
      lchain=icm
C
C     -----------------------------------------------
C     FIND THE GENERATORS FOR THE GROUPS IN THE CHAIN
C     AND GENERATE ALL THE GROUP-OPERATIONS
C     -----------------------------------------------
C
      IC=1
    5 NSYMOP=1
      IND=IGRID(IC)
      OPERATI(1,IC)=GENER(1,IND)
      DO 9 IG=1,3
      IGEN=GENER(IG,IND)
      IF (IGEN.EQ.0) GOTO 86
      MOP=NSYMOP
      DO 8 K=1,48
      OPER = OPTABEL (OPERATI (K, IC), IGEN)
      DO 6 IOP=1,NSYMOP
    6 IF (OPER.EQ.OPERATI(IOP,IC)) GOTO 7
      NSYMOP=NSYMOP+1
      OPERATI(NSYMOP,IC)=OPER
      GOTO 8
    7 IF (K.EQ.NSYMOP) GOTO 10
    8 CONTINUE
   10 DO 85 KOP=1,48
      IGEN=OPERATI(KOP,IC)
      DO 84 LOP=MOP,NSYMOP
      OPER = OPTABEL (IGEN,  OPERATI (LOP, IC))
      DO 83 IOP=1,NSYMOP
   83 IF(OPER.EQ.OPERATI(IOP,IC)) GOTO 84
      NSYMOP=NSYMOP+1
      OPERATI(NSYMOP,IC)=OPER
      IF (NSYMOP.EQ.48) GOTO 86
   84 CONTINUE
   85 IF(KOP.GE.NSYMOP) GOTO 9
    9 CONTINUE
   86 CONTINUE
      NOPER(IC)=NSYMOP
      IC=IC+1
      IF (IC.LE.ICM) GOTO 5
      NSYMOP=NOPER(1)
C
C     ----------------------------------------------------------------
C     NOW THE OPERATIONS OF ALL THE GROUPS IN THE CHAIN ARE KNOWN, BUT
C     IN ORDER TO FIND THE APPROPRIATE CHARACTERS AND TO MATCH THEM TO
C     THE CORRECT OPERATION-NAMES, THE OPERATION SEQUENCE HAS TO BE
C     IN CANONICAL ORDER
C     ----------------------------------------------------------------
C
      KOP=0
      LOP=0
      MOP=0
      IFIOP=49
      DO 17 K=1,48
      DO 11 IOP=1,NOPER(1)
   11 IF (OPERATI(IOP,1).EQ.OPTABEL(K,1)) GOTO 12
      GOTO 17
   12 KOP=KOP+1
      OPERA=OPERATI(IOP,1)
      OPERAT1(KOP)=OPERA
      INVOP1(K)=KOP
      IF (IFIOP.EQ.49.AND.K.GE.25) IFIOP=KOP
      DO 125 I=1,8
      KK=K-(K/25)*24
  125 UU(I,KOP)=U((KK-1)*8+I)
      DO 13 IOP=1,NOPER(2)
   13 IF (OPERA.EQ.OPERATI(IOP,2)) GOTO 14
      GOTO 17
   14 LOP=LOP+1
      OPERAT2(LOP)=OPERA
      INVOP2(K)=LOP
      DO 15 IOP=1,NOPER(3)
   15 IF (OPERA.EQ.OPERATI(IOP,3)) GOTO 16
      GOTO 17
   16 MOP=MOP+1
      OPERAT3(MOP)=OPERA
   17 CONTINUE
      DO 170 I=1,NSYMOP
  170 OPERN(I)=OPNAME(OPERAT1(I))
C
C     ------------------------------------------------------------
C     FOR TESTING PUPOSES THE NEXT STATEMENTS COULD BE HELPFULL :
C     PRINT*,' GEORDEND : ',(OPERAT1(I),I=1,NSYMOP)
C     PRINT*,' NUMMERS  : ',(OPERAT1(I),I=1,NSYMOP)
C     PRINT*,' OPERATIES: ',(OPNAME(OPERAT1(I)),I=1,NSYMOP)
C     PRINT*,'GEORDEND(2) ',(OPERAT2(I),I=1,NOPER(2))
C     PRINT*,' NUMMERS    ',(OPERAT2(I),I=1,NOPER(2))
C     PRINT*,'GECODEERD   ',(OPNAME(OPERAT2(I)),I=1,NOPER(2))
C     --------------------------------------------------------------
C
C
C     --------------------------------------------------
C     PRINT GROUPMULTIPLICATION TABLE IF "DEBUG" IS "ON"
C     --------------------------------------------------
C
      IF ((DEBUG.EQ.DBGOPTN).and.(mpirnk.eq.0)) THEN
          WRITE(*,1000) ' '
          NOP=MIN0(16,NSYMOP)
          DO 171 IOP=1,NSYMOP
          WRITE (*, 1010) IOP, (CHX (OPSIGN (IUX (OPERAT1 (IOP)),
     $                          IUX (OPERAT1 (KOP)))),
     $                          OPNAME (OPTABEL (OPERAT1 (IOP),
     $                          OPERAT1 (KOP))), KOP = 1, NOP)
  171     CONTINUE
          IF (NOP.EQ.NSYMOP) GOTO 174
          NOP=MIN0(32,NSYMOP)
          WRITE(*,1000) ' (CONTINUED) '
          DO 172 IOP=1,NSYMOP
          WRITE (*, 1010) IOP, (CHX (OPSIGN (IUX (OPERAT1 (IOP)),
     $                          IUX (OPERAT1 (KOP)))),
     $                          OPNAME (OPTABEL (OPERAT1 (IOP),
     $                          OPERAT1 (KOP))), KOP = 17, NOP)
  172     CONTINUE
          IF  (NOP.EQ.NSYMOP) GOTO 174
          NOP=NSYMOP
          WRITE(*,1000) ' (CONTINUED) '
          DO 173 IOP=1,NSYMOP
          WRITE (*, 1010) IOP, (CHX (OPSIGN (IUX (OPERAT1 (IOP)),
     $                          IUX (OPERAT1 (KOP)))),
     $                          OPNAME (OPTABEL (OPERAT1 (IOP),
     $                          OPERAT1 (KOP))), KOP = 33, NOP)
  173     CONTINUE
  174     CONTINUE
      ENDIF
      IF (MOP.LT.NOPER(3) .OR. LOP.LT.NOPER(2))
     $CALL Q2JOB(2,'ILLEGAL SUBGROUPCHAIN',' ',0)
C
C     ----------------------------------------------------------------
C     DETERMINE THE DIMENSIONS OF THE REPRESENTATIONS FROM THEIR NAMES
C     ----------------------------------------------------------------
C
      NSYMRP=ISYGEN(IGRID(1))
      KSIMRP=ISYGEN(IGRID(2))
      LSIMRP=ISYGEN(IGRID(3))
      NCLASS=NUMCLS(IGRID(1))
      KCLASS=NUMCLS(IGRID(2))
      LCLASS=NUMCLS(IGRID(3))
      CALL MOVLCH(GREP(INDREP(IGRID(1))),NREP(1),NSYMRP*4)
      DO 18 K=1,NSYMRP
      CH=NREP(K)(1:2)
      IDIMRP(K)=1
      IF (CH(1:1).EQ.'E' .OR. CH.EQ.'''F' .OR. CH.EQ.'"F') IDIMRP(K)=2
   18 IF (CH(1:1).EQ.'F') IDIMRP(K)=4
      DO 19 K=1,KSIMRP
      CH=GREP(INDREP(IGRID(2))+K-1)(1:2)
      KDIMRP(K)=1
      IF (CH(1:1).EQ.'E' .OR. CH.EQ.'''F' .OR. CH.EQ.'"F') KDIMRP(K)=2
   19 IF (CH(1:1).EQ.'F') KDIMRP(K)=4
C
C     --------------------------------------------------
C     SINCE CHARACTERS ARE STORED ONLY PER CLASS, SET UP
C     INDEXARRAY FOR THE CHARACTERS FOR EACH OPERATION
C     --------------------------------------------------
C
      IOP=1
      IND=INDCHAR(IGRID(1))-NSYMRP
      INC=INDCLAS(IGRID(1))
      DO 20 K=0,NCLASS-2
      IND=IND+NSYMRP
      DO 20 IC=1,ICCLAS(INC+K)
      IOP=IOP+1
   20 INDCH1(IOP)=IND
      IF (ICM.GT.1) THEN
          IOP=1
          IND=INDCHAR(IGRID(2))-KSIMRP
          INC=INDCLAS(IGRID(2))
          DO 21 K=0,KCLASS-2
          IND=IND+KSIMRP
          DO 21 IC=1,ICCLAS(INC+K)
          IOP=IOP+1
   21     INDCH2(IOP)=IND
          IF(ICM.GT.2) THEN
               IOP=1
               IND=INDCHAR(IGRID(3))-LSIMRP
               INC=INDCLAS(IGRID(3))
               DO 22 K=0,LCLASS-2
               IND=IND+LSIMRP
               IOP=IOP+1
   22          INDCH3(IOP)=IND
          ENDIF
      ENDIF
C
C     ---------------------------------
C     CONSTRUCT THE PROJECTION-OPERATOR
C     ---------------------------------
C
      DO 221 J=1,16
      DO 220 I=1,NSYMOP
      PROJOP(I,J)=(0.,0.)
  220 CONTINUE
  221 CONTINUE
      ISYMRP=0
C
      IF (LCHAIN.EQ.1) THEN
C     --------------------------------------------------------------------
C     Projection operators are character projection operators of the group
C     --------------------------------------------------------------------
      DO N=1,NSYMRP
         ICTRL=ISYMRP
         ISYMRP=ISYMRP+1
         REPN(ISYMRP)=NREP(N)
         PROJOP(1,ISYMRP)=TW
         DO IOP=2,NSYMOP
            IND=INDCH1(IOP)+N-1
            PROJOP(IOP,ISYMRP)=CHARACT(IND)
         ENDDO
      ENDDO
      ELSEIF (LCHAIN.EQ.2) THEN
C     ---------------------------------------------------------------------
C     Projection operators are product of char. proj. operators of 2 groups
C     ---------------------------------------------------------------------
      DO N=1,NSYMRP
         ICTRL=ISYMRP
         DO K=1,KSIMRP
#if defined (BIT64)
         SUM=4.* REAL( IDIMRP(N)*KDIMRP(K) )
#else
         SUM=4.0D0* DBLE( IDIMRP(N)*KDIMRP(K) )
#endif
         DO KOP=2,NOPER(2)
            IOPA=OPERAT2(KOP)
            IOQ=INVOP1(IOPA)
            INC=INDCH1(IOQ)+N-1
            IND=INDCH2(KOP)+K-1
#if defined (BIT64)
            SUM=SUM+REAL( CHARACT(INC)*CONJG(CHARACT(IND)) )
#else
            SUM=SUM+DREAL( CHARACT(INC)*DCONJG(CHARACT(IND)) )
#endif
         ENDDO
         IF (SUM.GT.0.0001) THEN
            ISYMRP=ISYMRP+1
            REPN(ISYMRP)=NREP(N)//' '//GREP(INDREP(IGRID(2))+K-1)
            DO IOP=1,NSYMOP
               IOPA=OPERAT1(IOP)
               IUPA=IOPA-(IOPA/25)*24
               IF (IOP.GT.1) THEN
                  CHRA=CHARACT(INDCH1(IOP)+N-1)
               ELSE
#if defined (BIT64)
                  CHRA=CMPLX( 2.0*REAL(IDIMRP(N)) )
#else
                  CHRA=DCMPLX( 2.0D0*DBLE(IDIMRP(N)) )
#endif
               ENDIF
               DO KOP=1,NOPER(2)
                  IOPB=OPERAT2(KOP)
                  IUPB=IOPB-(IOPB/25)*24
                  IOPB=OPTABEL(IOPB,IOPA)
                  SN=OPSIGN(IUPB,IUPA)
                  IF (KOP.GT.1) THEN
                     IND=INDCH2(KOP)+K-1
                     CHARA=CHARACT(IND)
                  ELSE
#if defined (BIT64)
                     CHARA=CMPLX( 2.0*REAL(KDIMRP(K)) )
#else
                     CHARA=DCMPLX( 2.0D0*DBLE(KDIMRP(K)) )
#endif
                   ENDIF
                   IOQ=INVOP1(IOPB)
                   PROJOP(IOQ,ISYMRP)=PROJOP(IOQ,ISYMRP)+CHARA*CHRA*SN
                ENDDO
             ENDDO
         ENDIF
         ENDDO
         IF (ISYMRP-ICTRL.NE.IDIMRP(N))     
     &      CALL Q2JOB(2,' SUBGROUP DOES NOT RESOLVE DEGENERACY',' ',0)
      ENDDO
      ELSEIF (LCHAIN.EQ.3) THEN
C     ---------------------------------------------------------------------
C     Projection operators are product of char. proj. operators of 3 groups
C     ---------------------------------------------------------------------
      DO N=1,NSYMRP
         ICTRL=ISYMRP
         DO K=1,KSIMRP
#if defined (BIT64)
         SUM=4.* REAL( IDIMRP(N)*KDIMRP(K) )
#else
         SUM=4.0D0 * DBLE( IDIMRP(N)*KDIMRP(K) )
#endif
         DO KOP=2,NOPER(2)
            IOPA=OPERAT2(KOP)
            IOQ=INVOP1(IOPA)
            INC=INDCH1(IOQ)+N-1
            IND=INDCH2(KOP)+K-1
#if defined (BIT64)
            SUM=SUM+REAL( CHARACT(INC)*CONJG(CHARACT(IND)) )
#else
            SUM=SUM+DREAL( CHARACT(INC)*DCONJG(CHARACT(IND)) )
#endif
         ENDDO
         IF (SUM.GT.0.0001) THEN
            DO L=1,LSIMRP
#if defined (BIT64)
               SUM=2.* REAL( KDIMRP(K) )
#else
               SUM=2.0D0* DBLE( KDIMRP(K) )
#endif
               DO LOP=2,NOPER(3)
                  IOPA=OPERAT3(LOP)
                  IOQ=INVOP2(IOPA)
                  INC=INDCH2(IOQ)+K-1
                  IND=INDCH3(LOP)+L-1
#if defined (BIT64)
                  SUM=SUM+REAL( CHARACT(INC)*CONJG(CHARACT(IND)) )
#else
                  SUM=SUM+DREAL( CHARACT(INC)*DCONJG(CHARACT(IND)) )
#endif
               ENDDO
               IF (SUM.GT.0.0001) THEN
                  ISYMRP=ISYMRP+1
                  REPN(ISYMRP)=NREP(N)//' '
     &                         //GREP(INDREP(IGRID(2))+K-1)//' '
     &                         //GREP(INDREP(IGRID(3))+L-1)
                  DO IOP=1,NSYMOP
                     IOPA=OPERAT1(IOP)
                     IUPA=IOPA-(IOPA/25)*24
                     IF (IOP.GT.1) THEN
                        CHRA=CHARACT(INDCH1(IOP)+N-1)
                     ELSE
#if defined (BIT64)
                       CHRA=CMPLX( 2.0*REAL(IDIMRP(N)) )
#else
                       CHRA=DCMPLX( 2.0D0*DBLE(IDIMRP(N)) )
#endif
                     ENDIF
                     DO KOP=1,NOPER(2)
                        IOPB=OPERAT2(KOP)
                        IUPB=IOPB-(IOPB/25)*24
                        IOPB=OPTABEL(IOPB,IOPA)
                        SN=OPSIGN(IUPB,IUPA)
                        IUPB=IOPB-(IOPB/25)*24
                        IF (KOP.GT.1) THEN
                           CHARA=CHARACT(INDCH2(KOP)+K-1)
                        ELSE
#if defined (BIT64)
                           CHARA=CMPLX( 2.0*REAL(KDIMRP(K)) )
#else
                           CHARA=DCMPLX( 2.0D0*DBLE(KDIMRP(K)) )
#endif
                        ENDIF
                        CHARA=CHARA*CHRA*SN
                        DO LOP=1,NOPER(3)
                           IOPC=OPERAT3(LOP)
                           IUPC=IOPC-(IOPC/25)*24
                           IOPC=OPTABEL(IOPC,IOPB)
                           SN=OPSIGN(IUPC,IUPB)
                           IF (LOP.GT.1) THEN
                               IND=INDCH3(LOP)+L-1
                               CHARB=CHARACT(IND)
                           ELSE
                               CHARB=(2.,0.)
                           ENDIF
                           IOQ=INVOP1(IOPC)
                           PROJOP(IOQ,ISYMRP)=PROJOP(IOQ,ISYMRP)
     &                                       +CHARB*CHARA*SN
                        ENDDO ! LOP
                     ENDDO ! KOP
                  ENDDO ! IOP
               ENDIF 
            ENDDO ! L
         ENDIF 
         ENDDO ! K
         IF (ISYMRP-ICTRL.NE.IDIMRP(N)) 
     &      CALL Q2JOB(2,' SUBGROUP DOES NOT RESOLVE DEGENERACY',' ',0)
      ENDDO ! N
      ELSE
        CALL Q2JOB(6,'ILLEGAL VALUE OF LCHAIN IN XYZTRAN',' ',0)
      ENDIF
C
C     ---------------------------------------------------
C     CHECK THE ORTHOGONALITY OF THE PROJECTION OPERATORS
C     ---------------------------------------------------
C
      DO 35 I=1,ISYMRP
      DO 34 J=1,I-1
      CSUM=(0.,0.)
      DO 33 IOP=1,NSYMOP
#if defined (BIT64)
      CSUM=CSUM+PROJOP(IOP,I)*CONJG(PROJOP(IOP,J))
#else
      CSUM=CSUM+PROJOP(IOP,I)*DCONJG(PROJOP(IOP,J))
#endif
   33 CONTINUE
#if defined (BIT64)
      IF (CABS(CSUM).GT.0.0001) THEN
#else
      IF (CDABS(CSUM).GT.0.0001D0) THEN
#endif
         if (mpirnk.eq.0)
     &   WRITE(*,'(A,2I4,2F6.3)') 'ERROR IN PROJOP ',I,J,CSUM
         CALL Q2JOB(6,'PROJ.OPER. NOT ORTHOGONAL P.XYZTRAN_S.34',' ',0)
      ENDIF
   34 CONTINUE
   35 CONTINUE
C
C     -----------------------------------------------------------
C     THE COEFFICIENTS MAY HAVE BECOME VERY LARGE FOR MULTIDIMEN-
C     SIONAL REPRESENTATIONS. DIVIDE BY THE COEFFICIENT OF THE
C     UNIT-OPERATION :
C     -----------------------------------------------------------
C
      LSIMRP=0
      DO 39 ISYMRP=1,NSYMRP
      KSIMRP=LSIMRP+1
      LSIMRP=LSIMRP+IDIMRP(ISYMRP)
      DO 38 I=KSIMRP,LSIMRP
#if defined (BIT64)
      IF(CABS(PROJOP(1,I)).LT.1.0)
     +   CALL Q2JOB(4,'P.XYZTRAN_PROJOP',' ',0)
      DELER=1./CABS(PROJOP(1,I))
#else
      IF(CDABS(PROJOP(1,I)).LT.1.0D0)
     +   CALL Q2JOB(4,'P.XYZTRAN_PROJOP',' ',0)
      DELER=1.D0/CDABS(PROJOP(1,I))
#endif
      DO 37 IOP=1,NSYMOP
#if defined (BIT64)
      PROJOP(IOP,I)=PROJOP(IOP,I)*CMPLX(DELER)
#else
      PROJOP(IOP,I)=PROJOP(IOP,I)*DCMPLX(DELER)
#endif
   37 CONTINUE
   38 CONTINUE
   39 CONTINUE
      NSUBSP=LSIMRP
C
C     ---------------------------------------------------------------
C     PUT THE DEFINITION OF THE OPERATIONS IN TERMS OF X,Y,Z IN 'XMAT'
C     ---------------------------------------------------------------
C
      DO 40 IOP=1,NSYMOP
      DO 40 IC =1,3
   40 XMAT(IC+1,IOP)=IDFOPER(IC,OPERAT1(IOP))
C
C     ---------------------------------------------------
C     PUT THE NUMBER OF GROUPS IN THE CHAIN IN IGRID(1)
C     PUT THE GROUPID OF THE ABELIAN SUBGROUP IN IGRID(2)
C     ---------------------------------------------------
C
      IGRID(1)=ICM
      IGRID(2)=IGAB
C
C     -----------------------------------------------------------------
C     FIND AN OPERATION OF THE GROUP (ONE THAT DOES NOT BELONG TO ONE
C     OF THE SUBGROUPS IN THE CHAIN) WHICH WILL NOT HAVE ZERO OFF-DIAG-
C     ONAL REPRESENTATION MATRIX ELEMENTS. THIS OPERATION CAN BE USED
C     TO FIX THE PHASE BETWEEN BASISFUNCTIONS OF A MULTIDIMENSIONAL
C     IRREDUCIBLE REPRESENTATION. FOR A FOUR DIMENSIONAL IREPS ONLY THE
C     FIRST ROW NEEDS TO BE NON-ZERO.
C     EACH OPERATION OF THE SELECTED SET IS PROJECTED OUT FROM THE LEFT
C     AND RIGHT TO TWO DIFFERENT SUBSPECIES OF THE IREPS. IF THE RESULT
C     IS A NULL OPERATOR THE NEXT OPERATION IS TRIED FOR.
C     -----------------------------------------------------------------
C
      NRP=NSYMRP
      IF (IFIOP.EQ.25) NRP=NSYMRP/2
      IDACT=2
      IDM=2
   41 IOP=0
   42 IOP=IOP+1
      IF (IOP.GT.NSYMOP) THEN
          IF (IDACT.EQ.2) CALL Q2JOB(6,'CAN''T FIND IFASOP(1) '//
     $    'P.XYZTRAN_IOP',' ',0)
          if (mpirnk.eq.0) then
          WRITE (6,*) 'This subgroup chain can not be used'
          WRITE (6,*) 'Choose the default subgroup or ',
     &                'use the subgroup as main group'
          endif
          CALL Q2JOB(2,'ILLEGAL SUBGROUPCHAIN',' ',0)
      ENDIF
      OPER=OPERAT1(IOP)
      IOPA=OPER
      IUPA=IUX(IOPA)
      KSIMRP=1
      DO 49 ISYMRP=1,NRP
      ID=IDIMRP(ISYMRP)
      IF (ID.EQ.4) IDM=4
      IF (ID.NE.IDACT) GOTO 48
      DO 47 IC=1,ID-1
      DO 43 I=1,NSYMOP
      CF(I)=(0.0)
   43 CONTINUE
      DO 45 KOP=1,NSYMOP
      IOPB=OPERAT1(KOP)
      IUPB=IOPB-(IOPB/25)*24
      IOPB=OPTABEL(IOPA,IOPB)
      SN1=OPSIGN(IUPA,IUPB)
      IUPB=IUX(IOPB)
      DO 44 MOP=1,NSYMOP
      IOPC=OPERAT1(MOP)
      IUPC=IOPC-(IOPC/25)*24
      IOPC=OPTABEL(IOPC,IOPB)
      SN=SN1*OPSIGN(IUPC,IUPB)
      IOQ=INVOP1(IOPC)
      CF(IOQ)=CF(IOQ)+PROJOP(KOP,KSIMRP+IC)*PROJOP(MOP,KSIMRP)
#if defined (BIT64)
     $*CMPLX(SN)
#else
     $*DCMPLX(SN)
#endif
   44 CONTINUE
   45 CONTINUE
      DO 46 I=1,NSYMOP
#if defined (BIT64)
      IF (CABS(CF(I)).GT.1.E-3) GOTO 47
#else
      IF (CDABS(CF(I)).GT.1.D-3) GOTO 47
#endif
   46 CONTINUE
C
C     ----------------------------------------------------------------
C     ALL COEFF'S ARE ZERO SO THIS IS A NULL OPERATOR : NEXT OPERATION
C     ----------------------------------------------------------------
C
      GOTO 42
   47 CONTINUE
   48 KSIMRP=KSIMRP+ID
   49 CONTINUE
      IFASOP(IDACT/2)=IOP
      IF (IDACT.NE.IDM) THEN
          IDACT=4
          GOTO 41
      ENDIF
C
C     ---------------------------------------------------------------
C     THIS FINAL CHECK IS TO SEE WHETHER THE PROJECTION OPERATORS ARE
C     PROJECTION OPERATORS INDEED
C     ---------------------------------------------------------------
C
      DO 54 I=1,NSUBSP
      DO 50 IOP=1,NSYMOP
      CF(IOP)=(0.,0.)
   50 CONTINUE
      DO 52 IOP=1,NSYMOP
      KOP=OPERAT1(IOP)
      IUPA=IUX(KOP)
      CHRA=PROJOP(IOP,I)
      DO 51 IOQ=1,NSYMOP
      LOP=OPERAT1(IOQ)
      IUPB=IUX(LOP)
      NOP=OPTABEL(LOP,KOP)
      NOP=INVOP1(NOP)
      SN=OPSIGN(IUPB,IUPA)
#if defined (BIT64)
      CF(NOP)=CF(NOP)+CMPLX(SN)*CHRA*PROJOP(IOQ,I)
#else
      CF(NOP)=CF(NOP)+DCMPLX(SN)*CHRA*PROJOP(IOQ,I)
#endif
   51 CONTINUE
   52 CONTINUE
      CHARA=PROJOP(1,I)/CF(1)
      DO 53 IOP=1,NSYMOP
#if defined (BIT64)
      IF (CABS(CF(IOP)*CHARA-PROJOP(IOP,I)).GT.1.E-10) CALL Q2JOB
#else
      IF (CDABS(CF(IOP)*CHARA-PROJOP(IOP,I)).GT.1.D-10) CALL Q2JOB
#endif
     $(6,'WRONG PROJECTION OPERATOR IN P.XYZTRAN_S.53',' ',0)
   53 CONTINUE
   54 CONTINUE
      RETURN
 1000 FORMAT(/' GROUP MULTIPLICATION TABLE ',A/)
 1010 FORMAT(I3,2X,16(1X,A1,A4))
      END
