C...   Copyright (c) 1998 by the authors of Dirac (see below).
C...   All Rights Reserved.
C...
C...   The source code in this file is part of 
C...   "Dirac, a relativistic ab initio electronic structure program, 
C...   Release 3.1 (1998), written by T. Saue, T. Enevoldsen, T. Helgaker,
C...   H. J. Aa. Jensen, J. Laerdahl, K. Ruud, J. Thyssen, and L. Visscher"
C...
C...   This source code is provided under a written licence and may be 
C...   used, copied, transmitted, or stored only in accord with that
C...   written licence.
C...
C...   In particular, no part of the source code or compiled modules may 
C...   be distributed outside the research group of the licence holder. 
C...   This means also that persons (e.g. post-docs) leaving the research
C...   group of the licence holder may not take any part of Dirac,
C...   including modified files, with him/her, unless that person has
C...   obtained his/her own licence. 
C...
C...   For questions concerning this copyright write to:
C...      dirac-admin@dirac.chem.ou.dk
C...
C...   For information on how to get a licence see:
C...      http://dirac.chem.ou.dk/Dirac
C
C
C VERSION : $Revision: 1.4 $
C DATE    : $Date: 2001/09/13 00:05:04 $
C FILE    : ccints.F
C SOURCE  : $Source: /home/tc/visscher/sources/Molfdir/Molfdir/source/relccsd/ccints.F,v $
C
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE RDINTS(IPRNT,NSP,INDEX,KR,INDK,INDL,CBUF,IBUF,NVT,
     &                  BUF1,BUF2,IREPSP,IREPSPI,
     &                  VOOOO,VOOOOA,VVOOO,VVOOOA,VVVOO,VVVOOA,
     &                  VVOVO,VVOVOA,VVOVV,VVOVVA,VVVVV,VVVVVA,
     &                  EPS,FOO,FVO,FVV,BREIT,SRTALL,INTERFACE,
     &                  MREMAIN,cc)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Reads 1-electron and 2-electron integrals
C     Constructs Fock matrix elements
C     This version reads from files created by TMOONE & ROTRAN or DIRAC
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      INTEGER NVT,NSP
      INTEGER KR(-NSP:NSP),INDK(*),INDL(*)
      INTEGER INDEX(NSP,NSP,4,*),IBUF(7,*)
      INTEGER IREPSP(NSP,4),IREPSPI(NSP,16,2)
      REAL*8 FOO(*),FVO(*),FVV(*)
      REAL*8 VOOOO(RCW,*),VOOOOA(RCW,*),VVOOO(RCW,*),VVOOOA(RCW,*)
      REAL*8 VVVOO(RCW,*),VVVOOA(RCW,*),VVOVO(RCW,*),VVOVOA(RCW,*)
      REAL*8 VVOVV(RCW,*),VVOVVA(RCW,*),VVVVV(RCW,*),VVVVVA(RCW,*)
      REAL*8 BUF1(*),BUF2(*),CBUF(RCW,*)
      REAL*8 EPS(*)
      LOGICAL BREIT,SRTALL
      CHARACTER*10 INTERFACE
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "symm.inc"
      INCLUDE "eqns.inc"
      INCLUDE "complex.inc"
      INCLUDE "results.inc"
      INCLUDE "files.inc"
      INCLUDE "ccpar.inc"
#if defined (VAR_MPI)
      INCLUDE 'mpif.h'
      DIMENSION ISTAT(MPI_STATUS_SIZE)
#endif
C
C---------------Local variables--------------------------------------
C
      REAL*8 E0,E1
      CHARACTER*10 DATEX,TIMEX*8
      LOGICAL FOCKSP
C
C---------------Executable code--------------------------------------
C
C
C     write(iw,*) '********** entering rdints **********'
      FOCKSP = EQNS.EQ.'FOCKSP'
      call memchk ('entering',cc,1)
C
      IF (FOCKSP) THEN
         CALL FKINDEX (NSP,IREPSP,IREPSPI,INDEX)
      ELSE
         CALL MKINDEX (NSP,IREPSP,IREPSPI,INDEX)
      ENDIF
C
C  Check buffer size for passing of VOVV and VVVV integrals
C
      IF (MOD(NVT*IRW*RCW,SEC2I(1)).NE.0)
     &    CALL QUIT("INVALID BUFFER SIZE IN RDINTS")
C
C  INITIALIZE READING OF COULOMB INTEGRALS
C
      IF (MYPROC.EQ.MASTER) THEN
         OPEN(MDINT,FILE='MDCINT',FORM='UNFORMATTED')
         REWIND(MDINT)
         READ (MDINT,ERR=10000,END=10000) DATEX,TIMEX,NKR,
     &                                    (KR(I),KR(-I),I=1,NKR)
      ENDIF
#if defined (VAR_MPI)
      CALL MPI_BCAST(NKR,1,MPI_INTEGER,MASTER,MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(KR,2*NSP+1,MPI_INTEGER,
     &               MASTER,MPI_COMM_WORLD,IERR)
#endif
      IF (2*NKR.NE.NSP) 
     &   CALL QUIT("INCONSISTENT MRCONEE AND MDCINT FILES")
      ITRSGN = 1
C
C  Initialize writing to sorted integral files
C  Use extra memory as buffer space
C
      NINTSB0 = MAX0(MREMAIN/512,20)
      NINTSB0 = MIN0((NFOO+NFVO+NFVV)*RCW/512+1,NINTSB0)
      CALL RFILE(ITAPT,NINTSB0)
      MREMAIN = MREMAIN - (NINTSB0-20)*512
C
      NINTSB1 = MAX0(MREMAIN/512,20)
      NINTSB1 = MIN0(NV1*RCW/512+1,NINTSB1)
      CALL RFILE(ITAPT+1,NINTSB1)
      MREMAIN = MREMAIN - (NINTSB1-20)*512
C
      NINTSB2 = MAX0(MREMAIN/512,20)
      NINTSB2 = MIN0(NV2*RCW/512+1,NINTSB2)
      CALL RFILE(ITAPT+2,NINTSB2)
      MREMAIN = MREMAIN - (NINTSB2-20)*512
C
      NINTSB3 = MAX0(MREMAIN/512,20)
      NINTSB3 = MIN0(NV3*RCW/512+1,NINTSB3)
      CALL RFILE(ITAPT+3,NINTSB3)
      MREMAIN = MREMAIN - (NINTSB3-20)*512
C
      NINTSB4 = MAX0(MREMAIN/512,20)
      NINTSB4 = MIN0(NV4*RCW/512+1,NINTSB4)
      CALL RFILE(ITAPT+4,NINTSB4)
      MREMAIN = MREMAIN - (NINTSB4-20)*512
C
      IF (IMSTAT(1).GT.1.AND..NOT.FOCKSP) THEN
         if (myproc.eq.master) WRITE (IW,1010)
         GOTO 150
      ENDIF
C
C     RE-ENTRY POINT IN CASE THE GAUNT INTERACTION HAS TO BE ADDED
C
    1 ICOUNT = 0
C
C     GET THE OOOO, VOOO, VVOO AND VOVO INTEGRALS
C
      CALL XCOPY (NV1,A0,0,VOOOO,1)
      CALL XCOPY (NV2,A0,0,VVOOO,1)
      CALL XCOPY (NV3,A0,0,VVVOO,1)
      CALL XCOPY (NV4,A0,0,VVOVO,1)
      CALL XCOPY (NV1,A0,0,VOOOOA,1)
      CALL XCOPY (NV2,A0,0,VVOOOA,1)
      CALL XCOPY (NV3,A0,0,VVVOOA,1)
      CALL XCOPY (NV4,A0,0,VVOVOA,1)
C
      call memchk ('after initialization',cc,1)
  100 CALL GET2INTS (MDINT,KR,NSP,INDEX,ITRSGN,1,INDK,INDL,
     &               NZ,NZE,CBUF,IBUF)
      IF (NZ.LT.0) GOTO 110
      ICOUNT = ICOUNT + NZ
      DO 102 INZE = 1, NZE
            ITYP = IBUF(1,INZE)
            IREP = IBUF(2,INZE) 
            IND1 = IBUF(3,INZE)
            IND2 = IBUF(4,INZE)
            ITRS = IBUF(5,INZE)
            JTRS = IBUF(6,INZE)
            I    = IBUF(7,INZE)
            IF (ITYP.EQ.1) THEN         
               IND = IOOOOTT(IREP) + (IND1-1)*NOOT(IREP) + IND2
               VOOOO(1,IND) = CBUF(1,I) * ITRS
               IF (CARITH) VOOOO(2,IND) = CBUF(2,I) * JTRS
            ELSEIF (ITYP.EQ.-1) THEN         
               IND = IOOOOTT(IREP) + (IND1-1)*NOOT(IREP) + IND2
               VOOOOA(1,IND) = CBUF(1,I) * ITRS
               IF (CARITH) VOOOOA(2,IND) = CBUF(2,I) * JTRS
            ELSEIF (ITYP.EQ.2) THEN       
               IND = IVOOOT(IREP) + (IND1-1)*NVO(IREP) + IND2
               VVOOO(1,IND) = CBUF(1,I) * ITRS
               IF (CARITH) VVOOO(2,IND) = CBUF(2,I) * JTRS
            ELSEIF (ITYP.EQ.-2) THEN
               IND = IVOOOT(IREP) + (IND1-1)*NVO(IREP) + IND2
               VVOOOA(1,IND) = CBUF(1,I) * ITRS
               IF (CARITH) VVOOOA(2,IND) = CBUF(2,I) * JTRS
            ELSEIF (ITYP.EQ.4) THEN
               IND = IVVOOTT(IREP) + (IND1-1)*NVVT(IREP) + IND2
               VVVOO(1,IND) = CBUF(1,I) * ITRS
               IF (CARITH) VVVOO(2,IND) = CBUF(2,I) * JTRS
            ELSEIF (ITYP.EQ.-4) THEN
               IND = IVVOOTT(IREP) + (IND1-1)*NVVT(IREP) + IND2
               VVVOOA(1,IND) = CBUF(1,I) * ITRS
               IF (CARITH) VVVOOA(2,IND) = CBUF(2,I) * JTRS
            ELSEIF (ITYP.EQ.6) THEN
               IND = IVOVO(IREP) + (IND1-1)*NVO(IREP) + IND2
               VVOVO(1,IND) = CBUF(1,I) * ITRS
               IF (CARITH) VVOVO(2,IND) = CBUF(2,I) * JTRS
            ELSEIF (ITYP.EQ.-6) THEN
               IND = IVOVO(IREP) + (IND1-1)*NVO(IREP) + IND2
               VVOVOA(1,IND) = CBUF(1,I) * ITRS
               IF (CARITH) VVOVOA(2,IND) = CBUF(2,I) * JTRS
            ENDIF
  102 CONTINUE
      GOTO 100
C
C     ADD THE DIRECT AND EXCHANGE INTEGRALS
C
  110 CALL XAXPY (NV1,-A1,VOOOOA,1,VOOOO,1)
      CALL XAXPY (NV2,-A1,VVOOOA,1,VVOOO,1)
      CALL XAXPY (NV3,-A1,VVVOOA,1,VVVOO,1)
      CALL XAXPY (NV4,-A1,VVOVOA,1,VVOVO,1)
C
C     ADD BREIT AND COULOMB INTEGRALS
C
      IF (BREIT.AND.ITRSGN.LT.0) THEN
         CALL RREAD(ITAPT+1,VOOOOA,NV1*RCW*IRW,1)
         CALL RREAD(ITAPT+2,VVOOOA,NV2*RCW*IRW,1)
         CALL RREAD(ITAPT+3,VVVOOA,NV3*RCW*IRW,1)
         CALL RREAD(ITAPT+4,VVOVOA,NV4*RCW*IRW,1)
         CALL XAXPY(NV1,A1,VOOOOA,1,VOOOO,1)
         CALL XAXPY(NV2,A1,VVOOOA,1,VVOOO,1)
         CALL XAXPY(NV3,A1,VVVOOA,1,VVVOO,1)
         CALL XAXPY(NV4,A1,VVOVOA,1,VVOVO,1)
      ENDIF
      CALL RWRIT(ITAPT+1,VOOOO,NV1*RCW*IRW,1)
      CALL RWRIT(ITAPT+2,VVOOO,NV2*RCW*IRW,1)
      CALL RWRIT(ITAPT+3,VVVOO,NV3*RCW*IRW,1)
      CALL RWRIT(ITAPT+4,VVOVO,NV4*RCW*IRW,1)
C
C     This is a restart point : save the files and update status.
C
  150 CONTINUE
      CALL RSAVE(ITAPT+1,NINTSB1)
      CALL RSAVE(ITAPT+2,NINTSB2)
      CALL RSAVE(ITAPT+3,NINTSB3)
      CALL RSAVE(ITAPT+4,NINTSB4)
      IMSTAT(1) = 3
      CALL CCDUMP
      call memchk ('after first sort',cc,1)
C
      NINTSB5 = MAX0(MREMAIN/512,20)
      NINTSB5 = MIN0(NV5*RCW/512+1,NINTSB5)
      CALL RFILE(ITAPT+5,NINTSB5)
      MREMAIN = MREMAIN - (NINTSB5-20)*512
C
      NINTSB6 = 20
      CALL RFILE(ITAPT+6,NINTSB6)
C
      IF (IMSTAT(2).GT.1.AND..NOT.FOCKSP) THEN
         if (myproc.eq.master) WRITE (IW,1011)
         GOTO 550
      ENDIF
C
C     Determine # of passes necessary for treating VOVV integrals
C     on a specific node
C
      NPASS = (NV5PAR-1) / NVT + 1
      NSEC5 = 1
      N5L = 0
      if (myproc.eq.master) WRITE (IW,1005) NPASS,'VOVV'
C
C
C     N5CTL=0
C   start of IPASS loop for the VOVV
C ----------------------------
      DO IPASS = 1, NPASS
C ----------------------------

      N5F = N5L + 1
      N5L = MIN0(N5L+NVT,NV5PAR)
      N5 = N5L - N5F + 1
      NSEC = N5*IRW*RCW / SEC2I(1)
      IF (MOD(N5*IRW*RCW,SEC2I(1)).NE.0) NSEC = NSEC + 1
C
      IF (INTERFACE(1:7).NE.'MOLFDIR'.OR.MYPROC.EQ.MASTER) THEN
         REWIND (MDINT)
         READ (MDINT)
      ENDIF
C
      CALL XCOPY (N5,A0,0,VVOVV,1)
      CALL XCOPY (N5,A0,0,VVOVVA,1)
C
  200 CALL GET2INTS (MDINT,KR,NSP,INDEX,ITRSGN,14,INDK,INDL,
     &               NZ,NZE,CBUF,IBUF)
      IF (NZ.LT.0) GOTO 210
      DO 202 INZE = 1, NZE
            ITYP = IBUF(1,INZE)
            IREP = IBUF(2,INZE) 
            IND1 = IBUF(3,INZE)
            IND2 = IBUF(4,INZE)
            ITRS = IBUF(5,INZE)
            JTRS = IBUF(6,INZE)
            I    = IBUF(7,INZE)
            IF (ITYP.EQ.14) THEN       

C             Check absolute address and decide if this integral goes
C             to this node

               IND = IVOVVT(IREP) + (IND1-1)*NVO(IREP) + IND2
               IF (IND.GT.IDIST(3,2,IREP).AND.
     &           IND.LE.IDIST(4,2,IREP)) THEN

C                now check relative offset if we need it in
C                the current path

                 IND = IND -IDIST(3,2,IREP) + IDIST(5,2,IREP)
                 IF (IND.GE.N5F.AND.IND.LE.N5L) THEN
                    IND = IND - N5F + 1
                    VVOVV(1,IND) = CBUF(1,I) * ITRS
                    IF (CARITH) VVOVV(2,IND) = CBUF(2,I) * JTRS
                 ENDIF
               ENDIF
            ELSEIF (ITYP.EQ.-14) THEN
               IND = IVOVVT(IREP) + (IND1-1)*NVO(IREP) + IND2

C             Check absolute address and decide if this integral goes
C             to this node

               IF (IND.GT.IDIST(3,2,IREP).AND.
     &           IND.LE.IDIST(4,2,IREP)) THEN

                 IND = IND -IDIST(3,2,IREP) + IDIST(5,2,IREP)
                 IF (IND.GE.N5F.AND.IND.LE.N5L) THEN
                    IND = IND - N5F + 1
                    VVOVVA(1,IND) = CBUF(1,I) * ITRS
                    IF (CARITH) VVOVVA(2,IND) = CBUF(2,I) * JTRS
                 ENDIF
               ENDIF
            ENDIF
  202 CONTINUE
      GOTO 200
C
C     ADD THE DIRECT AND EXCHANGE INTEGRALS
C
  210 CALL XAXPY (N5,-A1,VVOVVA,1,VVOVV,1)
C
C     ADD BREIT AND COULOMB INTEGRALS
C
      IF (BREIT.AND.ITRSGN.LT.0) THEN
         CALL RREAD(ITAPT+5,VVOVVA,N5*RCW*IRW,NSEC5)
         CALL XAXPY(N5,A1,VVOVVA,1,VVOVV,1)
      ENDIF
      CALL RWRIT(ITAPT+5,VVOVV,N5*RCW*IRW,NSEC5)
      NSEC5 = NSEC5 + NSEC
C
C    for debugging purposes: count nonzero VOVV ints in this batch.
C
C     DO IX=1,N5
C       IF(VVOVV(1,IX).NE.0.0d0) N5CTL=N5CTL+1
C     ENDDO
C
C  end of IPASS LOOP
C ----------------------
      ENDDO
C ----------------------
      if (myproc.eq.master) WRITE(IW,*) 'VOVV sorting done.'
C
      IF (.NOT.SRTALL) GOTO 600
C
C     GET THE VVVV INTEGRALS
C
      NPASS = (NV6PAR-1) / NVT + 1
      NSEC6 = 1
      N6L = 0
      if (myproc.eq.master) WRITE (IW,1005) NPASS,'VVVV'
C
C    start of IPASS loop for the VVVV
C     N6CTL=0
C --------------------------
      DO IPASS = 1, NPASS
C --------------------------

      N6F = N6L + 1
      N6L = MIN0(N6L+NVT,NV6PAR)
      N6 = N6L - N6F + 1
      NSEC = N6*IRW*RCW / SEC2I(1)
      IF (MOD(N6*IRW*RCW,SEC2I(1)).NE.0) NSEC = NSEC + 1
C
      IF (INTERFACE(1:7).NE.'MOLFDIR'.OR.MYPROC.EQ.MASTER) THEN
         REWIND (MDINT)
         READ (MDINT)
      ENDIF
C
      CALL XCOPY (N6,A0,0,VVVVV,1)
      CALL XCOPY (N6,A0,0,VVVVVA,1)
C
  300 CALL GET2INTS (MDINT,KR,NSP,INDEX,ITRSGN,16,INDK,INDL,
     &               NZ,NZE,CBUF,IBUF)
      IF (NZ.LT.0) GOTO 310
      DO 302 INZE = 1, NZE
            ITYP = IBUF(1,INZE)
            IREP = IBUF(2,INZE) 
            IND1 = IBUF(3,INZE)
            IND2 = IBUF(4,INZE)
            ITRS = IBUF(5,INZE)
            JTRS = IBUF(6,INZE)
            I    = IBUF(7,INZE)
            IF (ITYP.EQ.16) THEN       
C
C              The integral should go to this node : check the absolute
C              address to see if it belongs here. 
C
               IND = IVVVVTT(IREP) + (IND1-1)*NVVT(IREP) + IND2
               IF (IND.GT.IDIST(3,1,IREP).AND.
     &             IND.LE.IDIST(4,1,IREP)) THEN
C
C                 Now check the relative offset, whether we need it in
C                 the current pass.
C
                  IND = IND - IDIST(3,1,IREP) + IDIST(5,1,IREP)
                  IF (IND.GE.N6F.AND.IND.LE.N6L) THEN
                     IND = IND - N6F + 1
                     VVVVV(1,IND) = CBUF(1,I) * ITRS
                     IF (CARITH) VVVVV(2,IND) = CBUF(2,I) * JTRS
                  ENDIF
               ENDIF
            ELSEIF (ITYP.EQ.-16) THEN
              IND = IVVVVTT(IREP) + (IND1-1)*NVVT(IREP) + IND2
              IF (IND.GT.IDIST(3,1,IREP).AND.
     &             IND.LE.IDIST(4,1,IREP)) THEN
C
C                 Now check the relative offset, whether we need it in
C                 the current pass.
C
                  IND = IND - IDIST(3,1,IREP) + IDIST(5,1,IREP)
                  IF (IND.GE.N6F.AND.IND.LE.N6L) THEN
                     IND = IND - N6F + 1
                     VVVVVA(1,IND) = CBUF(1,I) * ITRS
                     IF (CARITH) VVVVVA(2,IND) = CBUF(2,I) * JTRS
                  ENDIF
               ENDIF
            ENDIF
  302 CONTINUE
      GOTO 300
C
C     ADD THE DIRECT AND EXCHANGE INTEGRALS
C
  310 CALL XAXPY (N6,-A1,VVVVVA,1,VVVVV,1)
C
C     ADD BREIT AND COULOMB INTEGRALS
C
      IF (BREIT.AND.ITRSGN.LT.0) THEN
         CALL RREAD(ITAPT+6,VVVVVA,N6*RCW*IRW,NSEC6)
         CALL XAXPY(N6,A1,VVVVVA,1,VVVVV,1)
      ENDIF
      CALL RWRIT(ITAPT+6,VVVVV,N6*RCW*IRW,NSEC6)
      NSEC6 = NSEC6 + NSEC
C
C    for debugging purposes: count nonzero VVVV ints in this batch.
C
C     DO IX=1,N6
C       IF(VVVVV(1,IX).NE.0.0d0) N6CTL=N6CTL+1
C     ENDDO
C
C     end of IPASS loop for the VVVV
C --------------------------
      ENDDO
C --------------------------
C
  500 CLOSE(MDINT)
      if (myproc.eq.master) WRITE(IW,*) 'VVVV sorting done.'

C
C  ... debugging ...
C
C     IF(N6CTL.NE.NV6) THEN
C       WRITE(IW,*) 'VVVV diff. in #:',N6CTL,NV6
C     ELSE
C       WRITE(IW,*) 'VVVV numbers ok.'
C     ENDIF
C
C     IF(N5CTL.NE.NV5) THEN
C       WRITE(IW,*) 'VOVV diff. in #:',N5CTL,NV5
C     ELSE
C       WRITE(IW,*) 'VOVV numbers ok.'
C     ENDIF
C
C  NOW PASS THE BREIT INTEGRAL FILE IF DESIRED
C  Breit is only used with MOLFDIR in this version
C
      IF (BREIT.AND.ITRSGN.EQ.1) THEN
         IF (MYPROC.EQ.MASTER) THEN
            WRITE (IW,1000) 'Coulomb',DATEX,TIMEX,ICOUNT
            OPEN(MDINT,FILE='MDBINT',FORM='UNFORMATTED')
            REWIND(MDINT)
            READ (MDINT,ERR=10001,END=10001) DATEX,TIMEX,NKR2
            IF (NKR2.NE.NKR) GOTO 10003
         ENDIF
         ITRSGN = -1
         GOTO 1
      ELSEIF (BREIT) THEN
         if (myproc.eq.master) 
     &      WRITE (IW,1000) 'Breit',DATEX,TIMEX,ICOUNT
      ELSE
         if (myproc.eq.master) 
     &      WRITE (IW,1000) 'Coulomb',DATEX,TIMEX,ICOUNT
      ENDIF
C
C     This is a restart point : save the files and update status.
C
  550 CONTINUE
      CALL RSAVE(ITAPT+5,NINTSB5)
      CALL RSAVE(ITAPT+6,NINTSB6)
      IMSTAT(2) = 3
      CALL CCDUMP
      call memchk ('after second sort',cc,1)
C
  600 if (myproc.eq.master) then
      IF (SRTALL) THEN
      WRITE (IW,1004) 'OOOO',NV1,'VOOO',NV2,'VVOO',NV3,'VOVO',NV4,
     &                'VOVV',NV5,'VVVV',NV6
      ELSE
      WRITE (IW,1004) 'OOOO',NV1,'VOOO',NV2,'VVOO',NV3,'VOVO',NV4,
     &                'VOVV',NV5
      ENDIF
      endif
C
      CALL GETFM (INDEX,NSP,EPS,BUF1,BUF2,
     &            VVOVO,VVOVOA,FOO,FVO,FVV,E0,E1)
      call memchk ('after fock build',cc,1)
C
C     This is a restart point : save the files and update status.
C
      CALL RSAVE(ITAPT+0,NINTSB0)
      IMSTAT(3) = 3
      CALL CCDUMP
C
      ESCF = E0 + E1 
      if (myproc.eq.master) WRITE(IW,1003) ECORE,E0,E1,ESCF,ESCF+ECORE
      ESCF = ESCF + ECORE
      ETOT = ESCF
C
 1000 FORMAT (/' Reading ',A7,' integrals :'/' File date :',T18,A10
     &/' File time :',T20,A8,/' # of integrals',I8)
 1005 FORMAT (/' Need ',I6,' passes to sort ',A4,' integrals')
 1004 FORMAT (/' Expanded and sorted integrals to 6 types :'/
     & (' Type ',A4,' :',I10,' integrals'))
 1003 FORMAT (//' Nuclear repulsion + core energy :',T40,F25.15
     &/' Zero order electronic energy :',T40,F25.15 
     &/' First order electronic energy :',T40,F25.15 
     &/' Electronic energy :',T40,F25.15 
     &/' SCF energy :',T40,F25.15) 
 1010 FORMAT (/' Skipping integral sorting step 1')
 1011 FORMAT (/' Skipping integral sorting step 2')
 1012 FORMAT (/' Skipping Fock-matrix build')
      RETURN
10000 CALL QUIT('ERROR READING HEADER MDCINT')
10001 CALL QUIT('ERROR READING HEADER MDBINT')
10003 CALL QUIT('MDCINT AND MDBINT FILES ARE INCOMPATIBLE')
10010 CALL QUIT('ERROR READING INTEGRALS FROM MDCINT')
10011 CALL QUIT('ERROR READING INTEGRALS FROM MDBINT')
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE RDPROP(IPRNT,NAMEA,NSP,RDDIAG,PROP,IREPSPI,APHASE,
     &                  AVO,AOV,AOO,AVV)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Reads property integral files for operator A
C     If (RDDIAG) it reads also the diagonal blocks (OO and VV)
C     This version reads from the file created by PRTRAN
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      INTEGER NSP,IREPSPI(NSP,16,2)
      REAL*8 AVO(*),AOV(*),AOO(*),AVV(*)
      REAL*8 PROP(2,NSP,NSP)
      REAL*8 APHASE(2)
      CHARACTER*8 NAMEA
      LOGICAL RDDIAG,PHASE
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "symm.inc"
      INCLUDE "complex.inc"
      INCLUDE "files.inc"
      INCLUDE "ccpar.inc"
#if defined (VAR_MPI)
      INCLUDE 'mpif.h'
      DIMENSION ISTAT(MPI_STATUS_SIZE)
#endif
C
C---------------Local variables--------------------------------------
C
      CHARACTER*32 ACHAR
      REAL*8 AVOI,AOVI,AMAXR,AMAXI,ANORM
C
C---------------Executable code--------------------------------------
C
C
C     Read symmetry information (master node only)
C
      IF (MYPROC.EQ.MASTER) THEN
C
      OPEN (MDPROP,FILE='MDPROP',FORM='UNFORMATTED')
      INOP = 0
    1 READ (MDPROP,ERR=10,END=11) ACHAR
      IF (ACHAR(1:8).NE.'********'.OR.ACHAR(25:32).NE.NAMEA) GOTO 1
      IF (IPRNT.GE.1) WRITE (IW,1000) NAMEA,ACHAR(9:16),ACHAR(17:24)
      READ (MDPROP) PROP
      CLOSE (MDPROP,STATUS='KEEP')
      GOTO 12
   10 INOP = 1
      GOTO 12
   11 INOP = 2
   12 CONTINUE
C
      ENDIF
C
C     Distribute the information read from MDPROP
C
#if defined (VAR_MPI)
      IF (NMPROC .GT. 1) THEN
         CALL MPI_BCAST(INOP,1,MPI_INTEGER,MASTER,MPI_COMM_WORLD,IERR)
         CALL MPI_BCAST(PROP,2*NSP*NSP,MPI_DOUBLE_PRECISION,MASTER,
     &                  MPI_COMM_WORLD,IERR)
      END IF
#endif
C
C     Error exit if the integrals could not be read
C
      IF (INOP.EQ.1) GOTO 101
      IF (INOP.EQ.2) GOTO 102
C
      APHASE(1) = D1
      APHASE(2) = D0
      PHASE = .FALSE.
C
    2 AMAXI = D0
      AAMAX = 0
      IIMAX = 0
      AI = 1
      DO AIREP = 1, NREP
         DO IREP = 1, NREP
            AREP = MULTB (IREP,AIREP+NREP,2)
            AREP = MULTB (AREP,NREP+1,2)
            DO I = 1, NO(IREP)
               II = IREPSPI(I,IREP,1)
               DO A = 1, NV(AREP)
                  AA = IREPSPI(A,AREP,2)
                  IF (CARITH) THEN
                      AVO(RCW*AI-1) = PROP(1,AA,II)
                      AVO(RCW*AI)   = PROP(2,AA,II)
                      AOV(RCW*AI-1) = PROP(1,II,AA)
                      AOV(RCW*AI)   = PROP(2,II,AA)
                   ELSE
                      AVO(AI) = PROP(1,AA,II)*APHASE(1) -
     &                          PROP(2,AA,II)*APHASE(2)
                      AOV(AI) = PROP(1,II,AA)*APHASE(1) -
     &                          PROP(2,II,AA)*APHASE(2)
                      AVOI = PROP(1,AA,II)*APHASE(2) +
     &                       PROP(2,AA,II)*APHASE(1)
                      AOVI = PROP(1,II,AA)*APHASE(2) +
     &                       PROP(2,II,AA)*APHASE(1)
                      IF (ABS(AVOI).GT.ABS(AMAXI)) THEN
                         AMAXR = AVO(AI) 
                         AMAXI = AVOI
                         AAMAX = AA
                         IIMAX = II
                      ENDIF
                      IF (ABS(AOVI).GT.ABS(AMAXI)) THEN
                         AMAXR = AOV(AI) 
                         AMAXI = AOVI 
                         AAMAX = II
                         IIMAX = AA
                      ENDIF
                   ENDIF
                   AI = AI + 1
               ENDDO
            ENDDO
         ENDDO
      ENDDO
C
      IF (.NOT.CARITH.AND.ABS(AMAXI).GT.ACCUR) THEN
         IF (PHASE) THEN
            WRITE (IW,1010) AA,II,AMAXI
            CALL QUIT('USE COMPLEX ARITHMETICS')
         ELSE
            APHASE(1) = AMAXR
            APHASE(2) = AMAXI 
            ANORM = SQRT(AMAXR*AMAXR+AMAXI*AMAXI)
            APHASE(1) = APHASE(1) / ANORM
            APHASE(2) = APHASE(2) / ANORM
            PHASE = .TRUE.
            IF (IPRNT.GE.1) WRITE (6,1020) NAMEA,APHASE
            GOTO 2
         ENDIF
      ENDIF
C
      IF (.NOT.RDDIAG) RETURN
C
      IJ = 1
      DO JREP = 1, NREP
         IREP = JREP
         DO J = 1, NO(JREP)
            JJ = IREPSPI(J,JREP,1)
            DO I = 1, NO(IREP)
               II = IREPSPI(I,IREP,1)
               IF (CARITH) THEN
                  AOO(RCW*IJ-1) = PROP(1,II,JJ)
                  AOO(RCW*IJ)   = PROP(2,II,JJ)
               ELSE
                  AOO(IJ) = PROP(1,II,JJ)*APHASE(1) -
     &                      PROP(2,II,JJ)*APHASE(2)
               ENDIF
               IJ = IJ + 1
            ENDDO
         ENDDO
      ENDDO
C
      AB = 1
      DO BREP = 1, NREP
         AREP = BREP
         DO B = 1, NV(BREP)
            BB = IREPSPI(B,BREP,2)
            DO A = 1, NV(AREP)
               AA = IREPSPI(A,AREP,2)
               IF (CARITH) THEN
                  AVV(RCW*AB-1) = PROP(1,AA,BB)
                  AVV(RCW*AB)   = PROP(2,AA,BB)
               ELSE
                  AVV(AB) = PROP(1,AA,BB)*APHASE(1) -
     &                      PROP(2,AA,BB)*APHASE(2)
               ENDIF
               AB = AB + 1
            ENDDO
         ENDDO
      ENDDO
C
      RETURN
  101 WRITE (6,*) ' Error reading property ',NAMEA,' on file MDPROP'
      CALL QUIT(' Error reading property integrals')
  102 WRITE (6,*) ' Property ',NAMEA,' not found on file MDPROP'
      CALL QUIT(' Property integrals missing')
 1000 FORMAT (/' Read integral type ',A8,' created ',
     & A8,' storage info : ',A8)
 1010 FORMAT (/' Largest imaginary part of matrix element',2I5,F10.2)
 1020 FORMAT (/' Property ',A8,' scaled with phase factor ',2F10.4)
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE GET2INTS (MDINT,KR,NSP,INDEX,ITRSGN,IATYP,INDK,INDL,
     &                     NZ,NZE,CBUF,IBUF)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Reads block of 2-electron integrals from MOLFDIR file MDINT
C     Expands Kramers' reduced list into full list.
C     Reorder from (ij|kl) to <ik||jl>
C     Expanded (16 permutations) indices are written in array IBUF :
C     IBUF(1,INZE) : Type (OOOO, VOOO, VVOO, VOVO, VVVO, VVVV)
C     IBUF(2,INZE) : Number of combined irrep
C     IBUF(3,INZE) : Index of ket pair
C     IBUF(4,INZE) : Index of bra pair
C     IBUF(5,INZE) : Sign under time-reversion (real part)
C     IBUF(6,INZE) : Sign under time-reversion and conjugations 
C     IBUF(7,INZE) : Pointer to integral values array 
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      INTEGER KR(-NSP:NSP),INDK(*),INDL(*)
      REAL*8 CBUF(RCW,*)
      INTEGER IBUF(7,*)
      INTEGER INDEX(NSP,NSP,4,*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "complex.inc"
      INCLUDE "eqns.inc"
      INCLUDE "inpt.inc"
      INCLUDE "ccpar.inc"
#if defined (VAR_MPI)
      INCLUDE 'mpif.h'
      DIMENSION ISTAT(MPI_STATUS_SIZE)
#endif
C
C---------------Local variables--------------------------------------
C
      INTEGER IKR,JKR,KKR,LKR
      REAL*8 RDUM
      LOGICAL FOCKSP
      CHARACTER*8 NEXTFIL
C
C---------------Executable code--------------------------------------
C
      FOCKSP = EQNS.EQ.'FOCKSP'
C
      IF (INTERFACE(1:7).NE.'MOLFDIR'.OR.MYPROC.EQ.MASTER) THEN
   1  IF (CARITH) THEN 
         READ (MDINT,END=10020,ERR=10010) IKR,JKR,NZ,
     &        (INDK(INZ),INDL(INZ),INZ=1,NZ),
     &        (CBUF(1,INZ),CBUF(2,INZ),INZ=1,NZ) 
      ELSE
         READ (MDINT,END=10020,ERR=10010) IKR,JKR,NZ,
     &        (INDK(INZ),INDL(INZ),INZ=1,NZ),
     &        (CBUF(1,INZ),RDUM,INZ=1,NZ) 
      ENDIF
      IF (IKR.EQ.0) THEN
         IF (JKR.EQ.0) THEN
            NZ = -1
         ELSE
            READ (MDINT) NEXTFIL
            CLOSE (MDINT,STATUS='KEEP')
            OPEN(MDINT,FILE=NEXTFIL,FORM='UNFORMATTED')
            GOTO 1
         ENDIF
      ENDIF
      ENDIF
#if defined (VAR_MPI)
      IF (NMPROC.GT.1 .AND. INTERFACE(1:7).EQ.'MOLFDIR') THEN
      CALL MPI_BCAST(IKR,1,MPI_INTEGER,MASTER,MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(JKR,1,MPI_INTEGER,MASTER,MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(NZ,1,MPI_INTEGER,MASTER,MPI_COMM_WORLD,IERR)
      IF (NZ.GT.0) THEN
         CALL MPI_BCAST(INDK,NZ,MPI_INTEGER,MASTER,MPI_COMM_WORLD,IERR)
         CALL MPI_BCAST(INDL,NZ,MPI_INTEGER,MASTER,MPI_COMM_WORLD,IERR)
         CALL MPI_BCAST(CBUF,NZ*RCW,MPI_DOUBLE_PRECISION,
     &                  MASTER,MPI_COMM_WORLD,IERR)
      ENDIF
      ENDIF
#endif
      IF (NZ.EQ.-1) RETURN
      IJSIGN = SIGN(1,IKR*JKR) * ITRSGN
C
C     Expand to the full list
C
      IF(FOCKSP) THEN
      NZE = 0 
      DO 50 INZ = 1, NZ
         KKR = INDK(INZ)
         LKR = INDL(INZ)
         I0 = KR(IKR)
         J0 = KR(JKR)
         IJSG = 1
         DO 40 ITR1 = 1, 2
            K0 = KR(KKR)
            L0 = KR(LKR)
            KLSG = 1
            DO 30 ITR2 = 1, 2
               I1 = I0
               J1 = J0
               K1 = K0
               L1 = L0
               DO 20 IPI = 1, 2
                  I = I1
                  J = J1
                  K = K1
                  L = L1
                  DO 10 ICC = 1, -1, -2
C
C                     One integral can contribute to a maximum of
C                     16 classes in Fock space CC calculations
C
                      DO ICASE = 1, 4
                      DO JCASE = 1, 4
                        JLTYP = INDEX(J,L,1,ICASE)
                        IKTYP = INDEX(I,K,1,JCASE)
                        KITYP = INDEX(K,I,1,JCASE)
                        JLIND = INDEX(J,L,4,JLTYP)
                        IKIND = INDEX(I,K,4,IKTYP)
                        KIIND = INDEX(K,I,4,KITYP)
                        IF (JLIND.NE.0.AND.IKIND.NE.0) THEN 
C                         Canonical integral : direct
                          NZE = NZE + 1
                          ITYP = (JLTYP-1)*4 + IKTYP
                          IBUF(1,NZE) = ITYP
                          IBUF(2,NZE) = INDEX(J,L,3,JLTYP)
                          IBUF(3,NZE) = JLIND
                          IBUF(4,NZE) = IKIND
                          IBUF(5,NZE) = IJSG * KLSG
                          IBUF(6,NZE) = ICC * IJSG * KLSG
                          IBUF(7,NZE) = INZ
C                         Skip unnecessary integrals
                          IF (IATYP.NE.ITYP.AND.ITYP.GT.6) NZE=NZE-1
                        ENDIF
C
                        IF (JLIND.NE.0.AND.KIIND.NE.0) THEN
C                         Canonical integral : exchange
                          NZE = NZE + 1
                          ITYP = (JLTYP-1)*4 + KITYP
                          ITYP = - ITYP
                          IBUF(1,NZE) = ITYP
                          IBUF(2,NZE) = INDEX(J,L,3,JLTYP)
                          IBUF(3,NZE) = JLIND
                          IBUF(4,NZE) = KIIND
                          IBUF(5,NZE) = IJSG * KLSG
                          IBUF(6,NZE) = ICC * IJSG * KLSG
                          IBUF(7,NZE) = INZ
C                         Skip unnecessary integrals
                          ITYP = - ITYP
                          IF (IATYP.NE.ITYP.AND.ITYP.GT.6) NZE=NZE-1
                        ENDIF
                      ENDDO
                      ENDDO
                     J = I1
                     I = J1
                     L = K1
                     K = L1
   10             CONTINUE
                  I1 = K0
                  J1 = L0
                  K1 = I0
                  L1 = J0
   20          CONTINUE
               K0 = KR(-LKR)
               L0 = KR(-KKR)
               KLSG = SIGN(1,KKR*LKR) * ITRSGN
   30       CONTINUE
            I0 = KR(-JKR)
            J0 = KR(-IKR)
            IJSG = IJSIGN
   40    CONTINUE
   50 CONTINUE
      ELSE
C
C     Expand to the full list
C
      NZE = 0 
      DO 150 INZ = 1, NZ
         KKR = INDK(INZ)
         LKR = INDL(INZ)
         I0 = KR(IKR)
         J0 = KR(JKR)
         IJSG = 1
         DO 140 ITR1 = 1, 2
            K0 = KR(KKR)
            L0 = KR(LKR)
            KLSG = 1
            DO 130 ITR2 = 1, 2
               I1 = I0
               J1 = J0
               K1 = K0
               L1 = L0
               DO 120 IPI = 1, 2
                  I = I1
                  J = J1
                  K = K1
                  L = L1
                  DO 110 ICC = 1, -1, -2
                       JLIND = INDEX(J,L,4,1)
                       IKIND = INDEX(I,K,4,1)
                       IF (JLIND.NE.0.AND.I.NE.K) THEN 
C                         Canonical integral : Direct or exchange
                          NZE = NZE + 1
                          IF (IKIND.NE.0) THEN  
C                            Contribution to direct part
                             ITYP = (INDEX(J,L,1,1)-1)*4+INDEX(I,K,1,1)
                             IBUF(1,NZE) = ITYP
                             IBUF(2,NZE) = INDEX(J,L,3,1)
                             IBUF(3,NZE) = INDEX(J,L,4,1)
                             IBUF(4,NZE) = INDEX(I,K,4,1)
                             IBUF(5,NZE) = IJSG * KLSG
                             IBUF(6,NZE) = ICC * IJSG * KLSG
                             IBUF(7,NZE) = INZ
C                            Skip unnecessary integrals
                             IF (IATYP.NE.ITYP.AND.ITYP.GT.6) NZE=NZE-1
                           ELSE
C                            Contribution to exchange part
                             ITYP = (INDEX(J,L,1,1)-1)*4+INDEX(K,I,1,1)
                             ITYP = - ITYP
                             IBUF(1,NZE) = ITYP
                             IBUF(2,NZE) = INDEX(J,L,3,1)
                             IBUF(3,NZE) = INDEX(J,L,4,1)
                             IBUF(4,NZE) = INDEX(K,I,4,1)
                             IBUF(5,NZE) = IJSG * KLSG
                             IBUF(6,NZE) = ICC * IJSG * KLSG
                             IBUF(7,NZE) = INZ
C                            Skip unnecessary integrals
                             ITYP = - ITYP
                             IF (IATYP.NE.ITYP.AND.ITYP.GT.6) NZE=NZE-1
                           ENDIF
                       ENDIF
                     J = I1
                     I = J1
                     L = K1
                     K = L1
  110             CONTINUE
                  I1 = K0
                  J1 = L0
                  K1 = I0
                  L1 = J0
  120          CONTINUE
               K0 = KR(-LKR)
               L0 = KR(-KKR)
               KLSG = SIGN(1,KKR*LKR) * ITRSGN
  130       CONTINUE
            I0 = KR(-JKR)
            J0 = KR(-IKR)
            IJSG = IJSIGN
  140    CONTINUE
  150 CONTINUE
      ENDIF
C
      RETURN
10010 CALL QUIT('ERROR READING 2-E INTEGRALS')
10020 CALL QUIT('END READING 2-E INTEGRALS')
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE GETFM (INDEX,NSP,EPS,BUF1,BUF2,VBUF1,VBUF2,
     &                  FOO,FVO,FVV,E0,E1)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Generates or converts Fock matrix. Calculates E0 and E1
C
C---------------Routines called----------------------------------------
C
C     MAKEFM, MPI_BCAST
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 E0,E1,EPS(*)
      REAL*8 FOO(*),FVO(*),FVV(*)
      REAL*8 BUF1(*),BUF2(*),VBUF1(*),VBUF2(*)
      INTEGER INDEX(NSP,NSP,4,*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "symm.inc"
      INCLUDE "eqns.inc"
      INCLUDE "inpt.inc"
      INCLUDE "complex.inc"
      INCLUDE "results.inc"
      INCLUDE "files.inc"
      INCLUDE "ccpar.inc"
#if defined (VAR_MPI)
      INCLUDE 'mpif.h'
      DIMENSION ISTAT(MPI_STATUS_SIZE)
#endif
C---------------Local variables--------------------------------------
C
      REAL*8 EPSDIF
      LOGICAL FOCKSP
      CHARACTER*2 ORBTYP
C
C---------------Executable code--------------------------------------
C
      NSPQ = NSP*NSP
      FOCKSP = EQNS.EQ.'FOCKSP'
C
      IF (NORECMP) GOTO 100
C
C  READ ONE ELECTRON INTEGRALS
C
      IF (MYPROC.EQ.MASTER) THEN
        OPEN (MRCONEE,FILE='MRCONEE',FORM='UNFORMATTED')
        READ (MRCONEE)
        READ (MRCONEE) 
        READ (MRCONEE) 
        READ (MRCONEE) 
        READ (MRCONEE) 
        READ (MRCONEE) (BUF1(IJ),BUF2(IJ),IJ=1,NSPQ)
        CLOSE (MRCONEE)
      ENDIF
C
#if defined (VAR_MPI)
      IF (NMPROC.GT.1) THEN
         CALL MPI_BCAST(BUF1,NSPQ,MPI_DOUBLE_PRECISION,MASTER,
     &                  MPI_COMM_WORLD,IERR)
         CALL MPI_BCAST(BUF2,NSPQ,MPI_DOUBLE_PRECISION,MASTER,
     &                  MPI_COMM_WORLD,IERR)
      END IF
#endif
C  
C  ORDER AND PUT IN FOCK MATRIX
C
      IF (FOCKSP) THEN
         NCASE = 4
      ELSE
         NCASE = 1
      ENDIF
C
      DO ICASE = 1, NCASE
         IJ = 0
         DO J = 1, NSP
            DO I = 1, NSP
               IJ = IJ + 1
               ITYP = INDEX(I,J,1,ICASE)
               IND = INDEX(I,J,2,ICASE)
               IF (FOCKSP) IND = INDEX(I,J,2,ITYP)
               IF (IND.NE.0) THEN
               IF (ITYP.EQ.1) THEN
                  IF (CARITH) THEN
                     FOO(RCW*IND-1) = BUF1(IJ)
                     FOO(RCW*IND)   = - BUF2(IJ)
                  ELSE
                     FOO(IND) = BUF1(IJ)
                  ENDIF
               ELSEIF (ITYP.EQ.2) THEN
                  IF (CARITH) THEN
                     FVO(RCW*IND-1) = BUF1(IJ)
                     FVO(RCW*IND)   = - BUF2(IJ)
                  ELSE
                     FVO(IND) = BUF1(IJ)
                  ENDIF
               ELSEIF (ITYP.EQ.4) THEN
                  IF (CARITH) THEN
                     FVV(RCW*IND-1) = BUF1(IJ)
                     FVV(RCW*IND)   = - BUF2(IJ)
                  ELSE
                     FVV(IND) = BUF1(IJ)
                  ENDIF
               ENDIF
               ENDIF
            ENDDO
         ENDDO
      ENDDO
C
  100 CONTINUE
C
      IF (.NOT.NORECMP) THEN
C
C        Form the Fock matrix by adding the two-electron contributions
C        to the (effective) one-electron matrix elements
C
C        We calculate the 1-determinant energy, which can be compared
C        to the value given by the DHF code as a check on the correctness
C        of the index transformations.
C
         CALL MAKEFM (VBUF1,VBUF2,FOO,FVO,FVV,E0,E1)
C
      ELSE
C
C        If the user specified norecmp = no recompute
C        We use the diagonal matrix of orbital energies and do not recalculate
C        the Fock matrix. This will only work if the orbitals are
C        generated with the same fock operator as used here (in general
C        only for closed shell systems) !
C
C        We use makefm to calculate E1. F and E0 are incorrect.
C
         CALL XCOPY (NFOO,A0,0,FOO,1)
         CALL XCOPY (NFVO,A0,0,FVO,1)
         CALL XCOPY (NFVV,A0,0,FVV,1)
         CALL MAKEFM (VBUF1,VBUF2,FOO,FVO,FVV,E0,E1)
         CALL XCOPY (NFOO,A0,0,FOO,1)
         CALL XCOPY (NFVO,A0,0,FVO,1)
         CALL XCOPY (NFVV,A0,0,FVV,1)
C
C        Now fill in the matrix and calculate E0.
C
         E0 = 0.D0
         I = 1
         II = 1
         DO IREP = 1, NREP
            DO J = 1, NO(IREP)
               IF (J.LE.NE(IREP)) E0 = E0 + EPS(I)
               FOO(II) = EPS(I)
               I = I + 1
               II = II + (NO(IREP) + 1) * RCW
            ENDDO
            II = II - NO(IREP) * RCW
         ENDDO
C
         II = 1
         DO IREP = 1, NREP
            DO J = 1, NV(IREP)
               FVV(II) = EPS(I)
               I = I + 1
               II = II + (NV(IREP) + 1) * RCW
            ENDDO
            II = II - NV(IREP) * RCW
         ENDDO
C
         IF (FOCKSP) THEN
C        
C        We also need to take the oa,oa and va,va elements in FVO into account
C        Copy them from FOO
C
            DO IREP = 1, NREP
C              The (oa,oa) elements
               IJOO = IOO(IREP)
               IJVO = IVO(IREP)
               IJOO = (IJOO + NO(IREP) * NIO(IREP) + NIO(IREP))*RCW + 1
               IJVO = (IJVO + NV(IREP) * NIO(IREP) + NSV(IREP))*RCW + 1
               DO J = NIO(IREP)+1, NSO(IREP)
                  CALL XCOPY (NAO(IREP),FOO(IJOO),1,FVO(IJVO),1)
                  IJOO = IJOO + NO(IREP) * RCW
                  IJVO = IJVO + NV(IREP) * RCW
               ENDDO
C              The (va,va) elements
               IJOO = IOO(IREP)
               IJVO = IVO(IREP)
               IJOO = (IJOO + NO(IREP) * NSO(IREP) + NSO(IREP))*RCW + 1
               IJVO = (IJVO + NV(IREP) * NSO(IREP)            )*RCW + 1
               DO J = NSO(IREP)+1, NO(IREP)
                  CALL XCOPY (NAV(IREP),FOO(IJOO),1,FVO(IJVO),1)
                  IJOO = IJOO + NO(IREP) * RCW
                  IJVO = IJVO + NV(IREP) * RCW
               ENDDO
            ENDDO
         ENDIF
C
      ENDIF
C
      CALL FMTOFILE (LTR,FVO,FOO,FVV)
C
      if (myproc.eq.master) WRITE (IW,1001)
      II = 1
      I  = 1
      DO IREP = 1, NREP
         DO J = 1, NO(IREP)
            ORBTYP = 'O '
            IF (FOCKSP) THEN
               IF (J.LE.NIO(IREP)) THEN
                  ORBTYP = 'Oi'
               ELSEIF (J.LE.NIO(IREP)+NAO(IREP)) THEN
                  ORBTYP = 'Oa'
               ELSE 
                  ORBTYP = 'Va'
               ENDIF
            ENDIF
            EPSDIF = ABS(EPS(I)-FOO(II))
#if defined (BIT64)
            IF ( ABS(FOO(II)).GT.1.0 ) EPSDIF = EPSDIF /  ABS(FOO(II))
#else
            IF (ABS(FOO(II)).GT.1.D0) EPSDIF = EPSDIF / ABS(FOO(II))
#endif
            IF ((EPSDIF.GT.1.D-7.OR.IPRNT.GE.1).and.myproc.eq.master)
     &         WRITE (IW,1002) ORBTYP,J,I,REPNA(IREP),EPS(I),FOO(II)
            IF (.NOT.USEOE) EPS(I) = FOO(II)
            I  = I  + 1
            II = II + (NO(IREP) + 1) * RCW
         ENDDO
         II = II - NO(IREP) * RCW
      ENDDO
      II = 1
      DO IREP = 1, NREP
         DO J = 1, NV(IREP)
            ORBTYP = 'V '
            IF (FOCKSP) THEN
               IF (J.LE.NAV(IREP)) THEN
                  ORBTYP = 'Va'
               ELSEIF (J.LE.NAV(IREP)+NIV(IREP)) THEN
                  ORBTYP = 'Vi'
               ELSE 
                  ORBTYP = 'Oa'
               ENDIF
            ENDIF
            EPSDIF = ABS(EPS(I)-FVV(II))
#if defined (BIT64)
            IF ( ABS(FVV(II)).GT.1.0 ) EPSDIF = EPSDIF /  ABS(FVV(II))
#else
            IF (ABS(FVV(II)).GT.1.D0) EPSDIF = EPSDIF / ABS(FVV(II))
#endif
            IF ((EPSDIF.GT.1.D-7.OR.IPRNT.GE.2).and.myproc.eq.master)
     &         WRITE (IW,1002) ORBTYP,J,I,REPNA(IREP),EPS(I),FVV(II)
            IF (.NOT.USEOE) EPS(I) = FVV(II)
            I  = I  + 1
            II = II + (NV(IREP) + 1) * RCW
         ENDDO
         II = II - NV(IREP) * RCW
      ENDDO
C
      if (myproc.eq.master) then
      IF (USEOE) THEN
         WRITE (IW,1003)
      ELSE
         WRITE (IW,1004)
      ENDIF
      endif
C
 1001 FORMAT (//' Checking the orbital energies, the program computes',
     & ' the diagonal elements of the'/' reconstructed Fock matrix.',
     & ' Differences with the reference orbital energies'/
     & ' are given if above a treshold or if iprnt > 1'
     &//' Spinor   Abelian Rep.',9X,'Energy   Recalc. Energy')
 1002 FORMAT (2x,A2,I4,I5,2X,A4,4X,2F16.10)
 1003 FORMAT(/' The original energies (left column) are used in',
     &' perturbation expressions.')
 1004 FORMAT(/' The diagonal elements of the recomputed Fock matrix',
     &' (right column) are used in perturbation expressions.')
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE MAKEFM (BUF1,BUF2,FOO,FVO,FVV,E0,E1)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Generates Fock matrix. Calculates E0 and E1
C
C---------------Routines called----------------------------------------
C
C     GETOOOO, GETVOOO, GETVOVO, SRT1TT4, SRT1ST4, SRT1SS4
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 E0,E1
      REAL*8 FOO(*),FVO(*),FVV(*)
      REAL*8 BUF1(*),BUF2(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "complex.inc"
      INCLUDE "symm.inc"
      INCLUDE "files.inc"
C
C---------------Local variables--------------------------------------
C
      REAL*8 XSUM
C
C---------------Executable code--------------------------------------
C
      E0 = AR0
      E1 = AR0
C
      CALL GETOOOO (BUF1)
      CALL SRT1TT4 (NREP,MULTB,LTR,LFA,NO,NO,NO,NO,MOO,JOOOO,JJOO,JJOO,
     &              BUF1,BUF2)
      KK = 1
      DO KREP = 1, NREP
         DO K = 1, NO(KREP)
C---------------------------------------------------------------------
C  F(IJ) = F(IJ) + V(IJ,KK)
C---------------------------------------------------------------------
            IF (K.LE.NE(KREP)) CALL XAXPY(NFOO,A1,BUF2(KK),1,FOO,1)
C---------------------------------------------------------------------
C  E1 = E1 - V(II,KK)
C---------------------------------------------------------------------
            II = 0
            DO IREP = 1, NREP
               IF (K.LE.NE(KREP)) THEN
                  E1 = E1 - XSUM(NE(IREP),BUF2(II+KK),NO(IREP)+1)
               ENDIF
               II = II + NO(IREP) * NO(IREP) * RCW
            ENDDO
            KK = KK + (NO(KREP)+1) * NFOO * RCW
         ENDDO
         KK = KK - NO(KREP) * NFOO * RCW
      ENDDO
      E1 = E1 * DP5
C---------------------------------------------------------------------
C  E0 = E0 + FOO(I,I)
C---------------------------------------------------------------------
      II = 1
      DO IREP = 1, NREP
         E0 = E0 + XSUM(NE(IREP),FOO(II),NO(IREP)+1)
         II = II + NO(IREP) * NO(IREP) * RCW
      ENDDO
C
      CALL GETVOOO (BUF1)
      CALL SRT1ST4 (NREP,MULTB,LTR,NV,NO,NO,NO,MVO,JVOOO,JJVO,JJOO,
     &              BUF1,BUF2)
      KK = 1
      DO KREP = 1, NREP
         DO K = 1, NO(KREP)
C---------------------------------------------------------------------
C  F(AI) = F(AI) + V(AI,KK)
C---------------------------------------------------------------------
            IF (K.LE.NE(KREP)) CALL XAXPY(NFVO,A1,BUF2(KK),1,FVO,1)
            KK = KK + (NO(KREP)+1) * NFVO * RCW
         ENDDO
         KK = KK - NO(KREP) * NFVO * RCW
      ENDDO
C
      CALL GETVOVO (BUF1)
      CALL SRT1SS4 (NREP,MULTB,NV,NO,NV,NO,MVV,JVVOO,JJVV,JJOO,
     &              BUF1,BUF2)
      KK = 1
      DO KREP = 1, NREP
         DO K = 1, NO(KREP)
C---------------------------------------------------------------------
C  F(AB) = F(AB) + V(AB,KK)
C---------------------------------------------------------------------
            IF (K.LE.NE(KREP)) CALL XAXPY(NFVV,A1,BUF2(KK),1,FVV,1)
            KK = KK + (NO(KREP)+1) * NFVV * RCW
         ENDDO
         KK = KK - NO(KREP) * NFVV * RCW
      ENDDO
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE MKINDEX (NSP,IREPSP,IREPSPI,INDEX)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Set up index array for initial integral sort
C     Index(i,j,1) : type (oo,vo,ov,vv)
C     Index(i,j,2) : position of fm(i,j) in appropriate part of fock matrix
C     Index(i,j,3) : symmetry of ij pair
C     Index(i,j,4) : position of ij pair in batch of this symmetry
C
C---------------Routines called----------------------------------------
C
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      INTEGER IREPSP(NSP,4),IREPSPI(NSP,16,2),INDEX(NSP,NSP,4)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "symm.inc"
C
C---------------Local variables--------------------------------------
C
C
C---------------Executable code--------------------------------------
C
      DO J = 1, NSP
         IF (IREPSP(J,3).GT.0) THEN
            JJ = 0 
         ELSE 
            JJ = 2
         ENDIF
         DO I = 1, NSP
            IF (IREPSP(I,3).GT.0) THEN
               II = 1 
            ELSE 
               II = 2
            ENDIF
            INDEX(I,J,1) = II + JJ
            INDEX(I,J,2) = 0
            INDEX(I,J,3) = 0
            INDEX(I,J,4) = 0
         ENDDO
      ENDDO
C
      IJ = 0
      DO IREP = 1, NREP
         DO JJ = 1, NO(IREP)
            J = IREPSPI(JJ,IREP,1)
            DO II = 1, NO(IREP)
               I = IREPSPI(II,IREP,1)
               IJ = IJ + 1
               INDEX(I,J,2) = IJ
            ENDDO
         ENDDO
      ENDDO
C
      IJ = 0
      DO IREP = 1, NREP
         DO JJ = 1, NO(IREP)
            J = IREPSPI(JJ,IREP,1)
            DO II = 1, NV(IREP)
               I = IREPSPI(II,IREP,2)
               IJ = IJ + 1
               INDEX(I,J,2) = IJ
            ENDDO
         ENDDO
      ENDDO
C
      IJ = 0
      DO IREP = 1, NREP
         DO JJ = 1, NV(IREP)
            J = IREPSPI(JJ,IREP,2)
            DO II = 1, NO(IREP)
               I = IREPSPI(II,IREP,1)
               IJ = IJ + 1
               INDEX(I,J,2) = IJ
            ENDDO
         ENDDO
      ENDDO
C
      IJ = 0
      DO IREP = 1, NREP
         DO JJ = 1, NV(IREP)
            J = IREPSPI(JJ,IREP,2)
            DO II = 1, NV(IREP)
               I = IREPSPI(II,IREP,2)
               IJ = IJ + 1
               INDEX(I,J,2) = IJ
            ENDDO
         ENDDO
      ENDDO
C
      DO IJREP = 1, NREP
         IJ = 0
         DO JREP = 1, NREP
            DO 10 IREP = JREP, NREP
               IF (MULTB(IREP,JREP,1).NE.IJREP) GOTO 10
               DO JJ = 1, NO(JREP)
                  J = IREPSPI(JJ,JREP,1)
                  IMIN = 1
                  IF (IREP.EQ.JREP) IMIN = JJ + 1
                  DO II = IMIN, NO(IREP)
                     I = IREPSPI(II,IREP,1)
                     IJ = IJ + 1
                     INDEX(I,J,3) = IJREP
                     INDEX(I,J,4) = IJ
                  ENDDO
               ENDDO
 10         CONTINUE
         ENDDO
      ENDDO
C
      DO IJREP = 1, NREP
         IJ = 0
         DO JREP = 1, NREP
            DO 20 IREP = 1, NREP
               IF (MULTB(IREP,JREP,1).NE.IJREP) GOTO 20
               DO JJ = 1, NO(JREP)
                  J = IREPSPI(JJ,JREP,1)
                  DO II = 1, NV(IREP)
                     I = IREPSPI(II,IREP,2)
                     IJ = IJ + 1
                     INDEX(I,J,3) = IJREP
                     INDEX(I,J,4) = IJ
                  ENDDO
               ENDDO
 20         CONTINUE
         ENDDO
      ENDDO
C
      DO IJREP = 1, NREP
         IJ = 0
         DO JREP = 1, NREP
            DO 40 IREP = JREP, NREP
               IF (MULTB(IREP,JREP,1).NE.IJREP) GOTO 40
               DO JJ = 1, NV(JREP)
                  J = IREPSPI(JJ,JREP,2)
                  IMIN = 1
                  IF (IREP.EQ.JREP) IMIN = JJ + 1
                  DO II = IMIN, NV(IREP)
                     I = IREPSPI(II,IREP,2)
                     IJ = IJ + 1
                     INDEX(I,J,3) = IJREP
                     INDEX(I,J,4) = IJ
                  ENDDO
               ENDDO
 40         CONTINUE
         ENDDO
      ENDDO
C
      RETURN 
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE FKINDEX (NSP,IREPSP,IREPSPI,INDEX)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Set up index array for initial integral sort
C     Index(i,j,1) : type (oo,vo,ov,vv)
C     Index(i,j,2) : position of fm(i,j) in appropriate part of fock matrix
C     Index(i,j,3) : symmetry of ij pair
C     Index(i,j,4) : position of ij pair in batch of this symmetry
C
C---------------Routines called----------------------------------------
C
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      INTEGER IREPSP(NSP,4),IREPSPI(NSP,16,2),INDEX(NSP,NSP,4,4)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "symm.inc"
C
C---------------Local variables--------------------------------------
C
C
C---------------Executable code--------------------------------------
C
      DO IJCASE = 1, 4
         JCASE = (IJCASE-1)/2
         ICASE = MOD(IJCASE-1,2)
         DO J = 1, NSP
            IF (IREPSP(J,3+JCASE).GT.0) THEN
               JJ = 0 
            ELSE 
               JJ = 2
            ENDIF
            DO I = 1, NSP
               IF (IREPSP(I,3+ICASE).GT.0) THEN
                  II = 1 
               ELSE 
                  II = 2
               ENDIF
               INDEX(I,J,1,IJCASE) = II + JJ
               INDEX(I,J,2,IJCASE) = 0
               INDEX(I,J,3,IJCASE) = 0
               INDEX(I,J,4,IJCASE) = 0
            ENDDO
         ENDDO
      ENDDO
C
      IJ = 0
      DO IREP = 1, NREP
         DO JJ = 1, NO(IREP)
            J = IREPSPI(JJ,IREP,1)
            DO II = 1, NO(IREP)
               I = IREPSPI(II,IREP,1)
               IJ = IJ + 1
               INDEX(I,J,2,1) = IJ
            ENDDO
         ENDDO
      ENDDO
C
      IJ = 0
      DO IREP = 1, NREP
         DO JJ = 1, NO(IREP)
            J = IREPSPI(JJ,IREP,1)
            DO II = 1, NV(IREP)
               I = IREPSPI(II,IREP,2)
               IJ = IJ + 1
               INDEX(I,J,2,2) = IJ
            ENDDO
         ENDDO
      ENDDO
C
      IJ = 0
      DO IREP = 1, NREP
         DO JJ = 1, NV(IREP)
            J = IREPSPI(JJ,IREP,2)
            DO II = 1, NO(IREP)
               I = IREPSPI(II,IREP,1)
               IJ = IJ + 1
               INDEX(I,J,2,3) = IJ
            ENDDO
         ENDDO
      ENDDO
C
      IJ = 0
      DO IREP = 1, NREP
         DO JJ = 1, NV(IREP)
            J = IREPSPI(JJ,IREP,2)
            DO II = 1, NV(IREP)
               I = IREPSPI(II,IREP,2)
               IJ = IJ + 1
               INDEX(I,J,2,4) = IJ
            ENDDO
         ENDDO
      ENDDO
C
      DO IJREP = 1, NREP
         IJ = 0
         DO JREP = 1, NREP
            DO 10 IREP = JREP, NREP
               IF (MULTB(IREP,JREP,1).NE.IJREP) GOTO 10
               DO JJ = 1, NO(JREP)
                  J = IREPSPI(JJ,JREP,1)
                  IMIN = 1
                  IF (IREP.EQ.JREP) IMIN = JJ + 1
                  DO II = IMIN, NO(IREP)
                     I = IREPSPI(II,IREP,1)
                     IJ = IJ + 1
                     INDEX(I,J,3,1) = IJREP
                     INDEX(I,J,4,1) = IJ
                  ENDDO
               ENDDO
 10         CONTINUE
         ENDDO
      ENDDO
C
      DO IJREP = 1, NREP
         IJ = 0
         DO JREP = 1, NREP
            DO 20 IREP = 1, NREP
               IF (MULTB(IREP,JREP,1).NE.IJREP) GOTO 20
               DO JJ = 1, NO(JREP)
                  J = IREPSPI(JJ,JREP,1)
                  DO II = 1, NV(IREP)
                     I = IREPSPI(II,IREP,2)
                     IJ = IJ + 1
                     INDEX(I,J,3,2) = IJREP
                     INDEX(I,J,4,2) = IJ
                  ENDDO
               ENDDO
 20         CONTINUE
         ENDDO
      ENDDO
C
      DO IJREP = 1, NREP
         IJ = 0
         DO JREP = 1, NREP
            DO 40 IREP = JREP, NREP
               IF (MULTB(IREP,JREP,1).NE.IJREP) GOTO 40
               DO JJ = 1, NV(JREP)
                  J = IREPSPI(JJ,JREP,2)
                  IMIN = 1
                  IF (IREP.EQ.JREP) IMIN = JJ + 1
                  DO II = IMIN, NV(IREP)
                     I = IREPSPI(II,IREP,2)
                     IJ = IJ + 1
                     INDEX(I,J,3,4) = IJREP
                     INDEX(I,J,4,4) = IJ
                  ENDDO
               ENDDO
 40         CONTINUE
         ENDDO
      ENDDO
C
      RETURN 
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
