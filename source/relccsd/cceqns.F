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
C VERSION : $Revision: 1.2 $
C DATE    : $Date: 2001/09/12 23:26:11 $
C FILE    : cceqns.F
C SOURCE  : $Source: /home/tc/visscher/sources/Molfdir/Molfdir/source/relccsd/cceqns.F,v $
C
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE MP2EQN(EPS,FVO,T1,S1,T2,S2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Solves MP2 amplitude or lambda equations.
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
C     Intermediaries etc.
C
      REAL*8 EPS(*),FVO(*),T1(*),S1(*),T2(*),S2(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "files.inc"
      INCLUDE "param.inc"
      INCLUDE "symm.inc"
      INCLUDE "eqns.inc"
      INCLUDE "inpt.inc"
      INCLUDE "results.inc"
      INCLUDE "complex.inc"
      INCLUDE "ccpar.inc"
C
C---------------Local variables--------------------------------------
C
      REAL*8 CPUDELTA,CPU0,CPUE,CPUVAR,CPUTOT
      REAL*8 DIAG
      LOGICAL TEQ
C
C---------------Executable code--------------------------------------
C
      TEQ = .TRUE.
      IF (EQNS.EQ.'AMPLTD') THEN
         TEQ = .TRUE.
      ELSEIF (EQNS.EQ.'LAMBDA') THEN
         TEQ = .FALSE.
      ELSEIF (EQNS.EQ.'FOCKSP') THEN
         TEQ = .TRUE.
      ELSE
         CALL QUIT('Wrong keyword in call to MP2EQNS')
      ENDIF
C
C     Initialize the timing.
C
      CPU0 = CPUDELTA()
      CALL CPUUSED(CPU0)
      CPUE = D0
C
C  INITIALIZE T2 AND T1 AMPLITUDES
C
      CALL XCOPY (NDIMT1,FVO,1,T1,1)
      CALL GETVVOO (T2)
      CALL DENOM (EPS,T1,T2,T1,T2)
      CALL ZCORE (T1,T2)
C
      IF (TEQ) THEN
C
C  EVALUATE CORRELATION ENERGY 
C
         CALL MP2ENG (T1,T2,FVO,S2,EMP2)
         CALL T1DIAG (DIAG,T1,S1)
         CPUE = CPUE + CPUDELTA()
C
         if (myproc.eq.master) then
         WRITE(IW,6071)
         WRITE(IW,6072) ESCF
         WRITE(IW,6073) EMP2
         ETOT = ESCF + EMP2
         WRITE(IW,6075) ESCF + EMP2
         WRITE(IW,6077) DIAG
         endif
C
      ELSE
C
C  INITIALIZE LAMDA AMPLITUDES
C
         CALL XCOPY (NDIMT1,T1,1,S1,1)
         CALL XCOPY (NDIMT2,T2,1,S2,1)
C
      ENDIF
C
      CALL CPUUSED(CPUTOT)
      CPUTOT = CPUTOT-CPU0
      CPUVAR = CPUTOT-CPUE
      IF (TIMING.and.myproc.eq.master) 
     &   WRITE(IW,7000) CPU0,CPUE,CPUVAR,CPUTOT
C
      RETURN
 6071 FORMAT(//,'  MP2 results',/)
 6072 FORMAT(' SCF energy :',T40,F25.15)
 6073 FORMAT(' MP2 correlation energy :',T40,F25.15)
 6075 FORMAT(' Total MP2 energy :',T40,F25.15)
 6077 FORMAT(' T1 diagnostic :',T40,F25.15)
 7000 FORMAT(//' Timing of routine MP2EQN :'
     &/' Before MP2EQN :',T30,F12.3,' seconds'
     &/' Energy, T1-Diag :',T30,F12.3,' seconds'
     &/' Untimed parts :',T30,F12.3,' seconds'
     &/' Total time in MP2EQN :',T30,F12.3,' seconds')
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE CCEQN(EPS,FOO,FVO,FVV,T1,S1,T2,S2,AZ,CZ,H,
     &                 HO,HV,HOV,GOO,GVV,
     &                 BUF1,BUF2,BUF3,CC1,BB1,BB2,CONV,
     &                 NBUF2,NBUF3)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Solves CCSD amplitude or lambda equations.
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
C     Intermediaries etc.
C
      REAL*8 EPS(*),FOO(*),FVO(*),FVV(*),T1(*),S1(*),T2(*),S2(*)
      REAL*8 AZ(*),H(*),HO(*),HV(*),HOV(*),BUF1(*),BUF2(*),BUF3(*)
      REAL*8 CZ(*) ! is only allocated for Lambda equations
      REAL*8 GOO(*),GVV(*)
C
C     for diis ...
C
      REAL*8 CC1(*)
      REAL*8 BB1(*),BB2(*)
C
      REAL*8 RMS,CONV
      REAL*8 DIAG
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "files.inc"
      INCLUDE "param.inc"
      INCLUDE "symm.inc"
      INCLUDE "diis.inc"
      INCLUDE "eqns.inc"
      INCLUDE "inpt.inc"
      INCLUDE "results.inc"
      INCLUDE "complex.inc"
      INCLUDE "ccpar.inc"
#if defined (VAR_MPI)
      INCLUDE 'mpif.h'
      DIMENSION ISTAT(MPI_STATUS_SIZE)
#endif
C
C---------------Local variables--------------------------------------
C
      REAL*8 CPUDELTA,CPU0,CPUT1,CPUT2,CPUINT,CPU4V,CPUE,CPUVAR,CPUTOT
      LOGICAL TEQ,FOCKSP
      real*8 ddot,sdot
      complex*16 zdotc,cdotc
      CHARACTER*2 DELS1
      CHARACTER*4 DELS2
C
C---------------Executable code--------------------------------------
C
      IF (EQNS.EQ.'AMPLTD') THEN
         TEQ = .TRUE.
         FOCKSP = .FALSE.
      ELSEIF (EQNS.EQ.'LAMBDA') THEN
         TEQ = .FALSE.
         FOCKSP = .FALSE.
#ifdef MOD_FOCKSPCC
      ELSEIF (EQNS.EQ.'FOCKSP') THEN
         TEQ = .TRUE.
         FOCKSP = .TRUE.
         CALL PUTAMPT (T1,T2)
         DELS1 = 'DD'
         IF (ICURFSS.EQ. 1) DELS1 = 'DK'
         IF (ICURFSS.EQ.10) DELS1 = 'KD'
         IF (ICURFSS.EQ.11) DELS1 = 'KK'
         IF (ICURFSS.EQ. 2) DELS1 = 'DK'
         IF (ICURFSS.EQ.20) DELS1 = 'KD'
         DELS2 = 'DDDD'
         IF (ICURFSS.EQ. 1) DELS2 = 'DDDK'
         IF (ICURFSS.EQ.10) DELS2 = 'KDDD'
         IF (ICURFSS.EQ.11) DELS2 = 'KDDK'
         IF (ICURFSS.EQ. 2) DELS2 = 'DDKK'
         IF (ICURFSS.EQ.20) DELS2 = 'KKDD'
         CALL DELFCK ('VO',DELS1,T1)
         CALL DELINT ('VVOO',DELS2,T2,0,0,0)
         CALL DENOMF (EPS,T1,T2,T1,T2,2)
#endif
      ELSE
         CALL QUIT('Wrong keyword in call to CCEQNS')
      ENDIF
C
C     Initialize the timing.
C
      CPU0 = CPUDELTA()
      CALL CPUUSED(CPU0)
      CPUE = D0
      CPUINT = D0
      CPUT1 = D0
      CPUT2 = D0
      CPU4V = D0
C
C     Initialize the work arrays to zero.
C
      CALL XCOPY (NDIMT2,A0,0,BUF1,1)
      CALL XCOPY (NBUF2,A0,0,BUF2,1)
      CALL XCOPY (NBUF3,A0,0,BUF3,1)
C
C     Calculate record size for DIIS files.
C
      ITC = 0
      IT = 0
      NST1 = INTOWP(RCW*NDIMT1)/SEC2I(1)
      IF(NST1*SEC2I(1).NE.INTOWP(RCW*NDIMT1)) NST1 = NST1 + 1
      NST2 = INTOWP(RCW*NDIMT2)/SEC2I(1)
      IF(NST2*SEC2I(1).NE.INTOWP(RCW*NDIMT2)) NST2 = NST2 + 1
C
C     Open DIIS scratch files
C
      NINTSB = 20
      CALL RFILE(ITAMPL,NINTSB)
      CALL RFILE(ITAMPD,NINTSB)
C
      NIT = 0
      RMS = 1.D0
      ECCSD = 0.D0
C
      CALL T1DIAG(DIAG,T1,BUF1)
C
      IF (IPRNT.GE.1.and.myproc.eq.master) THEN
         WRITE(IW,6003)
         WRITE(IW,6001) NIT,ECCSD,RMS,DIAG
      ENDIF
C
C     Calculate fixed intermediaries for Lambda equations
C
      IF (.NOT.TEQ) THEN
         CALL GETAMPT (S1,S2)
         CALL HOINTM (FOO,S1,S2,BUF1,BUF2,BUF3,HO)
         CALL HVINTM (FVV,S1,S2,BUF1,BUF2,BUF3,HV)
         CALL HOVINTM (FVO,S1,BUF1,BUF2,HOV)
         CALL GOINTM (FVO,S1,BUF1,BUF2,HO)
         CALL GVINTM (FVO,S1,BUF3,NBUF3,BUF2,NBUF2,HV)
         CALL AINTM (S1,S2,BUF1,BUF2,AZ)
         CALL CINTM (T1,T2,FVO,AZ,BUF1,BUF2,BUF3,NBUF3,CZ)
         CALL DINTM (S1,S2,HOV,BUF1,BUF2,NBUF2,BUF3,NBUF3,H)
         CALL HINTM (S1,S2,BUF1,BUF2,BUF3,NBUF3,H)
         CPUINT = CPUINT + CPUDELTA()
      ENDIF
C
C  START ITERATIVE PROCEDURE
C
 1000 CONTINUE
      NIT = NIT + 1
      IF (NIT.GT.MAXIT) THEN
         NIT = NIT - 1
         if (myproc.eq.master) WRITE(IW,6004) RMS
         GO TO 2000
      ENDIF
C
      IF (.NOT.TEQ) THEN
C        
C        Make Gpq intermediates for lambda equations
C
         CALL GETAMPT (S1,S2)
         CALL MKGPP (S2,T2,BUF1,BUF2,GOO,GVV)
      ENDIF

C  EVALUATE T1 EQUATIONS
C  CHECK FOR CASES WITH NO NON_ZERO T1'S TO AVOID CRASHES
C
      IF (TEQ) THEN
           CALL HOINTM (FOO,T1,T2,BUF1,BUF2,BUF3,HO)
      ENDIF
      IF (TEQ) THEN
           CALL HVINTM (FVV,T1,T2,BUF1,BUF2,BUF3,HV)
      ENDIF
      IF (NDIMT1.GT.0) THEN
         IF (TEQ) THEN
          CALL HOVINTM (FVO,T1,BUF1,BUF2,HOV)
          CALL XTIME(3,1,'T1EQNS                        ')
          CALL T1EQNS (FVO,T1,T2,HO,HV,HOV,H,CZ,GOO,GVV,
     &                 BUF1,BUF2,NBUF2,BUF3,NBUF3,S1)
          CALL XTIME(3,2,'T1EQNS                        ')
         ELSE
          CALL T1EQNS (HOV,T1,T2,HO,HV,HOV,H,CZ,GOO,GVV,
     &                 BUF1,BUF2,NBUF2,BUF3,NBUF3,S1)
         ENDIF
      ENDIF
      CPUT1 = CPUT1 + CPUDELTA()

      
      if (carith.and.debug) then
      write (*,111)"ho",MYPROC,zdotc(nfoo,ho,1,ho,1)
      write (*,111)"hv",MYPROC,zdotc(nfvv,hv,1,hv,1)
      write (*,111)"hov",MYPROC,zdotc(ndimt1,hov,1,hov,1)
      write (*,111)"s1",MYPROC,zdotc(ndimt1,s1,1,s1,1)
      elseif (debug) then
      write (*,111)"ho",MYPROC,ddot(nfoo,ho,1,ho,1)
      write (*,111)"hv",MYPROC,ddot(nfvv,hv,1,hv,1)
      write (*,111)"hov",MYPROC,ddot(ndimt1,hov,1,hov,1)
      write (*,111)"s1",MYPROC,ddot(ndimt1,s1,1,s1,1)
      endif
C
C  EVALUATE T2 INTERMEDIATES
C
      IF (TEQ) THEN
         CALL GOINTM (FVO,T1,BUF1,BUF2,HO)
         CALL GVINTM (FVO,T1,BUF3,NBUF3,BUF2,NBUF2,HV)
         CALL AINTM (T1,T2,BUF1,BUF2,AZ)
         CALL XTIME(10,1,'HINTM                         ')
         CALL HINTM (T1,T2,BUF1,BUF2,BUF3,NBUF3,H)
         CALL XTIME(10,2,'HINTM                         ')

#if defined (VAR_MPI)
C *******************
C ** parallel case: the H-intermediate was partially calculated
C ** on the separate slaves. In order to perform the H*T2
C ** contraction we need the complete H array ==> we complete it
C ** here and synchronize all the nodes. They then have the complete
C ** H-array and this lengthy contraction can also be parallelized.
C
      IF (NMPROC .GT. 1) THEN
C ... synchronization timing
C       CALL XTIME(30,1,'HINTM synchronization         ')
C       CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
C       CALL XTIME(30,2,'HINTM synchronization         ')

        IF(NBUF3.LT.IVOVO(NREP+1)) 
     &    CALL QUIT('BUF3 can not keep the VOVO integrals !')
        CALL XCOPY (NBUF3,A0,0,BUF3,1)

C ... we collect the H array

        CALL XTIME(31,1,'HINTM MPI_REDUCE (comm step)  ')
        CALL MPI_REDUCE(H,BUF3,RCW*IVOVO(NREP+1),
     &    MPI_DOUBLE_PRECISION,MPI_SUM,MASTER,MPI_COMM_WORLD,IERR)
        IF(IERR.GT.0) CALL QUIT('MPI_REDUCE ERROR !')
        CALL XTIME(31,2,'HINTM MPI_REDUCE (comm step)  ')
        CALL XCOPY(RCW*IVOVO(NREP+1),BUF3,1,H,1)

C ... and redistribute it

        CALL XTIME(32,1,'HINTM MPI_BCAST  (comm step)  ')
        CALL MPI_BCAST(H,RCW*IVOVO(NREP+1),MPI_DOUBLE_PRECISION,
     &     MASTER,MPI_COMM_WORLD,IERR)
        IF(IERR.GT.0) CALL QUIT('MPI_BCAST ERROR !')
        CALL XTIME(32,2,'HINTM MPI_BCAST  (comm step)  ')
      ENDIF

#endif

      CPUINT = CPUINT + CPUDELTA()
      ENDIF

C
      if (carith.and.debug) then
      write (*,111)"go",MYPROC,zdotc(nfoo,ho,1,ho,1)
      write (*,111)"gv",MYPROC,zdotc(nfvv,hv,1,hv,1)
      write (*,111)"az",MYPROC,a2*a2*zdotc(ioooott(nrep+1),az,1,az,1)
      write (*,111)"h ",MYPROC,zdotc(ivovo(nrep+1),h,1,h,1)
      elseif (debug) then
      write (*,111)"go",MYPROC,ddot(nfoo,ho,1,ho,1)
      write (*,111)"gv",MYPROC,ddot(nfvv,hv,1,hv,1)
      write (*,111)"az",MYPROC,4.d0*ddot(ioooott(nrep+1),az,1,az,1)
      write (*,111)"h ",MYPROC,ddot(ivovo(nrep+1),h,1,h,1)
      endif
C
C  INTERMEDIATES FORMED --- START T2 EQUATIONS
C
      CALL XTIME(12,1,'T2 EQNS                       ')
      CALL T2EQNS (T1,T2,HO,HV,HOV,H,AZ,CZ,GOO,GVV,
     &             BUF1,BUF2,BUF3,NBUF3,S2)
      CALL XTIME(12,2,'T2 EQNS                       ')
      CPUT2 = CPUT2 + CPUDELTA()


C
C
C  MUST NOW ADD IN B INTERMEDIATE CONTRIBUTION
C
      if (carith.and.debug) then
      write (*,111)"s2",MYPROC,zdotc(ndimt2,s2,1,s2,1)
      elseif (debug) then
      write (*,111)"s2",MYPROC,ddot(ndimt2,s2,1,s2,1)
      endif
C      
      CALL XTIME(13,1,'BINTM                         ')
      CALL BINTM (T1,T2,BUF1,BUF2,NBUF2,BUF3,NBUF3,S2,IW)
      CALL XTIME(13,2,'BINTM                         ')

      if (carith.and.debug) then
      write (*,111)"s2 after bintm",MYPROC,zdotc(ndimt2,s2,1,s2,1)
      elseif (debug) then
      write (*,111)"s2 after bintm",MYPROC,ddot(ndimt2,s2,1,s2,1)
      endif
      CPU4V = CPU4V + CPUDELTA()
C
C  ADD PARTIAL T1 AND T2 (PARALLEL RUNS)
C
C     CALL XTIME(35,1,'PARTS (sync before adding)    ')
C     CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
C     CALL XTIME(35,2,'PARTS (sync before adding)    ')
      CALL XTIME(36,1,'PARTS adding partial T1/T2    ')
      CALL PARTS (1,RMS,S1,S2,BUF1,BUF2)
      CALL XTIME(36,2,'PARTS adding partial T1/T2    ')
C
#ifdef MOD_FOCKSPCC
      IF (FOCKSP) THEN
C.......when debugging print out energies in every iteration
        IF (DEBUG) CALL HEFF1(ICURFSS,1,S1,S2,FVO,H,BUF1,BUF2,BUF3)
        CALL FOLDED (ICURFSS,T1,T2,BUF1,BUF2,BUF3,S1,S2)     
        if (carith.and.debug) then
        write (iw,111)"s1 after folded",MYPROC,zdotc(ndimt1,s1,1,s1,1)
        write (iw,111)"s2 after folded",MYPROC,zdotc(ndimt2,s2,1,s2,1)
        elseif (debug) then
        write (iw,111)"s1 after folded",MYPROC,ddot(ndimt1,s1,1,s1,1)
        write (iw,111)"s2 after folded",MYPROC,ddot(ndimt2,s2,1,s2,1)
        endif
      ENDIF
#endif
C
      if (carith.and.debug) then
      write (*,111)"s2 after parts",MYPROC,zdotc(ndimt2,s2,1,s2,1)
      write (*,111)"s1 after parts",MYPROC,zdotc(ndimt1,s1,1,s1,1)
      elseif (debug) then
      write (*,111)"s2 after parts",MYPROC,ddot(ndimt2,s2,1,s2,1)
      write (*,111)"s1 after parts",MYPROC,ddot(ndimt1,s1,1,s1,1)
      endif
C
C  DIVIDE NEW T1s AND T2s BY EIGENVALUES
C
      IF (FOCKSP) THEN
#ifdef MOD_FOCKSPCC
        CALL DENOMF (EPS,S1,S2,S1,S2,1)
#else
        CALL QUIT('FOCKSP not in this distibution')
#endif
      ELSE
        CALL DENOM (EPS,S1,S2,S1,S2)
      ENDIF
C
      CALL ZCORE (S1,S2)
C
C  EXTRAPOLATE THE AMPLITUDES
C
      IF (.NOT.FOCKSP) THEN
         CALL XTIME(15,1,'DIISX                         ')
         CALL DIISX(ITAMPL,ITAMPD,NDIMT1,NDIMT2,T1,S1,T2,S2,IT,
     &              CC1,BB1,ITC,BB2,RMS,NST1,NST2)
         CALL XTIME(15,2,'DIISX                         ')
#ifdef MOD_FOCKSPCC
      ELSE
C
C     In the Fockspace algorithm we only need to extrapolate the
C     amplitudes of the current sector. We delete the ones from
C     the higher sectors, the ones from the previous sectors are
C     constant and can be left inside.
C
C        Delete the wave function amps that belong to higher sectors
         CALL DELFCK ('VO',DELS1,S1)
         CALL DELINT ('VVOO',DELS2,S2,0,0,0)
C        Save the remaining S1 and S2 arrays (wf and matrix elements)
         CALL XCOPY(NDIMT1,S1,1,BUF1,1)
         CALL XCOPY(NDIMT2,S2,1,BUF2,1)
C        Delete the Hamiltonian matrix elements
C        They should not be taken into account in the extrapolation because
C        of the difference in magnitude with the wf (energy vs. dimensionless)
         CALL DENOMF (EPS,S1,S2,S1,S2,2)
         if (carith.and.debug) then
         write(iw,111)"s1 without Ham.",MYPROC,zdotc(ndimt1,s1,1,s1,1)
         write(iw,111)"s1 with Ham.",MYPROC,zdotc(ndimt1,buf1,1,buf1,1)
         write(iw,111)"s2 without Ham.",MYPROC,zdotc(ndimt2,s2,1,s2,1)
         write(iw,111)"s2 with Ham.",MYPROC,zdotc(ndimt2,buf2,1,buf2,1)
         elseif (debug) then
         write(iw,111)"s1 without Ham.",MYPROC,ddot(ndimt1,s1,1,s1,1)
         write(iw,111)"s1 with Ham.",MYPROC,ddot(ndimt1,buf1,1,buf1,1)
         write(iw,111)"s2 without Ham.",MYPROC,ddot(ndimt2,s2,1,s2,1)
         write(iw,111)"s2 with Ham.",MYPROC,ddot(ndimt2,buf2,1,buf2,1)
         endif
         CALL DIISX(ITAMPL,ITAMPD,NDIMT1,NDIMT2,T1,S1,T2,S2,IT,
     &              CC1,BB1,ITC,BB2,RMS,NST1,NST2)
C
C        Put the Hamiltonian elements back.
C
         CALL XAXPY(NDIMT1,A1,BUF1,1,S1,1)
         CALL XAXPY(NDIMT2,A1,BUF2,1,S2,1)
         CALL DENOMF (EPS,BUF1,BUF2,BUF1,BUF2,2)
         CALL XAXPY(NDIMT1,-A1,BUF1,1,S1,1)
         CALL XAXPY(NDIMT2,-A1,BUF2,1,S2,1)
#endif
      ENDIF
C
C  SYNCHRONIZE T1 AND T2 (PARALLEL RUNS)
C
C     CALL XTIME(37,1,'PARTS (sync before sync)      ')
C     CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
C     CALL XTIME(37,2,'PARTS (sync before sync)      ')
      CALL XTIME(38,1,'PARTS synchronizing           ')
      CALL PARTS (2,RMS,S1,S2,BUF1,BUF2)
      CALL XTIME(38,2,'PARTS synchronizing           ')
C
C  EVALUATE CORRELATION ENERGY AND WRITE OUT RESULTS
C
      IF (TEQ) THEN
         CALL CCENG  (S1,S2,FVO,BUF1,BUF2,ECCSD)
      ENDIF
C
      CALL T1DIAG (DIAG,S1,BUF1)
      IF (IPRNT.GE.1.and.myproc.eq.master) 
     &   WRITE(IW,6001) NIT,ECCSD,RMS,DIAG
      CALL FLSHFO(IW)
C
C  PUT S1,S2 INTO T1,T2 AND DO NEXT ITERATION
C
      CALL XCOPY(NDIMT1,S1,1,T1,1)
      CALL XCOPY(NDIMT2,S2,1,T2,1)
C
C     Remove the Hamiltonian elements, T1 and T2 should only contain
C     the wave function (amplitudes)
C
#ifdef MOD_FOCKSPCC
      IF (FOCKSP) CALL DENOMF (EPS,T1,T2,T1,T2,2)
#endif
C
      CPUE = CPUE + CPUDELTA()
C
      IF(RMS.LT.CONV) GO TO 2000
      GO TO 1000
C
 2000 CONTINUE
      CALL T1DIAG (DIAG,S1,BUF1)
      CPUE = CPUE + CPUDELTA()
      if (myproc.eq.master) WRITE(IW,6071)
C
      IF (TEQ.and.myproc.eq.master) THEN
         ETOT = ESCF + ECCSD
         WRITE(IW,6072) ESCF
         WRITE(IW,6074) ECCSD
         WRITE(IW,6076) ETOT
      ENDIF
C
      if (myproc.eq.master) then
      WRITE(IW,6077) DIAG
      WRITE(IW,6078) RMS
      WRITE(IW,6079) NIT
      endif
C
#ifdef MOD_FOCKSPCC
      IF (FOCKSP) THEN
         CALL HEFF1(ICURFSS,1,S1,S2,FVO,H,BUF1,BUF2,BUF3)
C        Now restore the parts that were not calculated in this sector
         CALL GETAMPT (BUF1,BUF2)
         CALL XAXPY(NDIMT1,A1,BUF1,1,S1,1)
         CALL XAXPY(NDIMT2,A1,BUF2,1,S2,1)
         CALL DELFCK ('VO',DELS1,BUF1)
         CALL DELINT ('VVOO',DELS2,BUF2,0,0,0)
         CALL DENOMF (EPS,BUF1,BUF2,BUF1,BUF2,2)
         CALL XAXPY(NDIMT1,-A1,BUF1,1,S1,1)
         CALL XAXPY(NDIMT2,-A1,BUF2,1,S2,1)
      ENDIF
#endif
C
      CALL CPUUSED(CPUTOT)
      CPUTOT = CPUTOT-CPU0
      CPUVAR = CPUTOT-CPUT1-CPUT2-CPUINT-CPU4V-CPUE
      IF (TIMING.and.myproc.eq.master)
     &WRITE(IW,7000) CPU0,CPUINT,CPUT1,CPUT2,CPU4V,CPUE,CPUVAR,CPUTOT
      IF (TIMING) CALL BFLOPS (CPU4V,NIT)
C
C     Close and delete DIIS scratch files
C
      JCODE = 4
      CALL RCLOSE(ITAMPL,JCODE)
      JCODE = 4
      CALL RCLOSE(ITAMPD,JCODE)
C
 111  FORMAT(A20,I4,2f25.15)
 6001 FORMAT(2X,I3,5X,2F20.15,F10.5)
 6002 FORMAT(3X,'MP2   =',F20.15/3X,'EMP2  =',F20.15/)
 6003 FORMAT(//3X,'NIT',10X,'ENERGY',20X,'RMS',7X,'T1-DIAGN')
 6004 FORMAT(//' Requested convergence not reached in CCSD : RMS = ',
     &F20.15)
 6071 FORMAT(//,'  CCSD results',/)
 6072 FORMAT(' SCF energy :',T40,F25.15)
 6073 FORMAT(' MP2 correlation energy :',T40,F25.15)
 6074 FORMAT(' CCSD correlation energy :',T40,F25.15)
 6075 FORMAT(' Total MP2 energy :',T40,F25.15)
 6076 FORMAT(' Total CCSD energy :',T40,F25.15)
 6077 FORMAT(' T1 diagnostic :',T40,F25.15)
 6078 FORMAT(' Convergence :',T40,F25.15)
 6079 FORMAT(' Number or iterations used :',T60,I5)
 7000 FORMAT(//' Timing of routine CCEQN :'
     &/' Before CCEQN :',T30,F12.3,' seconds'
     &/' Intermediates :',T30,F12.3,' seconds'
     &/' T1 equations :',T30,F12.3,' seconds'
     &/' T2 equations :',T30,F12.3,' seconds'
     &/' 4 virtual contr. :',T30,F12.3,' seconds'
     &/' Energy, DIIS, T1-Diag, etc.:',T30,F12.3,' seconds'
     &/' Untimed parts :',T30,F12.3,' seconds'
     &/' Total time in CCEQN :',T30,F12.3,' seconds')
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE T1EQNS (FVO,T1,T2,HO,HV,HOV,H,CZ,GOO,GVV,
     &                   BUF1,BUF2,NBUF2,BUF3,NBUF3,S1)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Evaluate T1 or Lambda1 equations
C     Here we have to take care of the fact that certain integral classes
C     are now split over individual nodes. The corresponding contributions
C     which are linear in the integrals are added at the end of the T1
C     routines. Therefore we have to take care of calculating the 
C     non-distributed contributions to the T1 amplitudes only on the
C     master node in order to avoid double-counting in the PARTS routine !
C     all direct VOVV/VVVV contributions and VOVV/VVVV contributions stemming 
C     from intermediates have to be calculated on EACH node.
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      INTEGER NBUF2,NBUF3
      REAL*8 FVO(*)
      REAL*8 T1(*),T2(*)
      REAL*8 S1(*)
      REAL*8 HO(*),HV(*),HOV(*),H(*)
      REAL*8 GOO(*),GVV(*),CZ(*)
      REAL*8 BUF1(*),BUF2(*),BUF3(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "symm.inc"
      INCLUDE "eqns.inc"
      INCLUDE "complex.inc"
      INCLUDE "ccpar.inc"
C
C---------------Local variables--------------------------------------
C
      LOGICAL DONE,TEQ,USEDZ,RIGHT
      REAL*8 SDOT,DDOT
      COMPLEX*16 CDOTU,ZDOTU
C
C---------------Executable code--------------------------------------
C
      IF (EQNS.EQ.'AMPLTD') THEN
         TEQ = .TRUE.
      ELSEIF (EQNS.EQ.'LAMBDA') THEN
         TEQ = .FALSE.
      ELSEIF (EQNS.EQ.'FOCKSP') THEN
         TEQ = .TRUE.
      ELSE
         CALL QUIT('Wrong keyword in call to L1EQNS')
      ENDIF

C------------------------------------------------------------------
C If not master initialize S1 buffer with zero otherwise with F(A,I)
C------------------------------------------------------------------
      IF (MYPROC.NE.MASTER) THEN
C-------------------------------
C S(A,I) = 0.0
C-------------------------------
        CALL XCOPY (NDIMT1,A0,0,S1,1)
      ELSE
C-------------------------------
C S(A,I) = F(A,I)
C-------------------------------
        CALL XCOPY (NDIMT1,FVO,1,S1,1)
C
C       Delete zeroth order Hamiltonian parts
C       for Fockspace
C
        IF (EQNS.EQ.'FOCKSP') CALL DENOMF (BUF1,S1,BUF2,S1,BUF2,2)
      ENDIF


C--------------------------------
C Terms specific for T1 equations
C--------------------------------
      IF (TEQ) THEN
C ********************
C ** TASK SPLITTING **
C ********************
        IF (MYPROC.EQ.MASTER) THEN
C------------------------------------------------------------
C S(A,I) = S(A,I) + T(A,K) * [HOV(K,C) - 2 * F(K,C)] * T(C,I)
C------------------------------------------------------------
         CALL XTIME(4,1,' ** T1EQNS T*[HOV - F]*T      ')
         CALL XCOPY (NFVO,HOV,1,BUF1,1)
         IF (CARITH) CALL CONJUGA (NFVO,BUF1,1)
         CALL XAXPY (NFVO,-A2,FVO,1,BUF1,1)
         CALL DELFCK ('VO','DD',BUF1)
         OFF1 = 1
         DO IRP = 1, NREP
            M = NV(IRP)
            N = NO(IRP)
            K1 = NO(IRP)
            K2 = NV(IRP)
            CALL XGEMM ('C','N',K1,N,K2,A1,BUF1(OFF1),K2,T1(OFF1),K2,
     &                   A0,BUF2,K1)
            CALL XGEMM ('N','N',M,N,K1,A1,T1(OFF1),M,BUF2,K1,
     &                   A1,S1(OFF1),M)
            OFF1 = OFF1 + M * N * RCW
         ENDDO
         CALL XTIME(4,2,' ** T1EQNS T*[HOV - F]*T      ')

      ENDIF
C ****************************************************
C ** END OF TASK SPLITTING. Parallel execution      **
C ****************************************************

C-----------------------------------------------------------------
C S(A,I) = S(A,I) + HOV(CK) * T2(A,C,I,K)
C-----------------------------------------------------------------
         CALL XTIME(5,1,' ** T1EQNS   HOV*T2(A,C,I,K   ')
         CALL SRT1TT4 (NREP,MULTB,LTR,LFA,NV,NV,NO,NO,
     &                 MVO,JVOVO,JJVO,JJVO,T2,BUF1)

         K    = IDIST(1,3,1)
         OFF1 = IDIST(2,3,1)
         OFF2 = IDIST(3,3,1)
         M    = MVO(1)
         CALL XGEMV ('N',M,K,A1,BUF1(OFF1),M,HOV(OFF2),1,A1,S1,1)
         CALL XTIME(5,2,' ** T1EQNS   HOV*T2(A,C,I,K   ')

C .... old serial code ....
C        M    = MVO(1)
C        K    = MVO(1)
C        CALL XGEMV ('N',M,K,A1,BUF1,M,HOV,1,A1,S1,1)


      ENDIF

C ********************
C ** TASK SPLITTING **
C ********************

      IF (MYPROC.EQ.MASTER) THEN

      CALL XTIME(6,1,' ** T1EQNS   HV*T / T*HO      ')
C-----------------------------------------------
C S(A,I) = S(A,I) + HV(A,C) * T(C,I)
C-----------------------------------------------
      CALL CNTRCT ('N','N',NV,NO,NV,A1,HV,T1,A1,S1,NREP)
C-----------------------------------------------
C S(A,I) = S(A,I) - T(A,K) * HO(K,I)
C-----------------------------------------------
      CALL CNTRCT ('N','N',NV,NO,NO,-A1,T1,HO,A1,S1,NREP)
      CALL XTIME(6,2,' ** T1EQNS   HV*T / T*HO      ')

C--------------------------------------------
C S(A,I) = S(A,I) + V(K,L,C,I) * TAU(A,C,K,L)
C--------------------------------------------
      CALL XTIME(7,1,' ** T1EQNS VOOO*TAU (+GET!)   ')
      IF (TEQ) THEN
         CALL GETTAU (T1,T2,BUF3)
      ELSE
         CALL XCOPY (NDIMT2,T2,1,BUF3,1)
      ENDIF
      CALL T1EQN1 (BUF1,BUF2,BUF3,NBUF3,S1)
      CALL XTIME(7,2,' ** T1EQNS VOOO*TAU (+GET!)   ')

C ****************************************************
C ** END OF TASK SPLITTING. THE COMMON STUFF STARTS **
C ****************************************************

      ENDIF

C--------------------------------------------
C S(A,I) = S(A,I) + V(A,K,C,D) * TAU(C,D,I,K)   ! VOVV contributions !
C--------------------------------------------
      IF (TEQ) THEN
         CALL GETTAU (T1,T2,BUF1)
      ELSE
C -------------------------------------------
C TAU'(CD,IK) = L(CD,IK) +  G(C,B) * T(BD,IK)   <<<<<<------- LAMBDA EQNS
C -------------------------------------------
         CALL GETAMPT (BUF1,BUF2)
         CALL SRT1T2 (NREP,MULTB,LFA,NVVT,NV,NV,NOOT,LVVOOT,LLVOOT,
     &                BUF2,BUF1)
         CALL CNTRCT ('N','N',NV,NVOOT,NV,A1,GVV,BUF1,A0,BUF2,NREP)
         CALL SRT1T2 (NREP,MULTB,LTR,NVVT,NV,NV,NOOT,LVVOOT,LLVOOT,
     &                BUF2,BUF1)
         CALL XAXPY (NDIMT2,A1,T2,1,BUF1,1)
      ENDIF
      CALL XTIME(8,1,' ** T1EQNS (VOVV contr.)     ')
      CALL T1EQN2 (BUF1,BUF2,BUF3,NBUF3,S1)   ! VOVV contractions, term 6 !
      CALL XTIME(8,2,' ** T1EQNS (VOVV contr.)     ')

C--------------------------------------
C S(A,I) = S(A,I) - W(A,K,C,I) * T(C,K)    ! VOVO contributions
C--------------------------------------
      CALL XTIME(9,1,' ** T1EQNS VOVO * T(C,K)     ')
      IF (TEQ) THEN
         CALL GETVOVO (BUF1)
         CALL DELINT ('VOVO','KDDK',BUF1,0,0,0)
         CALL SRT16 (NREP,MULTB,LTR,LFA,NV,NO,NV,NO,MVO,JVOVO,JJVO,JJVO,
     &              BUF1,BUF2)
      ELSE
         CALL XCOPY (JVOVO(2),H,1,BUF2,1)
      ENDIF

      M    = MVO(1)
      K    = IDIST(1,3,1)
      OFF1 = IDIST(2,3,1)
      OFF2 = IDIST(3,3,1)
      CALL XGEMV ('N',M,K,-A1,BUF2(OFF1),M,T1(OFF2),1,A1,S1,1)
      CALL XTIME(9,2,' ** T1EQNS VOVO * T(C,K)     ')
C------------------------------------
C Terms specific for Lambda1 equations
C------------------------------------
      IF (.NOT.TEQ) THEN
C----------------------------------------------------
C S(A,I) = S(A,I) - T(A,L) * V(K,L,C,D) * L(C,D,K,I)
C        = S(A,I) - T(A,L) * V*(C,D,K,L) * L(C,D,K,I)
C----------------------------------------------------
      CALL GETVVOO(BUF2)
      CALL SRT1T3 (NREP,MULTB,LFA,NVVT,NO,NO,NVVOT,KVVOOT,KKVVOT,
     &             BUF2,BUF1)
      CALL SRT1T3 (NREP,MULTB,LFA,NVVT,NO,NO,NVVOT,KVVOOT,KKVVOT,
     &             T2,BUF2)
      CALL CNTRCT ('C','N',NO,NO,NVVOT,A1,BUF1,BUF2,A0,BUF3,NREP)
      CALL GETAMPT (BUF1,BUF2)
      CALL CNTRCT ('N','N',NV,NO,NO,-A1,BUF1,BUF3,A1,S1,NREP)
C----------------------------------------------------
C S(A,I) = S(A,I) - L(A,C,K,L) * V(K,L,D,C) * T(D,I)
C        = S(A,I) - L(A,C,K,L) * V*(D,C,K,L) * T(D,I)
C----------------------------------------------------
      CALL GETVVOO(BUF2)
      CALL SRT1T2 (NREP,MULTB,LFA,NVVT,NV,NV,NOOT,LVVOOT,LLVOOT,
     &             BUF2,BUF1)
      CALL SRT1T2 (NREP,MULTB,LFA,NVVT,NV,NV,NOOT,LVVOOT,LLVOOT,
     &             T2,BUF2)
      CALL CNTRCT ('N','C',NV,NV,NVOOT,A1,BUF2,BUF1,A0,BUF3,NREP)
      CALL GETAMPT (BUF1,BUF2)
      OFF1 = 1
      OFF2 = 1
      OFF3 = 1
      DO IRP =1 , NREP
         M = NV(IRP)
         N = NO(IRP)
         K = NV(IRP)
         CALL XGEMM ('N','N',M,N,K,-A1,BUF3(OFF1),M,BUF1(OFF2),K,
     &               A1,S1(OFF3),M)
         OFF1 = OFF1 + M * K * RCW
         OFF2 = OFF2 + K * N * RCW
         OFF3 = OFF3 + M * N * RCW
      ENDDO
C--------------------------------------
C S(A,I) = S(A,I) - W(C,A,D,I) * G(C,D)
C--------------------------------------
      M = NFVV
      N = NFVO
      IF (NBUF3.LT.JVVVO(2)) THEN
         CALL QUIT(" OUT OF CORE STEP IN GV NOT IMPLEMENTED")
      ENDIF
      USEDZ = .TRUE.
      RIGHT = .FALSE.
clv : changed to SRT20D, untested as the rest of the lambda code
clv : addition should be supplied with a minus sign
      CALL SRT20D (NREP,MULTB,NVO,NV,NO,NV,NV,MVV,JVVVO,JJVV,
     &             JJVO,BUF2,NBUF2,S1,GVV,USEDZ,RIGHT)
C--------------------------------------
C S(A,I) = S(A,I) + W(A,K,L,I) * G(K,L)
C--------------------------------------
      CALL SRT1ST4 (NREP,MULTB,LTR,NV,NO,NO,NO,MVO,JVOOO,JJVO,JJOO,
     &              CZ,BUF2)
      M = NFVO
      K = NFOO
      CALL XGEMV ('N',M,K,A1,BUF2,M,GOO,1,A1,S1,1)
C---------------------------------------------------
C S(A,I) = S(A,I) + G(C,B) * W(B,K,C,D) * T(A,D,K,I)
C---------------------------------------------------
      USEDZ = .FALSE.
      RIGHT = .FALSE.
      CALL SRT20D (NREP,MULTB,NVO,NV,NO,NV,NV,MVV,JVVVO,JJVV,
     &            JJVO,BUF2,NBUF2,BUF1,GVV,USEDZ,RIGHT)
      CALL GETAMPT (BUF3,BUF2)
      CALL SRT1TT4 (NREP,MULTB,LTR,LFA,NV,NV,NO,NO,
     &              MVO,JVOVO,JJVO,JJVO,BUF2,BUF3)
      M = MVO(1)
      K = MVO(1)
      CALL XGEMV ('N',M,K,A1,BUF3,M,BUF1,1,A1,S1,1)
      ENDIF
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE T2EQNS (T1,T2,GO,GV,HOV,H,AZ,CZ,GOO,GVV,
     &                   BUF1,BUF2,BUF3,NBUF3,S2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Evaluate T2 equations
C
C---------------Routines called----------------------------------------
C
C     BLAS routines :                 XGEMM, XCOPY
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      INTEGER NBUF3
      REAL*8 T1(*),T2(*)
      REAL*8 S2(*)
      REAL*8 GO(*),GV(*),HOV(*)
      REAL*8 GOO(*),GVV(*)
      REAL*8 AZ(*),H(*),CZ(*)
      REAL*8 BUF1(*),BUF2(*),BUF3(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "complex.inc"
      INCLUDE "eqns.inc"
      INCLUDE "symm.inc"
      INCLUDE "ccpar.inc"
C
C---------------Local variables--------------------------------------
C
      LOGICAL DONE,TEQ
      REAL*8 T1VAL(2)
      DATA T1VAL /2*0.0D0/
      complex*16 zdotc
      real*8 ddot
C
C---------------Executable code--------------------------------------
C
C
      IF (EQNS.EQ.'AMPLTD') THEN
         TEQ = .TRUE.
      ELSEIF (EQNS.EQ.'LAMBDA') THEN
         TEQ = .FALSE.
      ELSEIF (EQNS.EQ.'FOCKSP') THEN
         TEQ = .TRUE.
      ELSE
         CALL QUIT('Wrong keyword in call to L1EQNS')
      ENDIF

C---- fill arrays according to node...

      IF (MYPROC.NE.MASTER) THEN
         CALL XCOPY (NDIMT2,A0,0,S2,1)
      ELSE
C-------------------------------------------
C S(AB,IJ) = W(AB,IJ)
C-------------------------------------------
        CALL GETVVOO (S2)
C
C       Delete zeroth order Hamiltonian parts
C       for Fockspace
C
        IF (EQNS.EQ.'FOCKSP') CALL DENOMF (BUF1,BUF1,S2,BUF1,S2,2)
      ENDIF
C---------------------------------------------
C S(AB,IJ) = S(AB,IJ) + TAU(AB,KL) * AZ(KL,IJ)
C---------------------------------------------
      CALL XTIME(14,1,'TAU*AINTM contraction         ')
      IF (TEQ) THEN
         CALL GETTAU (T1,T2,BUF1)
      ELSE
         CALL XCOPY (NDIMT2,T2,1,BUF1,1)
      ENDIF

      OFF3 = 1
      DO IREP = 1, NREP
        M    = NVVT(IREP)
        N    = NOOT(IREP)
        KLD  = NOOT(IREP)
        K    = IDIST(1,5,IREP)
        OFF1 = IDIST(2,5,IREP)
        OFF2 = IDIST(3,5,IREP)
        CALL XGEMM ('N','N',M,N,K,A1,BUF1(OFF1),M,AZ(OFF2),KLD,
     &              A1,S2(OFF3),M)
        OFF3 = OFF3 + M * N * RCW
      ENDDO

C ***
C *** original CNTRCT call in serial version
C ***
C *** CALL CNTRCT ('N','N',NVVT,NOOT,NOOT,A1,BUF1,AZ,A1,S2,NREP)
C ***

      CALL XTIME(14,2,'TAU*AINTM contraction         ')

C ** we will not parallelize a summation over just one occupied index
C ** if the system is small this can lead to zero batches. Only master
C ** does the T2(AB,IK) * GO(K,J) contraction

C ********************
C ** TASK SPLITTING **
C ********************

      IF(MYPROC.EQ.MASTER) THEN
C----------------------------------------------------------------
C S(AB,IJ) = S(AB,IJ) - PIJ [ T2(AB,IK) * GO(K,J) ]
C----------------------------------------------------------------
      CALL SRT1T3 (NREP,MULTB,LFA,NVVT,NO,NO,NVVOT,KVVOOT,KKVVOT,
     &             T2,BUF1)
      CALL CNTRCT ('N','N',NVVOT,NO,NO,A1,BUF1,GO,A0,BUF2,NREP)
      CALL SRT1T3 (NREP,MULTB,LTR,NVVT,NO,NO,NVVOT,KVVOOT,KKVVOT,
     &             BUF2,BUF1)
      CALL XAXPY (NDIMT2,-A1,BUF1,1,S2,1)

C     OFF3 = 1
C     DO IREP = 1, NREP
C        M    = NVVOT(IREP)
C        N    = NO   (IREP)
C        KLD  = NO   (IREP)
C        K    = IDIST(1,7,IREP)
C        OFF1 = IDIST(2,7,IREP)
C        OFF2 = IDIST(3,7,IREP)
C        CALL XGEMM ('N','N',M,N,K,A1,BUF1(OFF1),M,GO(OFF2),KLD,
C    &                A0,BUF2(OFF3),M)
C        OFF3 = OFF3 + M * N * RCW
C     ENDDO

C ***************************
C ** END OF TASK SPLITTING **
C ***************************

      ENDIF


C-----------------------------------------------------------------------
C S(AB,IJ) = S(AB,IJ) + PIJ [ W(AB,CJ) * T1(C,I) ]
C-----------------------------------------------------------------------
      CALL XTIME(26,1,' ** T2EQNS: VOVV*T1           ')
      DO 30 IRP = 1, NREP
         IF (NVO(IRP).EQ.0) GOTO 30
         DONE = .FALSE.
         ISTART = 0
         MINT = NBUF3/NVO(IRP)  ! max. # of NVO batches that fit in mem.
  10     IF (TEQ) THEN
            CALL GETVOVV (IRP,ISTART,NINT,DONE,BUF3,MINT)
            CALL DELINT ('VOVV','DKKK',BUF3,IRP,ISTART,NINT)
         ELSE
            CALL GETDZ   (IRP,ISTART,NINT,DONE,BUF3)
         ENDIF
         IF (CARITH) CALL CONJUGA (NVO(IRP)*NINT,BUF3,1)
         IJ = 0
         DO 20 JREP = 1, NREP
           CREP = MULTB(JREP,IRP+NREP,2)
           IREP = CREP
           IF (IREP.LT.JREP) GOTO 20
           DO J = 1, NO(JREP)
              IMIN = 1
              IF (JREP.EQ.IREP) IMIN = J + 1
              DO I = IMIN, NO(IREP)
                 IJ = IJ + 1
                 ABIJ = (IVVOOTT(IRP)+(IJ-1)*NVVT(IRP)+ISTART)*RCW+1
                 DO C = 1, NV(CREP)
                    CJ = (IIVO(CREP,JREP)+(J-1)*NV(CREP)+C-1) * RCW + 1
                    T1CI = (IVO(CREP)+(I-1)*NV(CREP)+C-1) * RCW + 1
                    T1VAL(1) = T1(T1CI)
                    IF (CARITH) T1VAL(2) = T1(T1CI+1)
                    CALL XAXPY(NINT,T1VAL,BUF3(CJ),NVO(IRP),S2(ABIJ),1)
                 ENDDO
              ENDDO
           ENDDO
 20      CONTINUE
         JI = 0
         DO 21 IREP = 1, NREP
           JREP = MULTB(IREP,IRP+NREP,2)
           CREP = IREP
           IF (JREP.LT.IREP) GOTO 21
           DO I = 1, NO(IREP)
              JMIN = 1
              IF (JREP.EQ.IREP) JMIN = I + 1
              DO J = JMIN, NO(JREP)
                 JI = JI + 1
                 ABJI = (IVVOOTT(IRP)+(JI-1)*NVVT(IRP)+ISTART)*RCW+1
                 DO C = 1, NV(CREP)
                    CJ = (IIVO(CREP,JREP)+(J-1)*NV(CREP)+C-1) * RCW + 1
                    T1CI = (IVO(CREP)+(I-1)*NV(CREP)+C-1) * RCW + 1
                    T1VAL(1) = - T1(T1CI)
                    IF (CARITH) T1VAL(2) = - T1(T1CI+1)
                    CALL XAXPY(NINT,T1VAL,BUF3(CJ),NVO(IRP),S2(ABJI),1)
                 ENDDO
              ENDDO
           ENDDO
 21      CONTINUE
         IF (.NOT.DONE) THEN
            ISTART = ISTART + NINT
            GOTO 10
         ENDIF
 30    CONTINUE
      CALL XTIME(26,2,' ** T2EQNS: VOVV*T1           ')

C ********************
C ** TASK SPLITTING **
C ********************
C ** here a VOOV dependent contribution (Term 7 in the paper) is to be
C ** calculated only by the master

      IF(MYPROC.EQ.MASTER) THEN

C--------------------------------
C Terms specific for T2 equations
C--------------------------------
      IF (TEQ) THEN
C-----------------------------------------------------------------------
C S(AB,IJ) = S(AB,IJ) + PAB.PIJ [T1(A,K)*W(BK,IC)*T1(C,J)]
C-----------------------------------------------------------------------
         CALL GETVOVO (BUF1)
         CALL DELINT ('VOVO','KDDK',BUF1,0,0,0)
         CALL SRT19 (NREP,MULTB,NVO,NV,NO,NVOO,KVOOV,KKVOO,
     &               BUF1,BUF2)
         CALL CNTRCT ('N','N',NVOO,NO,NV,A1,BUF2,T1,A0,BUF1,NREP)
         CALL SRT1T3 (NREP,MULTB,LTR,NVO,NO,NO,NVOO,KVOOO,KKVOO,
     &                BUF1,BUF3)
C-------------------------------------------------------------------
C        Integrals are now in VO,OOT format : combine with next term
C-------------------------------------------------------------------
      ENDIF

      ENDIF

C ****************************
C ** END OF TASK SPLITTING. **
C ****************************
C ** this contribution coming from the intermediate G(A,C) has to be
C ** calculated on each node. The fact that G(A,C) contains LOCAL +
C ** DISTRIBUTED parts as well is accounted for by the original calculation
C ** of the G(A,C) array before entering T2EQNS

C-------------------------------------------------
C S(AB,IJ) = S(AB,IJ) + PAB [ GV(A,C) * T2(CB,IJ)]
C-------------------------------------------------
      CALL SRT1T2 (NREP,MULTB,LFA,NVVT,NV,NV,NOOT,LVVOOT,LLVOOT,T2,BUF2)
      CALL CNTRCT ('N','N',NV,NVOOT,NV,A1,GV,BUF2,A0,BUF1,NREP)

C ** the contraction of G(A,C) with the T2 amplitudes now is in BUF1 !
C ** these are the partial contributions available on EACH node


C ***
C ****** task splitting
C ***
      IF(MYPROC.EQ.MASTER) THEN

C------------------------------------------------
C S(AB,IJ) = S(AB,IJ) - PAB [ T1(A,K) *W(KB,IJ) ]
C------------------------------------------------
      IF (TEQ) THEN
         CALL GETVOOO (BUF2)
         CALL DELINT ('VOOO','KDKK',BUF2,0,0,0)
         CALL XAXPY (NV2,-A1,BUF3,1,BUF2,1)
      ELSE
         CALL XCOPY (NV2,CZ,1,BUF2,1)
      ENDIF
      CALL SRT1L1 (NREP,MULTB,LFA,NVO,NV,NO,NOOT,IVOOOT,IIOV,BUF2,BUF3)
      CALL SRT1S2 (NREP,MULTB,LFA,NVO,NO,NV,NOOT,LOVOOT,LLVOOT,
     &             BUF3,BUF2)

C     OFF3 = 1
C     DO IREP = 1, NREP
C       M    = NV(IREP)
C       N    = NVOOT(IREP)
C       KLD  = NO(IREP)
C       K    = IDIST(1,8,IREP)
C       OFF1 = IDIST(2,8,IREP)
C       OFF2 = IDIST(3,8,IREP)
C       CALL XGEMM ('N','N',M,N,K,-A1,T1(OFF1),M,BUF2(OFF2),KLD,
C    &              A1,BUF1(OFF3),M)
C       OFF3 = OFF3 + M * N * RCW
C     ENDDO
      CALL CNTRCT ('N','N',NV,NVOOT,NO,-A1,T1,BUF2,A1,BUF1,NREP)


C ***
C *** end of task splitting
C ***
      ENDIF


      CALL SRT1T2 (NREP,MULTB,LTR,NVVT,NV,NV,NOOT,LVVOOT,LLVOOT,
     &             BUF1,BUF2)
      CALL XAXPY (NDIMT2,A1,BUF2,1,S2,1)


C ** now the final H*T2 contraction. H is complete since we communicated
C ** it already.

C-----------------------------------------------------------------------
C S(AB,IJ) = S(AB,IJ) + PAB.PIJ [H(AI,CK)*T2(CK,BJ)]
C-----------------------------------------------------------------------

      CALL XTIME(27,1,' ** T2EQNS: HINTM*T2          ')
      CALL SRT1TT4 (NREP,MULTB,LFA,LFA,NV,NV,NO,NO,MVO,J2VOVO,JJVO,JJVO,
     &              T2,BUF1)
      CALL XCOPY (J2VOVO(NREP+1),A0,0,BUF2,1)

      OFF3 = 1
      DO IREP = 1, NREP
         JREP = MULTB(IREP+NREP,1+NREP,2)
         M    = MVO(IREP)
         N    = MVO(JREP)
         KLD  = MVO(IREP)
         K    = IDIST(1,10,JREP)
         OFF1 = IDIST(2,10,JREP)
         OFF2 = IDIST(3,10,JREP)
         CALL XGEMM ('N','N',M,N,K,A1,H(OFF1),M,BUF1(OFF2),KLD,
     &                A0,BUF2(OFF3),M)
         OFF3 = OFF3 + M * N * RCW
      ENDDO

      CALL SRT1TT4 (NREP,MULTB,LFA,LTR,NV,NV,NO,NO,MVO,J2VOVO,JJVO,JJVO,
     &              BUF2,BUF1)
      CALL XAXPY (NDIMT2,A1,BUF1,1,S2,1)
      CALL XTIME(27,2,' ** T2EQNS: HINTM*T2          ')



C==========================================================
C============= L A M B D A   E Q U A T I O N S ============
C==========================================================


      IF (.NOT.TEQ) THEN
C-------------------------------------------------------
C S(AB,IJ) = S(AB,IJ) + TAU(AB,KL) * V(KL,CD) * L(CD,IJ)
C-------------------------------------------------------
      CALL GETAMPT (BUF1,BUF2)
      CALL GETTAU (BUF1,BUF2,BUF3)
      CALL GETVVOO (BUF1)
      CALL CNTRCT ('C','N',NOOT,NOOT,NVVT,A1,BUF1,T2,A0,BUF2,NREP)
      CALL CNTRCT ('N','N',NOOT,NOOT,NVVT,A1,BUF3,BUF2,A1,S2,NREP)
C-----------------------------------------------
C S(AB,IJ) = S(AB,IJ) + PAB.PIJ. [T(A,I)*H(B,J)]
C-----------------------------------------------
      CALL LAMBD21 (T1,HOV,S2)
C---------------------------------------------
C S(AB,IJ) = S(AB,IJ) + PAB. [G(A,C)*W(CB,IJ)]
C---------------------------------------------
      CALL GETVVOO (BUF1)
      CALL SRT1T2 (NREP,MULTB,LFA,NVVT,NV,NV,NOOT,LVVOOT,LLVOOT,
     &             BUF1,BUF2)
      CALL CNTRCT ('N','N',NV,NVOOT,NV,A1,GVV,BUF2,A0,BUF1,NREP)
      CALL SRT1T2 (NREP,MULTB,LTR,NVVT,NV,NV,NOOT,LVVOOT,LLVOOT,
     &             BUF1,BUF2)
      CALL XAXPY (NDIMT2,A1,BUF2,1,S2,1)
C---------------------------------------------
C S(AB,IJ) = S(AB,IJ) - PIJ. [W(AB,IK)*G(K,J)]
C---------------------------------------------
      CALL GETVVOO(BUF2)
      CALL SRT1T3 (NREP,MULTB,LFA,NVVT,NO,NO,NVVOT,KVVOOT,KKVVOT,
     &             BUF2,BUF1)
      CALL CNTRCT ('N','N',NVVOT,NO,NO,A1,BUF1,GOO,A0,BUF2,NREP)
      CALL SRT1T3 (NREP,MULTB,LTR,NVVT,NO,NO,NVVOT,KVVOOT,KKVVOT,
     &             BUF2,BUF1)
      CALL XAXPY (NDIMT2,-A1,BUF1,1,S2,1)
C-----------------------------------------------------------------------
C S(AB,IJ) = S(AB,IJ) + PAB.PIJ [L(D,I)*V(AK,CD)*T2(BC,KJ)]
C-----------------------------------------------------------------------
      CALL GETAMPT (BUF1,BUF3)
      CALL DINTM2 (T1,BUF1,BUF3,NBUF3,BUF2)
      CALL SRT1TT4 (NREP,MULTB,LFA,LFA,NV,NV,NO,NO,MVO,J2VOVO,JJVO,JJVO,
     &              BUF3,BUF1)
      CALL XCOPY (J2VOVO(NREP+1),A0,0,BUF3,1)
      OFF1 = 1
      OFF2 = 1
      OFF3 = 1
      DO IRP =1 , NREP
         JRP = MULTB(IRP+NREP,1+NREP,2)
         M = MVO(IRP)
         N = MVO(JRP)
         K = MVO(IRP)
         CALL XGEMM ('N','N',M,N,K,A1,BUF2(OFF1),M,BUF1(OFF2),K,
     &               A0,BUF3(OFF3),M)
         OFF1 = OFF1 + M * K * RCW
         OFF2 = OFF2 + K * N * RCW
         OFF3 = OFF3 + M * N * RCW
      ENDDO
      CALL SRT1TT4 (NREP,MULTB,LFA,LTR,NV,NV,NO,NO,MVO,J2VOVO,JJVO,JJVO,
     &              BUF3,BUF1)
      CALL XAXPY (NDIMT2,A1,BUF1,1,S2,1)
      ENDIF
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE T3CORR (FVO,EPS,T1,T2,T2S,T2T,BUF1,BUF2,BUF3,BUF4,
     &                   VVVO,VVOO,VOOO)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Evaluate non-iterative T3 corrections DE(+T), DE((T)), DE(-T)
C     Use formulation of Deegan and Knowles, CPL 227 (1994), 321.
C     Note that their W is called in the description because we use
C     W for the anti-symmetrized integrals.
C     This version keeps everything in core.
C
C---------------Routines called----------------------------------------
C
C     BLAS routines :                 XGEMM, XCOPY
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 FVO(*),EPS(*)
      REAL*8 T1(*),T2(*),T2S(*),T2T(*)
      REAL*8 BUF1(*),BUF2(*),BUF3(*),BUF4(*)
      REAL*8 VVVO(*)
      REAL*8 VOOO(*),VVOO(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "files.inc"
      INCLUDE "symm.inc"
      INCLUDE "inpt.inc"
      INCLUDE "results.inc"
      INCLUDE "complex.inc"
      INCLUDE "ccpar.inc"
#if defined (VAR_MPI)
      INCLUDE 'mpif.h'
      DIMENSION ISTAT(MPI_STATUS_SIZE)
#endif

C
C---------------Local variables--------------------------------------
C
      REAL*8 EPSIJK,EIJKABC,VMIN(2)
      REAL*8 CPUDELTA,CPU0,CPU1,CPU2,CPU3,CPUTOT

C---------------Executable code--------------------------------------
C
C     Initialize the timing.
C
      CPU0 = CPUDELTA()
      CALL CPUUSED(CPU0)
      CPU1 = D0
      CPU2 = D0
      CPU3 = D0
C
      ET1 = D0
      ET2 = D0
      ET3 = D0

      CALL GETVOOO(VOOO)
      CALL GETVVOO(VVOO)
C
C     Bring arrays in correct order
C
      CALL SRT1LS1 (NREP,MULTB,NVVT,NVV,NV,NV,NOOT,IVVOOT,IIVV,T2,T2S)
      CALL SRT22 (NREP,MULTB,NVVT,NO,NO,NVVOT,LOVVOT,LLOVVT,T2,T2T)

C call SRT23 with appropriate arguments

#if defined (VAR_MPI)
      CALL SRT23 (NREP,MULTB,NVO,NV,NO,NT3VVT,NT3VVVT,KT3VVVOT,
     &            LLT3VVVT,BUF4,VVVO,NVVVOTMX)
      IF (CARITH) CALL CONJUGA (KT3VVVOT(NREP+1),VVVO,1)
#else
      CALL SRT23 (NREP,MULTB,NVO,NV,NO,NVVT,NVVVT,KVVVOT,LLVVVT,
     &            BUF4,VVVO,NVVVOTMX)
      IF (CARITH) CALL CONJUGA (KVVVOT(NREP+1),VVVO,1)
#endif
C
C     Put TAU3 in block BUF4 for use in evaluating -T correction
C
      CALL GETTAU3 (T1,T2,BUF4)
C
      CPU1 = CPUDELTA()
C
      DO 300 IJKREP = 1, NREP
      DO 200 KREP = 1, NREP
      IJREP = MULTB(KREP,IJKREP,2)
      DO 100 JREP = KREP, NREP
      IREP = MULTB(JREP,IJREP+NREP,2)
      IF (IREP.LT.JREP) GOTO 100
      IKREP = MULTB(IREP,KREP,1)
      JKREP = MULTB(JREP,KREP,1)
      DO K = 1, NO(KREP)
      KK = IO(KREP) + K
      JMIN = 1
      IF (KREP.EQ.JREP) JMIN = K + 1
      DO J = JMIN, NO(JREP)
      JJ = IO(JREP) + J
      IMIN = 1
      IF (JREP.EQ.IREP) IMIN = J + 1
      DO I = IMIN, NO(IREP)
      II = IO(IREP) + I
      EPSIJK = - EPS(II) - EPS(JJ) - EPS(KK)
C
      IJ = IIOOT(IREP,JREP) + (J-1) * NO(IREP) + I
      IK = IIOOT(IREP,KREP) + (K-1) * NO(IREP) + I
      JK = IIOOT(JREP,KREP) + (K-1) * NO(JREP) + J
      IF (IREP.EQ.JREP) IJ = IJ - J*(J+1)/2
      IF (IREP.EQ.KREP) IK = IK - K*(K+1)/2
      IF (JREP.EQ.KREP) JK = JK - K*(K+1)/2
      IJOFF1 = IVVOOT(IJREP) + (IJ-1) * NVV(IJREP)
      IKOFF1 = IVVOOT(IKREP) + (IK-1) * NVV(IKREP)
      JKOFF1 = IVVOOT(JKREP) + (JK-1) * NVV(JKREP)
      IJOFF2 = IVOOOT(IJREP) + (IJ-1) * NVO(IJREP)
      IKOFF2 = IVOOOT(IKREP) + (IK-1) * NVO(IKREP)
      JKOFF2 = IVOOOT(JKREP) + (JK-1) * NVO(JKREP)
      IJOFF3 = IVVOOTT(IJREP) + (IJ-1) * NVVT(IJREP)
      IKOFF3 = IVVOOTT(IKREP) + (IK-1) * NVVT(IKREP)
      JKOFF3 = IVVOOTT(JKREP) + (JK-1) * NVVT(JKREP)

      IOFF1  = KVVVOT(IREP) + (I-1) * NVVVT(IREP)
      JOFF1  = KVVVOT(JREP) + (J-1) * NVVVT(JREP)
      KOFF1  = KVVVOT(KREP) + (K-1) * NVVVT(KREP)

      IT3OFF1  = KT3VVVOT(IREP) + (I-1) * NT3VVVT(IREP)
      JT3OFF1  = KT3VVVOT(JREP) + (J-1) * NT3VVVT(JREP)
      KT3OFF1  = KT3VVVOT(KREP) + (K-1) * NT3VVVT(KREP)

      IOFF2  = LOVVOT(IREP) + (I-1) * NOVVT(IREP)
      JOFF2  = LOVVOT(JREP) + (J-1) * NOVVT(JREP)
      KOFF2  = LOVVOT(KREP) + (K-1) * NOVVT(KREP)
C
      CALL XCOPY (IVVVT(NREP+1,IJKREP),A0,0,BUF1,1)
      OFF1 = 1
      DO BCREP = 1, NREP
      AREP = MULTB(BCREP+NREP,IJKREP,2)
      M = NV(AREP)
      N = NVVT(BCREP)
      NN = NT3VVT(BCREP)
      OFF2 = M * IDIST(1,2,BCREP) * RCW
C-------------------------------------------
C U'(ABC) = T(A,E:IJ) * W(E,BC:K)
C-------------------------------------------
      EREP = MULTB(KREP,BCREP+NREP,2)
      O = NV(EREP)
      AEIJ = (IJOFF1 + IIVV(AREP,EREP)) * RCW + 1
      EBCK = (KT3OFF1 + LLT3VVVT(EREP,BCREP)) * RCW + 1
      CALL XGEMM ('N','N',M,NN,O,A1,T2S(AEIJ),M,
     &     VVVO(EBCK),O,A1,BUF1(OFF1+OFF2),M)
C-------------------------------------------
C U'(ABC) = U'(ABC) + T(A,E:JK) * W(E,BC:I)
C-------------------------------------------
      EREP = MULTB(IREP,BCREP+NREP,2)
      O = NV(EREP)
      AEJK = (JKOFF1 + IIVV(AREP,EREP)) * RCW + 1
      EBCI = (IT3OFF1 + LLT3VVVT(EREP,BCREP)) * RCW + 1
      CALL XGEMM ('N','N',M,NN,O,A1,T2S(AEJK),M,
     &     VVVO(EBCI),O,A1,BUF1(OFF1+OFF2),M)
C-------------------------------------------
C U'(ABC) = U'(ABC) - T(A,E:IK) * W(E,BC:J)
C-------------------------------------------
      EREP = MULTB(JREP,BCREP+NREP,2)
      O = NV(EREP)
      AEIK = (IKOFF1 + IIVV(AREP,EREP)) * RCW + 1
      EBCJ = (JT3OFF1 + LLT3VVVT(EREP,BCREP)) * RCW + 1
      CALL XGEMM ('N','N',M,NN,O,-A1,T2S(AEIK),M,
     &     VVVO(EBCJ),O,A1,BUF1(OFF1+OFF2),M)

C ** here the distributed VOVV integrals are used. we 
C ** communicate the partial U(ABC) and proceed on one 
C ** node only. BUF1, BUF2 and BUF3 have the same size so we
C ** can uns them as send/receive buffers here

#if defined (VAR_MPI)
      IF(NMPROC.GT.1) THEN
        CALL MPI_REDUCE(BUF1(OFF1),BUF2(1),RCW*M*N,
     &    MPI_DOUBLE_PRECISION,MPI_SUM,MASTER,
     &    MPI_COMM_WORLD,IERR)
        CALL XCOPY(M*N,BUF2(1),1,BUF1(OFF1),1)
      ENDIF
#endif

C *******************
C *** TASK SPLITTING
C *******************

      IF(MYPROC.EQ.MASTER) THEN

C-------------------------------------------
C U'(ABC) = U'(ABC) + W(A,L:IJ) * T(L,BC:K)
C-------------------------------------------
      LREP = MULTB(KREP,BCREP+NREP,2)
      O = NO(LREP)
      ALIJ = (IJOFF2 + IIVO(AREP,LREP)) * RCW + 1
      LBCK = (KOFF2 + LLOVVT(LREP,BCREP)) * RCW + 1
      CALL XGEMM ('N','N',M,N,O,A1,VOOO(ALIJ),M,
     &            T2T(LBCK),O,A1,BUF1(OFF1),M)
C-------------------------------------------
C U'(ABC) = U'(ABC) + W(A,L:JK) * T(L,BC:I)
C-------------------------------------------
      LREP = MULTB(IREP,BCREP+NREP,2)
      O = NO(LREP)
      ALJK = (JKOFF2 + IIVO(AREP,LREP)) * RCW + 1
      LBCI = (IOFF2 + LLOVVT(LREP,BCREP)) * RCW + 1
      CALL XGEMM ('N','N',M,N,O,A1,VOOO(ALJK),M,
     &            T2T(LBCI),O,A1,BUF1(OFF1),M)
C-------------------------------------------
C U'(ABC) = U'(ABC) - W(A,L:IK) * T(L,BC:J)
C-------------------------------------------
      LREP = MULTB(JREP,BCREP+NREP,2)
      O = NO(LREP)
      ALIK = (IKOFF2 + IIVO(AREP,LREP)) * RCW + 1
      LBCJ = (JOFF2 + LLOVVT(LREP,BCREP)) * RCW + 1
      CALL XGEMM ('N','N',M,N,O,-A1,VOOO(ALIK),M,
     &            T2T(LBCJ),O,A1,BUF1(OFF1),M)
C
C ************************************
C **** end of task splitting 
C **** offsets have to be corrected
C ************************************
      ENDIF

      OFF1 = OFF1 + M * N * RCW
      ENDDO
C
      CPU2 = CPU2 + CPUDELTA()
C
C ** the U array is complete on the master node. 
C ** we can permute the indices and compute on the
C ** master only...


      IF(MYPROC.EQ.MASTER) THEN

C-------------------------------------
C U(ABC) = U'(ABC) - U'(BAC) + U'(CAB)
C-------------------------------------
      DO BCREP = 1, NREP
      AREP = MULTB(BCREP+NREP,IJKREP,2)
      DO 10 CREP = 1, NREP
      BREP = MULTB(CREP,BCREP+NREP,2)
      IF (BREP.LT.CREP.OR.AREP.LT.BREP) GOTO 10
      ABREP = MULTB(AREP,BREP,1)
      ACREP = MULTB(AREP,CREP,1)
      DO C = 1, NV(CREP)
      BMIN = 1
      IF (BREP.EQ.CREP) BMIN = C + 1
      DO B = BMIN, NV(BREP)
      AMIN = 1
      IF (AREP.EQ.BREP) AMIN = B + 1
      BC = IIVVT(BREP,CREP) + (C-1) * NV(BREP) + B
      AB = IIVVT(AREP,BREP) + (B-1) * NV(AREP) + AMIN
      AC = IIVVT(AREP,CREP) + (C-1) * NV(AREP) + AMIN
      IF (AREP.EQ.BREP) AB = AB - B*(B+1)/2
      IF (AREP.EQ.CREP) AC = AC - C*(C+1)/2
      IF (BREP.EQ.CREP) BC = BC - C*(C+1)/2
      ABC = (IVVVT(BCREP,IJKREP) + (BC-1)*NV(AREP) + AMIN-1) * RCW + 1
      BAC = (IVVVT(ACREP,IJKREP) + (AC-1)*NV(BREP) + B - 1) * RCW + 1
      CAB = (IVVVT(ABREP,IJKREP) + (AB-1)*NV(CREP) + C - 1) * RCW + 1
      DO A = AMIN, NV(AREP)
       BUF3(ABC) = BUF1(ABC) - BUF1(BAC) + BUF1(CAB)
       IF (CARITH) BUF3(ABC+1) = BUF1(ABC+1) - BUF1(BAC+1) + BUF1(CAB+1)
       ABC = ABC + RCW
       BAC = BAC + NV(BREP) * RCW
       CAB = CAB + NV(CREP) * RCW
      ENDDO
      ENDDO
      ENDDO
 10   CONTINUE
      ENDDO
C-------------------------------------------------
C Y2'(ABC) = V(AB:IJ) * T(C:K) + T(AB:IJ) * F(C:K)
C          + V(AB:JK) * T(C:I) + T(AB:JK) * F(C:I)
C          - V(AB:IK) * T(C:J) - T(AB:IK) * F(C:J)
C-------------------------------------------------
      CALL XCOPY (IVVVT(NREP+1,IJKREP),A0,0,BUF2,1)
      ABIJ = IJOFF3 * RCW + 1
      ABJK = JKOFF3 * RCW + 1
      ABIK = IKOFF3 * RCW + 1
      DO C = 1, NV(KREP)
         CK = (IVO(KREP) + (K-1) * NV(KREP) + C - 1) * RCW + 1
         ABC = (JVVVT(KREP,IJKREP)+(C-1)*NVVT(IJREP)) * RCW + 1
         CALL XAXPY (NVVT(IJREP),T1 (CK),VVOO(ABIJ),1,BUF2(ABC),1)
         CALL XAXPY (NVVT(IJREP),FVO(CK),T2  (ABIJ),1,BUF2(ABC),1)
      ENDDO
      DO C = 1, NV(IREP)
         CI = (IVO(IREP) + (I-1) * NV(IREP) + C - 1) * RCW + 1
         ABC = (JVVVT(IREP,IJKREP)+(C-1)*NVVT(JKREP)) * RCW + 1
         CALL XAXPY (NVVT(JKREP),T1 (CI),VVOO(ABJK),1,BUF2(ABC),1)
         CALL XAXPY (NVVT(JKREP),FVO(CI),T2  (ABJK),1,BUF2(ABC),1)
      ENDDO
      DO C = 1, NV(JREP)
         CJ = (IVO(JREP) + (J-1) * NV(JREP) + C - 1) * RCW + 1
         ABC = (JVVVT(JREP,IJKREP)+(C-1)*NVVT(IKREP)) * RCW + 1
         VMIN(1) = - T1(CJ)
         IF (CARITH) VMIN(2) = - T1(CJ+1)
         CALL XAXPY (NVVT(IKREP),VMIN,VVOO(ABIK),1,BUF2(ABC),1)
         VMIN(1) = - FVO(CJ)
         IF (CARITH) VMIN(2) = - FVO(CJ+1)
         CALL XAXPY (NVVT(IKREP),VMIN,T2  (ABIK),1,BUF2(ABC),1)
      ENDDO
C-----------------------------------------
C Y2(ABC) = Y2'(ABC) + Y2'(BCA) - Y2'(ACB)
C-----------------------------------------
      DO BCREP = 1, NREP
      AREP = MULTB(BCREP+NREP,IJKREP,2)
      DO 20 CREP = 1, NREP
      BREP = MULTB(CREP,BCREP+NREP,2)
      IF (BREP.LT.CREP.OR.AREP.LT.BREP) GOTO 20
      ABREP = MULTB(AREP,BREP,1)
      ACREP = MULTB(AREP,CREP,1)
      DO C = 1, NV(CREP)
      BMIN = 1
      IF (BREP.EQ.CREP) BMIN = C + 1
      DO B = BMIN, NV(BREP)
      AMIN = 1
      IF (AREP.EQ.BREP) AMIN = B + 1
      AB = IIVVT(AREP,BREP) + (B-1) * NV(AREP) + AMIN
      AC = IIVVT(AREP,CREP) + (C-1) * NV(AREP) + AMIN
      BC = IIVVT(BREP,CREP) + (C-1) * NV(BREP) + B
      IF (AREP.EQ.BREP) AB = AB - B*(B+1)/2
      IF (AREP.EQ.CREP) AC = AC - C*(C+1)/2
      IF (BREP.EQ.CREP) BC = BC - C*(C+1)/2
      ABC1 = (IVVVT(BCREP,IJKREP) + (BC-1)*NV(AREP)+AMIN-1)  * RCW + 1
      ABC2 = (JVVVT(CREP,IJKREP) + (C-1)*NVVT(ABREP)+AB-1)   * RCW + 1
      BCA  = (JVVVT(AREP,IJKREP) + (AMIN-1)*NVVT(BCREP)+BC-1)* RCW + 1
      ACB  = (JVVVT(BREP,IJKREP) + (B-1)*NVVT(ACREP)+AC-1)   * RCW + 1
      DO A = AMIN, NV(AREP)
       BUF1(ABC1) = BUF2(ABC2) + BUF2(BCA) - BUF2(ACB)
       IF (CARITH) BUF1(ABC1+1) = BUF2(ABC2+1)+BUF2(BCA+1)-BUF2(ACB+1)
       ABC1 = ABC1 + RCW
       ABC2 = ABC2 + RCW
       ACB = ACB + RCW
       BCA = BCA + NVVT(BCREP) * RCW
      ENDDO
      ENDDO
      ENDDO
 20   CONTINUE
      ENDDO
C-----------------------------------------
C DE(+T)   = - U(ABC) * U(ABC) / EPS(ABC)
C DE'((T)) = - U(ABC) * Y2(ABC) / EPS(ABC)
C-----------------------------------------
      DO BCREP = 1, NREP
      AREP = MULTB(BCREP+NREP,IJKREP,2)
      DO 30 CREP = 1, NREP
      BREP = MULTB(CREP,BCREP+NREP,2)
      IF (BREP.LT.CREP.OR.AREP.LT.BREP) GOTO 30
      DO C = 1, NV(CREP)
      CC = IO(NREP+1) + IV(CREP) + C
      BMIN = 1
      IF (BREP.EQ.CREP) BMIN = C + 1
      DO B = BMIN, NV(BREP)
      BB = IO(NREP+1) + IV(BREP) + B
      AMIN = 1
      IF (AREP.EQ.BREP) AMIN = B + 1
      BC = IIVVT(BREP,CREP) + (C-1) * NV(BREP) + B
      IF (BREP.EQ.CREP) BC = BC - C*(C+1)/2
      ABC = (IVVVT(BCREP,IJKREP)+(BC-1)*NV(AREP)+AMIN-1) * RCW + 1
      DO A = AMIN, NV(AREP)
       AA = IO(NREP+1) + IV(AREP) + A
       EIJKABC = EPSIJK + EPS(AA) + EPS(BB) + EPS(CC)
       ET1 = ET1 - BUF3(ABC) * BUF3(ABC) / EIJKABC
       ET2 = ET2 - BUF3(ABC) * BUF1(ABC) / EIJKABC
       IF (CARITH) THEN
         ET1 = ET1 - BUF3(ABC+1) * BUF3(ABC+1) / EIJKABC
         ET2 = ET2 - BUF3(ABC+1) * BUF1(ABC+1) / EIJKABC
       ENDIF
       ABC = ABC + RCW
      ENDDO
      ENDDO
      ENDDO
 30   CONTINUE
      ENDDO
C--------------------------------
C Y3'(ABC) = TAU3(AB:IJ) * T(C:K)
C          + TAU3(AB:JK) * T(C:I)
C          - TAU3(AB:IK) * T(C:J)
C--------------------------------
      CALL XCOPY (IVVVT(NREP+1,IJKREP),A0,0,BUF2,1)
      ABIJ = IJOFF3 * RCW + 1
      ABJK = JKOFF3 * RCW + 1
      ABIK = IKOFF3 * RCW + 1
      DO C = 1, NV(KREP)
         CK = (IVO(KREP) + (K-1) * NV(KREP) + C - 1) * RCW + 1
         ABC = (JVVVT(KREP,IJKREP)+(C-1)*NVVT(IJREP)) * RCW + 1
         CALL XAXPY (NVVT(IJREP),T1 (CK),BUF4(ABIJ),1,BUF2(ABC),1)
      ENDDO
      DO C = 1, NV(IREP)
         CI = (IVO(IREP) + (I-1) * NV(IREP) + C - 1) * RCW + 1
         ABC = (JVVVT(IREP,IJKREP)+(C-1)*NVVT(JKREP)) * RCW + 1
         CALL XAXPY (NVVT(JKREP),T1 (CI),BUF4(ABJK),1,BUF2(ABC),1)
      ENDDO
      DO C = 1, NV(JREP)
         CJ = (IVO(JREP) + (J-1) * NV(JREP) + C - 1) * RCW + 1
         ABC = (JVVVT(JREP,IJKREP)+(C-1)*NVVT(IKREP)) * RCW + 1
         VMIN(1) = - T1(CJ)
         IF (CARITH) VMIN(2) = - T1(CJ+1)
         CALL XAXPY (NVVT(IKREP),VMIN,BUF4(ABIK),1,BUF2(ABC),1)
      ENDDO
C-----------------------------------------
C Y3(ABC) = Y3'(ABC) + Y3'(BCA) - Y3'(ACB)
C-----------------------------------------
      DO BCREP = 1, NREP
      AREP = MULTB(BCREP+NREP,IJKREP,2)
      DO 40 CREP = 1, NREP
      BREP = MULTB(CREP,BCREP+NREP,2)
      IF (BREP.LT.CREP.OR.AREP.LT.BREP) GOTO 40
      ABREP = MULTB(AREP,BREP,1)
      ACREP = MULTB(AREP,CREP,1)
      DO C = 1, NV(CREP)
      BMIN = 1
      IF (BREP.EQ.CREP) BMIN = C + 1
      DO B = BMIN, NV(BREP)
      AMIN = 1
      IF (AREP.EQ.BREP) AMIN = B + 1
      AB = IIVVT(AREP,BREP) + (B-1) * NV(AREP) + AMIN
      AC = IIVVT(AREP,CREP) + (C-1) * NV(AREP) + AMIN
      BC = IIVVT(BREP,CREP) + (C-1) * NV(BREP) + B
      IF (AREP.EQ.BREP) AB = AB - B*(B+1)/2
      IF (AREP.EQ.CREP) AC = AC - C*(C+1)/2
      IF (BREP.EQ.CREP) BC = BC - C*(C+1)/2
      ABC1 = (IVVVT(BCREP,IJKREP) + (BC-1)*NV(AREP)+AMIN-1)  * RCW + 1
      ABC2 = (JVVVT(CREP,IJKREP) + (C-1)*NVVT(ABREP)+AB-1)   * RCW + 1
      BCA  = (JVVVT(AREP,IJKREP) + (AMIN-1)*NVVT(BCREP)+BC-1)* RCW + 1
      ACB  = (JVVVT(BREP,IJKREP) + (B-1)*NVVT(ACREP)+AC-1)   * RCW + 1
      DO A = AMIN, NV(AREP)
       BUF1(ABC1) = BUF2(ABC2) + BUF2(BCA) - BUF2(ACB)
       IF (CARITH) BUF1(ABC1+1) = BUF2(ABC2+1)+BUF2(BCA+1)-BUF2(ACB+1)
       ABC1 = ABC1 + RCW
       ABC2 = ABC2 + RCW
       ACB = ACB + RCW
       BCA = BCA + NVVT(BCREP) * RCW
      ENDDO
      ENDDO
      ENDDO
 40   CONTINUE
      ENDDO
C-----------------------------
C DE'(-T) = + U(ABC) * Y3(ABC)
C-----------------------------
      DO BCREP = 1, NREP
      AREP = MULTB(BCREP+NREP,IJKREP,2)
      DO 50 CREP = 1, NREP
      BREP = MULTB(CREP,BCREP+NREP,2)
      IF (BREP.LT.CREP.OR.AREP.LT.BREP) GOTO 50
      DO C = 1, NV(CREP)
      CC = IO(NREP+1) + IV(CREP) + C
      BMIN = 1
      IF (BREP.EQ.CREP) BMIN = C + 1
      DO B = BMIN, NV(BREP)
      BB = IO(NREP+1) + IV(BREP) + B
      AMIN = 1
      IF (AREP.EQ.BREP) AMIN = B + 1
      BC = IIVVT(BREP,CREP) + (C-1) * NV(BREP) + B
      IF (BREP.EQ.CREP) BC = BC - C*(C+1)/2
      ABC = (IVVVT(BCREP,IJKREP)+(BC-1)*NV(AREP)+AMIN-1) * RCW + 1
      DO A = AMIN, NV(AREP)
       AA = IO(NREP+1) + IV(AREP) + A
       ET3 = ET3 + BUF3(ABC) * BUF1(ABC) 
       IF (CARITH) THEN
         ET3 = ET3 + BUF3(ABC+1) * BUF1(ABC+1)
       ENDIF
       ABC = ABC + RCW
      ENDDO
      ENDDO
      ENDDO
 50   CONTINUE
      ENDDO
C
      CPU3 = CPU3 + CPUDELTA()
C

C ******** end of task splitting *******
      ENDIF
C ******** end of task splitting *******

      ENDDO
      ENDDO
      ENDDO
  100 CONTINUE
  200 CONTINUE
  300 CONTINUE
C
      if (myproc.eq.master) then
      WRITE (IW,1071)
      WRITE (IW,1072) ESCF
      WRITE (IW,1073) ECCSD
      WRITE (IW,1074) ET1
      WRITE (IW,1075) ET2
      WRITE (IW,1076) ET3
      WRITE (IW,1077) ESCF+ECCSD+ET1
      WRITE (IW,1078) ESCF+ECCSD+ET1+ET2
      WRITE (IW,1079) ESCF+ECCSD+ET1+ET3
      endif
      ETOT = ESCF+ECCSD+ET1+ET2
C
      CALL CPUUSED (CPUTOT)
      CPUTOT = CPUTOT - CPU0
      IF (TIMING.and.myproc.eq.master)
     &WRITE (IW,7000) CPU0,CPU1,CPU2,CPU3,CPUTOT-CPU1-CPU2-CPU3,CPUTOT
C
 1071 FORMAT(//,'  Triple excitation estimates',/)
 1072 FORMAT(' SCF energy :',T40,F25.15)
 1073 FORMAT(' CCSD correlation energy :',T40,F25.15)
 1074 FORMAT(' 4th order triples correction :',T40,F25.15)
 1075 FORMAT(' 5th order triples (T) correction :',T40,F25.15)
 1076 FORMAT(' 5th order triples -T  correction :',T40,F25.15)
 1077 FORMAT(' Total CCSD+T  energy :',T40,F25.15)
 1078 FORMAT(' Total CCSD(T) energy :',T40,F25.15)
 1079 FORMAT(' Total CCSD-T  energy :',T40,F25.15)
C
 7000 FORMAT(//' Timing of routine T3CORR :'
     &/' Before T3CORR :',T30,F12.3,' seconds'
     &/' Sorting :',T30,F12.3,' seconds'
     &/' N^7 steps :',T30,F12.3,' seconds'
     &/' Additions :',T30,F12.3,' seconds'
     &/' Untimed parts :',T30,F12.3,' seconds'
     &/' Total time in T3CORR :',T30,F12.3,' seconds')
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE ZCORE(T1,T2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Zeroes out core contributions (frozen spinors)
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 T1(*),T2(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "freeze.inc"
      INCLUDE "symm.inc"
      INCLUDE "complex.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      OFF = 1
      DO IRP = 1, NREP
         DO I = 1, NO(IRP)
            IF (IFROZ(I,IRP).NE.0)
     &      CALL XCOPY (NV(IRP),A0,0,T1(OFF),1)
            OFF = OFF + NV(IRP) * RCW
         ENDDO
      ENDDO
C
      OFF = 1
      DO IJRP = 1, NREP
      N = NVVT(IJRP)
      DO 10 JRP = 1, NREP
      IRP = MULTB(JRP,IJRP+NREP,2)
      IF (IRP.LT.JRP) GOTO 10
      DO J = 1, NO(JRP)
         IMIN = 1
         IF (IRP.EQ.JRP) IMIN = J + 1
         DO I = IMIN, NO(IRP)
            IF (IFROZ(I,IRP).NE.0.OR.IFROZ(J,JRP).NE.0)
     &         CALL XCOPY (N,A0,0,T2(OFF),1)
            OFF = OFF + N * RCW
         ENDDO
      ENDDO
 10   CONTINUE
      ENDDO
C
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE PARTS(IMODE,RMS,T1,T2,BUF1,BUF2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     IMODE : 
C     1) Sums partial contributions to T1 and T2
C     2) Synchronizes T1 and T2
C
C     BUF1 and BUF2 have the length of T1 and T2 respectively and are
C     not used with imode 2. RMS is also updated because it controls
C     the iterative process
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 T1(*),T2(*),BUF1(*),BUF2(*)
      INTEGER IMODE
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "symm.inc"
      INCLUDE "complex.inc"
      INCLUDE "ccpar.inc"
#if defined (VAR_MPI)
      INCLUDE 'mpif.h'
      DIMENSION ISTAT(MPI_STATUS_SIZE)
#endif
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      IF (NMPROC.EQ.1) RETURN
#if defined (VAR_MPI)
      IF (IMODE.EQ.1) THEN
          CALL MPI_REDUCE(T1,BUF1,NDIMT1*RCW,MPI_DOUBLE_PRECISION,
     &                    MPI_SUM,MASTER,MPI_COMM_WORLD,IERR)
          CALL MPI_REDUCE(T2,BUF2,NDIMT2*RCW,MPI_DOUBLE_PRECISION,
     &                    MPI_SUM,MASTER,MPI_COMM_WORLD,IERR)
          IF (MYPROC.EQ.MASTER) CALL XCOPY (NDIMT1,BUF1,1,T1,1)
          IF (MYPROC.EQ.MASTER) CALL XCOPY (NDIMT2,BUF2,1,T2,1)
      ENDIF
C
      CALL MPI_BCAST(T1,NDIMT1*RCW,MPI_DOUBLE_PRECISION,MASTER,
     &               MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(T2,NDIMT2*RCW,MPI_DOUBLE_PRECISION,MASTER,
     &               MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(RMS,1,MPI_DOUBLE_PRECISION,MASTER,
     &               MPI_COMM_WORLD,IERR)
#endif
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE DIISX(ITAPL,ITAPD,NDIMT1,NDIMT2,T1O,T1N,T2O,T2N,IT,
     &                 CC,BB,ITC,BB2,RMS,NST1,NST2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     DIIS extrapolation of the t1 and t2 amplitudes
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher (based on a routine by T.J. Lee)
C
C---------------Calling variables--------------------------------------
C
      REAL*8 T2O(*),T2N(*),T1O(*),T1N(*)
      REAL*8 CC(MAXDIM),BB(MAXDIM+1,MAXDIM+2),BB2(MAXDIM+1,MAXDIM+2)
      REAL*8 RMS
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc" 
      INCLUDE "complex.inc" 
      INCLUDE "diis.inc"
      INCLUDE "ccpar.inc"
C
C---------------Local variables--------------------------------------
C 
      REAL*8 XM,XMAX,XEN,XFAC,XADD,DET
      REAL*8 SUM1,SUM2,SSUM,DSUM,SDOT,DDOT
      COMPLEX*16 CSUM1,ZSUM,CSUM,ZDOTC,CDOTC,CFAC
C
C---------------Executable code--------------------------------------
C
      IF (MYPROC.NE.MASTER) RETURN
C
      IT = IT + 1
      IF(IT.GT.MAXDIM) IT = MAXDIM
      ITC = ITC + 1
      IF(ITC.GT.MAXDIM) ITC = 1
C
      CALL XAXPY (NDIMT1,-A1,T1N,1,T1O,1)
      CALL XAXPY (NDIMT2,-A1,T2N,1,T2O,1)
      CALL XSCAL (NDIMT1,-A1,T1O,1)
      CALL XSCAL (NDIMT2,-A1,T2O,1)
      IF (CARITH) THEN
#if defined (BIT64)
         CSUM1 = CSUM (NDIMT1,T1O,1)
         SUM2 = CDOTC (NDIMT1,T1O,1,T1O,1)
         CSUM1 = CSUM1 + CSUM (NDIMT2,T2O,1)
         SUM2 = SUM2 + CDOTC (NDIMT2,T2O,1,T2O,1)
         RMS = CONJG(CSUM1)*CSUM1
#else
         CSUM1 = ZSUM (NDIMT1,T1O,1)
         SUM2 = ZDOTC (NDIMT1,T1O,1,T1O,1)
         CSUM1 = CSUM1 + ZSUM (NDIMT2,T2O,1)
         SUM2 = SUM2 + ZDOTC (NDIMT2,T2O,1,T2O,1)
         RMS = DCONJG(CSUM1)*CSUM1
#endif
      ELSE
#if defined (BIT64)
         SUM1 = SSUM (NDIMT1,T1O,1)
         SUM2 = SDOT (NDIMT1,T1O,1,T1O,1)
         SUM1 = SUM1 + SSUM (NDIMT2,T2O,1)
         SUM2 = SUM2 + SDOT (NDIMT2,T2O,1,T2O,1)
#else
         SUM1 = DSUM (NDIMT1,T1O,1)
         SUM2 = DDOT (NDIMT1,T1O,1,T1O,1)
         SUM1 = SUM1 + DSUM (NDIMT2,T2O,1)
         SUM2 = SUM2 + DDOT (NDIMT2,T2O,1,T2O,1)
#endif
         RMS = SUM1 * SUM1
      ENDIF
C
#if defined (BIT64)
      RMS = SQRT(ABS(SUM2-RMS))
#else
      RMS = SQRT(ABS(SUM2-RMS))
#endif
C
      SECT1 = (ITC-1)*(NST1+NST2) + 1
      SECT2 = SECT1 + NST1
      CALL RWRIT(ITAPD,T1O,INTOWP(NDIMT1*RCW),SECT1)
      CALL RWRIT(ITAPL,T1N,INTOWP(NDIMT1*RCW),SECT1)
      CALL RWRIT(ITAPD,T2O,INTOWP(NDIMT2*RCW),SECT2)
      CALL RWRIT(ITAPL,T2N,INTOWP(NDIMT2*RCW),SECT2)
C
      DO 30 N = 1,IT
C
      SECT1 = (N-1)*(NST1+NST2) + 1
      SECT2 = SECT1 + NST1
      CALL RREAD(ITAPD,T1N,INTOWP(NDIMT1*RCW),SECT1)
      CALL RREAD(ITAPD,T2N,INTOWP(NDIMT2*RCW),SECT2)
C
      IF (CARITH) THEN
#if defined (BIT64)
         CSUM1 = CDOTC (NDIMT1,T1N,1,T1O,1)
         CSUM1 = CSUM1 + CDOTC (NDIMT2,T2N,1,T2O,1)
         SUM2 = REAL(CSUM1) + AIMAG(CSUM1)
#else
         CSUM1 = ZDOTC (NDIMT1,T1N,1,T1O,1)
         CSUM1 = CSUM1 + ZDOTC (NDIMT2,T2N,1,T2O,1)
         SUM2 = DBLE(CSUM1) + DIMAG(CSUM1)
#endif
      ELSE
#if defined (BIT64)
         SUM2 = SDOT (NDIMT1,T1N,1,T1O,1)
         SUM2 = SUM2 + SDOT (NDIMT2,T2N,1,T2O,1)
#else
         SUM2 = DDOT (NDIMT1,T1N,1,T1O,1)
         SUM2 = SUM2 + DDOT (NDIMT2,T2N,1,T2O,1)
#endif
      ENDIF
C
      BB2(N,ITC) = SUM2
      BB2(ITC,N) = BB2(N,ITC)
C
   30 CONTINUE
C
C  TRANSFER INTO THE BB ARRAY
C
      DO 60 N = 1,IT
      DO 70 M = 1,N-1
      BB(N,M) = BB2(N,M)
      BB(M,N) = BB2(M,N)
  70  CONTINUE
      BB(N,N) = BB2(N,N)
  60  CONTINUE
C
C  FIND THE MAXIMUM AND SCALE
C
      XM =  ABS(BB(1,1))
      DO 80 N = 1,IT
      DO 90 M = 1,N
      XFAC =  ABS(BB(N,M))
#if defined (BIT64)
      XMAX =  MAX (XM,XFAC)
#else
      XMAX =  DMAX1(XM,XFAC)
#endif
      XM = XMAX
   90 CONTINUE
   80 CONTINUE
C
      XM = AR1/XM
      DO 100 N = 1,IT
      DO 110 M = 1,IT
      BB(N,M) = BB(N,M)*XM
  110 CONTINUE
  100 CONTINUE
C
      IT1 = IT+1
      IT2 = IT+2
      DO 120 N = 1,IT
      BB(N,IT1) = -AR1
      BB(IT1,N) = -AR1
  120 CONTINUE
C
      BB(IT1,IT1) = AR0
      DO 130 N = 1,IT
      BB(N,IT2) = AR0
  130 CONTINUE
      BB(IT1,IT2) = -AR1
C
      CALL FLIN(BB,MAXDIM+1,IT1,1,DET)
      XADD = AR0
      DO 140 N = 1,IT
      CC(N) = BB(N,IT2)
      XADD = XADD + CC(N)
  140 CONTINUE
C
      CALL XCOPY(NDIMT1,A0,0,T1N,1)
      CALL XCOPY(NDIMT2,A0,0,T2N,1)
C
      DO 150 N = 1,IT
C
      SECT1 = (N-1)*(NST1+NST2) + 1
      SECT2 = SECT1 + NST1
      CALL RREAD(ITAPL,T1O,INTOWP(NDIMT1*RCW),SECT1)
      CALL RREAD(ITAPL,T2O,INTOWP(NDIMT2*RCW),SECT2)
C
#if defined (BIT64)
      CFAC = CMPLX(CC(N),AR0)
#else
      CFAC = DCMPLX(CC(N),AR0)
#endif
      CALL XAXPY (NDIMT1,CFAC,T1O,1,T1N,1)
      CALL XAXPY (NDIMT2,CFAC,T2O,1,T2N,1)
C
  150 CONTINUE
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE CCENG(T1,T2,FVO,BUF1,BUF2,ECC)        
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates CC energy. Checks result (should be real).
C
C---------------Routines called----------------------------------------
C
C     BLAS routines
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 T1(*),T2(*)
      REAL*8 FVO(*)
      REAL*8 BUF1(*),BUF2(*)
      REAL*8 ECC
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "symm.inc"
      INCLUDE "complex.inc"
C
C---------------Local variables--------------------------------------
C
      COMPLEX*16 CECC1,CECC2
#if defined (BIT64)
      COMPLEX*16 CDOTC
#else
      COMPLEX*16 ZDOTC
#endif
      REAL*8 DDOT,SDOT
C
C---------------Executable code--------------------------------------
C
      CALL XCOPY (NDIMT1,FVO,1,BUF1,1)
      CALL DELFCK ('VO','DD',BUF1)
C
      IF (CARITH) THEN
#if defined (BIT64)
         CECC1 = CDOTC (NFVO,BUF1,1,T1,1)
         IF (ABS(AIMAG(CECC1)).GT.ACCUR) 
     &      WRITE (6,1000) 'T1',AIMAG(CECC1)
#else
         CECC1 = ZDOTC (NFVO,BUF1,1,T1,1)
         IF (ABS(DIMAG(CECC1)).GT.ACCUR) 
     &      WRITE (6,1000) 'T1',DIMAG(CECC1)
#endif
      ELSE
#if defined (BIT64)
         ECC = SDOT (NFVO,BUF1,1,T1,1)
#else
         ECC = DDOT (NFVO,BUF1,1,T1,1)
#endif
      ENDIF
C
c     print*,'T1 contribution to CC energy',ecc
C
      CALL GETVVOO (BUF1)
      CALL DELINT ('VVOO','DDDD',BUF1,0,0,0)
      CALL GETTAU  (T1,T2,BUF2)
      IF (CARITH) THEN
#if defined (BIT64)
         CECC2 = CDOTC (NDIMT2,BUF1,1,BUF2,1)
         IF (ABS(AIMAG(CECC2)).GT.ACCUR) 
     &      WRITE (6,1000) 'T2',AIMAG(CECC2)
         ECC = REAL(CECC1 + CECC2)
#else
         CECC2 = ZDOTC (NDIMT2,BUF1,1,BUF2,1)
         IF (ABS(DIMAG(CECC2)).GT.ACCUR) 
     &      WRITE (6,1000) 'T2',DIMAG(CECC2)
         ECC = DBLE(CECC1 + CECC2)
#endif
      ELSE
#if defined (BIT64)
         ECC = ECC +  SDOT (NDIMT2,BUF1,1,BUF2,1)
#else
         ECC = ECC +  DDOT (NDIMT2,BUF1,1,BUF2,1)
#endif
      ENDIF
C
      RETURN
1000  FORMAT (/' WARNING : Imaginary contribution to ',A2,
     &' part of CC energy :',F20.15)
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE MP2ENG(T1,T2,FVO,BUF1,EMP2)        
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates MP2 energy. Checks result (should be real).
C
C---------------Routines called----------------------------------------
C
C     BLAS routines
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 T1(*),T2(*)
      REAL*8 FVO(*)
      REAL*8 BUF1(*)
      REAL*8 EMP2
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "symm.inc"
      INCLUDE "complex.inc"
C
C---------------Local variables--------------------------------------
C
      COMPLEX*16 CEMP21,CEMP22
      COMPLEX*16 CDOTC,ZDOTC
      REAL*8 DDOT,ZDOT
C
C---------------Executable code--------------------------------------
C
      IF (CARITH) THEN
#if defined (BIT64)
         CEMP21 = CDOTC (NFVO,FVO,1,T1,1)
         IF (ABS(AIMAG(CEMP21)).GT.ACCUR) 
     &      WRITE (6,1000) 'T1',AIMAG(CEMP21)
#else
         CEMP21 = ZDOTC (NFVO,FVO,1,T1,1)
         IF (ABS(DIMAG(CEMP21)).GT.ACCUR) 
     &      WRITE (6,1000) 'T1',DIMAG(CEMP21)
#endif
      ELSE
#if defined (BIT64)
         EMP2 = SDOT (NFVO,FVO,1,T1,1)
#else
         EMP2 = DDOT (NFVO,FVO,1,T1,1)
#endif
      ENDIF
C
c     print*,'T1 contribution to MP2 energy',emp2
C
      CALL GETVVOO(BUF1)
      IF (CARITH) THEN
#if defined (BIT64)
         CEMP22 = CDOTC (NDIMT2,BUF1,1,T2,1)
         IF (ABS(AIMAG(CEMP22)).GT.ACCUR) 
     &      WRITE (6,1000) 'T2',AIMAG(CEMP22)
         EMP2 = REAL(CEMP21 + CEMP22)
#else
         CEMP22 = ZDOTC (NDIMT2,BUF1,1,T2,1)
         IF (ABS(DIMAG(CEMP22)).GT.ACCUR) 
     &      WRITE (6,1000) 'T2',DIMAG(CEMP22)
         EMP2 = DBLE(CEMP21 + CEMP22)
#endif
      ELSE
#if defined (BIT64)
         EMP2 = EMP2 + SDOT (NDIMT2,BUF1,1,T2,1)
#else
         EMP2 = EMP2 + DDOT (NDIMT2,BUF1,1,T2,1)
#endif
      ENDIF
C
      RETURN
1000  FORMAT (/' WARNING : Imaginary contribution to ',A2,
     &' part of MP2 energy :',F20.15)
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE DENOM (EPS,T1,T2,CT1,CT2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Divide T1 and T2 by denominators
C
C---------------Routines called----------------------------------------
C
C     XCOPY
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 EPS(*)
      REAL*8 T1(*),T2(*)
      COMPLEX*16 CT1(*),CT2(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "symm.inc"
      INCLUDE "complex.inc"
C
C---------------Local variables--------------------------------------
C
      REAL*8 FAC,FAC1,FAC2,FAC3
C
C---------------Executable code--------------------------------------
C
      II = 0
      AI = 0
      DO IRP = 1, NREP
      DO I = 1, NO(IRP)
         II = II + 1
         FAC1 = EPS(II)
         AOFF = IO(NREP+1) + IV(IRP)
         DO A = 1, NV(IRP)
             AA = AOFF + A
             FAC = FAC1 - EPS(AA)
             AI = AI + 1
             IF (CARITH) THEN
                CT1(AI) = CT1(AI)/FAC
             ELSE
                T1(AI) = T1(AI)/FAC
             ENDIF
         ENDDO
      ENDDO
      ENDDO
C
      ABIJ = 0
      DO IJRP = 1, NREP
      DO 10 JRP = 1, NREP
      JJ = IO(JRP)
      IRP = MULTB(JRP,IJRP+NREP,2)
      IF (IRP.LT.JRP) GOTO 10
      IOFF = IO(IRP) 
      DO J = 1, NO(JRP)
         JJ = JJ + 1
         FAC1 = EPS(JJ)
         IMIN = 1
         IF (IRP.EQ.JRP) IMIN = J + 1
         DO I = IMIN, NO(IRP)
            II = IOFF + I
            FAC2 = EPS(II) + FAC1
            DO 20 BRP = 1, NREP
            BB = IV(BRP) + IO(NREP+1)
            ARP = MULTB(BRP,IJRP+NREP,2)
            IF (ARP.LT.BRP) GOTO 20
            AOFF = IV(ARP) + IO(NREP+1)
            DO B = 1, NV(BRP)
               BB = BB + 1
               FAC3 = FAC2 - EPS(BB)
               AMIN = 1
               IF (ARP.EQ.BRP) AMIN = B + 1
               DO A = AMIN, NV(ARP)
                  AA = AOFF + A
                  FAC = FAC3 - EPS(AA)
                  ABIJ = ABIJ + 1
                  IF (CARITH) THEN
                     CT2(ABIJ) = CT2(ABIJ)/FAC
                  ELSE
                     T2(ABIJ) = T2(ABIJ)/FAC
                  ENDIF
               ENDDO
            ENDDO
 20         CONTINUE
         ENDDO
      ENDDO
 10   CONTINUE
      ENDDO
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE T1DIAG(DIAG,T1,BUF1)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Calculate T1-diagnostic
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 T1(*),BUF1(*),DIAG
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "complex.inc"
      INCLUDE "symm.inc"
C
C---------------Local variables--------------------------------------
C
      INTEGER SUM
      REAL*8 RSUM,DDOT,SDOT
      COMPLEX*16 ZDOTC,CDOTC
C
C---------------Executable code--------------------------------------
C
      CALL XCOPY (NDIMT1,T1,1,BUF1,1)
      CALL DELFCK ('VO','DD',BUF1)
C
      SUM = 0
      DO I = 1, NREP
         SUM = SUM + NE(I)
      ENDDO
#if defined (BIT64)
      RSUM = REAL(SUM)
#else
      RSUM = DBLE(SUM)
#endif
C
      IF (CARITH) THEN
#if defined (BIT64)
         DIAG = REAL(CDOTC (NDIMT1,BUF1,1,BUF1,1))
#else
         DIAG = DBLE(ZDOTC (NDIMT1,BUF1,1,BUF1,1))
#endif
      ELSE
#if defined (BIT64)
         DIAG = SDOT (NDIMT1,BUF1,1,BUF1,1)
#else
         DIAG = DDOT (NDIMT1,BUF1,1,BUF1,1)
#endif
      ENDIF
C
#if defined (BIT64)
      DIAG =  SQRT(DIAG/RSUM)
#else
      DIAG =  SQRT(DIAG/RSUM)
#endif
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE T1EQN1 (BUF1,BUF2,BUF3,NBUF3,S1)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Evaluate part of T1 or Lambda1 equations
C     The same contraction is used in the construction of the
C     X-intermediate.
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      INTEGER NBUF3
      REAL*8 S1(*)
      REAL*8 BUF1(*),BUF2(*),BUF3(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "symm.inc"
      INCLUDE "complex.inc"
      INCLUDE "ccpar.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
C--------------------------------------------
C S(A,I) = S(A,I) + V(K,L,C,I) * TAU(A,C,K,L)
C--------------------------------------------

C ** TAU now reorganised in BUF1: NV*NVOOT

      CALL SRT1T2 (NREP,MULTB,LFA,NVVT,NV,NV,NOOT,LVVOOT,LLVOOT,
     &             BUF3,BUF1)
      CALL GETVOOO (BUF3)
      CALL DELINT ('VOOO','DKDD',BUF3,0,0,0)

C     --------------------------------
C     Sort from 12,34 to 134,2 (VOO,O)
C     --------------------------------
      CALL SRT6 (NREP,MULTB,NVO,NV,NO,NOOT,NVOOT,LOVOOT,LLVOOT,
     &           BUF3,BUF2)

C ** VOOO integrals now reorganised in BUF2: NVOOT * NO

C     ------------------------
C     V(K,L,C,I) = V*(C,I,K,L)
C     ------------------------
      IF (CARITH) CALL CONJUGA (LOVOOT(NREP+1),BUF2,1)

C     OFF3 = 1
C     DO IREP = 1, NREP
C       M    = NV(IREP)
C       N    = NO(IREP)
C       KLD  = NVOOT(IREP)
C       K    = IDIST(1,6,IREP)
C       OFF1 = IDIST(2,6,IREP)
C       OFF2 = IDIST(3,6,IREP)
C       CALL XGEMM ('N','N',M,N,K,A1,BUF1(OFF1),M,BUF2(OFF2),KLD,
C    &              A1,S1(OFF3),M)
C       OFF3 = OFF3 + M * N * RCW
C     ENDDO

C***
C*** original CNTRCT call in serial version
C*** since parallel code (above) is not debugged
C***
C
      CALL CNTRCT ('N','N',NV,NO,NVOOT,A1,BUF1,BUF2,A1,S1,NREP)
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE T1EQN2 (BUF1,BUF2,BUF3,NBUF3,S1)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Evaluate part of T1 or Lambda1 equations
C     The same contraction is used in the construction of the
C     X-intermediate.
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      INTEGER NBUF3
      REAL*8 S1(*)
      REAL*8 BUF1(*),BUF2(*),BUF3(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "symm.inc"
      INCLUDE "complex.inc"
C
C---------------Local variables--------------------------------------
C
      LOGICAL DONE
      REAL*8 SDOT,DDOT
      COMPLEX*16 CDOTU,ZDOTU,CT1
C
C---------------Executable code--------------------------------------
C
C--------------------------------------------
C S(A,I) = S(A,I) + V(A,K,C,D) * TAU(C,D,I,K)
C--------------------------------------------
      DO 30 IRP = 1, NREP
         IF (NVO(IRP).EQ.0) GOTO 30
         DONE = .FALSE.
         ISTART = 0
         NINT = NBUF3/NVO(IRP)  ! maximum # of NVO batches

C ** NINT is updated on exit of GETVOVV !

  10     CALL GETVOVV (IRP,ISTART,NINT,DONE,BUF3,NINT)
         CALL DELINT ('VOVV','KDDD',BUF3,IRP,ISTART,NINT)
         IK = 0
         DO 20 KREP = 1, NREP
           IREP = MULTB(KREP,IRP+NREP,2)
           IF (KREP.GT.IREP) GOTO 20
           DO K = 1, NO(KREP)
              IMIN = 1
              IF (KREP.EQ.IREP) IMIN = K + 1
              DO I = IMIN, NO(IREP)
                 IK = IK + 1
                 CDIK = (IVVOOTT(IRP)+(IK-1)*NVVT(IRP)+ISTART)*RCW+1
                 DO A = 1, NV(IREP)
                    AK = (IIVO(IREP,KREP)+(K-1)*NV(IREP)+A-1) * RCW + 1
                    T1AI = (IVO(IREP)+(I-1)*NV(IREP)+A-1) * RCW + 1
                    IF (CARITH) THEN
#if defined (BIT64)
                       CT1 = CDOTU(NINT,BUF1(CDIK),1,BUF3(AK),NVO(IRP))
                       S1(T1AI) = S1(T1AI) + REAL(CT1)
                       S1(T1AI+1) = S1(T1AI+1) + AIMAG(CT1)
#else
                       CT1 = ZDOTU(NINT,BUF1(CDIK),1,BUF3(AK),NVO(IRP))
                       S1(T1AI) = S1(T1AI) + DBLE(CT1)
                       S1(T1AI+1) = S1(T1AI+1) + DIMAG(CT1)
#endif
                    ELSE
                       S1(T1AI) = S1(T1AI)
#if defined (BIT64)
     &                     + SDOT(NINT,BUF1(CDIK),1,BUF3(AK),NVO(IRP))
#else
     &                     + DDOT(NINT,BUF1(CDIK),1,BUF3(AK),NVO(IRP))
#endif
                    ENDIF
                 ENDDO
                 DO A = 1, NV(KREP)
                    AI = (IIVO(KREP,IREP)+(I-1)*NV(KREP)+A-1) * RCW + 1
                    T1AK = (IVO(KREP)+(K-1)*NV(KREP)+A-1) * RCW + 1
                    IF (CARITH) THEN
#if defined (BIT64)
                       CT1 = CDOTU(NINT,BUF1(CDIK),1,BUF3(AI),NVO(IRP))
                       S1(T1AK) = S1(T1AK) - REAL(CT1)
                       S1(T1AK+1) = S1(T1AK+1) - AIMAG(CT1)
#else
                       CT1 = ZDOTU(NINT,BUF1(CDIK),1,BUF3(AI),NVO(IRP))
                       S1(T1AK) = S1(T1AK) - DBLE(CT1)
                       S1(T1AK+1) = S1(T1AK+1) - DIMAG(CT1)
#endif
                    ELSE
                       S1(T1AK) = S1(T1AK)
#if defined (BIT64)
     &                     - SDOT(NINT,BUF1(CDIK),1,BUF3(AI),NVO(IRP))
#else
     &                     - DDOT(NINT,BUF1(CDIK),1,BUF3(AI),NVO(IRP))
#endif
                    ENDIF
                 ENDDO
              ENDDO
           ENDDO
 20      CONTINUE
         IF (.NOT.DONE) THEN
            ISTART = ISTART + NINT
            GOTO 10
         ENDIF
 30   CONTINUE
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE BFLOPS(CPU4V,NIT)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Calculate performance of largest contraction
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 CPU4V
      INTEGER NIT
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
      REAL*8 MFLOP,MFLOPS,DRCW2,DRCW,DNVVT,DNOOT,DNVO,DNVOOT,DNV,DNO
C
C---------------Executable code--------------------------------------
C
      IF (CPU4V.EQ.0.0) RETURN
      MFLOP = 0.D0
C
      DRCW2 = DBLE(2*RCW**2)
      DRCW  = DBLE(RCW)
      DO IRP = 1, NREP

         DNVVT = DBLE(NVVT(IRP))
         DNOOT = DBLE(NOOT(IRP))
         DNVO  = DBLE(NVO(IRP))
         DNVOOT= DBLE(NVOOT(IRP))
         DNV   = DBLE(NV(IRP))
         DNO   = DBLE(NO(IRP))

         MFLOP = MFLOP + DNVVT  * DNVVT * DNOOT * DRCW2
         MFLOP = MFLOP + DNVVT  * DNVO  * DNOOT * DRCW2
         MFLOP = MFLOP + DNVOOT * DNV   * DNO   * DRCW2
         MFLOP = MFLOP + DNVVT  * DNOOT * DRCW

C        MFLOP = MFLOP + NVVT(IRP) * NVVT(IRP) * NOOT(IRP) * 2 * RCW**2
C        MFLOP = MFLOP + NVVT(IRP) * NVO (IRP) * NOOT(IRP) * 2 * RCW**2
C        MFLOP = MFLOP + NVOOT(IRP) * NV (IRP) * NO  (IRP) * 2 * RCW**2
C        MFLOP = MFLOP + NVVT(IRP) * NOOT(IRP) * RCW

      ENDDO 
C
      MFLOP = MFLOP * 0.000001
      MFLOPS = MFLOP * DBLE(NIT) / CPU4V
      WRITE (IW,1000) MFLOP,MFLOPS
C
 1000 FORMAT (/' Performance of routine BINTM (largest contraction)'
     &        /' MFlop in each iteration : ',T30,F10.2 
     &        /' MFlops (average) : ',T30,F10.2)
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE DELFCK (CLASS,DODEL,FXX)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Delete integrals that contribute to unphysical excitations
C     in the Fock space calculations.
C
C     The spinors space is arranged as
C
C
C      #       O         V
C     -----------------------
C      1      IO        AV
C      2      AO        IV
C      3      AV        AO
C  
C      This routine deletes the AV integrals in the occupied space if
C      the integrals is to be used in a contraction over truly O-type
C      spinor, while in the virtual space it will delete the AO 
C      class, if desired. The parameter CLASS specifies the type
C      of the incoming integrals, the parameter DODEL specifies the
C      indices that are to be cleaned.
C
C---------------Routines called----------------------------------------
C
C     XCOPY
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 FXX(*)
      CHARACTER*2 CLASS,DODEL
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "symm.inc"
      INCLUDE "complex.inc"
      INCLUDE "param.inc"
      INCLUDE "eqns.inc"
C
C---------------Local variables--------------------------------------
C
      DIMENSION NINDX(0:2,MXREP,2)
C
C---------------Executable code--------------------------------------
C
C     Return when we do not do Fock space calculations
C
      IF (EQNS.NE.'FOCKSP') RETURN
c     write (6,*) class,dodel
C
C     NINDX(1,IREP,INDEX) : Spinors to be kept
C     NINDX(2,IREP,INDEX) : Additional spinors to be zeroed.
C
      DO INDEX = 1, 2
         IF (CLASS(INDEX:INDEX).EQ.'O') THEN
            DO IREP = 1, NREP
               IF (DODEL(INDEX:INDEX).EQ.'D') THEN
                  NINDX(0,IREP,INDEX) = 0
                  NINDX(1,IREP,INDEX) = NSO(IREP)
                  NINDX(2,IREP,INDEX) = NO(IREP)
               ELSE
                  NINDX(0,IREP,INDEX) = 0
                  NINDX(1,IREP,INDEX) = NO(IREP)
                  NINDX(2,IREP,INDEX) = NO(IREP)
               ENDIF
            ENDDO
         ELSEIF (CLASS(INDEX:INDEX).EQ.'V') THEN
            DO IREP = 1, NREP
               IF (DODEL(INDEX:INDEX).EQ.'D') THEN
                  NINDX(0,IREP,INDEX) = 0
                  NINDX(1,IREP,INDEX) = NSV(IREP)
                  NINDX(2,IREP,INDEX) = NV(IREP)
               ELSE
                  NINDX(0,IREP,INDEX) = 0
                  NINDX(1,IREP,INDEX) = NV(IREP)
                  NINDX(2,IREP,INDEX) = NV(IREP)
               ENDIF
            ENDDO
         ELSE
            CALL QUIT ('DELFCK integral class ')
         ENDIF
      ENDDO
C
      IJ = 1
C
C     Loop over J : irrep, spinor class, spinors
C
      DO 10 JREP = 1, NREP
         IREP = JREP
         DO JC =  1, 2
         DO J =  NINDX(JC-1,JREP,2)+1, NINDX(JC,JREP,2)
C
C    Loop over I : (irrep fixed via JREP), spinor class
C    The loop over spinors is done within XCOPY
C
            DO IC = 1, 2
               IMIN = NINDX(IC-1,IREP,1)+1
               LNGTH=NINDX(IC,IREP,1)-IMIN+1
               IDEL = IC + JC
C
C              Only when IDEL = 2 all indices are allowed
C
               IF (LNGTH.GT.0.AND.IDEL.GT.2) 
     &            CALL XCOPY (LNGTH,A0,0,FXX(IJ),1)
               IF (LNGTH.GT.0) IJ = IJ + RCW * LNGTH
            ENDDO    ! IC
C
         ENDDO    ! J
         ENDDO    ! JC
 10   CONTINUE ! JREP
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE DELINT (CLASS,DODEL,VXXXX,IREPVV,ISTART,NINT)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Delete integrals that contribute to unphysical excitations
C     in the Fock space calculations.
C
C     The spinors space is arranged as
C
C
C      #       O         V
C     -----------------------
C      1      IO        AV
C      2      AO        IV
C      3      AV        AO
C  
C      This routine deletes the AV integrals in the occupied space if
C      the integrals is to be used in a contraction over truly O-type
C      spinor, while in the virtual space it will delete the AO 
C      class, if desired. The parameter CLASS specifies the type
C      of the incoming integrals, the parameter DODEL specifies the
C      indices that are to be cleaned.
C
C---------------Routines called----------------------------------------
C
C     XCOPY
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 VXXXX(*)
      CHARACTER*4 CLASS,DODEL
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "symm.inc"
      INCLUDE "param.inc"
      INCLUDE "complex.inc"
      INCLUDE "eqns.inc"
C
C---------------Local variables--------------------------------------
C
      DIMENSION NINDX(0:2,MXREP,4)
      LOGICAL TRIANG(2),ALLOWED,ALLOW1,ALLOW2
C
C---------------Executable code--------------------------------------
C
C     Return when we do not do Fock space calculations
C
      IF (EQNS.NE.'FOCKSP') RETURN
c     write (6,*) class,dodel
C
      TRIANG(1) = CLASS(1:1).EQ.CLASS(2:2)
      TRIANG(2) = CLASS(3:3).EQ.CLASS(4:4)
C
C     NINDX(1,IREP,INDEX) : Spinors to be kept
C     NINDX(2,IREP,INDEX) : Additional spinors to be zeroed.
C
      INDEXS = 1
      INDEXE = 2
      DO INDEXP = 1, 2
      DO INDEX = INDEXS, INDEXE
         IF (CLASS(INDEX:INDEX).EQ.'O') THEN
            DO IREP = 1, NREP
               IF (DODEL(INDEX:INDEX).EQ.'D') THEN
                  NINDX(0,IREP,INDEX) = 0
                  NINDX(1,IREP,INDEX) = NSO(IREP)
                  NINDX(2,IREP,INDEX) = NO(IREP)
               ELSE
                  NINDX(0,IREP,INDEX) = 0
                  NINDX(1,IREP,INDEX) = NO(IREP)
                  NINDX(2,IREP,INDEX) = NO(IREP)
               ENDIF
C
C              Triangular arrays need special treatment, we can have the
C              case that out to be deleted index is related by anti-
C              symmetry to the index to be kept. 
C              We treat this as the condition 'DD'to have the proper
C              loop structure but modify the delete condition somewhat
C    
               IF (TRIANG(INDEXP).AND.
     &             (DODEL(INDEXS:INDEXE).EQ.'DK'.OR.
     &              DODEL(INDEXS:INDEXE).EQ.'KD')) THEN
                  NINDX(0,IREP,INDEX) = 0
                  NINDX(1,IREP,INDEX) = NSO(IREP)
                  NINDX(2,IREP,INDEX) = NO(IREP)
               ENDIF
            ENDDO
         ELSEIF (CLASS(INDEX:INDEX).EQ.'V') THEN
            DO IREP = 1, NREP
               IF (DODEL(INDEX:INDEX).EQ.'D') THEN
                  NINDX(0,IREP,INDEX) = 0
                  NINDX(1,IREP,INDEX) = NSV(IREP)
                  NINDX(2,IREP,INDEX) = NV(IREP)
               ELSE
                  NINDX(0,IREP,INDEX) = 0
                  NINDX(1,IREP,INDEX) = NV(IREP)
                  NINDX(2,IREP,INDEX) = NV(IREP)
               ENDIF
C
C              The same treatment for the triangular condition as above
C    
               IF (TRIANG(INDEXP).AND.
     &             (DODEL(INDEXS:INDEXE).EQ.'DK'.OR.
     &              DODEL(INDEXS:INDEXE).EQ.'KD')) THEN
                  NINDX(0,IREP,INDEX) = 0
                  NINDX(1,IREP,INDEX) = NSV(IREP)
                  NINDX(2,IREP,INDEX) = NV(IREP)
               ENDIF
            ENDDO
         ELSE
            CALL QUIT ('DELINT integral class ')
         ENDIF
      ENDDO
      INDEXS = INDEXS + 2
      INDEXE = INDEXE + 2
      ENDDO
C
      IJKL = 1
C
C     Loop over compound irreps
C
      DO 40 KLREP = 1, NREP
         KL = 0
C
C        Loop over L : irrep, spinor classes, spinors
C
         DO 30 LREP = 1, NREP
         KREP = MULTB(LREP,KLREP+NREP,2)
         IF (TRIANG(2).AND.KREP.LT.LREP) GOTO 30
            
         DO LC = 1, 2
         DO L =  NINDX(LC-1,LREP,4)+1, NINDX(LC,LREP,4)
C
C           Loop over K: irrep(fixed by KLREP,JREP),spinor class,spinors
C
            DO KC =  1, 2
            KMIN = NINDX(KC-1,KREP,3)+1
            IF (KREP.EQ.LREP.AND.TRIANG(2)) KMIN = MAX0(KMIN,L+1)
C
C           Condition for keep/delete of this index pair
C
            IF (KC+LC.EQ.2) THEN
               ALLOW2 = .TRUE.
            ELSEIF (KC+LC.EQ.3.AND.TRIANG(2).AND.
     &             (DODEL(3:4).EQ.'DK'.OR.
     &              DODEL(3:4).EQ.'KD')) THEN
               ALLOW2 = .TRUE.
            ELSE
               ALLOW2 = .FALSE.
            ENDIF
C
            DO 20 K = KMIN, NINDX(KC,KREP,3)
C
C           For the VOVV and VVVV integrals we have only a batch in
C           memory, jump out if we are outside this area
C
            KL = KL + 1
            IF (CLASS.EQ.'VOVV'.OR.CLASS.EQ.'VVVV') THEN
               IF (KLREP.NE.IREPVV) GOTO 40
               IF (KL.LE.ISTART.OR.KL.GT.ISTART+NINT) GOTO 20
            ENDIF
C
C    Loop over J : irrep, spinor class, spinors
C
               DO 10 JREP = 1, NREP
               IREP = MULTB(JREP,KLREP+NREP,2)
               IF (IREP.LT.JREP.AND.TRIANG(1)) GOTO 10
C
               DO JC =  1, 2
               DO J =  NINDX(JC-1,JREP,2)+1, NINDX(JC,JREP,2)
C
C    Loop over I : (irrep fixed via KLREP and JREP), spinor class
C    The loop over spinors is done within XCOPY
C
                  DO IC = 1, 2
                     IMIN = NINDX(IC-1,IREP,1)+1
                     IF (IREP.EQ.JREP.AND.TRIANG(1)) IMIN=MAX0(IMIN,J+1)
                     LNGTH=NINDX(IC,IREP,1)-IMIN+1
C
C                    Condition for keep/delete of this index pair
C
                     IF (IC+JC.EQ.2) THEN
                        ALLOW1 = .TRUE.
                     ELSEIF (IC+JC.EQ.3.AND.TRIANG(1).AND.
     &                      (DODEL(1:2).EQ.'DK'.OR.
     &                       DODEL(1:2).EQ.'KD')) THEN
                        ALLOW1 = .TRUE.
                     ELSE
                        ALLOW1 = .FALSE.
                     ENDIF
C
                     ALLOWED = ALLOW1.AND.ALLOW2
                     IF (LNGTH.GT.0.AND.(.NOT.ALLOWED))
     &                  CALL XCOPY (LNGTH,A0,0,VXXXX(IJKL),1)
                     IF (LNGTH.GT.0) IJKL = IJKL + RCW * LNGTH
                  ENDDO    ! IC
C
               ENDDO    ! J
               ENDDO    ! JC
 10            CONTINUE ! JREP
C                    
 20         CONTINUE ! K
            ENDDO ! KC
C
         ENDDO    ! L
         ENDDO    ! LC
 30      CONTINUE ! LREP
C
 40   CONTINUE ! KLREP
C
      RETURN
      END
