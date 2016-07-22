C
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
C DATE    : $Date: 2001/09/12 23:26:12 $
C FILE    : ccrpa.F
C SOURCE  : $Source: /home/tc/visscher/sources/Molfdir/Molfdir/source/relccsd/ccrpa.F,v $
C
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE RPAA(NAOPER,NAMEA,NAMEB,APHASE,BPHASE,BDONE,EPS,EFREQ,
     &           AVO,AOV,BVO,BOV,X1,X2,Y1,Y2,DG,BUF1,BUF2,CC1,BB1,BB2,
     &           CONV,IPRNT,DEBUG,TIMING)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates RPA amplitudes
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
      REAL*8 EPS(*),DG(*),AVO(*),AOV(*),BVO(*),BOV(*)
      REAL*8 X1(*),X2(*),Y1(*),Y2(*)
      REAL*8 BUF1(*),BUF2(*)
C
C     for diis ...
C
      REAL*8 CC1(*)
      REAL*8 BB1(*),BB2(*)
C
      REAL*8 RNORM,RMS,CONV,EFREQ
      COMPLEX*16 CERPA,APHASE(*),BPHASE
      CHARACTER*8 NAMEA(*),NAMEB
      LOGICAL BDONE,DEBUG,TIMING
      INTEGER NAOPER
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "files.inc"
      INCLUDE "param.inc"
      INCLUDE "symm.inc"
      INCLUDE "diis.inc"
      INCLUDE "complex.inc"
      INCLUDE "ccpar.inc"
C
C---------------Local variables--------------------------------------
C
      REAL*8 CPUDELTA,CPU0,CPUAB,CPUE,CPUVAR,CPUTOT
      real*8 sdot,ddot
      complex*16 cdotc,zdotc
      CHARACTER*17 TEXT
C
C---------------Executable code--------------------------------------
C
C     Initialize the timing.
C
      CPU0 = CPUDELTA()
      CALL CPUUSED(CPU0)
      CPUE = D0
      CPUAB = D0
C
C     Calculate record size for DIIS files.
C
      ITC = 0
      IT = 0
      NST1 = INTOWP(RCW*NDIMX)/SEC2I(1)
      IF(NST1*SEC2I(1).NE.INTOWP(RCW*NDIMX)) NST1 = NST1 + 1
      NST2 = NST1
C
C     Open DIIS scratch files
C
      NINTSB = 20
      CALL RFILE(IOAMPL,NINTSB)
      CALL RFILE(IOAMPS,NINTSB)
C
C  INITIALIZE X AND Y AMPLITUDES IF THIS IS A NEW B OPERATOR
C
      IF (.NOT.BDONE) THEN
         CALL XCOPY (NDIMX,BVO,1,X1,1)
         CALL XCOPY (NDIMX,BOV,1,Y1,1)
         CALL GETDG (EPS,BUF1,BUF2,DG)
         CALL PRECD (EFREQ,DG,X1,Y1)
      ENDIF
C
      CALL RPAVAL (X1,Y1,AVO,AOV,APHASE,BPHASE,CERPA)
C
c     if (debug) then
c     if (carith) then
c     do i = 1, ndimx
c     if (abs(avo(2*i-1)).gt.1d-10) print*,'real avo',i,avo(2*i-1)
c     if (abs(avo(2*i)).gt.1d-10) print*,'imag avo',i,avo(2*i)
c     if (abs(aov(2*i-1)).gt.1d-10) print*,'real aov',i,aov(2*i-1)
c     if (abs(aov(2*i)).gt.1d-10) print*,'imag aov',i,aov(2*i)
c     enddo
c     do i = 1, ndimx
c     if (abs(bvo(2*i-1)).gt.1d-10) print*,'real bvo',i,bvo(2*i-1)
c     if (abs(bvo(2*i)).gt.1d-10) print*,'imag bvo',i,bvo(2*i)
c     if (abs(bov(2*i-1)).gt.1d-10) print*,'real bov',i,bov(2*i-1)
c     if (abs(bov(2*i)).gt.1d-10) print*,'imag bov',i,bov(2*i)
c     enddo
c     endif
c     endif
C
      NIT = 0
      RMS = 1.D0
      IF (IPRNT.GE.1.and.myproc.eq.master) THEN
         WRITE(IW,6003) NAMEA(1),NAMEB
         WRITE(IW,6001) NIT,CERPA,RMS
      ENDIF
C
C  START ITERATIVE PROCEDURE
C
 1000 CONTINUE
c     if (debug) then
c     if (carith) then
c     do i = 1, ndimx
c     if (abs(x1(2*i-1)).gt.1d-10) print*,'real x1',i,x1(2*i-1)
c     if (abs(x1(2*i)).gt.1d-10) print*,'imag x1',i,x1(2*i)
c     if (abs(y1(2*i-1)).gt.1d-10) print*,'real y1',i,y1(2*i-1)
c     if (abs(y1(2*i)).gt.1d-10) print*,'imag y1',i,y1(2*i)
c     enddo
c     endif
c     endif
      NIT = NIT + 1
      IF (NIT.GT.MAXIT) THEN
         IF (IPRNT.GE.0.and.myproc.eq.master) 
     &     WRITE (IW,6004) RMS
         GO TO 2000
      ENDIF
C
C  GET THE CONTRIBUTIONS TO X2
C
      CALL XCOPY (NDIMX,A0,0,X2,1)
      CALL DOAX (BUF1,DG,X1,BUF2,X2,LTR)
      CALL DOBY (BUF1,Y1,BUF2,X2)
      CALL XAXPY (NDIMX,A1,BVO,1,X2,1)
C
C  GET THE CONTRIBUTIONS TO Y2*
C
      IF (CARITH) THEN
         CALL CONJUGA (NDIMX,X1,1)
         CALL CONJUGA (NDIMX,Y1,1)
         CALL CONJUGA (NDIMX,BOV,1)
      ENDIF
C
      CALL XCOPY (NDIMX,A0,0,Y2,1)
      CALL DOAX (BUF1,DG,Y1,BUF2,Y2,LTR)
      CALL DOBY (BUF1,X1,BUF2,Y2)
      CALL XAXPY (NDIMX,A1,BOV,1,Y2,1)
C
      IF (CARITH) THEN
         CALL CONJUGA (NDIMX,X1,1)
         CALL CONJUGA (NDIMX,Y1,1)
         CALL CONJUGA (NDIMX,Y2,1)
         CALL CONJUGA (NDIMX,BOV,1)
      ENDIF
C
 111  format (a,2f30.15)
      if (carith.and.debug.and.myproc.eq.master) then
      write (iw,111)"x1",zdotc(ndimx,x1,1,x1,1)
      write (iw,111)"y1",zdotc(ndimx,y1,1,y1,1)
      write (iw,111)"dg",ddot (ndimx,dg,1,dg,1)
      write (iw,111)"x2",zdotc(ndimx,x2,1,x2,1)
      write (iw,111)"y2",zdotc(ndimx,y2,1,y2,1)
      elseif (debug) then
      write (iw,111)"x1",ddot(ndimx,x1,1,x1,1)
      write (iw,111)"y1",ddot(ndimx,y1,1,y1,1)
      write (iw,111)"dg",ddot(ndimx,dg,1,dg,1)
      write (iw,111)"x2",ddot(ndimx,x2,1,x2,1)
      write (iw,111)"y2",ddot(ndimx,y2,1,y2,1)
      endif
C
      CPUAB = CPUAB + CPUDELTA()
C
C  EXTRAPOLATE THE AMPLITUDES
C
      CALL PRECD (EFREQ,DG,X2,Y2)
      CALL DIISX(IOAMPL,IOAMPS,NDIMX,NDIMX,X1,X2,Y1,Y2,IT,
     &           CC1,BB1,ITC,BB2,RMS,NST1,NST2)
C
C  WRITE OUT RESULTS
C
      CALL RPAVAL (X2,Y2,AVO,AOV,APHASE,BPHASE,CERPA)
      IF (IPRNT.GE.1.and.myproc.eq.master) 
     &    WRITE(IW,6001) NIT,CERPA,RMS
      CALL FLSHFO(IW)
C
C  PUT X2,Y2 INTO X1,Y1 AND DO NEXT ITERATION
C
      CALL XCOPY(NDIMX,X2,1,X1,1)
      CALL XCOPY(NDIMX,Y2,1,Y1,1)
C
      CPUE = CPUE + CPUDELTA()
C
      IF(RMS.LT.CONV) GO TO 2000
      GO TO 1000
C
 2000 CONTINUE
      CPUE = CPUE + CPUDELTA()
      IF (IPRNT.GT.1) THEN
         TEXT = 'property '//NAMEB
         CALL PRRPAB (TEXT,BVO,BOV,0.01)
         CALL PRRPAB ('final solution   ',X2,Y2,0.01)
         DO AOPER = 1, NAOPER
            AOFF = (AOPER-1) * NDIMX * RCW + 1
            TEXT = 'property '//NAMEA(AOPER)
            CALL PRRPAB (TEXT,AVO(AOFF),AOV(AOFF),0.01)
         ENDDO
      ENDIF
C
      IF (IPRNT.GE.0) THEN
         if (myproc.eq.master) WRITE(IW,6071)
         DO AOPER = 1, NAOPER
            AOFF = (AOPER-1) * NDIMX * RCW + 1
            CALL RPAVAL (X2,Y2,AVO(AOFF),AOV(AOFF),APHASE(AOPER),BPHASE,
     &                   CERPA)
            if (myproc.eq.master) 
     &         WRITE(IW,6072) NAMEA(AOPER),NAMEB,EFREQ,CERPA
         ENDDO
      ENDIF
      CALL CPUUSED(CPUTOT)
      CPUTOT = CPUTOT-CPU0
      CPUVAR = CPUTOT-CPUAB-CPUE
      IF (TIMING.and.(myproc.eq.master))
     &WRITE(IW,7000) CPU0,CPUAB,CPUE,CPUVAR,CPUTOT
      BDONE = .TRUE.
C
C     Close and delete DIIS scratch files
C
      JCODE = 4
      CALL RCLOSE(IOAMPL,JCODE)
      JCODE = 4
      CALL RCLOSE(IOAMPS,JCODE)
C
      RETURN
 6001 FORMAT(2X,I3,5X,2F20.15,13X,E10.5)
 6002 FORMAT(3X,'MP2   =',F20.15/3X,'EMP2  =',F20.15/)
 6003 FORMAT(//3X,'NIT',12X,' << ',A8,' ; ',A8,' >>',20X, 'RMS')
 6004 FORMAT (//' Requested convergence not reached in RPA : RMS = ',
     & F20.15)
 6071 FORMAT(//,'  RPA results',//3X,'Propagator',12X,'Frequency',13X,
     & 'Real part',8X,'Imaginary part')
 6072 FORMAT('@ <<',A8,';',A8,'>> ','(',F10.5,')',T40,2F20.13)
 7000 FORMAT(//' Timing of routine RPAA :'
     &/' Before RPAA :',T35,F12.3,' seconds'
     &/' A & B multiplications :',T35,F12.3,' seconds'
     &/' Expectation value, DIIS, etc.:',T35,F12.3,' seconds'
     &/' Untimed parts :',T35,F12.3,' seconds'
     &/' Total time in RPA :',T35,F12.3,' seconds')
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE RPAB(EPS,X1,X2,Y1,Y2,DG,BUF1,BUF2,CC1,BB1,BB2,BB3,BB4,
     &                FREQR,FREQI,FREQD,NEX,IEXREP,CONV,
     &                IPRNT,DEBUG,TIMING)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates excitation energies
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
      REAL*8 EPS(*),DG(*)
      REAL*8 X1(*),X2(*),Y1(*),Y2(*)
      REAL*8 BUF1(*),BUF2(*)
C
C     for redeq...
C
      REAL*8 FREQR(*),FREQI(*),FREQD(*)
      REAL*8 CC1(*)
      REAL*8 BB1(*),BB2(*),BB3(*),BB4(*)
C
      REAL*8 CNV,CONV,EFREQ,RNORM
      LOGICAL DEBUG,TIMING
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "files.inc"
      INCLUDE "param.inc"
      INCLUDE "symm.inc"
      INCLUDE "diis.inc"
      INCLUDE "complex.inc"
      INCLUDE "ccpar.inc"
C
C---------------Local variables--------------------------------------
C
      REAL*8 CPUDELTA,CPU0,CPUAB,CPUE,CPUVAR,CPUTOT
C
C---------------Executable code--------------------------------------
C
C     Initialize the timing.
C
      CPU0 = CPUDELTA()
      CALL CPUUSED(CPU0)
      CPUE = D0
      CPUAB = D0
C
C     Calculate record size for vector files.
C
      ITC = 0
      IT = 0
      NST1 = INTOWP(RCW*NDIMX)/SEC2I(1)
      IF(NST1*SEC2I(1).NE.INTOWP(RCW*NDIMX)) NST1 = NST1 + 1
      NST2 = NST1
C
C     Open vector scratch files
C
      NINTSB = 20
      CALL RFILE(IOAMPL,NINTSB)
      CALL RFILE(IOAMPS,NINTSB)
C
C  INITIALIZE X AND Y AMPLITUDES
C
      CALL GETDG (EPS,BUF1,BUF2,DG)
C
C  Take the lowest diagonal value as start.
C
      IS = JVOXX(IEXREP) + 1
#if defined (BIT64)
      INDMIN = ISMIN(MVO(IEXREP),DG(IS),1)
#else
      INDMIN = IDMIN(MVO(IEXREP),DG(IS),1)
#endif
      INDMIN = INDMIN + IS - 1
      CALL XCOPY (NDIMX,A0,0,X1,1)
      CALL XCOPY (NDIMX,A0,0,Y1,1)
      CALL XCOPY (1,A1,1,X1((INDMIN-1)*RCW+1),1)
C
      NIT = 0
      CNV = 1.D0
      EFREQ = DG(INDMIN)
C
      if (myproc.eq.master) then
      WRITE(IW,6003)
      IF (IPRNT.GE.1) WRITE(IW,6001) NIT,1,EFREQ,CNV
      endif
C
C  START ITERATIVE PROCEDURE
C
 1000 CONTINUE
      NIT = NIT + 1
      IF (NIT.GT.MAXIT) THEN
         if (myproc.eq.master) WRITE (IW,6004) CNV
         GO TO 2000
      ENDIF
C
C  GET THE CONTRIBUTIONS TO X2
C
      CALL XCOPY (NDIMX,A0,0,X2,1)
      CALL DOAX (BUF1,DG,X1,BUF2,X2,LFA)
      CALL DOBY (BUF1,Y1,BUF2,X2)
C
C  GET THE CONTRIBUTIONS TO Y2*
C
      IF (CARITH) THEN
         CALL CONJUGA (NDIMX,X1,1)
         CALL CONJUGA (NDIMX,Y1,1)
      ENDIF
C
      CALL XCOPY (NDIMX,A0,0,Y2,1)
      CALL DOAX (BUF1,DG,Y1,BUF2,Y2,LFA)
      CALL DOBY (BUF1,X1,BUF2,Y2)
C
      IF (CARITH) THEN
         CALL CONJUGA (NDIMX,X1,1)
         CALL CONJUGA (NDIMX,Y1,1)
         CALL CONJUGA (NDIMX,Y2,1)
      ENDIF
C
      CPUAB = CPUAB + CPUDELTA()
C
C  SOLVE THE REDUCED EQUATIONS
C
      CALL REDEQ(NDIMX,DG,X1,X2,Y1,Y2,IT,CC1,BB1,ITC,
     &           BB2,BB3,BB4,FREQR,FREQI,FREQD,EFREQ,CNV,NST1,NST2)
C
C  WRITE OUT RESULTS
C
      CALL FLSHFO(IW)
C
C  PUT X2,Y2 INTO X1,Y1 AND DO NEXT ITERATION
C
      CALL XCOPY(NDIMX,X2,1,X1,1)
      CALL XCOPY(NDIMX,Y2,1,Y1,1)
C
      CPUE = CPUE + CPUDELTA()
C
      IF(CNV.LT.CONV) GO TO 2000
      IF (IPRNT.GE.1.and.myproc.eq.master) 
     &    WRITE(IW,6001) NIT,1,EFREQ,CNV
      GO TO 1000
C
 2000 CONTINUE
      if (myproc.eq.master) WRITE(IW,6001) NIT,1,EFREQ,CNV
      CPUE = CPUE + CPUDELTA()
      CALL CPUUSED(CPUTOT)
      CPUTOT = CPUTOT-CPU0
      CPUVAR = CPUTOT-CPUAB-CPUE
      IF (TIMING.and.myproc.eq.master)
     &WRITE(IW,7000) CPU0,CPUAB,CPUE,CPUVAR,CPUTOT
C
C     Close and delete DIIS scratch files
C
      JCODE = 4
      CALL RCLOSE(IOAMPL,JCODE)
      JCODE = 4
      CALL RCLOSE(IOAMPS,JCODE)
C
      RETURN
 6001 FORMAT(2X,2I5,5X,F20.15,13X,E10.5)
 6002 FORMAT(3X,'MP2   =',F20.15/3X,'EMP2  =',F20.15/)
 6003 FORMAT(//3X,'Iter. Root  Excitation energy',20X, 'CNV')
 6004 FORMAT (//' Requested convergence not reached in RPAB : CNV = ',
     & F20.15)
 6071 FORMAT(//,'  Excitation energy ',12X,'Frequency',13X,
     & 'Real part',8X,'Imaginary part')
 6072 FORMAT(' <<',A8,';',A8,'>> ','(',F10.5,')',T40,2F20.13)
 7000 FORMAT(//' Timing of routine RPAB :'
     &/' Before RPAB :',T35,F12.3,' seconds'
     &/' A & B multiplications :',T35,F12.3,' seconds'
     &/' Expectation value, DIIS, etc.:',T35,F12.3,' seconds'
     &/' Untimed parts :',T35,F12.3,' seconds'
     &/' Total time in RPA :',T35,F12.3,' seconds')
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE DOAX(AA,DG,X1,BUF1,X2,LINRES)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates A X1 = X2  with A the RPA matrix <AK||IC> and
C                                X1 the vector X(CK)
C                                X2 the vector X(AI)
C     The diagonal of A is already calculated, zero it out when
C     we do the linear response, keep it when we do the eigenvalue
C     equations.
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 AA(*),X1(*),X2(*),DG(*)
      REAL*8 BUF1(*)
      LOGICAL LINRES
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "complex.inc"
      INCLUDE "symm.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
C---------------------------------------------------------------------
C AA(AK,IC) = - W(AK,CI)
C The minus sign will cancel for the linear response as A needs to be
C subtracted from Q. For the evaluation of the eigenvalue equations
C we do need to use the correct value.
C---------------------------------------------------------------------
      CALL GETVOVO (BUF1)
C------------------------------------------------------------------
C Now sort AA(AK,CI) to AA(AI,CK)
C------------------------------------------------------------------
      CALL SRT16 (NREP,MULTB,LFA,LFA,NV,NO,NV,NO,MVO,JVOVO,JJVO,JJVO,
     &            BUF1,AA)
C------------------------------------------------------------------
C X2(AI) = X2(AI) + AA(AI,CK) * X1(CK)
C------------------------------------------------------------------
      IF (.NOT.LINRES) CALL XSCAL (JVOVO(NREP+1),-A1,AA,1)
C
      OFF1 = 1
      OFF3 = 1
      OFF3D = 1
      DO IREP = 1, NREP
         JREP = MULTB(IREP+NREP,1+NREP,2)
         M = MVO(IREP)
         K = MVO(JREP)
         OFF2 = JVOXX(IREP) * RCW + 1
         IF (LINRES) THEN
            CALL XCOPY (M,A0,0,AA(OFF1),(M+1))
         ELSE
#if defined (BIT64)
            CALL SCOPY (M,DG(OFF3D),1,AA(OFF1),RCW*(M+1))
#else
            CALL DCOPY (M,DG(OFF3D),1,AA(OFF1),RCW*(M+1))
#endif
         ENDIF
         IF (M.GT.0)
     &   CALL XGEMV ('N',M,K,A1,AA(OFF1),M,X1(OFF2),1,A1,X2(OFF3),1)
         OFF1 = OFF1 + M * K * RCW
         OFF3 = OFF3 + M * RCW
         OFF3D = OFF3D + M
      ENDDO
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE DOBY(BB,Y1,BUF1,X2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates B Y1 = X2  with B the RPA matrix <AC||KI> and
C                                Y1 the vector Y(CK)
C                                X2 the vector X(AI)
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 BB(*),Y1(*),X2(*)
      REAL*8 BUF1(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "complex.inc"
      INCLUDE "symm.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
C---------------------------------------------------------------------
      CALL GETVVOO (BUF1)
C------------------------------------------------------------------
C Now sort (AC,IK) to (AI,CK)
C------------------------------------------------------------------
      CALL SRT1TT4 (NREP,MULTB,LFA,LFA,NV,NV,NO,NO,MVO,J2VOVO,JJVO,JJVO,
     &              BUF1,BB)
C------------------------------------------------------------------
C X2(AI) = X2(AI) + BB(AI,CK) * Y1(CK)
C Note that we started with (AC, IK) instead of (AC, KI), so we need
C to put in a minus sign in the matrix multiplication.
C------------------------------------------------------------------
      OFF1 = 1
      OFF3 = 1
      DO IREP = 1, NREP
         JREP = MULTB(IREP+NREP,1+NREP,2)
         M = MVO(IREP)
         K = MVO(JREP)
         OFF2 = JVOXX(JREP) * RCW + 1
         IF (M.GT.0)
     &   CALL XGEMV ('N',M,K,-A1,BB(OFF1),M,Y1(OFF2),1,A1,X2(OFF3),1)
         OFF1 = OFF1 + M * K * RCW
         OFF3 = OFF3 + M * RCW
      ENDDO
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE NORMX(NDIMX,FACTOR,X1,Y1,RNORM)        
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C---------------Description--------------------------------------------
C
C     Normalizes the expansion vector (X, Y) over the RPA metric.
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
      REAL*8 X1(*),Y1(*),FACTOR
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "complex.inc"
C
C---------------Local variables--------------------------------------
C
      COMPLEX*16 CDOTC,ZDOTC
      REAL*8 DDOT,SDOT
      REAL*8 RNORM
C
C---------------Executable code--------------------------------------
C
      IF (CARITH) THEN
#if defined (BIT64)
         RNORM = CDOTC (NDIMX,X1,1,X1,1)
#else
         RNORM = ZDOTC (NDIMX,X1,1,X1,1)
#endif
      ELSE
#if defined (BIT64)
         RNORM = SDOT (NDIMX,X1,1,X1,1)
#else
         RNORM = DDOT (NDIMX,X1,1,X1,1)
#endif
      ENDIF
C
      IF (CARITH) THEN
#if defined (BIT64)
         RNORM = RNORM + FACTOR * CDOTC (NDIMX,Y1,1,Y1,1)
#else
         RNORM = RNORM + FACTOR * ZDOTC (NDIMX,Y1,1,Y1,1)
#endif 
      ELSE
#if defined (BIT64)
         RNORM = RNORM + FACTOR * SDOT (NDIMX,Y1,1,Y1,1)
#else
         RNORM = RNORM + FACTOR * DDOT (NDIMX,Y1,1,Y1,1)
#endif
      ENDIF
C
      IF (RNORM.LT.1.D-32) CALL QUIT('RNORM TOO SMALL')
C
#if defined (BIT64)
      RNORM = 1.0 / SQRT(RNORM)
#else
      RNORM = 1.D0 / SQRT(RNORM)
#endif
C
      IF (CARITH) THEN
#if defined (BIT64)
         CALL CSCAL (NDIMX,RNORM,X1,1)
         CALL CSCAL (NDIMX,RNORM,Y1,1)
#else
         CALL ZSCAL (NDIMX,RNORM,X1,1)
         CALL ZSCAL (NDIMX,RNORM,Y1,1)
#endif
      ELSE
#if defined (BIT64)
         CALL SSCAL (NDIMX,RNORM,X1,1)
         CALL SSCAL (NDIMX,RNORM,Y1,1)
#else
         CALL DSCAL (NDIMX,RNORM,X1,1)
         CALL DSCAL (NDIMX,RNORM,Y1,1)
#endif
      ENDIF
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE PRECD (EFREQ,DG,X1,Y1)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Divide  X1(AI) by DG(AI)
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 EFREQ,DG(*)
      REAL*8 X1(*),Y1(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "symm.inc"
      INCLUDE "complex.inc"
C
C---------------Local variables--------------------------------------
C
      INTEGER AI
C
C---------------Executable code--------------------------------------
C
      IF (CARITH) THEN
         DO AI = 1, 2 * NDIMX
            IF (EFREQ - DG((AI+1)/2).NE.0.D0) THEN
               X1(AI) = X1(AI) / ( DG((AI+1)/2) - EFREQ)
            ELSE
#if defined (BIT64)
               X1(AI) = X1(AI) * 10.E16
#else
               X1(AI) = X1(AI) * 10.D16
#endif
            ENDIF
            Y1(AI) = Y1(AI) / ( DG((AI+1)/2) + EFREQ)
         ENDDO
      ELSE
         DO AI = 1, NDIMX
            IF (EFREQ - DG(AI).NE.0.D0) THEN
               X1(AI) = X1(AI) / ( DG(AI) - EFREQ)
            ELSE
#if defined (BIT64)
               X1(AI) = X1(AI) * 10.E16
#else
               X1(AI) = X1(AI) * 10.D16
#endif
            ENDIF
            Y1(AI) = Y1(AI) / ( DG(AI) + EFREQ)
         ENDDO
      ENDIF
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE REDEQ(NDIMX,DG,X1,X2,Y1,Y2,IT,CC,HH,ITC,
     &                 SS,HH1,SS1,FREQR,FREQI,FREQD,EFREQ,CNV,NST1,NST2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Solve the reduced equations
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 Y1(*),Y2(*),X1(*),X2(*),DG(*)
      REAL*8 CC(MAXDIM,MAXDIM),HH(MAXDIM,MAXDIM),SS(MAXDIM,MAXDIM)
      REAL*8 HH1(MAXDIM,MAXDIM),SS1(MAXDIM,MAXDIM)
      REAL*8 FREQR(MAXDIM),FREQI(MAXDIM),FREQD(MAXDIM),EFREQ,CNV
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "files.inc" 
      INCLUDE "param.inc" 
      INCLUDE "complex.inc" 
      INCLUDE "diis.inc"
C
C---------------Local variables--------------------------------------
C 
      REAL*8 SUM1,SUM2,SSUM,DSUM,SDOT,DDOT,RNORM,ANGLE,C1COS,C1SIN
      COMPLEX*16 CSUM1,ZSUM,CSUM,ZDOTC,CDOTC,CFAC
      LOGICAL COMPRESS
C
C---------------Executable code--------------------------------------
C
      IT = IT + 1
      MXDIM = MAXDIM/2 - 2
      IF (IT.LE.MXDIM) THEN
         COMPRESS = .FALSE.
      ELSE
         COMPRESS = .TRUE.
      ENDIF
C
      ITC = IT
C
C     Write out vectors and sigmavectors
C
      SECT1 = (ITC-1)*(NST1+NST2) + 1
      SECT2 = SECT1 + NST1
      CALL RWRIT(IOAMPL,X1,INTOWP(NDIMX*RCW),SECT1)
      CALL RWRIT(IOAMPS,X2,INTOWP(NDIMX*RCW),SECT1)
      CALL RWRIT(IOAMPL,Y1,INTOWP(NDIMX*RCW),SECT2)
      CALL RWRIT(IOAMPS,Y2,INTOWP(NDIMX*RCW),SECT2)
C
      DO 30 N = 1,IT
C
C     Make X+ H X Put in HH array.
C     We have paired eigenvectors so the dimension is twice the
C     number of stored eigenvectors
C
      SECT1 = (N-1)*(NST1+NST2) + 1
      SECT2 = SECT1 + NST1
      CALL RREAD(IOAMPS,X2,INTOWP(NDIMX*RCW),SECT1)
      CALL RREAD(IOAMPS,Y2,INTOWP(NDIMX*RCW),SECT2)
C
      IF (CARITH) THEN
         CALL QUIT('No complex eigensolver implemented')
      ELSE
#if defined (BIT64)
         SUM1 = SDOT (NDIMX,X1,1,X2,1)
         SUM1 = SUM1 + SDOT (NDIMX,Y1,1,Y2,1)
         SUM2 = SDOT (NDIMX,Y1,1,X2,1)
         SUM2 = SUM2 + SDOT (NDIMX,X1,1,Y2,1)
#else
         SUM1 = DDOT (NDIMX,X1,1,X2,1)
         SUM1 = SUM1 + DDOT (NDIMX,Y1,1,Y2,1)
         SUM2 = DDOT (NDIMX,Y1,1,X2,1)
         SUM2 = SUM2 + DDOT (NDIMX,X1,1,Y2,1)
#endif
      ENDIF
C
      HH(2*N-1,2*ITC-1) = SUM1
      HH(2*ITC-1,2*N-1) = SUM1
      HH(2*N-1,2*ITC) = SUM2
      HH(2*ITC,2*N-1) = SUM2
      HH(2*N,2*ITC-1) = SUM2
      HH(2*ITC-1,2*N) = SUM2
      HH(2*N,2*ITC) = SUM1
      HH(2*ITC,2*N) = SUM1
C
C     Make X+ S X Put in SS array.
C
      CALL RREAD(IOAMPL,X2,INTOWP(NDIMX*RCW),SECT1)
      CALL RREAD(IOAMPL,Y2,INTOWP(NDIMX*RCW),SECT2)
C
      IF (CARITH) THEN
         CALL QUIT('No complex eigensolver implemented')
      ELSE
#if defined (BIT64)
         SUM1 = SDOT (NDIMX,X1,1,X2,1)
         SUM1 = SUM1 - SDOT (NDIMX,Y1,1,Y2,1)
         SUM2 = SDOT (NDIMX,X1,1,Y2,1)
         SUM2 = SUM2 - SDOT (NDIMX,Y1,1,X2,1)
#else
         SUM1 = DDOT (NDIMX,X1,1,X2,1)
         SUM1 = SUM1 - DDOT (NDIMX,Y1,1,Y2,1)
         SUM2 = DDOT (NDIMX,X1,1,Y2,1)
         SUM2 = SUM2 - DDOT (NDIMX,Y1,1,X2,1)
#endif
      ENDIF
C
      SS(2*N-1,2*ITC-1) = SUM1
      SS(2*ITC-1,2*N-1) = SUM1
      SS(2*ITC-1,2*N) = SUM2
      SS(2*ITC,2*N-1) = - SUM2
      SS(2*N-1,2*ITC) = - SUM2
      SS(2*N,2*ITC-1) = SUM2
      SS(2*N,2*ITC) = - SUM1
      SS(2*ITC,2*N) = - SUM1
C
   30 CONTINUE
C
C  Call general EISPACK eigensolver
C
      IERR = 0
      DO I = 1, 2*IT
      DO J = 1, 2*IT
      HH1(I,J) = HH(I,J)
      SS1(I,J) = SS(I,J)
c     print*,i,j,hh(i,j),ss(i,j)
      ENDDO
      ENDDO
      CALL RGG (MAXDIM,2*IT,HH1,SS1,FREQR,FREQI,FREQD,1,CC,IERR)
      DO I = 1, 2*IT
         FREQR(I) = FREQR(I)/FREQD(I)
c        print*,'eigenvalue ',i,'  ',FREQR(I)
         IF (FREQI(I).GT.ACCUR) then
            PRINT*, 'Complex eigenvalue found'
            PRINT*,' value = ',FREQI(I)/FREQD(I)
         ENDIF
      ENDDO
      CALL SRTEV (MAXDIM,IT,SS,CC,SS1,FREQR,FREQI,FREQD)
      EFREQ = FREQR(1)
      IF (IERR.NE.0) THEN
         PRINT*,' Failed to solve reduced equations'
         CALL QUIT('ERROR CONDITION IN RGG')
      ENDIF
C
C  Form the solution vector
C
      CALL XCOPY(NDIMX,A0,0,X2,1)
      CALL XCOPY(NDIMX,A0,0,Y2,1)
C
c     do ix = 1, it*2
c     do n = 1, it
c     print*,'coefficient for positive ',n,ix,cc(2*n-1,ix)
c     enddo
c     do n = 1, it
c     print*,'coefficient for negative ',n,ix,cc(2*n,ix)
c     enddo
c     enddo
      DO N = 1,IT
C
      SECT1 = (N-1)*(NST1+NST2) + 1
      SECT2 = SECT1 + NST1
      CALL RREAD(IOAMPL,X1,INTOWP(NDIMX*RCW),SECT1)
      CALL RREAD(IOAMPL,Y1,INTOWP(NDIMX*RCW),SECT2)
C
#if defined (BIT64)
      CFAC = CMPLX(CC(2*N-1,1),AR0)
#else
      CFAC = DCMPLX(CC(2*N-1,1),AR0)
#endif
      CALL XAXPY (NDIMX,CFAC,X1,1,X2,1)
      CALL XAXPY (NDIMX,CFAC,Y1,1,Y2,1)
#if defined (BIT64)
      CFAC = CMPLX(CC(2*N,1),AR0)
#else
      CFAC = DCMPLX(CC(2*N,1),AR0)
#endif
      CALL XAXPY (NDIMX,CFAC,Y1,1,X2,1)
      CALL XAXPY (NDIMX,CFAC,X1,1,Y2,1)
C
      ENDDO
C
C  Store this vector temporarily and form sigma vector
C
      SECT1 = IT*(NST1+NST2) + 1
      SECT2 = SECT1 + NST1
      CALL RWRIT(IOAMPL,X2,INTOWP(NDIMX*RCW),SECT1)
      CALL RWRIT(IOAMPL,Y2,INTOWP(NDIMX*RCW),SECT2)
C
      CALL XCOPY(NDIMX,A0,0,X2,1)
      CALL XCOPY(NDIMX,A0,0,Y2,1)
C
      DO N = 1,IT
C
      SECT1 = (N-1)*(NST1+NST2) + 1
      SECT2 = SECT1 + NST1
      CALL RREAD(IOAMPS,X1,INTOWP(NDIMX*RCW),SECT1)
      CALL RREAD(IOAMPS,Y1,INTOWP(NDIMX*RCW),SECT2)
C
#if defined (BIT64)
      CFAC = CMPLX(CC(2*N-1,1),AR0)
#else
      CFAC = DCMPLX(CC(2*N-1,1),AR0)
#endif
      CALL XAXPY (NDIMX,CFAC,X1,1,X2,1)
      CALL XAXPY (NDIMX,CFAC,Y1,1,Y2,1)
#if defined (BIT64)
      CFAC = CMPLX(CC(2*N,1),AR0)
#else
      CFAC = DCMPLX(CC(2*N,1),AR0)
#endif
      CALL XAXPY (NDIMX,CFAC,Y1,1,X2,1)
      CALL XAXPY (NDIMX,CFAC,X1,1,Y2,1)
C
      ENDDO
C
C  Store the sigma vector
C
      SECT1 = IT*(NST1+NST2) + 1
      SECT2 = SECT1 + NST1
      CALL RWRIT(IOAMPS,X2,INTOWP(NDIMX*RCW),SECT1)
      CALL RWRIT(IOAMPS,Y2,INTOWP(NDIMX*RCW),SECT2)
C
C  Get the solution vector back
C
      SECT1 = IT*(NST1+NST2) + 1
      SECT2 = SECT1 + NST1
      CALL RREAD(IOAMPL,X1,INTOWP(NDIMX*RCW),SECT1)
      CALL RREAD(IOAMPL,Y1,INTOWP(NDIMX*RCW),SECT2)
C
C     Calculate residual
C                       
      CALL RESID (NDIMX,EFREQ,X1,X2,Y1,Y2,CNV)
      CALL PRECD (EFREQ,DG,X2,Y2)
C
C     Orthonormalize the new trial function
C     We need to do the Gram-Schmidt twice for accuracy
C
      DO IGS = 1, 2
C
      DO N = 1,IT
C
      CALL NORMX (NDIMX,A1,X2,Y2,RNORM)
C
C     Make X+ X 
C
      SECT1 = (N-1)*(NST1+NST2) + 1
      SECT2 = SECT1 + NST1
      CALL RREAD(IOAMPL,X1,INTOWP(NDIMX*RCW),SECT1)
      CALL RREAD(IOAMPL,Y1,INTOWP(NDIMX*RCW),SECT2)
C
      IF (CARITH) THEN
         CALL QUIT('No complex eigensolver implemented')
      ELSE
#if defined (BIT64)
         SUM1 = SDOT (NDIMX,X1,1,X2,1)
         SUM1 = SUM1 + SDOT (NDIMX,Y1,1,Y2,1)
         SUM2 = SDOT (NDIMX,Y1,1,X2,1)
         SUM2 = SUM2 + SDOT (NDIMX,X1,1,Y2,1)
#else
         SUM1 = DDOT (NDIMX,X1,1,X2,1)
         SUM1 = SUM1 + DDOT (NDIMX,Y1,1,Y2,1)
         SUM2 = DDOT (NDIMX,Y1,1,X2,1)
         SUM2 = SUM2 + DDOT (NDIMX,X1,1,Y2,1)
#endif
      ENDIF
C
      CALL XAXPY (NDIMX,-SUM1,X1,1,X2,1)
      CALL XAXPY (NDIMX,-SUM1,Y1,1,Y2,1)
      CALL XAXPY (NDIMX,-SUM2,Y1,1,X2,1)
      CALL XAXPY (NDIMX,-SUM2,X1,1,Y2,1)
C
      ENDDO
C
      ENDDO
C
C     Symmetric orthonormalization of the pair of new expansion vectors
C
      CALL NORMX (NDIMX,A1,X2,Y2,RNORM)
#if defined (BIT64)
      SUM2 = 2.0 *  SDOT (NDIMX,Y2,1,X2,1)
#else
      SUM2 = 2.D0 * DDOT (NDIMX,Y2,1,X2,1)
#endif
      ANGLE = DASIN(SUM2)/2
      C1COS = DCOS(ANGLE)
      C1SIN = - DSIN(ANGLE)
      CALL XCOPY (NDIMX,X2,1,X1,1)
      CALL XCOPY (NDIMX,Y2,1,Y1,1)
#if defined (BIT64)
      CALL SSCAL (RCW*NDIMX,C1COS,X2,1)
      CALL SSCAL (RCW*NDIMX,C1COS,Y2,1)
      CALL SAXPY (RCW*NDIMX,C1SIN,Y1,1,X2,1)
      CALL SAXPY (RCW*NDIMX,C1SIN,X1,1,Y2,1)
#else
      CALL DSCAL (RCW*NDIMX,C1COS,X2,1)
      CALL DSCAL (RCW*NDIMX,C1COS,Y2,1)
      CALL DAXPY (RCW*NDIMX,C1SIN,Y1,1,X2,1)
      CALL DAXPY (RCW*NDIMX,C1SIN,X1,1,Y2,1)
#endif
      CALL NORMX (NDIMX,A1,X2,Y2,RNORM)
C
      IF (COMPRESS) THEN
C
C  We exceeded the maximum space allocated, continue with current
C  solution vector and the new expansion vector as reduced space
C
C  Save the new expansion vector temporarily on record 2
C
      SECT1 = (NST1+NST2) + 1
      SECT2 = SECT1 + NST1
      CALL RWRIT(IOAMPL,X2,INTOWP(NDIMX*RCW),SECT1)
      CALL RWRIT(IOAMPL,Y2,INTOWP(NDIMX*RCW),SECT2)
C
C  The norm of the solution is in general not equal to 1
C
#if defined (BIT64)
      SUM1 = SDOT (2*IT,CC(1,1),1,CC(1,1),1)
      SUM2 = 2.0  * SDOT (IT,CC(1,1),2,CC(2,1),2)
#else
      SUM1 = DDOT (2*IT,CC(1,1),1,CC(1,1),1)
      SUM2 = 2.D0 * DDOT (IT,CC(1,1),2,CC(2,1),2)
#endif
      SUM2 = SUM2 / SUM1
C
C  Combine normalization and symmetric orthogonalization
C  The normalization constant is that of the first normalization
C  multiplied with the one from the symmetric orthonormalization
C
#if defined (BIT64)
      SUM1 =  SQRT(1.0 /SUM1) *  SQRT(1.0 / (1.D0-SUM2*SUM2))
#else
      SUM1 = SQRT(1.D0/SUM1) 
      SUM1 = SUM1 * SQRT(1.D0/ (1.D0-SUM2*SUM2))
#endif
      ANGLE = DASIN(SUM2)/2
      C1COS = DCOS(ANGLE)
      C1SIN = - DSIN(ANGLE)
      C1COS = C1COS * SUM1
      C1SIN = C1SIN * SUM1
      DO N = 1, IT
         SUM1 = CC(2*N-1,1)
         SUM2 = CC(2*N  ,1)
         CC(2*N-1,1) = C1COS*SUM1 + C1SIN*SUM2
         CC(2*N  ,1) = C1SIN*SUM1 + C1COS*SUM2
      ENDDO
C
C  Get the solution vector back and orthonormalize the pair
C
      SECT1 = IT*(NST1+NST2) + 1
      SECT2 = SECT1 + NST1
      CALL RREAD(IOAMPL,X2,INTOWP(NDIMX*RCW),SECT1)
      CALL RREAD(IOAMPL,Y2,INTOWP(NDIMX*RCW),SECT2)
C
      CALL XCOPY (NDIMX,X2,1,X1,1)
      CALL XCOPY (NDIMX,Y2,1,Y1,1)
#if defined (BIT64)
      CALL SSCAL (RCW*NDIMX,C1COS,X2,1)
      CALL SSCAL (RCW*NDIMX,C1COS,Y2,1)
      CALL SAXPY (RCW*NDIMX,C1SIN,Y1,1,X2,1)
      CALL SAXPY (RCW*NDIMX,C1SIN,X1,1,Y2,1)
#else
      CALL DSCAL (RCW*NDIMX,C1COS,X2,1)
      CALL DSCAL (RCW*NDIMX,C1COS,Y2,1)
      CALL DAXPY (RCW*NDIMX,C1SIN,Y1,1,X2,1)
      CALL DAXPY (RCW*NDIMX,C1SIN,X1,1,Y2,1)
#endif
C
C  Write the orthonormalized solution vector to the first record
C
      SECT1 = 1
      SECT2 = SECT1 + NST1
      CALL RWRIT(IOAMPL,X2,INTOWP(NDIMX*RCW),SECT1)
      CALL RWRIT(IOAMPL,Y2,INTOWP(NDIMX*RCW),SECT2)
C
C  Get the new sigma vector and write it to the first record
C
      SECT1 = IT*(NST1+NST2) + 1
      SECT2 = SECT1 + NST1
      CALL RREAD(IOAMPS,X2,INTOWP(NDIMX*RCW),SECT1)
      CALL RREAD(IOAMPS,Y2,INTOWP(NDIMX*RCW),SECT2)
C
      CALL XCOPY (NDIMX,X2,1,X1,1)
      CALL XCOPY (NDIMX,Y2,1,Y1,1)
#if defined (BIT64)
      CALL SSCAL (RCW*NDIMX,C1COS,X2,1)
      CALL SSCAL (RCW*NDIMX,C1COS,Y2,1)
      CALL SAXPY (RCW*NDIMX,C1SIN,Y1,1,X2,1)
      CALL SAXPY (RCW*NDIMX,C1SIN,X1,1,Y2,1)
#else
      CALL DSCAL (RCW*NDIMX,C1COS,X2,1)
      CALL DSCAL (RCW*NDIMX,C1COS,Y2,1)
      CALL DAXPY (RCW*NDIMX,C1SIN,Y1,1,X2,1)
      CALL DAXPY (RCW*NDIMX,C1SIN,X1,1,Y2,1)
#endif
C
      SECT1 = 1
      SECT2 = SECT1 + NST1
      CALL RWRIT(IOAMPS,X2,INTOWP(NDIMX*RCW),SECT1)
      CALL RWRIT(IOAMPS,Y2,INTOWP(NDIMX*RCW),SECT2)
C
C  Correct SS and HH matrices and set IT back to 1
C
      SUM1 = D0
      SUM2 = D0
      DO N2 = 1, 2*IT
         DO N1 = 1, 2*IT
            IF (MOD(N1,2).NE.0) THEN
               N1N = N1 + 1
            ELSE
               N1N = N1 - 1
            ENDIF
            SUM1 = SUM1 + CC(N1 ,1) * HH(N1,N2) * CC(N2,1)
            SUM2 = SUM2 + CC(N1N,1) * HH(N1,N2) * CC(N2,1)
         ENDDO
      ENDDO
      HH(1,1) = SUM1
      HH(1,2) = SUM2
      HH(2,1) = SUM2
      HH(2,2) = SUM1
C
C  The off-diagonal metric matrix is always zero for one pair.
C
      SUM1 = D0
      DO N2 = 1, 2*IT
         DO N1 = 1, 2*IT
            SUM1 = SUM1 + CC(N1,1) * SS(N1,N2) * CC(N2,1)
         ENDDO
      ENDDO
      SS(1,1) = SUM1
      SS(1,2) = D0
      SS(2,1) = D0
      SS(2,2) = - SUM1
C
      IT = 1
C
C  Read the new expansion vector back to X2 and Y2
C
      SECT1 = (NST1+NST2) + 1
      SECT2 = SECT1 + NST1
      CALL RREAD(IOAMPL,X2,INTOWP(NDIMX*RCW),SECT1)
      CALL RREAD(IOAMPL,Y2,INTOWP(NDIMX*RCW),SECT2)
C
      ENDIF
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE RESID (NDIMX,EFREQ,X1,X2,Y1,Y2,CNV)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates residue
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 EFREQ,CNV,CEFREQ(2)
      REAL*8 X1(*),Y1(*),X2(*),Y2(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "complex.inc"
C
C---------------Local variables--------------------------------------
C
      COMPLEX*16 CDOTC,ZDOTC
      REAL*8 DDOT,SDOT
C
C---------------Executable code--------------------------------------
C
      CEFREQ(2) = D0
C
      CEFREQ(1) = - EFREQ
      CALL XAXPY (NDIMX,CEFREQ,X1,1,X2,1)
C
      CEFREQ(1) = EFREQ
      CALL XAXPY (NDIMX,CEFREQ,Y1,1,Y2,1)
C
      IF (CARITH) THEN
#if defined (BIT64)
         CNV = CDOTC (NDIMX,X2,1,X2,1) + CDOTC (NDIMX,Y2,1,Y2,1)
#else
         CNV = ZDOTC (NDIMX,X2,1,X2,1) + ZDOTC (NDIMX,Y2,1,Y2,1)
#endif
      ELSE
#if defined (BIT64)
         CNV = SDOT (NDIMX,X2,1,X2,1) + SDOT (NDIMX,Y2,1,Y2,1)
#else
         CNV = DDOT (NDIMX,X2,1,X2,1) + DDOT (NDIMX,Y2,1,Y2,1)
#endif
      ENDIF
C     
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE RPAVAL (X1,Y1,AVO,AOV,APHASE,BPHASE,CERPA)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates RPA propagator. Checks result (imaginary / real).
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
      REAL*8 X1(*),Y1(*)
      REAL*8 AVO(*),AOV(*)
      COMPLEX*16 CERPA,APHASE,BPHASE
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "symm.inc"
      INCLUDE "complex.inc"
C
C---------------Local variables--------------------------------------
C
      COMPLEX*16 CERPA1,CERPA2
      COMPLEX*16 CDOTU,ZDOTU
      REAL*8 DDOT,ZDOT
C
C---------------Executable code--------------------------------------
C
      IF (CARITH) THEN
#if defined (BIT64)
         CERPA1 = CDOTU (NDIMX,AOV,1,X1,1)
#else
         CERPA1 = ZDOTU (NDIMX,AOV,1,X1,1)
#endif
      ELSE
#if defined (BIT64)
         ERPA = SDOT (NDIMX,AOV,1,X1,1)
#else
         ERPA = DDOT (NDIMX,AOV,1,X1,1)
#endif
      ENDIF
C
      IF (CARITH) THEN
#if defined (BIT64)
         CERPA2 = CDOTU (NDIMX,AVO,1,Y1,1)
#else
         CERPA2 = ZDOTU (NDIMX,AVO,1,Y1,1)
#endif
         CERPA = APHASE * BPHASE * (CERPA1 + CERPA2)
      ELSE
#if defined (BIT64)
         ERPA = ERPA + SDOT (NDIMX,AVO,1,Y1,1)
#else
         ERPA = ERPA + DDOT (NDIMX,AVO,1,Y1,1)
#endif
         CERPA = APHASE * BPHASE * ERPA
      ENDIF
C
      RETURN
1000  FORMAT (/' WARNING : Imaginary contribution to ',A2,
     &' part of RPA  propagator :',F20.15)
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE SRTEV(NM,N,SS,CC,CCS,ER,ERS,IND)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Sort eigenvalues according to norm (+/-1) 
C     Order them also by increasing eigenvalue
C     Variables : NM matrix dimension
C                 N  actual dimension (number of pairs)
C                 SS on input metric matrix
C                 CC on input coefficient matrix
C                    on output reordered normalized coefficients
C                 ER on input eigenvalues
C                    on output reordered eigenvalues
C                 ERS,CCS,IND scratch arrays
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      INTEGER N,NM,IND(NM)
      REAL*8 CC(NM,*),SS(NM,*),ER(NM),CCS(NM,*),ERS(NM)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc" 
      INCLUDE "complex.inc" 
C
C---------------Local variables--------------------------------------
C 
      REAL*8 RNORM,SDOT,DDOT
      COMPLEX*16 ZDOTC,CDOTC
C
C---------------Executable code--------------------------------------
C
C
C     Determine correct ordering
C
      NS = 0
      DO I = 1, 2*N
         CALL DGEMV ('N',2*N,2*N,A1,SS,NM,CC(1,I),1,A0,ERS,1)
#if defined (BIT64)
         RNORM = SDOT (2*N,CC(1,I),1,ERS,1)
#else
         RNORM = DDOT (2*N,CC(1,I),1,ERS,1)
#endif
         IF (RNORM.GT.AR0) THEN
#if defined (BIT64)
            RNORM = 1.0 / SQRT(RNORM)
#else
            RNORM = 1.D0 / SQRT(RNORM)
#endif
            DO J = 1, NS
               IF (ER(I).LT.ER(IND(J))) THEN
                  DO K = NS+1, J+1, -1
                     IND(K) = IND(K-1)
                  ENDDO
                  IND(J) = I
                  GOTO 1
               ENDIF
            ENDDO
            IND(NS+1) = I
  1         CONTINUE
            NS = NS + 1
         ENDIF
      ENDDO
      NSP = NS
      NS = 0
      DO I = 1, 2*N
         CALL DGEMV ('N',2*N,2*N,A1,SS,NM,CC(1,I),1,A0,ERS,1)
#if defined (BIT64)
         RNORM = SDOT (2*N,CC(1,I),1,ERS,1)
#else
         RNORM = DDOT (2*N,CC(1,I),1,ERS,1)
#endif
         IF (RNORM.LT.AR0) THEN
#if defined (BIT64)
            RNORM = 1.0 / SQRT(-RNORM)
#else
            RNORM = 1.D0 / SQRT(-RNORM)
#endif
            DO J = 1, NS
               IF (ER(I).GT.ER(IND(NSP+J))) THEN
                  DO K = NS+1, J+1, -1
                     IND(NSP+K) = IND(NSP+K-1)
                  ENDDO
                  IND(NSP+J) = I
                  GOTO 2
               ENDIF
            ENDDO
            IND(NSP+NS+1) = I
  2         CONTINUE
            NS = NS + 1
         ENDIF
      ENDDO
C
C     Reorder.
C
      DO I = 1, 2*N
c        print*,'ind(',i,')',ind(i)
         CALL XCOPY (NM,CC(1,IND(I)),1,CCS(1,I),1)
         ERS(I) = ER(IND(I))
      ENDDO
      CALL XCOPY (2*NM*N,CCS,1,CC,1)
      CALL XCOPY (2*N,ERS,1,ER,1)
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE GETDG(EPS,AA,BUF1,DG)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates diagonal of A, needed as preconditioner
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 AA(*),EPS(*),DG(*)
      REAL*8 BUF1(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "complex.inc"
      INCLUDE "symm.inc"
C
C---------------Local variables--------------------------------------
C
      REAL*8 FAC,FAC1
C
C---------------Executable code--------------------------------------
C
C---------------------------------------------------------------------
C AA(AK,IC) = - W(AK,CI)
C Put in minus sign when adding to orbital energies
C---------------------------------------------------------------------
      CALL GETVOVO (BUF1)
C------------------------------------------------------------------
C Now sort AA(AK,CI) to AA(AI,CK)
C------------------------------------------------------------------
      CALL SRT16 (NREP,MULTB,LFA,LFA,NV,NO,NV,NO,MVO,JVOVO,JJVO,JJVO,
     &            BUF1,AA)
C------------------------------------------------------------------
C DG(AI) = X2(AI) + AA(AI,AI)
C------------------------------------------------------------------
      OFF1 = 1
      OFF3 = 1
      DO IREP = 1, NREP
         M = MVO(IREP)
         OFF2 = JVOXX(IREP) * RCW + 1
#if defined (BIT64)
         CALL SCOPY (M,AA(OFF1),RCW*(M+1),DG(OFF3),1)
#else
         CALL DCOPY (M,AA(OFF1),RCW*(M+1),DG(OFF3),1)
#endif
         OFF1 = OFF1 + M * M * RCW
         OFF3 = OFF3 + M
      ENDDO
C
      AI = 0
      DO IREP = NREP+1, 2*NREP
         II = 0
         DO IRP = 1, NREP
            JRP = MULTB(IRP,IREP,2)
            JRP = MULTB (JRP,NREP+1,2)
            DO I = 1, NO(IRP)
               II = II + 1
               FAC1 = EPS(II)
               AOFF = IO(NREP+1) + IV(JRP)
               DO A = 1, NV(JRP)
                  FAC = EPS(AOFF+A) - FAC1
                  AI = AI + 1
                  DG(AI) = FAC - DG(AI)
               ENDDO
            ENDDO
         ENDDO
      ENDDO
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE PRRPAB (TEXT,X1,Y1,TRESH)
C
      REAL*8 X1(*),Y1(*),THRESH
      CHARACTER*17 TEXT
      INTEGER OFF
C 
      INCLUDE "files.inc"
      INCLUDE "symm.inc"
      INCLUDE "ccpar.inc"
C
      if (myproc.eq.master) then 
C
      WRITE (*,1000) TEXT,TRESH
C
      OFF = 1
      DO IREP = 1, NREP
         M = MVO(IREP)
         DO I = OFF, OFF+M-1
            IF (ABS(X1(I)).GT.TRESH.OR.ABS(Y1(I)).GT.TRESH) THEN
               WRITE (IW,1001) IREP,REPNA(IREP+NREP),I-OFF+1,X1(I),Y1(I)
            ENDIF
         ENDDO
         OFF = OFF + M
      ENDDO
C
      endif
C
 1000 FORMAT (/' Analysis of ',A17,' vector'/
     &' Printing contributions above ',E6.1//
     &' Symmetry  Excitation           X Value              Y value')
 1001 FORMAT (I4,A4,I10,4X,2F20.10)
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
