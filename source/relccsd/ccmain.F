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
C VERSION : $Revision: 1.11 $
C DATE    : $Date: 2001/09/20 20:21:08 $
C FILE    : ccmain.F
C SOURCE  : $Source: /home/tc/visscher/sources/Molfdir/Molfdir/source/relccsd/ccmain.F,v $
C
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      PROGRAM RELCCSD
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description-----------------------------------------------
C
C    Double group symmetry adapted unrestricted coupled cluster code
C    CRAY version uses dynamic memory allocation and variable file buffers
C
C---------------Routines called-------------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
C---------------Common Blocks--------------------------------------
C
      PARAMETER (MAXCOR = 16 000 000)
      INCLUDE "files.inc"
      INCLUDE "ccpar.inc"
#if defined (VAR_MPI)
      INCLUDE 'mpif.h'
      DIMENSION ISTAT(MPI_STATUS_SIZE)
#endif
C
C---------------Local variables--------------------------------------
C
      REAL*8 SEC,SEC0
      LOGICAL CA
      CHARACTER*10 INTFCE
#if defined (CRAY) || defined (IBM) || defined (SGI) || defined (HP9) || defined (SUN)
C No static allocation of work memory
#else
      REAL*8 CC(MAXCOR)
#endif
C
C---------------Executable code--------------------------------------
C
      CALL CPUUSED(SEC0)
      IN = 5
      IW = 6
      INTFCE = 'MOLFDIR '
      CA = .FALSE.
#if defined (VAR_MPI)
C
C     MPI initialization
C
      CALL MPI_INIT(IERR)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD,MYPROC,IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NMPROC,IERR)
      MASTER = 0
#else
      MASTER = 0
      MYPROC = 0
      NMPROC = 1
#endif
C
#if defined (CRAY) || defined (IBM) || defined (SGI) || defined (HP9) || defined (SUN)
      CALL CCMAIN(MAXCOR,IN,IW,CA,INTFCE)
#else
      CALL CCMAIN(MAXCOR,CC,IN,IW,CA,INTFCE)
#endif
C
      CALL CPUUSED(SEC)
      if (myproc.eq.master) then
      WRITE (IW,1000) SEC-SEC0,SEC0,SEC
      WRITE (IW,'(//80A1)') ('#',I=1,80)
      endif
#if defined (VAR_MPI)
C
C     MPI finalization
C
      CALL MPI_FINALIZE(IERR)
#endif
C
 1000 FORMAT (//' CPU time (seconds) used in RELCCSD:',T50,F14.4/
     & ' CPU time (seconds) used before RELCCSD:',T50,F14.4/
     & ' CPU time (seconds) used in total sofar:',T50,F14.4//
     & '  --- Normal end of RELCCSD Run ---')
C
      END
