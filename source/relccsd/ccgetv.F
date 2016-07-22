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
C VERSION : $Revision: 1.1 $
C DATE    : $Date: 2001/09/11 12:57:32 $
C FILE    : ccgetv.F
C SOURCE  : $Source: /home/tc/visscher/sources/Molfdir/Molfdir/source/relccsd/ccgetv.F,v $
C
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE FMTOFILE (PUT,FVO,FOO,FVV)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Read/write of the Fock matrix: FOO, FVO and FVV parts
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 FVO(*),FOO(*),FVV(*)
      LOGICAL PUT
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "files.inc"
      INCLUDE "complex.inc"
      INCLUDE "symm.inc"
      INCLUDE "eqns.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
C     Calculate start of sectors
C
      NSEC0 = 1
      NSEC = NFVO*IRW*RCW / SEC2I(1)
      IF (MOD(NFVO*IRW*RCW,SEC2I(1)).NE.0) NSEC = NSEC + 1
      NSEC1 = NSEC0 + NSEC
      NSEC = NFOO*IRW*RCW / SEC2I(1)
      IF (MOD(NFOO*IRW*RCW,SEC2I(1)).NE.0) NSEC = NSEC + 1
      NSEC2 = NSEC1 + NSEC
C
C     Read/write blocks
C
      IF (PUT) THEN
         CALL RWRIT(ITAPT+0,FVO,NFVO*RCW*IRW,NSEC0)
         CALL RWRIT(ITAPT+0,FOO,NFOO*RCW*IRW,NSEC1)
         CALL RWRIT(ITAPT+0,FVV,NFVV*RCW*IRW,NSEC2)
      ELSE
         CALL RREAD(ITAPT+0,FVO,NFVO*RCW*IRW,NSEC0)
         CALL RREAD(ITAPT+0,FOO,NFOO*RCW*IRW,NSEC1)
         CALL RREAD(ITAPT+0,FVV,NFVV*RCW*IRW,NSEC2)
      ENDIF
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE GETOOOO (VOOOO)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Get OOOO part of two-electron integrals
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 VOOOO(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "files.inc"
      INCLUDE "complex.inc"
      INCLUDE "symm.inc"
      INCLUDE "eqns.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      CALL RREAD(ITAPT+1,VOOOO,NV1*IRW*RCW,1)
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE GETVOOO (VVOOO)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Get VOOO part of two-electron integrals
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 VVOOO(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "files.inc"
      INCLUDE "complex.inc"
      INCLUDE "symm.inc"
      INCLUDE "eqns.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      CALL RREAD(ITAPT+2,VVOOO,NV2*IRW*RCW,1)
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE GETVVOO (VVVOO)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Get VVOO part of two-electron integrals
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 VVVOO(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "files.inc"
      INCLUDE "complex.inc"
      INCLUDE "symm.inc"
      INCLUDE "eqns.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      CALL RREAD(ITAPT+3,VVVOO,NV3*IRW*RCW,1)
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE GETVOVO (VVOVO)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Get VOVO part of two-electron integrals
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 VVOVO(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "files.inc"
      INCLUDE "complex.inc"
      INCLUDE "symm.inc"
      INCLUDE "eqns.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      CALL RREAD(ITAPT+4,VVOVO,NV4*IRW*RCW,1)
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE GETVOVV (IREP,ISTART,NINT,DONE,VVOVV,MINT)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Get VOVV part of two-electron integrals in the
C     Representation                        IREP
C     with start address of integrals       ISTART (updated)
C     Number of integrals in batch          NINT (updated)
C     if there are read all in a batch      DONE=.TRUE.
C     Buffer:                               VVOVV(*)
C     Maximum number of VO batches that can
C     be buffered:                          MINT
C     
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 VVOVV(*)
      INTEGER IREP,ISTART,NINT
      LOGICAL DONE
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "files.inc"
      INCLUDE "complex.inc"
      INCLUDE "symm.inc"
      INCLUDE "iobuf.inc"
      INCLUDE "ccpar.inc"
C
C---------------Local variables--------------------------------------
C
       PARAMETER (NIOBUF=4096)
       INTEGER IOBUFFER(NIOBUF)
       DATA IOBUFFER /NIOBUF*0/
C
C---------------Executable code--------------------------------------
C
      IF (NIOBUF.LT.MINBUF*RCW*IRW) THEN
         PRINT*,"CHANGE NIOBUF IN GETVOVV TO MINIMALLY",MINBUF*RCW*IRW
         CALL QUIT("UPDATE GETVOVV")
      ENDIF
C
      DONE = .FALSE.
      MTOT = NVO(IREP)     ! one (VV)_ij batch has length NVO
      ISTART = MAX0(ISTART,IDIST(1,2,IREP))
      NTOT = IDIST(2,2,IREP)     ! local # of VO batches in this IRREP
      NINT = MIN0(MINT,NTOT-ISTART)  ! # of batches which will actually be read
      JSTART = IDIST(5,2,IREP)      ! compute relative local offset
      JSTART = JSTART + (ISTART-IDIST(1,2,IREP)) * MTOT

C     Read partial batch from first record
C     JINT : Total number of integrals to be read
C     JINT1: Number of integrals of previous batch on first record
C     JINT2: Number of integrals of current batch on first record
C     JINT3: Total number of integrals on first record
C
      JINT = MTOT * NINT
      JSEC = (JSTART / MINBUF) * MINSEC + 1
      JINT1 = MOD (JSTART,MINBUF)
      JINT2 = 0
      IF (JINT1.NE.0) THEN
         JINT2 = MIN0(MINBUF-JINT1,JINT)
         JINT3 = JINT1 + JINT2
         CALL RREAD(ITAPT+5,IOBUFFER,JINT3*RCW*IRW,JSEC)
         JINT1 = JINT1*RCW*IRW + 1
         CALL XCOPY (JINT2,IOBUFFER(JINT1),1,VVOVV,1)
         JSEC = JSEC + MINSEC
      ENDIF
C
C     Read remaining integrals
C
      JINT = JINT - JINT2
      JINT2 = JINT2*RCW + 1
      IF (JINT.GT.0) CALL RREAD(ITAPT+5,VVOVV(JINT2),JINT*IRW*RCW,JSEC)
C
C     Are we done with this IRREP ?
C
      IF (ISTART+NINT.EQ.NTOT) DONE = .TRUE.
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE GETVVVV (IREP,ISTART,NINT,DONE,VVVVV,MINT)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Get VVVV part of two-electron integrals
C     Representation 			IREP
C     Start address of integrals 	ISTART
C     Maximum number of integrals 	MINT
C     Actual number of integrals  	NINT (updated on output)
C     Done with this representation ?	DONE
C     
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 VVVVV(*)
      INTEGER IREP,ISTART,NINT
      LOGICAL DONE
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "files.inc"
      INCLUDE "complex.inc"
      INCLUDE "symm.inc"
      INCLUDE "iobuf.inc"
      INCLUDE "ccpar.inc"
C
C---------------Local variables--------------------------------------
C
       PARAMETER (NIOBUF=4096)
       INTEGER IOBUFFER(NIOBUF)
       DATA IOBUFFER /NIOBUF*0/
C
C---------------Executable code--------------------------------------
C
      IF (NIOBUF.LT.MINBUF*RCW*IRW) THEN
         PRINT*,"CHANGE NIOBUF IN GETVVVV TO MINIMALLY",MINBUF*RCW*IRW
         CALL QUIT("UPDATE GETVVVV")
      ENDIF
C
C     Calculate start address. The batches are distributed over the
C     nodes for parallel runs. Take only the part that is on our node
C
      DONE = .FALSE.
      MTOT = NVVT(IREP)
      ISTART = MAX0(ISTART,IDIST(1,1,IREP))
      NTOT = IDIST(2,1,IREP)
      NINT = MIN0(MINT,NTOT-ISTART)
      JSTART = IDIST(5,1,IREP)
      JSTART = JSTART + (ISTART-IDIST(1,1,IREP)) * MTOT
C
C     Read partial batch from first record
C     JINT : Total number of integrals to be read
C     JINT1: Number of integrals of previous batch on first record
C     JINT2: Number of integrals of current batch on first record
C     JINT3: Total number of integrals on first record
C
      JINT = MTOT * NINT
      JSEC = (JSTART / MINBUF) * MINSEC + 1
      JINT1 = MOD (JSTART,MINBUF)
      JINT2 = 0
      IF (JINT1.NE.0) THEN
         JINT2 = MIN0(MINBUF-JINT1,JINT)
         JINT3 = JINT1 + JINT2
         CALL RREAD(ITAPT+6,IOBUFFER,JINT3*RCW*IRW,JSEC)
         JINT1 = JINT1*RCW*IRW + 1
         CALL XCOPY (JINT2,IOBUFFER(JINT1),1,VVVVV,1)
         JSEC = JSEC + MINSEC
      ENDIF
C
C     Read remaining integrals
C
      JINT = JINT - JINT2
      JINT2 = JINT2*RCW + 1
      IF (JINT.GT.0) CALL RREAD(ITAPT+6,VVVVV(JINT2),JINT*IRW*RCW,JSEC)
C
C     Are we done with this IRREP ?
C
      IF (ISTART+NINT.EQ.NTOT) DONE = .TRUE.
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE PUTDZ (IREP,ISTART,NINT,DONE,DZ)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Puts D intermediate to file
C     Representation 			IREP
C     Start address of integrals 	ISTART 
C     Number of integrals in batch 	NINT
C     
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 DZ(*)
      INTEGER IREP,ISTART,NINT
      LOGICAL DONE
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "files.inc"
      INCLUDE "complex.inc"
      INCLUDE "symm.inc"
      INCLUDE "eqns.inc"
      INCLUDE "iobuf.inc"
C
C---------------Local variables--------------------------------------
C
       PARAMETER (NIOBUF=2048)
       INTEGER IOBUFFER(NIOBUF)
       DATA IOBUFFER /NIOBUF*0/
C
C---------------Executable code--------------------------------------
C
      IF (NIOBUF.LT.MINBUF*RCW*IRW) THEN
         PRINT*,"CHANGE NIOBUF IN PUTDZ TO MINIMALLY",MINBUF*RCW*IRW
         CALL QUIT("UPDATE PUTDZ")
      ENDIF
C
      DONE = .FALSE.
      MTOT = NVO(IREP)
      NTOT = NVVT(IREP)
      NINT = MIN0(NINT,NTOT-ISTART)
C
C     Calculate absolute starting address
C
      JSTART = IVOVVT(IREP) + ISTART * MTOT
      JINT = MTOT * NINT
      JSEC = (JSTART / MINBUF) * MINSEC + 1
C
C     Read partial batch from first record
C     JINT : Total number of integrals to be read
C     JINT1: Number of integrals of previous batch on first record
C     JINT2: Number of integrals of current batch on first record
C     JINT3: Total number of integrals on first record
C
      JINT1 = MOD (JSTART,MINBUF)
      JINT2 = 0
      IF (JINT1.NE.0) THEN
         JINT2 = MIN0(MINBUF-JINT1,JINT)
         JINT3 = JINT1 + JINT2
         CALL RREAD(ITAPL,IOBUFFER,JINT3*RCW*IRW,JSEC)
         JINT1 = JINT1*RCW*IRW + 1
         CALL XCOPY (JINT2,IOBUFFER(JINT1),1,DZ,1)
         JSEC = JSEC + MINSEC
      ENDIF
C
C     Read remaining integrals
C
      JINT = JINT - JINT2
      JINT2 = JINT2*RCW + 1
      IF (JINT.GT.0) CALL RREAD(ITAPL,DZ(JINT2),JINT*IRW*RCW,JSEC)
C
C     Are we done with this IRREP ?
C
      IF (ISTART+NINT.EQ.NTOT) DONE = .TRUE.
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE GETDZ (IREP,ISTART,NINT,DONE,DZ)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Gets D intermediate from file
C     Representation 			IREP
C     Start address of integrals 	ISTART 
C     Number of integrals in batch 	NINT
C     
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 DZ(*)
      INTEGER IREP,ISTART,NINT
      LOGICAL DONE
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "files.inc"
      INCLUDE "complex.inc"
      INCLUDE "symm.inc"
      INCLUDE "eqns.inc"
      INCLUDE "iobuf.inc"
C
C---------------Local variables--------------------------------------
C
       PARAMETER (NIOBUF=2048)
       INTEGER IOBUFFER(NIOBUF)
       DATA IOBUFFER /NIOBUF*0/
C
C---------------Executable code--------------------------------------
C
      IF (NIOBUF.LT.MINBUF*RCW*IRW) THEN
         PRINT*,"CHANGE NIOBUF IN GETDZ TO MINIMALLY",MINBUF*RCW*IRW
         CALL QUIT("UPDATE GETDZ")
      ENDIF
C
      DONE = .FALSE.
      MTOT = NVO(IREP)
      NTOT = NVVT(IREP)
      NINT = MIN0(NINT,NTOT-ISTART)
C
C     Calculate absolute starting address
C
      JSTART = IVOVVT(IREP) + ISTART * MTOT
      JINT = MTOT * NINT
      JSEC = (JSTART / MINBUF) * MINSEC + 1
C
C     Read partial batch from first record and extend with new integrals
C     JINT : Total number of integrals to be written
C     JINT1: Number of integrals of previous batch on first record
C     JINT2: Number of integrals of current batch on first record
C     JINT3: Total number of integrals on first record
C
      JINT1 = MOD (JSTART,MINBUF)
      JINT2 = 0
      IF (JINT1.NE.0) THEN
         JINT2 = MIN0(MINBUF-JINT1,JINT)
         JINT3 = JINT1 + JINT2
         CALL RREAD(ITAPL,IOBUFFER,JINT1*RCW*IRW,JSEC)
         JINT1 = JINT1*RCW*IRW + 1
         CALL XCOPY (JINT2,DZ,1,IOBUFFER(JINT1),1)
         CALL RWRIT(ITAPL,IOBUFFER,JINT3*RCW*IRW,JSEC)
         JSEC = JSEC + MINSEC
      ENDIF
C
C     Write remaining integrals
C
      JINT = JINT - JINT2
      JINT2 = JINT2*RCW + 1
      IF (JINT.GT.0) CALL RWRIT(ITAPL,DZ(JINT2),JINT*IRW*RCW,JSEC)
C
C     Are we done with this IRREP ?
C
      IF (ISTART+NINT.EQ.NTOT) DONE = .TRUE.
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE PUTAMPT (T1,T2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Write amplitudes to restart file
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
      INCLUDE "files.inc"
      INCLUDE "complex.inc"
      INCLUDE "symm.inc"
C
C---------------Local variables--------------------------------------
C
      REAL*8 BUFF(RSRCLEN)
      DATA BUFF /RSRCLEN*0.D0/
C
C---------------Executable code--------------------------------------
C
      IRECL = IRECLEN (RSRCLEN,0,0)
      OPEN (MCCRES,FILE=RSFILN(1:LRSFIL),ACCESS='DIRECT',STATUS='OLD',
     &      RECL=IRECL)
C
      NRECT1 = NDIMT1*RCW / RSRCLEN + 1
      IRECT2 = IRECT1 + NRECT1 ! T2 amplitudes
      NRECT2 = NDIMT2*RCW / RSRCLEN + 1
C
      IREC = IRECT1
      N = NDIMT1
      IOFF = 1
    1 NBUF = MIN0(N,(RSRCLEN/RCW))
      CALL XCOPY (NBUF,T1(IOFF),1,BUFF,1)
      WRITE (MCCRES,REC=IREC) BUFF
      IREC = IREC + 1
      N = N - NBUF
      IOFF = IOFF + NBUF * RCW
      IF (N.GT.0) GOTO 1
C
      IREC = IRECT2
      N = NDIMT2
      IOFF = 1
    2 NBUF = MIN0(N,(RSRCLEN/RCW))
      CALL XCOPY (NBUF,T2(IOFF),1,BUFF,1)
      WRITE (MCCRES,REC=IREC) BUFF
      IREC = IREC + 1
      N = N - NBUF
      IOFF = IOFF + NBUF * RCW
      IF (N.GT.0) GOTO 2
C
      CLOSE (MCCRES,STATUS='KEEP')
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE GETAMPT (T1,T2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Read amplitudes from restart file
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
      INCLUDE "files.inc"
      INCLUDE "complex.inc"
      INCLUDE "symm.inc"
C
C---------------Local variables--------------------------------------
C
      REAL*8 BUFF(RSRCLEN)
      DATA BUFF /RSRCLEN*0.D0/
C
C---------------Executable code--------------------------------------
C
      IRECL = IRECLEN (RSRCLEN,0,0)
      OPEN (MCCRES,FILE=RSFILN(1:LRSFIL),ACCESS='DIRECT',STATUS='OLD',
     &      RECL=IRECL)
C
      NRECT1 = NDIMT1*RCW / RSRCLEN + 1
      IRECT2 = IRECT1 + NRECT1 ! T2 amplitudes
      NRECT2 = NDIMT2*RCW / RSRCLEN + 1
C
      IREC = IRECT1
      N = NDIMT1
      IOFF = 1
    1 NBUF = MIN0(N,(RSRCLEN/RCW))
      READ (MCCRES,REC=IREC) BUFF
      CALL XCOPY (NBUF,BUFF,1,T1(IOFF),1)
      IREC = IREC + 1
      N = N - NBUF
      IOFF = IOFF + NBUF * RCW
      IF (N.GT.0) GOTO 1
C
      IREC = IRECT2
      N = NDIMT2
      IOFF = 1
    2 NBUF = MIN0(N,(RSRCLEN/RCW))
      READ (MCCRES,REC=IREC) BUFF
      CALL XCOPY (NBUF,BUFF,1,T2(IOFF),1)
      IREC = IREC + 1
      N = N - NBUF
      IOFF = IOFF + NBUF * RCW
      IF (N.GT.0) GOTO 2
C
      CLOSE (MCCRES,STATUS='KEEP')
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

