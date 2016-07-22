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
C FILE    : ccsort.F
C SOURCE  : $Source: /home/tc/visscher/sources/Molfdir/Molfdir/source/relccsd/ccsort.F,v $
C
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C
C This module contains the integral sorters.
C 4 Types of symmetry-packed integral orderings can be defined
C All integrals are assumed antisymmetrized and in Dirac notation.
C
C Order 1 (default and on file) ..... < IJ | KL> as (IJ,KL,KLREP)
C Order 2 ........................... < IJ | KL> as (I,JKL,JKLREP)
C Order 3 ........................... < IJ | KL> as (IJK,L,LREP)
C Order 4 ........................... < IJ | KL> as (IK,JL,JLREP)
C
C When I and J belong to the same class (occupied or virtual), 
C triangular packing (I>J) can be used in IJ or IJK.
C
C 9-12-97 : Introduced systematic names for most of the routines,
C           some special sorters remain.
C 1-3-01 : Note that MXREP is still hardwired, better to have a small
C          common block that just contains NREP and MULTB ?
C
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE SRT1TT4 (NREP,MULTB,FIRST,DOINV,
     &                    IFIE,JFIE,KFIE,LFIE,NPAIR,
     &                    OFF,OFF1,OFF2,BUF1,BUF2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     BUF(I>J,K>L:KLREP) <--> BUF(IK,JL)
C     If (FIRST) take only the totally symmetric (first) irrep.
C     If (DOINV) inverse sort.
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 BUF1(*),BUF2(*)
      INTEGER NREP, MULTB(64,64,2)
      INTEGER IFIE(NREP),JFIE(NREP),KFIE(NREP),LFIE(NREP),NPAIR(NREP)
      INTEGER OFF(NREP+1),OFF1(32,32),OFF2(32,32)
      LOGICAL FIRST,DOINV
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
      INCLUDE "param.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      IF (.NOT.DOINV) THEN
         IF (FIRST) THEN
            CALL XCOPY(OFF(2),A0,0,BUF2,1)
         ELSE
            CALL XCOPY(OFF(NREP+1),A0,0,BUF2,1)
         ENDIF
      ENDIF
C
      IJKL = 1
      DO KLREP = 1, NREP
       DO 10 LREP = 1, NREP
        KREP = MULTB(LREP,KLREP+NREP,2)
        IF (KREP.LT.LREP) GOTO 10
        DO L = 1, LFIE(LREP)
         KMIN = 1
         IF (KREP.EQ.LREP) KMIN = L + 1
         DO K = KMIN, KFIE(KREP)
          DO 20 JREP = 1, NREP
           IREP = MULTB(JREP,KLREP+NREP,2)
           IF (IREP.LT.JREP) GOTO 20
           JLREP = MULTB(JREP,LREP,2)
           IKREP = MULTB(IREP,KREP,2)
           ILREP = MULTB(IREP,LREP,2)
           JKREP = MULTB(JREP,KREP,2)
           IKJL0 = OFF(IKREP) 
     &          + (OFF2(JREP,LREP)+(L-1)*JFIE(JREP)) * NPAIR(IKREP)
           IKJL0 = IKJL0 + OFF1(IREP,KREP) + (K-1)*IFIE(IREP)
           ILJK0 = OFF(ILREP) 
     &          + (OFF2(JREP,KREP)+(K-1)*JFIE(JREP)) * NPAIR(ILREP)
           ILJK0 = ILJK0 + OFF1(IREP,LREP) + (L-1)*IFIE(IREP)
           JKIL0 = OFF(JKREP) 
     &          + (OFF2(IREP,LREP)+(L-1)*IFIE(IREP)) * NPAIR(JKREP)
           JKIL0 = JKIL0 + OFF1(JREP,KREP) + (K-1)*JFIE(JREP)
           JLIK0 = OFF(JLREP) 
     &          + (OFF2(IREP,KREP)+(K-1)*IFIE(IREP)) * NPAIR(JLREP)
           JLIK0 = JLIK0 + OFF1(JREP,LREP) + (L-1)*JFIE(JREP)
           DO J = 1, JFIE(JREP)
             IMIN = 1
             IF (IREP.EQ.JREP) IMIN = J + 1
             N = IFIE(IREP) - IMIN + 1
             IKJL = IKJL0 + IMIN-1
             ILJK = ILJK0 + IMIN-1
             JKIL = JKIL0 + (IMIN-1)*NPAIR(JKREP)
             JLIK = JLIK0 + (IMIN-1)*NPAIR(JLREP)
             IF (DOINV) THEN
                CALL XCOPY (N,BUF1(IKJL*RCW+1),1,BUF2(IJKL),1)
                CALL XAXPY (N,-A1,BUF1(ILJK*RCW+1),1,BUF2(IJKL),1)
                NP = NPAIR(JKREP)
                CALL XAXPY(N,-A1,BUF1(JKIL*RCW+1),NP,BUF2(IJKL),1)
                NP = NPAIR(JLREP)
                CALL XAXPY (N,A1,BUF1(JLIK*RCW+1),NP,BUF2(IJKL),1)
             ELSE
                IF ((.NOT.FIRST).OR.(IKREP.EQ.1))
     &            CALL XCOPY (N,BUF1(IJKL),1,BUF2(IKJL*RCW+1),1)
                IF ((.NOT.FIRST).OR.(ILREP.EQ.1)) THEN
                  CALL XCOPY (N,BUF1(IJKL),1,BUF2(ILJK*RCW+1),1)
                  CALL XSCAL (N,-A1,BUF2(ILJK*RCW+1),1)
                ENDIF
                IF ((.NOT.FIRST).OR.(JKREP.EQ.1)) THEN
                  NP = NPAIR(JKREP)
                  CALL XCOPY(N,BUF1(IJKL),1,BUF2(JKIL*RCW+1),NP)
                  CALL XSCAL(N,-A1,BUF2(JKIL*RCW+1),NP)
                ENDIF
                IF ((.NOT.FIRST).OR.(JLREP.EQ.1))  THEN
                  NP = NPAIR(JLREP)
                  CALL XCOPY (N,BUF1(IJKL),1,BUF2(JLIK*RCW+1),NP)
                ENDIF
             ENDIF
             IJKL = IJKL + RCW * N
             IKJL0 = IKJL0 + NPAIR(IKREP)
             ILJK0 = ILJK0 + NPAIR(ILREP)
             JKIL0 = JKIL0 + 1
             JLIK0 = JLIK0 + 1
           ENDDO
 20       CONTINUE
         ENDDO
        ENDDO
 10    CONTINUE
      ENDDO
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE SRT1ST4 (NREP,MULTB,FIRST,IFIE,JFIE,KFIE,LFIE,NPAIR,
     &                    OFF,OFF1,OFF2,BUF1,BUF2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Sort array BUF1(IJ,K>L:KLREP) to array BUF2(IK,JL:JLREP=1)
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 BUF1(*),BUF2(*)
      INTEGER NREP, MULTB(64,64,2)
      INTEGER IFIE(NREP),JFIE(NREP),KFIE(NREP),LFIE(NREP),NPAIR(NREP)
      INTEGER OFF(NREP+1),OFF1(32,32),OFF2(32,32)
      LOGICAL FIRST
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
      INCLUDE "param.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      IF (FIRST) THEN
        CALL XCOPY(OFF(2),A0,0,BUF2,1)
      ELSE
        CALL XCOPY(OFF(NREP+1),A0,0,BUF2,1)
      ENDIF
      IJKL = 1
      DO KLREP = 1, NREP
       DO 10 LREP = 1, NREP
        KREP = MULTB(LREP,KLREP+NREP,2)
        IF (KREP.LT.LREP) GOTO 10
        DO L = 1, LFIE(LREP)
         KMIN = 1
         IF (KREP.EQ.LREP) KMIN = L + 1
         DO K = KMIN, KFIE(KREP)
          DO 20 JREP = 1, NREP
           IREP = MULTB(JREP,KLREP+NREP,2)
           JLREP = MULTB(JREP,LREP,2)
           IKREP = MULTB(IREP,KREP,2)
           ILREP = MULTB(IREP,LREP,2)
           JKREP = MULTB(JREP,KREP,2)
           IKJL0 = OFF(JLREP) 
     &          + (OFF2(JREP,LREP)+(L-1)*JFIE(JREP)) * NPAIR(IKREP)
           IKJL0 = IKJL0 + OFF1(IREP,KREP) + (K-1)*IFIE(IREP)
           IKJL0 = IKJL0 * RCW + 1
           ILJK0 = OFF(JKREP) 
     &          + (OFF2(JREP,KREP)+(K-1)*JFIE(JREP)) * NPAIR(ILREP)
           ILJK0 = ILJK0 + OFF1(IREP,LREP) + (L-1)*IFIE(IREP)
           ILJK0 = ILJK0 * RCW + 1
           DO J = 1, JFIE(JREP)
            IF (JLREP.EQ.1.OR.(.NOT.FIRST)) 
     &         CALL XCOPY (IFIE(IREP),BUF1(IJKL),1,BUF2(IKJL0),1)
            IF (JKREP.EQ.1.OR.(.NOT.FIRST)) THEN 
               CALL XCOPY (IFIE(IREP),BUF1(IJKL),1,BUF2(ILJK0),1)
               CALL XSCAL (IFIE(IREP),-A1,BUF2(ILJK0),1)
            ENDIF
            IJKL = IJKL + IFIE(IREP) * RCW
            IKJL0 = IKJL0 + NPAIR(IKREP) * RCW
            ILJK0 = ILJK0 + NPAIR(ILREP) * RCW
           ENDDO
 20       CONTINUE
         ENDDO
        ENDDO
 10    CONTINUE
      ENDDO
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE SRT1SS4 (NREP,MULTB,IFIE,JFIE,KFIE,LFIE,NPAIR,
     &                    OFF,OFF1,OFF2,BUF1,BUF2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Sort array BUF1(IJ,KL:KLREP) to array BUF2(IK,JL:JLREP=1)
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 BUF1(*),BUF2(*)
      INTEGER NREP, MULTB(64,64,2)
      INTEGER IFIE(NREP),JFIE(NREP),KFIE(NREP),LFIE(NREP),NPAIR(NREP)
      INTEGER OFF(NREP+1),OFF1(32,32),OFF2(32,32)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
      INCLUDE "param.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      CALL XCOPY(OFF(2),A0,0,BUF2,1)
      IJKL = 1
      DO KLREP = 1, NREP
       DO 10 LREP = 1, NREP
        KREP = MULTB(LREP,KLREP+NREP,2)
        DO L = 1, LFIE(LREP)
         DO K = 1, KFIE(KREP)
          DO 20 JREP = 1, NREP
           IREP = MULTB(JREP,KLREP+NREP,2)
           JLREP = MULTB(JREP,LREP,2)
           IKREP = MULTB(IREP,KREP,2)
           IKJL0 = OFF(JLREP) 
     &          + (OFF2(JREP,LREP)+(L-1)*JFIE(JREP)) * NPAIR(IKREP)
           IKJL0 = IKJL0 + OFF1(IREP,KREP) + (K-1)*IFIE(IREP)
            DO J = 1, JFIE(JREP)
            CALL XCOPY (IFIE(IREP),BUF1(IJKL),1,BUF2(IKJL0*RCW+1),1)
            IJKL = IJKL + IFIE(IREP) * RCW
            IKJL0 = IKJL0 + NPAIR(IKREP)
           ENDDO
 20       CONTINUE
         ENDDO
        ENDDO
 10    CONTINUE
      ENDDO
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE SRT1T3 (NREP,MULTB,DOINV,NPAIR,KFIE,LFIE,NTRIPL,
     &                   OFF,OFF1,BUF1,BUF2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     BUF(IJ,K>L:KLREP) <--> BUF(IJ K,L:LREP)
C     If (DOINV) inverse sort.
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 BUF1(*),BUF2(*)
      INTEGER NREP, MULTB(64,64,2)
      INTEGER NPAIR(NREP),KFIE(NREP),LFIE(NREP),NTRIPL(NREP)
      INTEGER OFF(NREP+1),OFF1(32,32)
      LOGICAL DOINV
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
      INCLUDE "param.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      IF (.NOT.DOINV) THEN
         CALL XCOPY(OFF(NREP+1),A0,0,BUF2,1)
      ENDIF
      IJKL = 1
      DO KLREP = 1, NREP
       IJREP = KLREP
       N = NPAIR(IJREP)
       DO 10 LREP = 1, NREP
        KREP = MULTB(LREP,KLREP+NREP,2)
        IF (KREP.LT.LREP) GOTO 10
        LOFF = (OFF(LREP) + OFF1(IJREP,KREP)) * RCW
        KOFF = (OFF(KREP) + OFF1(IJREP,LREP)) * RCW
        DO L = 1, LFIE(LREP)
         IJL  = (L-1) * N * RCW + 1
         KMIN = 1
         IF (KREP.EQ.LREP) KMIN = L + 1
         DO K = KMIN, KFIE(KREP)
            IJK  = (K-1) * N * RCW + 1
            IJKL1 = LOFF + (L-1) * NTRIPL(LREP) * RCW + IJK 
            IJLK1 = KOFF + (K-1) * NTRIPL(KREP) * RCW + IJL 
            IF (DOINV) THEN
               CALL XCOPY (N,BUF1(IJKL1),1,BUF2(IJKL),1)
               CALL XAXPY (N,-A1,BUF1(IJLK1),1,BUF2(IJKL),1)
            ELSE
               CALL XCOPY (N,BUF1(IJKL),1,BUF2(IJKL1),1)
               CALL XCOPY (N,BUF1(IJKL),1,BUF2(IJLK1),1)
               CALL XSCAL (N,-A1,BUF2(IJLK1),1)
            ENDIF
            IJKL = IJKL + N * RCW
         ENDDO
        ENDDO
 10    CONTINUE
      ENDDO
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE SRT1T2 (NREP,MULTB,DOINV,NPAIR1,IFIE,JFIE,NPAIR2,OFF,
     &                   OFF2,BUF1,BUF2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     BUF(I>J,KL:KLREP) <--> BUF(I,J KL:JKLREP)
C     If (DOINV) inverse sort.
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 BUF1(*),BUF2(*)
      INTEGER NREP, MULTB(64,64,2)
      INTEGER NPAIR1(NREP),IFIE(NREP),JFIE(NREP),NPAIR2(NREP)
      INTEGER OFF(NREP+1),OFF2(32,32)
      LOGICAL DOINV
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
      INCLUDE "param.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      IF (.NOT.DOINV) THEN
         CALL XCOPY(OFF(NREP+1),A0,0,BUF2,1)
      ENDIF
      IJKL = 0
      DO KLREP = 1, NREP
       IJREP = KLREP
       M = NPAIR1(IJREP)
       N = NPAIR2(KLREP)
       IJ = 1
       DO 10 JREP = 1, NREP
        IREP = MULTB(JREP,IJREP+NREP,2)
        IF (IREP.LT.JREP) GOTO 10
        JKLREP = IREP
        IKLREP = JREP
        JKLOFF = (OFF(JKLREP)+OFF2(JREP,KLREP)*IFIE(IREP)) * RCW
        IKLOFF = (OFF(IKLREP)+OFF2(IREP,KLREP)*JFIE(JREP)) * RCW
        M2 = IFIE(IREP) * JFIE(JREP)
        DO J = 1, JFIE(JREP)
         IMIN = 1
         IF (IREP.EQ.JREP) IMIN = J + 1
         DO I = IMIN, IFIE(IREP)
            IJKL1 = JKLOFF + ((J-1) * IFIE(IREP) + I - 1) * RCW + 1
            JIKL1 = IKLOFF + ((I-1) * JFIE(JREP) + J - 1) * RCW + 1
            IF (DOINV) THEN
               CALL XCOPY(N,BUF1(IJKL1),M2,BUF2(IJKL+IJ),M)
               CALL XAXPY(N,-A1,BUF1(JIKL1),M2,BUF2(IJKL+IJ),M)
            ELSE
               CALL XCOPY(N,BUF1(IJKL+IJ),M,BUF2(IJKL1),M2)
               CALL XCOPY(N,BUF1(IJKL+IJ),M,BUF2(JIKL1),M2)
               CALL XSCAL(N,-A1,BUF2(JIKL1),M2)
            ENDIF
            IJ = IJ + RCW
         ENDDO
        ENDDO
 10    CONTINUE
       IJKL = IJKL + M * N * RCW
      ENDDO
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE SRT6 (NREP,MULTB,NPAIR1,IFIE,JFIE,NPAIR2,NTRIPL,
     &                 OFF,OFF1,BUF1,BUF2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Sort array BUF1(I,J;KL:KLREP) to array BUF2(I KL, J:JREP)
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 BUF1(*),BUF2(*)
      INTEGER NREP, MULTB(64,64,2)
      INTEGER NPAIR1(NREP),IFIE(NREP),JFIE(NREP),NPAIR2(NREP)
      INTEGER NTRIPL(NREP)
      INTEGER OFF(NREP+1),OFF1(32,32)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
      INCLUDE "param.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      CALL XCOPY(OFF(NREP+1),A0,0,BUF2,1)
      IJKL = 0
      DO KLREP = 1, NREP
       IJREP = KLREP
       M = NPAIR1(IJREP)
       N = NPAIR2(KLREP)
       IJ = 1
       DO 10 JREP = 1, NREP
        IREP = MULTB(JREP,IJREP+NREP,2)
        JOFF = (OFF(JREP) + OFF1(IREP,KLREP)) * RCW
        DO J = 1, JFIE(JREP)
         DO I = 1, IFIE(IREP)
            IKLJ1 = JOFF + ((J-1)*NTRIPL(JREP)+I-1) * RCW + 1
            CALL XCOPY(N,BUF1(IJKL+IJ),M,BUF2(IKLJ1),IFIE(IREP))
            IJ = IJ + RCW
         ENDDO
        ENDDO
 10    CONTINUE
       IJKL = IJKL + M * N * RCW
      ENDDO
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE SRT9 (NREP,MULTB,NPAIR1,IFIE,JFIE,NPAIR2,NTRIPL,
     &                 OFF,OFF1,BUF1,BUF2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Sort array BUF1(I,J;KL:KLREP) to array -BUF2*(KL J,I:IREP)
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 BUF1(*),BUF2(*)
      INTEGER NREP, MULTB(64,64,2)
      INTEGER NPAIR1(NREP),IFIE(NREP),JFIE(NREP),NPAIR2(NREP)
      INTEGER NTRIPL(NREP)
      INTEGER OFF(NREP+1),OFF1(32,32)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
      INCLUDE "param.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      CALL XCOPY(OFF(NREP+1),A0,0,BUF2,1)
      IJKL = 0
      DO KLREP = 1, NREP
       IJREP = KLREP
       M = NPAIR1(IJREP)
       N = NPAIR2(KLREP)
       IJ = 1
       DO 10 JREP = 1, NREP
        IREP = MULTB(JREP,IJREP+NREP,2)
        IOFF = (OFF(IREP)+OFF1(KLREP,JREP)) * RCW + 1
        DO J = 1, JFIE(JREP)
         DO I = 1, IFIE(IREP)
            KLJI = IOFF+((I-1)*NTRIPL(IREP)+(J-1)*N)*RCW
            CALL XCOPY(N,BUF1(IJKL+IJ),M,BUF2(KLJI),1)
            IJ = IJ + RCW
         ENDDO
        ENDDO
 10    CONTINUE
       IJKL = IJKL + M * N * RCW
      ENDDO
      CALL XSCAL (OFF(NREP+1),-A1,BUF2,1)
      IF (CARITH) CALL CONJUGA (OFF(NREP+1),BUF2,1)
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE SRT1L1 (NREP,MULTB,DOINV,NPAIR1,IFIE,JFIE,NPAIR2,
     &                   OFF,OFF1,BUF1,BUF2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Sort array BUF1(IJ,KL:KLREP) to array -BUF2(JI,KL:KLREP)
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 BUF1(*),BUF2(*)
      INTEGER NREP, MULTB(64,64,2)
      INTEGER NPAIR1(NREP),IFIE(NREP),JFIE(NREP),NPAIR2(NREP)
      INTEGER OFF(NREP+1),OFF1(32,32)
      LOGICAL DOINV
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
      INCLUDE "param.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      JIKLOFF = 0
      DO KLREP = 1, NREP
       IJREP = KLREP
       M = NPAIR1(IJREP)
       N = NPAIR2(KLREP)
       IJKL = OFF(KLREP) * RCW + 1
       JIKLOFF = OFF(KLREP)
       DO 10 JREP = 1, NREP
        IREP = MULTB(JREP,IJREP+NREP,2)
        NIS = IFIE(IREP)
        NJS = JFIE(JREP)
        DO J = 1, NJS
         DO I = 1, NIS
           JIKL = (JIKLOFF+OFF1(JREP,IREP)+(I-1)*NJS+J-1)*RCW + 1
           IF (DOINV) THEN
              CALL XCOPY (N,BUF1(JIKL),M,BUF2(IJKL),M)
           ELSE
              CALL XCOPY (N,BUF1(IJKL),M,BUF2(JIKL),M)
           ENDIF
           IJKL = IJKL + RCW
         ENDDO
        ENDDO
 10    CONTINUE
      ENDDO
C
      CALL XSCAL (OFF(NREP+1),-A1,BUF2,1)
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE SRT1S2 (NREP,MULTB,DOINV,NPAIR1,IFIE,JFIE,NPAIR2,OFF,
     &                   OFF2,BUF1,BUF2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     BUF(I J,KL:KLREP) <--> BUF(I,J KL:JKLREP)
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 BUF1(*),BUF2(*)
      INTEGER NREP, MULTB(64,64,2)
      INTEGER NPAIR1(NREP),IFIE(NREP),JFIE(NREP),NPAIR2(NREP)
      INTEGER OFF(NREP+1),OFF2(32,32)
      LOGICAL DOINV
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
      INCLUDE "param.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      IJKL = 0
      DO KLREP = 1, NREP
       IJREP = KLREP
       M = NPAIR1(IJREP)
       N = NPAIR2(KLREP)
       IJ = 1
       DO 10 JREP = 1, NREP
        IREP = MULTB(JREP,IJREP+NREP,2)
        JKLREP = IREP
        JKLOFF = (OFF(JKLREP)+OFF2(JREP,KLREP)*IFIE(IREP)) * RCW
        M2 = IFIE(IREP) * JFIE(JREP)
        DO J = 1, JFIE(JREP)
         DO I = 1, IFIE(IREP)
            IJKL1 = JKLOFF + ((J-1) * IFIE(IREP) + I - 1) * RCW + 1
            IF (DOINV) THEN
               CALL XCOPY(N,BUF1(IJKL1),M2,BUF2(IJKL+IJ),M)
            ELSE
               CALL XCOPY(N,BUF1(IJKL+IJ),M,BUF2(IJKL1),M2)
            ENDIF
            IJ = IJ + RCW
         ENDDO
        ENDDO
 10    CONTINUE
       IJKL = IJKL + M * N * RCW
      ENDDO
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE SRT1C1 (NREP,NPAIR1,NPAIR2,BUF1,BUF2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Sort array BUF1(IJ,KL:KLREP) to array BUF2*(KL,IJ:KLREP)
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 BUF1(*),BUF2(*)
      INTEGER NREP,NPAIR1(NREP),NPAIR2(NREP)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
      INCLUDE "param.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      IJKL = 1
      KLIJ = 1
      DO KLREP = 1, NREP
       M = NPAIR1(KLREP)
       N = NPAIR2(KLREP)
       DO IJ = 1, M
         CALL XCOPY (N,BUF1(IJKL),M,BUF2(KLIJ),1)
         IJKL = IJKL + RCW
         KLIJ = KLIJ + N * RCW
       ENDDO
       IJKL = KLIJ
      ENDDO
C
      N = (KLIJ - 1) / RCW
      IF (CARITH) CALL CONJUGA (N,BUF2,1)
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE SRT16 (NREP,MULTB,FIRST,DOINV,IFIE,JFIE,KFIE,LFIE,
     &                  NPAIR,OFF,OFF1,OFF2,BUF1,BUF2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Sort array BUF1(IJ,KL:KLREP) to array BUF2(IL,KJ:KJREP)
C     If (FIRST) take only the totally symmetric (first) irrep.
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 BUF1(*),BUF2(*)
      INTEGER NREP, MULTB(64,64,2)
      INTEGER IFIE(NREP),JFIE(NREP),KFIE(NREP),LFIE(NREP),NPAIR(NREP)
      INTEGER OFF(NREP+1),OFF1(32,32),OFF2(32,32)
      LOGICAL FIRST, DOINV
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
      INCLUDE "param.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      IF (FIRST) THEN
         CALL XCOPY(OFF(2),A0,0,BUF2,1)
      ELSE
         CALL XCOPY(OFF(NREP+1),A0,0,BUF2,1)
      ENDIF
      IJKL = 1
      DO KLREP = 1, NREP
       DO 10 LREP = 1, NREP
        KREP = MULTB(LREP,KLREP+NREP,2)
        DO L = 1, LFIE(LREP)
         DO K = 1, KFIE(KREP)
          DO 20 JREP = 1, NREP
           IREP = MULTB(JREP,KLREP+NREP,2)
           KJREP = MULTB(KREP,JREP,2)
           ILREP = MULTB(IREP,LREP,2)
           ILKJ0 = OFF(KJREP) 
     &          + (OFF2(KREP,JREP)+K-1)*NPAIR(ILREP)
           ILKJ0 = ILKJ0 + OFF1(IREP,LREP) + (L-1)*IFIE(IREP)
            DO J = 1, JFIE(JREP)
            IF ((.NOT.FIRST).OR.(KJREP.EQ.1)) 
     &      CALL XCOPY(IFIE(IREP),BUF1(IJKL),1,BUF2(ILKJ0*RCW+1),1)
            IJKL = IJKL + RCW * IFIE(IREP)
            ILKJ0 = ILKJ0 + NPAIR(ILREP) * KFIE(KREP)
           ENDDO
 20       CONTINUE
         ENDDO
        ENDDO
 10    CONTINUE
      ENDDO
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE SRT19 (NREP,MULTB,NPAIR,KFIE,LFIE,NTRIPL,OFF,OFF1,
     &                  BUF1,BUF2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Sort array BUF1(IJ,KL:KLREP) to array BUF2(IJ L,K:KREP)
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 BUF1(*),BUF2(*)
      INTEGER NREP, MULTB(64,64,2)
      INTEGER NPAIR(NREP),KFIE(NREP),LFIE(NREP),NTRIPL(NREP)
      INTEGER OFF(NREP+1),OFF1(32,32)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
      INCLUDE "param.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      CALL XCOPY(OFF(NREP+1),A0,0,BUF2,1)
      IJKL = 1
      DO KLREP = 1, NREP
       IJREP = KLREP
       N = NPAIR(KLREP)
       DO 10 LREP = 1, NREP
        KREP = MULTB(LREP,KLREP+NREP,2)
        KOFF = (OFF(KREP) + OFF1(IJREP,LREP)) * RCW
        DO L = 1, LFIE(LREP)
         IJL  = (L-1) * N * RCW + 1
         DO K = 1, KFIE(KREP)
            IJLK1 = KOFF + (K-1) * NTRIPL(KREP) * RCW + IJL 
            CALL XCOPY (N,BUF1(IJKL),1,BUF2(IJLK1),1)
            IJKL = IJKL + N * RCW
         ENDDO
        ENDDO
 10    CONTINUE
      ENDDO
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE SRT20 (NREP,MULTB,IJPAIR,IFIE,JFIE,KFIE,LFIE,
     &                  NPAIR,OFF,OFF1,OFF2,BUF1,NBUF1,BUF2,USEDZ)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Sort array BUF1(IJ,K>L:KLREP) to array BUF2(IK,LJ:LJREP=1)
C     Integrals are fetched from disk
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      LOGICAL USEDZ
      INTEGER NBUF1
      REAL*8 BUF1(*),BUF2(*)
      INTEGER NREP,MULTB(64,64,2),IJPAIR(NREP)
      INTEGER IFIE(NREP),JFIE(NREP),KFIE(NREP),LFIE(NREP),NPAIR(NREP)
      INTEGER OFF(NREP+1),OFF1(32,32),OFF2(32,32)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
      INCLUDE "param.inc"
C
C---------------Local variables--------------------------------------
C
      LOGICAL DONE
C
C---------------Executable code--------------------------------------
C
      CALL XCOPY (OFF(2),A0,0,BUF2,1)
      DO 10 KLREP = 1, NREP
       IF (IJPAIR(KLREP).EQ.0) GOTO 10
       IJKL = 1
       KL = 0
       KLMAX = NBUF1 / IJPAIR(KLREP)
       KLSTAR = 0
       KLINCR = KLMAX
       IF (.NOT.USEDZ) THEN
          CALL GETVOVV (KLREP,KLSTAR,KLINCR,DONE,BUF1,KLMAX)
          CALL DELINT ('VOVV','KDDD',BUF1,KLREP,KLSTAR,KLINCR)
       ELSE
          CALL GETDZ   (KLREP,KLSTAR,KLINCR,DONE,BUF1)
       ENDIF
C
       DO 20 LREP = 1, NREP
        KREP = MULTB(LREP,KLREP+NREP,2)
        IF (KREP.LT.LREP) GOTO 20
        DO 30 L = 1, LFIE(LREP)
         KMIN = 1
         IF (KREP.EQ.LREP) KMIN = L + 1
         DO 40 K = KMIN, KFIE(KREP)
          KL = KL + 1
          IF (KL.EQ.KLINCR+1) THEN
C -----------------------
C We need the next buffer
C -----------------------
             KLSTAR = KLSTAR + KLINCR
             KLINCR = KLMAX
             IF (.NOT.USEDZ) THEN
                CALL GETVOVV (KLREP,KLSTAR,KLINCR,DONE,BUF1,KLMAX)
                CALL DELINT ('VOVV','KDDD',BUF1,KLREP,KLSTAR,KLINCR)
             ELSE
                CALL GETDZ   (KLREP,KLSTAR,KLINCR,DONE,BUF1)
             ENDIF
             KL = 1
             IJKL = 1
          ENDIF
          DO 50 JREP = 1, NREP
           IREP = MULTB(JREP,KLREP+NREP,2)
           LJREP = MULTB(LREP,JREP,2)
           IKREP = MULTB(IREP,KREP,2)
           ILREP = MULTB(IREP,LREP,2)
           KJREP = MULTB(KREP,JREP,2)
           IKLJ0 = OFF(LJREP) + (OFF2(LREP,JREP)+L-1) * NPAIR(IKREP)
           IKLJ0 = IKLJ0 + OFF1(IREP,KREP) + (K-1)*IFIE(IREP)
           IKLJ0 = IKLJ0 * RCW + 1
           ILKJ0 = OFF(KJREP) + (OFF2(KREP,JREP)+K-1) * NPAIR(ILREP)
           ILKJ0 = ILKJ0 + OFF1(IREP,LREP) + (L-1)*IFIE(IREP)
           ILKJ0 = ILKJ0 * RCW + 1
           DO 60 J = 1, JFIE(JREP)
            IF (LJREP.EQ.1) THEN
               CALL XCOPY (IFIE(IREP),BUF1(IJKL),1,BUF2(IKLJ0),1)
            ENDIF
            IF (KJREP.EQ.1) THEN 
               CALL XCOPY (IFIE(IREP),BUF1(IJKL),1,BUF2(ILKJ0),1)
               CALL XSCAL (IFIE(IREP),-A1,BUF2(ILKJ0),1)
            ENDIF
            IJKL = IJKL + IFIE(IREP) * RCW
            IKLJ0 = IKLJ0 + NPAIR(IKREP) * LFIE(LREP) * RCW
            ILKJ0 = ILKJ0 + NPAIR(ILREP) * KFIE(KREP) * RCW
 60        CONTINUE
 50       CONTINUE
 40      CONTINUE
 30     CONTINUE
 20    CONTINUE
 10   CONTINUE
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE SRT1LS1 (NREP,MULTB,NPAIR1A,NPAIR1B,IFIE,JFIE,NPAIR2,
     &                    OFF,OFF1,BUF1,BUF2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Sort array BUF1(I>J,KL:KLREP) to array BUF2(I J,KL:KLREP)
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 BUF1(*),BUF2(*)
      INTEGER NREP, MULTB(64,64,2)
      INTEGER NPAIR1A(NREP),NPAIR1B(NREP),NPAIR2(NREP)
      INTEGER IFIE(NREP),JFIE(NREP)
      INTEGER OFF(NREP+1),OFF1(32,32)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
      INCLUDE "param.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      CALL XCOPY(OFF(NREP+1),A0,0,BUF2,1)
      IJKL1 = 1
      DO KLREP = 1, NREP
       IJREP = KLREP
       M1 = NPAIR1A(IJREP)
       M2 = NPAIR1B(IJREP)
       N = NPAIR2(KLREP)
       DO 10 JREP = 1, NREP
        IREP = MULTB(JREP,IJREP+NREP,2)
        IF (IREP.LT.JREP) GOTO 10
        DO J = 1, JFIE(JREP)
         IMIN = 1
         IF (IREP.EQ.JREP) IMIN = J + 1
         DO I = IMIN, IFIE(IREP)
            IJKL2 = (OFF(KLREP)+OFF1(IREP,JREP)
     &               +(J-1)*IFIE(IREP)+I-1)*RCW + 1
            JIKL2 = (OFF(KLREP)+OFF1(JREP,IREP)
     &               +(I-1)*JFIE(JREP)+J-1)*RCW + 1
            CALL XCOPY(N,BUF1(IJKL1),M1,BUF2(IJKL2),M2)
            CALL XCOPY(N,BUF1(IJKL1),M1,BUF2(JIKL2),M2)
            CALL XSCAL(N,-A1,BUF2(JIKL2),M2)
            IJKL1 = IJKL1 + RCW
         ENDDO
        ENDDO
 10    CONTINUE
      IJKL1 = IJKL1 + M1 * (N-1) * RCW
      ENDDO
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE SRT22 (NREP,MULTB,NPAIR,KFIE,LFIE,NTRIPL,OFF,OFF1,
     &                  BUF1,BUF2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Sort array BUF1(IJ,K>L:KLREP) to array BUF2(L IJ,K:KREP)
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 BUF1(*),BUF2(*)
      INTEGER NREP, MULTB(64,64,2)
      INTEGER NPAIR(NREP),KFIE(NREP),LFIE(NREP),NTRIPL(NREP)
      INTEGER OFF(NREP+1),OFF1(32,32)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
      INCLUDE "param.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      CALL XCOPY(OFF(NREP+1),A0,0,BUF2,1)
      IJKL = 1
      DO KLREP = 1, NREP
       IJREP = KLREP
       N = NPAIR(KLREP)
       DO 10 LREP = 1, NREP
        KREP = MULTB(LREP,KLREP+NREP,2)
        IF (KREP.LT.LREP) GOTO 10
        KOFF = OFF(KREP) + OFF1(LREP,IJREP)
        LOFF = OFF(LREP) + OFF1(KREP,IJREP)
        DO L = 1, LFIE(LREP)
         KMIN = 1
         IF (KREP.EQ.LREP) KMIN = L + 1
         DO K = KMIN, KFIE(KREP)
            LIJK = (KOFF + (K-1) * NTRIPL(KREP) + L - 1) * RCW + 1 
            KIJL = (LOFF + (L-1) * NTRIPL(LREP) + K - 1) * RCW + 1 
            CALL XCOPY (N,BUF1(IJKL),1,BUF2(LIJK),LFIE(LREP))
            CALL XCOPY (N,BUF1(IJKL),1,BUF2(KIJL),KFIE(KREP))
            CALL XSCAL (N,-A1,BUF2(KIJL),KFIE(KREP))
            IJKL = IJKL + N * RCW
         ENDDO
        ENDDO
 10    CONTINUE
      ENDDO
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE SRT23 (NREP,MULTB,NPAIR1,IFIE,JFIE,NPAIR2,NTRIPL,
     &                  OFF,OFF1,BUF1,BUF2,NVVVOTMX)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Sort array VOVV(I,J;KL:KLREP) to array BUF2(I KL, J:JREP)
C     Read first array from file, representationwise
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 BUF1(*),BUF2(*)
      INTEGER NREP, MULTB(64,64,2)
      INTEGER NPAIR1(NREP),IFIE(NREP),JFIE(NREP),NPAIR2(NREP)
      INTEGER NTRIPL(NREP)
      INTEGER OFF(NREP+1),OFF1(32,32)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
      INCLUDE "param.inc"
      INCLUDE "files.inc"
C
C---------------Local variables--------------------------------------
C
      LOGICAL DONE
      INTEGER ISTART,NINT,MINT
C
C---------------Executable code--------------------------------------
C
      CALL XCOPY(OFF(NREP+1),A0,0,BUF2,1)
      DO KLREP = 1, NREP
       IJREP = KLREP
       M = NPAIR1(IJREP)
       N = NPAIR2(KLREP)
       ISTART = 0
       NINT = N
       MINT=NVVVOTMX/(MAX0(M,1))
       CALL GETVOVV (IJREP,ISTART,NINT,DONE,BUF1,MINT)
       IF(.NOT.DONE) CALL 
     & QUIT (' SRT23: IRREP BATCH TOO LARGE FOR INCORE TRIPLES')
       CALL DELINT ('VOVV','DKKK',BUF1,IJREP,ISTART,NINT)
       IJ = 1
       DO 10 JREP = 1, NREP
        IREP = MULTB(JREP,IJREP+NREP,2)
        JOFF = (OFF(JREP) + OFF1(IREP,KLREP)) * RCW
        DO J = 1, JFIE(JREP)
         DO I = 1, IFIE(IREP)
            IKLJ1 = JOFF + ((J-1)*NTRIPL(JREP)+I-1) * RCW + 1
            CALL XCOPY(N,BUF1(IJ),M,BUF2(IKLJ1),IFIE(IREP))
            IJ = IJ + RCW
         ENDDO
        ENDDO
 10    CONTINUE
      ENDDO
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE SRT1S3 (NREP,MULTB,DOINV,NPAIR,KFIE,LFIE,NTRIPL,
     &                   OFF,OFF1,BUF1,BUF2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     BUF(IJ,K L:KLREP) <--> BUF(IJ K,L:LREP)
C     If (DOINV) inverse sort.
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 BUF1(*),BUF2(*)
      INTEGER NREP, MULTB(64,64,2)
      INTEGER NPAIR(NREP),KFIE(NREP),LFIE(NREP),NTRIPL(NREP)
      INTEGER OFF(NREP+1),OFF1(32,32)
      LOGICAL DOINV
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
      INCLUDE "param.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      CALL XCOPY(OFF(NREP+1),A0,0,BUF2,1)
      IJKL = 1
      DO KLREP = 1, NREP
       IJREP = KLREP
       N = NPAIR(KLREP)
       DO 10 LREP = 1, NREP
        KREP = MULTB(LREP,KLREP+NREP,2)
        LOFF = (OFF(LREP) + OFF1(IJREP,KREP)) * RCW
        DO L = 1, LFIE(LREP)
         DO K = 1, KFIE(KREP)
            IJK  = (K-1) * N * RCW + 1
            IJKL1 = LOFF + (L-1) * NTRIPL(LREP) * RCW + IJK 
            IF (DOINV) THEN
                CALL XCOPY (N,BUF1(IJKL1),1,BUF2(IJKL),1)
            ELSE
                CALL XCOPY (N,BUF1(IJKL),1,BUF2(IJKL1),1)
            ENDIF
            IJKL = IJKL + N * RCW
         ENDDO
        ENDDO
 10    CONTINUE
      ENDDO
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE SRT26 (NREP,MULTB,FIRST,DOINV,
     &                  IFIE,JFIE,KFIE,LFIE,NPAIR,
     &                  OFF,OFF1,OFF2,BUF1,BUF2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     BUF(IJ,K>L:KLREP) <--> BUF(IL,KJ:KJREP=1)
C     If (FIRST) take only the totally symmetric (first) irrep.
C     If (DOINV) inverse sort.
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 BUF1(*),BUF2(*)
      INTEGER NREP, MULTB(64,64,2)
      INTEGER IFIE(NREP),JFIE(NREP),KFIE(NREP),LFIE(NREP),NPAIR(NREP)
      INTEGER OFF(NREP+1),OFF1(32,32),OFF2(32,32)
      LOGICAL FIRST,DOINV
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
      INCLUDE "param.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      IF (.NOT.DOINV) THEN
         IF (FIRST) THEN
            CALL XCOPY(OFF(2),A0,0,BUF2,1)
         ELSE
            CALL XCOPY(OFF(NREP+1),A0,0,BUF2,1)
         ENDIF
      ENDIF
C
      IJKL = 1
      DO KLREP = 1, NREP
       DO 10 LREP = 1, NREP
        KREP = MULTB(LREP,KLREP+NREP,2)
        IF (KREP.LT.LREP) GOTO 10
        DO L = 1, LFIE(LREP)
         KMIN = 1
         IF (KREP.EQ.LREP) KMIN = L + 1
         DO K = KMIN, KFIE(KREP)
          DO 20 JREP = 1, NREP
           IREP = MULTB(JREP,KLREP+NREP,2)
           KJREP = MULTB(KREP,JREP,2)
           LJREP = MULTB(LREP,JREP,2)
           ILREP = MULTB(IREP,LREP,2)
           IKREP = MULTB(IREP,KREP,2)
           ILKJ0 = OFF(KJREP) 
     &          + (OFF2(KREP,JREP)+K-1)*NPAIR(ILREP)
           ILKJ0 = ILKJ0 + OFF1(IREP,LREP) + (L-1)*IFIE(IREP)
           IKLJ0 = OFF(LJREP) 
     &          + (OFF2(LREP,JREP)+L-1)*NPAIR(IKREP)
           IKLJ0 = IKLJ0 + OFF1(IREP,KREP) + (K-1)*IFIE(IREP)
            DO J = 1, JFIE(JREP)
            IF (DOINV) THEN
             CALL XCOPY(IFIE(IREP),BUF1(ILKJ0*RCW+1),1,BUF2(IJKL),1)
             CALL XAXPY(IFIE(IREP),-A1,BUF1(IKLJ0*RCW+1),1,BUF2(IJKL),1)
            ELSE
             IF ((.NOT.FIRST).OR.(KJREP.EQ.1))
     &          CALL XCOPY(IFIE(IREP),BUF1(IJKL),1,BUF2(ILKJ0*RCW+1),1)
             IF ((.NOT.FIRST).OR.(LJREP.EQ.1)) THEN
                CALL XCOPY(IFIE(IREP),BUF1(IJKL),1,BUF2(IKLJ0*RCW+1),1)
                CALL XSCAL(IFIE(IREP),-A1,BUF2(IKLJ0*RCW+1),1)
             ENDIF
            ENDIF
            IJKL = IJKL + RCW * IFIE(IREP)
            ILKJ0 = ILKJ0 + NPAIR(ILREP) * KFIE(KREP)
            IKLJ0 = IKLJ0 + NPAIR(IKREP) * LFIE(LREP)
           ENDDO
 20       CONTINUE
         ENDDO
        ENDDO
 10    CONTINUE
      ENDDO
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE SRT1R1 (NREP,MULTB,DOINV,NPAIR1,KFIE,LFIE,NPAIR2,
     &                   OFF,OFF1,BUF1,BUF2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Sort array BUF1(IJ,KL:KLREP) to array -BUF2(IJ,LK:KLREP)
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 BUF1(*),BUF2(*)
      INTEGER NREP, MULTB(64,64,2)
      INTEGER NPAIR1(NREP),KFIE(NREP),LFIE(NREP),NPAIR2(NREP)
      INTEGER OFF(NREP+1),OFF1(32,32)
      LOGICAL DOINV
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
      INCLUDE "param.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      IJLKOFF = 0
      DO KLREP = 1, NREP
       IJREP = KLREP
       M = NPAIR1(IJREP)
       N = NPAIR2(KLREP)
       IJKL = OFF(KLREP) * RCW + 1
       IJLKOFF = OFF(KLREP)
       DO 10 LREP = 1, NREP
        KREP = MULTB(LREP,KLREP+NREP,2)
        NKS = KFIE(KREP)
        NLS = LFIE(LREP)
        DO L = 1, NLS
         DO K = 1, NKS
           IJLK = (IJLKOFF+(OFF1(LREP,KREP)+(K-1)*NLS+L-1)*M)*RCW + 1
           IF (DOINV) THEN
              CALL XCOPY (N,BUF1(IJLK),1,BUF2(IJKL),1)
           ELSE
              CALL XCOPY (N,BUF1(IJKL),1,BUF2(IJLK),1)
           ENDIF
           IJKL = IJKL + RCW
         ENDDO
        ENDDO
 10    CONTINUE
      ENDDO
C
      CALL XSCAL (OFF(NREP+1),-A1,BUF2,1)
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE SRT20D(NREP,MULTB,IJPAIR,IFIE,JFIE,KFIE,LFIE,
     &                  NPAIR,OFF,OFF1,OFF2,BUF1,NBUF1,T1,GV,USEDZ,
     &                  RIGHT)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Sort array BUF1(IJ,K>L:KLREP) to array BUF2(IK,LJ:LJREP=1)
C     Do contraction GV(LJ) = BUF2(IK,LJ) * T(LJ)
C     OR do contraction T(LJ) = BUF2*(IK,LJ) * GV(LJ)
C     Integrals are fetched from disk and BUF2 is never formed.
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      LOGICAL USEDZ,RIGHT
      INTEGER NBUF1
      REAL*8 BUF1(*),T1(*),GV(*)
      INTEGER NREP,MULTB(64,64,2),IJPAIR(NREP)
      INTEGER IFIE(NREP),JFIE(NREP),KFIE(NREP),LFIE(NREP),NPAIR(NREP)
      INTEGER OFF(NREP+1),OFF1(32,32),OFF2(32,32)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "complex.inc"
      INCLUDE "param.inc"
      INCLUDE "ccpar.inc"
C
C---------------Local variables--------------------------------------
C
      LOGICAL DONE
      REAL*8 TVAL(2)
      DATA TVAL /2*0.0D0/
C ** control variable for counting upwards to the value of KLSTAR
      INTEGER KCO
C
C---------------Executable code--------------------------------------
C
      DONE=.FALSE.
      DO 10 KLREP = 1, NREP
       IF (IJPAIR(KLREP).EQ.0) GOTO 10
       IJKL = 1
       KL = 0
       KCO = 0
C ** IJPAIR contains the length of the NVO batch
       KLMAX = NBUF1 / IJPAIR(KLREP)
       KLSTAR = 0
C
C ** KLINCR is updated on exit of GETVOVV. pure output parameter !
C
       IF (.NOT.USEDZ) THEN
          CALL GETVOVV (KLREP,KLSTAR,KLINCR,DONE,BUF1,KLMAX)
          CALL DELINT ('VOVV','KDDD',BUF1,KLREP,KLSTAR,KLINCR)
       ELSE
          CALL GETDZ   (KLREP,KLSTAR,KLINCR,DONE,BUF1)
       ENDIF
C
C      If we do a left hand contraction the integrals need to be
C      conjugated 
C
       IF (CARITH.AND..NOT.RIGHT)
     &    CALL CONJUGA (KLINCR*IJPAIR(KLREP),BUF1,1)
       DO 20 LREP = 1, NREP
        KREP = MULTB(LREP,KLREP+NREP,2)
        IF (KREP.LT.LREP) GOTO 20
        DO 30 L = 1, LFIE(LREP)
         KMIN = 1
         IF (KREP.EQ.LREP) KMIN = L + 1
         DO 40 K = KMIN, KFIE(KREP)
C
          IF(KCO.LT.KLSTAR) THEN
            KCO = KCO + 1
            GOTO 40
          ENDIF
C
          KL = KL + 1
C 
          IF (KL.EQ.KLINCR+1) THEN
C -----------------------
C We need the next buffer
C -----------------------
             KLSTAR = KLSTAR + KLINCR
             KLINCR = KLMAX
             IF (.NOT.USEDZ) THEN
                IF(.NOT.DONE) THEN
                  CALL GETVOVV (KLREP,KLSTAR,KLINCR,DONE,BUF1,KLMAX)
                  CALL DELINT ('VOVV','KDDD',BUF1,KLREP,KLSTAR,KLINCR)
                ELSE
                  GOTO 10
                ENDIF
             ELSE
                CALL GETDZ   (KLREP,KLSTAR,KLINCR,DONE,BUF1)
             ENDIF
             IF (CARITH.AND..NOT.RIGHT) 
     &          CALL CONJUGA (KLINCR*IJPAIR(KLREP),BUF1,1)
             KL = 1
             IJKL = 1
             KCO = KLSTAR
          ENDIF
C
          IF (RIGHT) THEN
C
             DO 50 JREP = 1, NREP
              IREP = MULTB(JREP,KLREP+NREP,2)
              LJREP = MULTB(LREP,JREP,2)
              KJREP = MULTB(KREP,JREP,2)
              LJ    = OFF2(LREP,JREP) + L - 1
              LJ    = LJ * RCW + 1
              KJ    = OFF2(KREP,JREP) + K - 1
              KJ    = KJ * RCW + 1
              IK0   = OFF1(IREP,KREP) + (K-1)*IFIE(IREP)
              IK0   = IK0 * RCW + 1
              IL0   = OFF1(IREP,LREP) + (L-1)*IFIE(IREP)
              IL0   = IL0 * RCW + 1
              DO 60 J = 1, JFIE(JREP)
               IF (LJREP.EQ.1) THEN
                  CALL XAXPY (IFIE(IREP),T1(LJ),BUF1(IJKL),1,GV(IK0),1)
               ENDIF
               IF (KJREP.EQ.1) THEN 
                  CALL XCOPY (1,T1(KJ),1,TVAL,1)
                  CALL XSCAL (1,-A1,TVAL,1)
                  CALL XAXPY (IFIE(IREP),TVAL,BUF1(IJKL),1,GV(IL0),1)
               ENDIF
               IJKL = IJKL + IFIE(IREP) * RCW
               LJ   = LJ   + LFIE(LREP) * RCW
               KJ   = KJ   + KFIE(KREP) * RCW
 60           CONTINUE
 50          CONTINUE
C
          ELSE
C
             DO 51 JREP = 1, NREP
              IREP = MULTB(JREP,KLREP+NREP,2)
              IKREP = MULTB(IREP,KREP,2)
              ILREP = MULTB(IREP,LREP,2)
              DO 61 I = 1, IFIE(IREP)
                 IK    = OFF1(IREP,KREP) + (K-1)*IFIE(IREP) + I - 1
                 IK    = IK * RCW + 1
                 IL    = OFF1(IREP,LREP) + (L-1)*IFIE(IREP) + I - 1
                 IL    = IL * RCW + 1
                 LJ0   = OFF2(LREP,JREP) + L - 1
                 LJ0   = LJ0 * RCW + 1
                 KJ0   = OFF2(KREP,JREP) + K - 1
                 KJ0   = KJ0 * RCW + 1
                 IJKL1 = IJKL + (I-1) * RCW 
                 IF (IKREP.EQ.1) THEN
                  CALL XAXPY (JFIE(JREP),GV(IK),BUF1(IJKL1),IFIE(IREP),
     &                        T1(LJ0),LFIE(LREP))
                 ENDIF
                 IF (ILREP.EQ.1) THEN 
                  CALL XCOPY (1,GV(IL),1,TVAL,1)
                  CALL XSCAL (1,-A1,TVAL,1)
                  CALL XAXPY (JFIE(JREP),TVAL,BUF1(IJKL1),IFIE(IREP),
     &                        T1(KJ0),KFIE(KREP))
                 ENDIF
 61           CONTINUE
              IJKL = IJKL + IFIE(IREP) * JFIE(JREP) * RCW
 51          CONTINUE
C
          ENDIF
C
 40      CONTINUE
 30     CONTINUE
 20    CONTINUE
 10   CONTINUE
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
