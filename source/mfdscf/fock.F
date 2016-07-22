C
C     =========================================================
      SUBROUTINE FOCK (IT,NT,ILAB,JLAB,KLAB,LLAB,RINT,
     +                 DAAR,DBAR,DAAI,DBAI,
     +                 FAAR,FBAR,FAAI,FBAI,F)
C     =========================================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     DISTRIBUTE ONE BLOCK OF INTEGRALS OVER THE SKELETON FOCK MATRIX
C
      INCLUDE 'paracom.inc'
      INCLUDE 'memory.inc'
C
      REAL*8 DAAR (*), DBAR (*)
      REAL*8 DAAI (*), DBAI (*)
      REAL*8 FAAR (*), FBAR (*)
      REAL*8 FAAI (*), FBAI (*)
      REAL*8 F(*)
C
      DIMENSION ILAB ( N2 ) , JLAB ( N2 ) , KLAB ( N2 ) , LLAB ( N2 ) ,
     +          FCAAR( N2 ) , FCABR( N2 ) , FCBAR( N2 ) , 
     +          FCAAI( N2 ) , FCABI( N2 ) , FCBAI( N2 ) , 
     +          RINT (N2), FC(18,N2)
C
      INTEGER IKL(N2),IIJ(N2),IIL(N2),IKJ(N2),IIK(N2),ILJ(N2),IJL(N2),
     +        IJK(N2)
      INTEGER IND(18,N2)
C
      NBAR = LENGTH
      NAAI = 2 * LENGTH
      NBAI = 3 * LENGTH
C
C ....  JUMP TO CODE FOR APPROPRIATE TYPE
C
        GOTO ( 100 , 200 , 300 , 400 , 500 , 600 , 700 ,
     +         800 , 900 , 1000 , 1100 , 1200 , 1300 , 1400 ) , IT
C
C===========================================================================
C
C                  CODE FOR THE DIFFERENT TYPES
C
C===========================================================================
C
C
C
C=============================================================================
C                             TYPE 1
C=============================================================================
C
  100 DO 110 I = 1, NT
      IIJ(I) = ILAB (I) * (ILAB (I) - 1) / 2 + JLAB (I)
C
      FCAAR (I) =   RINT (I) *   DAAR (IIJ(I))
      FCAAI (I) = - RINT (I) *   DAAI (IIJ(I))
      FCBAR (I) = - RINT (I) *   DBAR (IIJ(I))
      FCBAI (I) = - RINT (I) *   DBAI (IIJ(I))
C
      FAAR (IIJ(I)) = FAAR (IIJ(I)) + FCAAR (I)
      FAAI (IIJ(I)) = FAAI (IIJ(I)) + FCAAI (I)
      FBAR (IIJ(I)) = FBAR (IIJ(I)) + FCBAR (I)
      FBAI (IIJ(I)) = FBAI (IIJ(I)) + FCBAI (I)
C
 110  CONTINUE
      RETURN
C
C ==========================================================================
C                    TYPE 2
C ==========================================================================
C
  200 DO 210 I = 1, NT
      IIJ(I) = ILAB (I) * (ILAB (I) - 1) / 2 + JLAB (I)
      IIK(I) = ILAB (I) * (ILAB (I) - 1) / 2 + KLAB (I)
      IJL(I) = JLAB (I) * (JLAB (I) - 1) / 2 + LLAB (I)
C
      FCAAR (I) = 3.0D0 * RINT (I) * DAAR (IIJ(I))
      FCAAI (I) =         RINT (I) * DAAI (IIJ(I))
      FCBAR (I) =         RINT (I) * DBAR (IIJ(I))
      FCBAI (I) =         RINT (I) * DBAI (IIJ(I))
C
      FAAR (IIJ(I)) = FAAR (IIJ(I)) + FCAAR (I)
      FAAI (IIJ(I)) = FAAI (IIJ(I)) + FCAAI (I)
      FBAR (IIJ(I)) = FBAR (IIJ(I)) + FCBAR (I)
      FBAI (IIJ(I)) = FBAI (IIJ(I)) + FCBAI (I)
C
      FCAAR (I) = - RINT (I) *   DAAR (IIK(I))
      FCAAI (I) = - RINT (I) *   DAAI (IIK(I))
      FCBAR (I) = - RINT (I) *   DBAR (IIK(I))
      FCBAI (I) = - RINT (I) *   DBAI (IIK(I))
C
      FAAR (IJL(I)) = FAAR (IJL(I)) + FCAAR (I)
      FAAI (IJL(I)) = FAAI (IJL(I)) + FCAAI (I)
      FBAR (IJL(I)) = FBAR (IJL(I)) + FCBAR (I)
      FBAI (IJL(I)) = FBAI (IJL(I)) + FCBAI (I)
C
      FCAAR (I) = - RINT (I) *   DAAR (IJL(I))
      FCAAI (I) = - RINT (I) *   DAAI (IJL(I))
      FCBAR (I) = - RINT (I) *   DBAR (IJL(I))
      FCBAI (I) = - RINT (I) *   DBAI (IJL(I))
C
      FAAR (IIK(I)) = FAAR (IIK(I)) + FCAAR (I)
      FAAI (IIK(I)) = FAAI (IIK(I)) + FCAAI (I)
      FBAR (IIK(I)) = FBAR (IIK(I)) + FCBAR (I)
      FBAI (IIK(I)) = FBAI (IIK(I)) + FCBAI (I)
C
  210 CONTINUE
      RETURN
C
C
C=============================================================================
C                             TYPE 3
C=============================================================================
C
  300 DO 310 I = 1, NT
      IKL(I) = KLAB (I) * (KLAB (I) - 1) / 2 + LLAB (I)
      IIJ(I) = ILAB (I) * (ILAB (I) - 1) / 2 + JLAB (I)
      IIK(I) = ILAB (I) * (ILAB (I) - 1) / 2 + KLAB (I)
C
      FCAAR (I) =   RINT (I) * 2.0D0 * DAAR (IKL(I))
C
      FAAR (IIJ(I)) = FAAR (IIJ(I)) + FCAAR (I)
C
      FCAAR (I) =   RINT (I) * 2.0D0 * DAAR (IIJ(I))
C
      FAAR (IKL(I)) = FAAR (IKL(I)) + FCAAR (I)
C
      FCAAR (I) = - RINT (I) *   DAAR (IIK(I))
      FCAAI (I) = - RINT (I) *   DAAI (IIK(I))
      FCBAR (I) = - RINT (I) *   DBAR (IIK(I))
      FCBAI (I) = - RINT (I) *   DBAI (IIK(I))
C
      FAAR (IIK(I)) = FAAR (IIK(I)) + FCAAR (I)
      FAAI (IIK(I)) = FAAI (IIK(I)) + FCAAI (I)
      FBAR (IIK(I)) = FBAR (IIK(I)) + FCBAR (I)
      FBAI (IIK(I)) = FBAI (IIK(I)) + FCBAI (I)
C
  310 CONTINUE
      RETURN
C
C
C=============================================================================
C                             TYPE 4
C=============================================================================
C
  400 DO 410 I = 1, NT
      IKL(I) = KLAB (I) * (KLAB (I) - 1) / 2 + LLAB (I)
      IIJ(I) = ILAB (I) * (ILAB (I) - 1) / 2 + JLAB (I)
C
      FCAAR (I) =   RINT (I) * 2.0D0 * DAAR (IKL(I))
C
      FAAR (IIJ(I)) = FAAR (IIJ(I)) + FCAAR (I)
C
      FCABR (I) = - RINT (I) *   DBAR (IIJ(I))
      FCABI (I) =   RINT (I) *   DBAI (IIJ(I))
      FCAAR (I) =   RINT (I) *   DAAR (IIJ(I))
      FCAAI (I) = - RINT (I) *   DAAI (IIJ(I))
C
      FAAR (IKL(I)) = FAAR (IKL(I)) + FCAAR (I)
      FAAI (IKL(I)) = FAAI (IKL(I)) + FCAAI (I)
      FBAR (IKL(I)) = FBAR (IKL(I)) + FCABR (I)
      FBAI (IKL(I)) = FBAI (IKL(I)) - FCABI (I)
C
  410 CONTINUE
      RETURN
C
C
C=============================================================================
C                             TYPE 5
C=============================================================================
C
  500 DO 510 I = 1, NT
      IKL(I) = KLAB (I) * (KLAB (I) - 1) / 2 + LLAB (I)
      IIJ(I) = ILAB (I) * (ILAB (I) - 1) / 2 + JLAB (I)
      IIL(I) = ILAB (I) * (ILAB (I) - 1) / 2 + LLAB (I)
      IJK(I) = JLAB (I) * (JLAB (I) - 1) / 2 + KLAB (I)
C
      FCAAR (I) =   RINT (I) * DAAR (IKL(I)) * 4.0D0
C
      FAAR (IIJ(I)) = FAAR (IIJ(I)) + FCAAR (I)
C
      FCAAR (I) =   RINT (I) * DAAR (IIJ(I)) * 2.0D0
C
      FAAR (IKL(I)) = FAAR (IKL(I)) + FCAAR (I)
C
      FCAAR (I) = - RINT (I) *   DAAR (IJK(I))
      FCAAI (I) = - RINT (I) *   DAAI (IJK(I))
      FCBAR (I) = - RINT (I) *   DBAR (IJK(I))
      FCBAI (I) = - RINT (I) *   DBAI (IJK(I))
C
      FAAR (IIL(I)) = FAAR (IIL(I)) + FCAAR (I)
      FAAI (IIL(I)) = FAAI (IIL(I)) + FCAAI (I)
      FBAR (IIL(I)) = FBAR (IIL(I)) + FCBAR (I)
      FBAI (IIL(I)) = FBAI (IIL(I)) + FCBAI (I)
C
      FCAAR (I) = - RINT (I) *   DAAR (IIL(I))
      FCAAI (I) = - RINT (I) *   DAAI (IIL(I))
      FCBAR (I) = - RINT (I) *   DBAR (IIL(I))
      FCBAI (I) = - RINT (I) *   DBAI (IIL(I))
C
      FAAR (IJK(I)) = FAAR (IJK(I)) + FCAAR (I)
      FAAI (IJK(I)) = FAAI (IJK(I)) + FCAAI (I)
      FBAR (IJK(I)) = FBAR (IJK(I)) + FCBAR (I)
      FBAI (IJK(I)) = FBAI (IJK(I)) + FCBAI (I)
C
  510 CONTINUE
      RETURN
C
C
C=============================================================================
C                             TYPE 6
C=============================================================================
C
  600 DO 610 I = 1, NT
      IKL(I) = KLAB (I) * (KLAB (I) - 1) / 2 + LLAB (I)
      IIJ(I) = ILAB (I) * (ILAB (I) - 1) / 2 + JLAB (I)
C
      FCAAR (I) =   RINT (I) *   DAAR (IIJ(I)) * 2.0D0
C
      FAAR (IKL(I)) = FAAR (IKL(I)) + FCAAR (I)
C
      FCABR (I) = - RINT (I) *   DBAR (IKL(I))
      FCABI (I) =   RINT (I) *   DBAI (IKL(I))
      FCAAR (I) =   RINT (I) *   DAAR (IKL(I))
      FCAAI (I) = - RINT (I) *   DAAI (IKL(I))
C
      FAAR (IIJ(I)) = FAAR (IIJ(I)) + FCAAR (I)
      FAAI (IIJ(I)) = FAAI (IIJ(I)) + FCAAI (I)
      FBAR (IIJ(I)) = FBAR (IIJ(I)) + FCABR (I)
      FBAI (IIJ(I)) = FBAI (IIJ(I)) - FCABI (I)
C
  610 CONTINUE
      RETURN
C
C
C=============================================================================
C                             TYPE 7
C=============================================================================
C
  700 DO 710 I = 1, NT
      IKL(I) = KLAB (I) * (KLAB (I) - 1) / 2 + LLAB (I)
      IIJ(I) = ILAB (I) * (ILAB (I) - 1) / 2 + JLAB (I)
      IJL(I) = JLAB (I) * (JLAB (I) - 1) / 2 + LLAB (I)
      IIK(I) = ILAB (I) * (ILAB (I) - 1) / 2 + KLAB (I)
C
      FCAAR (I) =   RINT (I) *  DAAR (IIJ(I)) * 4.0D0
C
      FAAR (IKL(I)) = FAAR (IKL(I)) + FCAAR (I)
C
      FCAAR (I) =   RINT (I) *  DAAR (IKL(I)) * 2.0D0
C
      FAAR (IIJ(I)) = FAAR (IIJ(I)) + FCAAR (I)
C
      FCAAR (I) = - RINT (I) *   DAAR (IJL(I))
      FCAAI (I) = - RINT (I) *   DAAI (IJL(I))
      FCBAR (I) = - RINT (I) *   DBAR (IJL(I))
      FCBAI (I) = - RINT (I) *   DBAI (IJL(I))
C
      FAAR (IIK(I)) = FAAR (IIK(I)) + FCAAR (I)
      FAAI (IIK(I)) = FAAI (IIK(I)) + FCAAI (I)
      FBAR (IIK(I)) = FBAR (IIK(I)) + FCBAR (I)
      FBAI (IIK(I)) = FBAI (IIK(I)) + FCBAI (I)
C
      FCAAR (I) = - RINT (I) *   DAAR (IIK(I))
      FCAAI (I) = - RINT (I) *   DAAI (IIK(I))
      FCBAR (I) = - RINT (I) *   DBAR (IIK(I))
      FCBAI (I) = - RINT (I) *   DBAI (IIK(I))
C
      FAAR (IJL(I)) = FAAR (IJL(I)) + FCAAR (I)
      FAAI (IJL(I)) = FAAI (IJL(I)) + FCAAI (I)
      FBAR (IJL(I)) = FBAR (IJL(I)) + FCBAR (I)
      FBAI (IJL(I)) = FBAI (IJL(I)) + FCBAI (I)
C
  710 CONTINUE
      RETURN
C
C
C=============================================================================
C                             TYPE 8
C=============================================================================
C
  800 DO 810 I = 1, NT
      IKL(I) = KLAB (I) * (KLAB (I) - 1) / 2 + LLAB (I)
      IIJ(I) = ILAB (I) * (ILAB (I) - 1) / 2 + JLAB (I)
      ILJ(I) = LLAB (I) * (LLAB (I) - 1) / 2 + JLAB (I)
      IIK(I) = ILAB (I) * (ILAB (I) - 1) / 2 + KLAB (I)
C
      FCAAR (I) =   RINT (I) * DAAR (IIJ(I)) * 4.0D0
C
      FAAR (IKL(I)) = FAAR (IKL(I)) + FCAAR (I)
C
      FCAAR (I) =   RINT (I) * DAAR (IKL(I)) * 2.0D0
C
      FAAR (IIJ(I)) = FAAR (IIJ(I)) + FCAAR (I)
C
      FCAAR (I) = - RINT (I) *   DAAR (ILJ(I))
      FCAAI (I) =   RINT (I) *   DAAI (ILJ(I))
      FCBAR (I) =   RINT (I) *   DBAR (ILJ(I))
      FCBAI (I) =   RINT (I) *   DBAI (ILJ(I))
C
      FAAR (IIK(I)) = FAAR (IIK(I)) + FCAAR (I)
      FAAI (IIK(I)) = FAAI (IIK(I)) + FCAAI (I)
      FBAR (IIK(I)) = FBAR (IIK(I)) + FCBAR (I)
      FBAI (IIK(I)) = FBAI (IIK(I)) + FCBAI (I)
C
      FCAAR (I) = - RINT (I) *   DAAR (IIK(I))
      FCAAI (I) =   RINT (I) *   DAAI (IIK(I))
      FCBAR (I) =   RINT (I) *   DBAR (IIK(I))
      FCBAI (I) =   RINT (I) *   DBAI (IIK(I))
C
      FAAR (ILJ(I)) = FAAR (ILJ(I)) + FCAAR (I)
      FAAI (ILJ(I)) = FAAI (ILJ(I)) + FCAAI (I)
      FBAR (ILJ(I)) = FBAR (ILJ(I)) + FCBAR (I)
      FBAI (ILJ(I)) = FBAI (ILJ(I)) + FCBAI (I)
C
  810 CONTINUE
      RETURN
C
C
C=============================================================================
C                             TYPE 9
C=============================================================================
C
  900 DO 910 I = 1, NT
      IKL(I) = KLAB (I) * (KLAB (I) - 1) / 2 + LLAB (I)
      IIJ(I) = ILAB (I) * (ILAB (I) - 1) / 2 + JLAB (I)
      IIL(I) = ILAB (I) * (ILAB (I) - 1) / 2 + LLAB (I)
      IJK(I) = JLAB (I) * (JLAB (I) - 1) / 2 + KLAB (I)
C
      FCAAR (I) =   RINT (I) *  DAAR (IKL(I)) * 3.0D0
      FCAAI (I) = - RINT (I) *  DAAI (IKL(I))
      FCBAR (I) = - RINT (I) *  DBAR (IKL(I))
      FCBAI (I) = - RINT (I) *  DBAI (IKL(I))
C
      FAAR (IIJ(I)) = FAAR (IIJ(I)) + FCAAR (I)
      FAAI (IIJ(I)) = FAAI (IIJ(I)) + FCAAI (I)
      FBAR (IIJ(I)) = FBAR (IIJ(I)) + FCBAR (I)
      FBAI (IIJ(I)) = FBAI (IIJ(I)) + FCBAI (I)
C
      FCAAR (I) =   RINT (I) *   DAAR (IIJ(I)) * 3.0D0
      FCAAI (I) = - RINT (I) *   DAAI (IIJ(I))
      FCBAR (I) = - RINT (I) *   DBAR (IIJ(I))
      FCBAI (I) = - RINT (I) *   DBAI (IIJ(I))
C
      FAAR (IKL(I)) = FAAR (IKL(I)) + FCAAR (I)
      FAAI (IKL(I)) = FAAI (IKL(I)) + FCAAI (I)
      FBAR (IKL(I)) = FBAR (IKL(I)) + FCBAR (I)
      FBAI (IKL(I)) = FBAI (IKL(I)) + FCBAI (I)
C
      FCAAR (I) = - RINT (I) *   DAAR (IJK(I))
      FCAAI (I) =   RINT (I) *   DAAI (IJK(I))
      FCABR (I) = - RINT (I) *   DBAR (IJK(I))
      FCABI (I) =   RINT (I) *   DBAI (IJK(I))
C
      FAAR (IIL(I)) = FAAR (IIL(I)) + FCAAR (I)
      FAAI (IIL(I)) = FAAI (IIL(I)) + FCAAI (I)
      FBAR (IIL(I)) = FBAR (IIL(I)) + FCABR (I)
      FBAI (IIL(I)) = FBAI (IIL(I)) - FCABI (I)
C
      FCAAR (I) = - RINT (I) *   DAAR (IIL(I)) * 2.0D0
C
      FAAR (IJK(I)) = FAAR (IJK(I)) + FCAAR (I)
C
  910 CONTINUE
      RETURN
C
C
C=============================================================================
C                             TYPE 10
C=============================================================================
C
 1000 DO 1010 I = 1, NT
      IKL(I) = KLAB (I) * (KLAB (I) - 1) / 2 + LLAB (I)
      IIJ(I) = ILAB (I) * (ILAB (I) - 1) / 2 + JLAB (I)
      IIK(I) = ILAB (I) * (ILAB (I) - 1) / 2 + KLAB (I)
      IJL(I) = JLAB (I) * (JLAB (I) - 1) / 2 + LLAB (I)
C
      FCAAR (I) =   RINT (I) *   DAAR (IKL(I)) * 3.0D0
      FCAAI (I) =   RINT (I) *   DAAI (IKL(I))
      FCBAR (I) =   RINT (I) *   DBAR (IKL(I))
      FCBAI (I) =   RINT (I) *   DBAI (IKL(I))
C
      FAAR (IIJ(I)) = FAAR (IIJ(I)) + FCAAR (I)
      FAAI (IIJ(I)) = FAAI (IIJ(I)) + FCAAI (I)
      FBAR (IIJ(I)) = FBAR (IIJ(I)) + FCBAR (I)
      FBAI (IIJ(I)) = FBAI (IIJ(I)) + FCBAI (I)
C
      FCAAR (I) =   RINT (I) *   DAAR (IIJ(I)) * 3.0D0
      FCAAI (I) =   RINT (I) *   DAAI (IIJ(I))
      FCBAR (I) =   RINT (I) *   DBAR (IIJ(I))
      FCBAI (I) =   RINT (I) *   DBAI (IIJ(I))
C
      FAAR (IKL(I)) = FAAR (IKL(I)) + FCAAR (I)
      FAAI (IKL(I)) = FAAI (IKL(I)) + FCAAI (I)
      FBAR (IKL(I)) = FBAR (IKL(I)) + FCBAR (I)
      FBAI (IKL(I)) = FBAI (IKL(I)) + FCBAI (I)
C
      FCAAR (I) = - RINT (I) *   DAAR (IJL(I)) * 2.0D0
C
      FAAR (IIK(I)) = FAAR (IIK(I)) + FCAAR (I)
C
      FCAAR (I) = - RINT (I) *   DAAR (IIK(I))
      FCAAI (I) = - RINT (I) *   DAAI (IIK(I))
      FCABR (I) = - RINT (I) *   DBAR (IIK(I))
      FCABI (I) =   RINT (I) *   DBAI (IIK(I))
C
      FAAR (IJL(I)) = FAAR (IJL(I)) + FCAAR (I)
      FAAI (IJL(I)) = FAAI (IJL(I)) + FCAAI (I)
      FBAR (IJL(I)) = FBAR (IJL(I)) + FCABR (I)
      FBAI (IJL(I)) = FBAI (IJL(I)) - FCABI (I)
C
 1010 CONTINUE
      RETURN
C
C
C=============================================================================
C                             TYPE 11
C=============================================================================
C
 1100 DO 1110 I = 1, NT
      IKL(I) = KLAB (I) * (KLAB (I) - 1) / 2 + LLAB (I)
      IIJ(I) = ILAB (I) * (ILAB (I) - 1) / 2 + JLAB (I)
      IIK(I) = ILAB (I) * (ILAB (I) - 1) / 2 + KLAB (I)
      IJL(I) = JLAB (I) * (JLAB (I) - 1) / 2 + LLAB (I)
C
      FCAAR (I) =   RINT (I) *   DAAR (IKL(I)) * 3.0D0
      FCAAI (I) =   RINT (I) *   DAAI (IKL(I))
      FCBAR (I) =   RINT (I) *   DBAR (IKL(I))
      FCBAI (I) =   RINT (I) *   DBAI (IKL(I))
C
      FAAR (IIJ(I)) = FAAR (IIJ(I)) + FCAAR (I)
      FAAI (IIJ(I)) = FAAI (IIJ(I)) + FCAAI (I)
      FBAR (IIJ(I)) = FBAR (IIJ(I)) + FCBAR (I)
      FBAI (IIJ(I)) = FBAI (IIJ(I)) + FCBAI (I)
C
      FCAAR (I) =   RINT (I) *   DAAR (IIJ(I)) * 3.0D0
      FCAAI (I) =   RINT (I) *   DAAI (IIJ(I))
      FCBAR (I) =   RINT (I) *   DBAR (IIJ(I))
      FCBAI (I) =   RINT (I) *   DBAI (IIJ(I))
C
      FAAR (IKL(I)) = FAAR (IKL(I)) + FCAAR (I)
      FAAI (IKL(I)) = FAAI (IKL(I)) + FCAAI (I)
      FBAR (IKL(I)) = FBAR (IKL(I)) + FCBAR (I)
      FBAI (IKL(I)) = FBAI (IKL(I)) + FCBAI (I)
C
      FCAAR (I) = - RINT (I) *   DAAR (IJL(I))
      FCAAI (I) = - RINT (I) *   DAAI (IJL(I))
      FCABR (I) = - RINT (I) *   DBAR (IJL(I))
      FCABI (I) =   RINT (I) *   DBAI (IJL(I))
C
      FAAR (IIK(I)) = FAAR (IIK(I)) + FCAAR (I)
      FAAI (IIK(I)) = FAAI (IIK(I)) + FCAAI (I)
      FBAR (IIK(I)) = FBAR (IIK(I)) + FCABR (I)
      FBAI (IIK(I)) = FBAI (IIK(I)) - FCABI (I)
C
      FCAAR (I) = - RINT (I) *   DAAR (IIK(I)) * 2.0D0
C
      FAAR (IJL(I)) = FAAR (IJL(I)) + FCAAR (I)
C
 1110 CONTINUE
      RETURN
C
C
C=============================================================================
C                             TYPE 12
C=============================================================================
C
 1200 DO 1210 I = 1, NT
      IKL(I) = KLAB (I) * (KLAB (I) - 1) / 2 + LLAB (I)
      IIJ(I) = ILAB (I) * (ILAB (I) - 1) / 2 + JLAB (I)
      IIL(I) = ILAB (I) * (ILAB (I) - 1) / 2 + LLAB (I)
      IJK(I) = JLAB (I) * (JLAB (I) - 1) / 2 + KLAB (I)
      IIK(I) = ILAB (I) * (ILAB (I) - 1) / 2 + KLAB (I)
      IJL(I) = JLAB (I) * (JLAB (I) - 1) / 2 + LLAB (I)
C
      FC (1,I)  =   RINT (I) *  DAAR (IKL(I)) * 4.0D0
      FC (2,I)  =   RINT (I) *  DAAR (IIJ(I)) * 4.0D0
      FC (3,I)  = - RINT (I) *   DAAR (IJK(I))
      FC (4,I)  = - RINT (I) *   DAAI (IJK(I))
      FC (5,I)  = - RINT (I) *   DBAR (IJK(I))
      FC (6,I)  = - RINT (I) *   DBAI (IJK(I))
      FC (7,I)  = - RINT (I) *   DAAR (IIL(I))
      FC (8,I)  = - RINT (I) *   DAAI (IIL(I))
      FC (9,I)  = - RINT (I) *   DBAR (IIL(I))
      FC (10,I) = - RINT (I) *   DBAI (IIL(I))
      FC (11,I) = - RINT (I) *   DAAR (IJL(I))
      FC (12,I) = - RINT (I) *   DAAI (IJL(I))
      FC (13,I) = - RINT (I) *   DBAR (IJL(I))
      FC (14,I) = - RINT (I) *   DBAI (IJL(I))
      FC (15,I) = - RINT (I) *   DAAR (IIK(I))
      FC (16,I) = - RINT (I) *   DAAI (IIK(I))
      FC (17,I) = - RINT (I) *   DBAR (IIK(I))
      FC (18,I) = - RINT (I) *   DBAI (IIK(I))
C
        IND(1,I)=IIJ(I)
        IND(2,I)=IKL(I)
        IND(3,I)=IIL(I)
        IND(4,I)=NAAI+IIL(I)
        IND(5,I)=NBAR+IIL(I)
        IND(6,I)=NBAI+IIL(I)
        IND(7,I)=IJK(I)
        IND(8,I)=NAAI+IJK(I)
        IND(9,I)=NBAR+IJK(I)
        IND(10,I)=NBAI+IJK(I)
        IND(11,I)=IIK(I)
        IND(12,I)=NAAI+IIK(I)
        IND(13,I)=NBAR+IIK(I)
        IND(14,I)=NBAI+IIK(I)
        IND(15,I)=IJL(I)
        IND(16,I)=NAAI+IJL(I)
        IND(17,I)=NBAR+IJL(I)
        IND(18,I)=NBAI+IJL(I)
 1210   CONTINUE
C
      DO 1230 I=1,NT
C
C$DIR NO_RECURRENCE
CDIR$ IVDEP
        DO 1240 J=1,18
          F(IND(J,I))=F(IND(J,I))+FC(J,I)
 1240     CONTINUE
 1230   CONTINUE
      RETURN
C
C
C=============================================================================
C                             TYPE 13
C=============================================================================
C
 1300 DO 1310 I = 1, NT
      IKL(I) = KLAB (I) * (KLAB (I) - 1) / 2 + LLAB (I)
      IIJ(I) = ILAB (I) * (ILAB (I) - 1) / 2 + JLAB (I)
      IIL(I) = ILAB (I) * (ILAB (I) - 1) / 2 + LLAB (I)
      IKJ(I) = KLAB (I) * (KLAB (I) - 1) / 2 + JLAB (I)
      IIK(I) = ILAB (I) * (ILAB (I) - 1) / 2 + KLAB (I)
      IJL(I) = JLAB (I) * (JLAB (I) - 1) / 2 + LLAB (I)
C
      FC (1,I) =    RINT (I) *  DAAR (IKL(I)) * 4.0D0
      FC (2,I) =    RINT (I) *  DAAR (IIJ(I)) * 4.0D0
      FC (3,I)  = - RINT (I) *   DAAR (IKJ(I))
      FC (4,I)  =   RINT (I) *   DAAI (IKJ(I))
      FC (5,I)  =   RINT (I) *   DBAR (IKJ(I))
      FC (6,I)  =   RINT (I) *   DBAI (IKJ(I))
      FC (7,I)  = - RINT (I) *   DAAR (IIL(I))
      FC (8,I)  =   RINT (I) *   DAAI (IIL(I))
      FC (9,I)  =   RINT (I) *   DBAR (IIL(I))
      FC (10,I) =   RINT (I) *   DBAI (IIL(I))
      FC (11,I) = - RINT (I) *   DAAR (IJL(I))
      FC (12,I) = - RINT (I) *   DAAI (IJL(I))
      FC (13,I) = - RINT (I) *   DBAR (IJL(I))
      FC (14,I) = - RINT (I) *   DBAI (IJL(I))
      FC (15,I) = - RINT (I) *   DAAR (IIK(I))
      FC (16,I) = - RINT (I) *   DAAI (IIK(I))
      FC (17,I) = - RINT (I) *   DBAR (IIK(I))
      FC (18,I) = - RINT (I) *   DBAI (IIK(I))
C
        IND(1,I)=IIJ(I)
        IND(2,I)=IKL(I)
        IND(3,I)=IIL(I)
        IND(4,I)=NAAI+IIL(I)
        IND(5,I)=NBAR+IIL(I)
        IND(6,I)=NBAI+IIL(I)
        IND(7,I)=IKJ(I)
        IND(8,I)=NAAI+IKJ(I)
        IND(9,I)=NBAR+IKJ(I)
        IND(10,I)=NBAI+IKJ(I)
        IND(11,I)=IIK(I)
        IND(12,I)=NAAI+IIK(I)
        IND(13,I)=NBAR+IIK(I)
        IND(14,I)=NBAI+IIK(I)
        IND(15,I)=IJL(I)
        IND(16,I)=NAAI+IJL(I)
        IND(17,I)=NBAR+IJL(I)
        IND(18,I)=NBAI+IJL(I)
 1310   CONTINUE
C
      DO 1330 I=1,NT
C
C$DIR NO_RECURRENCE
CDIR$ IVDEP
        DO 1340 J=1,18
          F(IND(J,I))=F(IND(J,I))+FC(J,I)
 1340     CONTINUE
 1330   CONTINUE
C
      RETURN
C
C
C=============================================================================
C                             TYPE 14
C=============================================================================
C
 1400 DO 1410 I=1,NT
      IKL(I) = KLAB (I) * (KLAB (I) - 1) / 2 + LLAB (I)
      IIJ(I) = ILAB (I) * (ILAB (I) - 1) / 2 + JLAB (I)
      IIL(I) = ILAB (I) * (ILAB (I) - 1) / 2 + LLAB (I)
      IKJ(I) = KLAB (I) * (KLAB (I) - 1) / 2 + JLAB (I)
      IIK(I) = ILAB (I) * (ILAB (I) - 1) / 2 + KLAB (I)
      ILJ(I) = LLAB (I) * (LLAB (I) - 1) / 2 + JLAB (I)
C
      FC (1,I)  =   RINT (I) *  DAAR (IKL(I)) * 4.0D0
      FC (2,I)  =   RINT (I) *  DAAR (IIJ(I)) * 4.0D0
      FC (3,I)  = - RINT (I) *   DAAR (IKJ(I))
      FC (4,I)  =   RINT (I) *   DAAI (IKJ(I))
      FC (5,I)  =   RINT (I) *   DBAR (IKJ(I))
      FC (6,I)  =   RINT (I) *   DBAI (IKJ(I))
      FC (7,I)  = - RINT (I) *   DAAR (IIL(I))
      FC (8,I)  =   RINT (I) *   DAAI (IIL(I))
      FC (9,I)  =   RINT (I) *   DBAR (IIL(I))
      FC (10,I) =   RINT (I) *   DBAI (IIL(I))
      FC (11,I) = - RINT (I) *   DAAR (ILJ(I))
      FC (12,I) =   RINT (I) *   DAAI (ILJ(I))
      FC (13,I) =   RINT (I) *   DBAR (ILJ(I))
      FC (14,I) =   RINT (I) *   DBAI (ILJ(I))
      FC (15,I) = - RINT (I) *   DAAR (IIK(I))
      FC (16,I) =   RINT (I) *   DAAI (IIK(I))
      FC (17,I) =   RINT (I) *   DBAR (IIK(I))
      FC (18,I) =   RINT (I) *   DBAI (IIK(I))
C
        IND(1,I)=IIJ(I)
        IND(2,I)=IKL(I)
        IND(3,I)=IIL(I)
        IND(4,I)=NAAI+IIL(I)
        IND(5,I)=NBAR+IIL(I)
        IND(6,I)=NBAI+IIL(I)
        IND(7,I)=IKJ(I)
        IND(8,I)=NAAI+IKJ(I)
        IND(9,I)=NBAR+IKJ(I)
        IND(10,I)=NBAI+IKJ(I)
        IND(11,I)=IIK(I)
        IND(12,I)=NAAI+IIK(I)
        IND(13,I)=NBAR+IIK(I)
        IND(14,I)=NBAI+IIK(I)
        IND(15,I)=ILJ(I)
        IND(16,I)=NAAI+ILJ(I)
        IND(17,I)=NBAR+ILJ(I)
        IND(18,I)=NBAI+ILJ(I)
 1410   CONTINUE
C
      DO 1430 I=1,NT
C
C$DIR NO_RECURRENCE
CDIR$ IVDEP
        DO 1440 J=1,18
          F(IND(J,I))=F(IND(J,I))+FC(J,I)
 1440     CONTINUE
 1430   CONTINUE
C
      RETURN
C
C=============================================================================
C                        END OF CODE FOR DIFFERENT TYPES
C=============================================================================
C
C
      END
