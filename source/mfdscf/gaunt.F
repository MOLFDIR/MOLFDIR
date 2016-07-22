C
C     ==========================================================
      SUBROUTINE GAUNT (IT,NT,ILAB,JLAB,KLAB,LLAB,RINT,
     +                  DAAR,DBAR,DAAI,DBAI,
     +                  FAAR,FBAR,FAAI,FBAI)
C     ==========================================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     DISTRIBUTE ONE BLOCK OF INTEGRALS OVER THE SKELETON BREIT MATRIX
C
      INCLUDE 'paracom.inc'
C
      REAL*8 DAAR (*), DBAR (*)
      REAL*8 DAAI (*), DBAI (*)
      REAL*8 FAAR (*), FBAR (*)
      REAL*8 FAAI (*), FBAI (*)
C
      DIMENSION ILAB ( N2 ) , JLAB ( N2 ) , KLAB ( N2 ) , LLAB ( N2 ) ,
     +          FCAAR( N2 ) , FCABR( N2 ) , FCBAR( N2 ) , 
     +          FCAAI( N2 ) , FCABI( N2 ) , FCBAI( N2 ) , 
     +          RINT (N2)
C
C ....  JUMP TO CODE FOR APPROPRIATE TYPE
C
      GOTO ( 100 , 200 , 100 , 100 , 100 , 100 , 100 ,
     +       100 , 100 , 1000 , 1100 , 100 , 1300 , 1400 ) , IT
C
  100 CALL Q2JOB(6,'INVALID TYPE IN BREIT',' ',0)
C
C===========================================================================
C
C                  CODE FOR THE DIFFERENT TYPES
C
C===========================================================================
C
C
C ==========================================================================
C                    TYPE 2   (I = K > J = L)
C ==========================================================================
C
  200 DO 210 I = 1, NT
      IIJ = ILAB (I) * (ILAB (I) - 1) / 2 + JLAB (I)
      IIK = ILAB (I) * (ILAB (I) - 1) / 2 + KLAB (I)
      IJL = JLAB (I) * (JLAB (I) - 1) / 2 + LLAB (I)
C
      FCAAR (I) = - RINT (I) * DAAR (IIJ) * 3.0D0
      FCAAI (I) = - RINT (I) * DAAI (IIJ)
      FCBAR (I) = - RINT (I) * DBAR (IIJ)
      FCBAI (I) = - RINT (I) * DBAI (IIJ)
C
      FAAR (IIJ) = FAAR (IIJ) - FCAAR (I)
      FAAI (IIJ) = FAAI (IIJ) - FCAAI (I)
      FBAR (IIJ) = FBAR (IIJ) - FCBAR (I)
      FBAI (IIJ) = FBAI (IIJ) - FCBAI (I)
C
      FCAAR (I) = - RINT (I) *  DAAR (IIK) * 3.0D0
      FCAAI (I) =   RINT (I) *  DAAI (IIK)
      FCBAR (I) =   RINT (I) *  DBAR (IIK)
      FCBAI (I) =   RINT (I) *  DBAI (IIK)
C
      FAAR (IJL) = FAAR (IJL) - FCAAR (I)
      FAAI (IJL) = FAAI (IJL) - FCAAI (I)
      FBAR (IJL) = FBAR (IJL) - FCBAR (I)
      FBAI (IJL) = FBAI (IJL) - FCBAI (I)
C
      FCAAR (I) = - RINT (I) *  DAAR (IJL) * 3.0D0
      FCAAI (I) =   RINT (I) *  DAAI (IJL)
      FCBAR (I) =   RINT (I) *  DBAR (IJL)
      FCBAI (I) =   RINT (I) *  DBAI (IJL)
C
      FAAR (IIK) = FAAR (IIK) - FCAAR (I)
      FAAI (IIK) = FAAI (IIK) - FCAAI (I)
      FBAR (IIK) = FBAR (IIK) - FCBAR (I)
      FBAI (IIK) = FBAI (IIK) - FCBAI (I)
C
  210 CONTINUE
      RETURN
C
C
C=============================================================================
C                 TYPE 10   (I = K > J > L)
C=============================================================================
C
 1000 DO 1010 I = 1, NT
      IKL = KLAB (I) * (KLAB (I) - 1) / 2 + LLAB (I)
      IIJ = ILAB (I) * (ILAB (I) - 1) / 2 + JLAB (I)
      IIK = ILAB (I) * (ILAB (I) - 1) / 2 + KLAB (I)
      IJL = JLAB (I) * (JLAB (I) - 1) / 2 + LLAB (I)
C
      FCAAR (I) = - RINT (I) * DAAR (IKL) * 3.0D0
      FCAAI (I) = - RINT (I) * DAAI (IKL)
      FCBAR (I) = - RINT (I) * DBAR (IKL)
      FCBAI (I) = - RINT (I) * DBAI (IKL)
C
      FAAR (IIJ) = FAAR (IIJ) - FCAAR (I)
      FAAI (IIJ) = FAAI (IIJ) - FCAAI (I)
      FBAR (IIJ) = FBAR (IIJ) - FCBAR (I)
      FBAI (IIJ) = FBAI (IIJ) - FCBAI (I)
C
      FCAAR (I) = - RINT (I) * DAAR (IIJ) * 3.0D0
      FCAAI (I) = - RINT (I) * DAAI (IIJ)
      FCBAR (I) = - RINT (I) * DBAR (IIJ)
      FCBAI (I) = - RINT (I) * DBAI (IIJ)
C
      FAAR (IKL) = FAAR (IKL) - FCAAR (I)
      FAAI (IKL) = FAAI (IKL) - FCAAI (I)
      FBAR (IKL) = FBAR (IKL) - FCBAR (I)
      FBAI (IKL) = FBAI (IKL) - FCBAI (I)
C
      FCAAR (I) = - RINT (I) * DAAR (IJL) * 6.0D0
C
      FAAR (IIK) = FAAR (IIK) - FCAAR (I)
C
      FCAAR (I) = - RINT (I) *  DAAR (IIK) * 3.0D0
      FCAAI (I) =   RINT (I) *  DAAI (IIK)
      FCABR (I) = - RINT (I) *  DBAR (IIK)
      FCABI (I) =   RINT (I) *  DBAI (IIK)
C
      FAAR (IJL) = FAAR (IJL) - FCAAR (I)
      FAAI (IJL) = FAAI (IJL) - FCAAI (I)
      FBAR (IJL) = FBAR (IJL) - FCABR (I)
      FBAI (IJL) = FBAI (IJL) + FCABI (I)
C
 1010 CONTINUE
      RETURN
C
C
C=============================================================================
C                   TYPE 11   (I > K > J = L)
C=============================================================================
C
 1100 DO 1110 I = 1, NT
      IKL = KLAB (I) * (KLAB (I) - 1) / 2 + LLAB (I)
      IIJ = ILAB (I) * (ILAB (I) - 1) / 2 + JLAB (I)
      IIK = ILAB (I) * (ILAB (I) - 1) / 2 + KLAB (I)
      IJL = JLAB (I) * (JLAB (I) - 1) / 2 + LLAB (I)
C
      FCAAR (I) = - RINT (I) * DAAR (IKL) * 3.0D0
      FCAAI (I) = - RINT (I) * DAAI (IKL)
      FCBAR (I) = - RINT (I) * DBAR (IKL)
      FCBAI (I) = - RINT (I) * DBAI (IKL)
C
      FAAR (IIJ) = FAAR (IIJ) - FCAAR (I)
      FAAI (IIJ) = FAAI (IIJ) - FCAAI (I)
      FBAR (IIJ) = FBAR (IIJ) - FCBAR (I)
      FBAI (IIJ) = FBAI (IIJ) - FCBAI (I)
C
      FCAAR (I) = - RINT (I) * DAAR (IIJ) * 3.0D0
      FCAAI (I) = - RINT (I) * DAAI (IIJ)
      FCBAR (I) = - RINT (I) * DBAR (IIJ)
      FCBAI (I) = - RINT (I) * DBAI (IIJ)
C
      FAAR (IKL) = FAAR (IKL) - FCAAR (I)
      FAAI (IKL) = FAAI (IKL) - FCAAI (I)
      FBAR (IKL) = FBAR (IKL) - FCBAR (I)
      FBAI (IKL) = FBAI (IKL) - FCBAI (I)
C
      FCAAR (I) = - RINT (I) *  DAAR (IJL) * 3.0D0
      FCAAI (I) =   RINT (I) *  DAAI (IJL)
      FCABR (I) =   RINT (I) *  DBAR (IJL)
      FCABI (I) = - RINT (I) *  DBAI (IJL)
C
      FAAR (IIK) = FAAR (IIK) - FCAAR (I)
      FAAI (IIK) = FAAI (IIK) - FCAAI (I)
      FBAR (IIK) = FBAR (IIK) - FCABR (I)
      FBAI (IIK) = FBAI (IIK) + FCABI (I)
C
      FCAAR (I) = - RINT (I) * DAAR (IIK) * 6.0D0
C
      FAAR (IJL) = FAAR (IJL) - FCAAR (I)
C
 1110 CONTINUE
      RETURN
C
C
C=============================================================================
C                    TYPE 13     (I > K > J > L)
C=============================================================================
C
 1300 DO 1310 I = 1, NT
      IKL = KLAB (I) * (KLAB (I) - 1) / 2 + LLAB (I)
      IIJ = ILAB (I) * (ILAB (I) - 1) / 2 + JLAB (I)
      IIL = ILAB (I) * (ILAB (I) - 1) / 2 + LLAB (I)
      IKJ = KLAB (I) * (KLAB (I) - 1) / 2 + JLAB (I)
      IIK = ILAB (I) * (ILAB (I) - 1) / 2 + KLAB (I)
      IJL = JLAB (I) * (JLAB (I) - 1) / 2 + LLAB (I)
C
      FCAAR (I) = - RINT (I) * DAAR (IKJ) * 3.0D0
      FCAAI (I) = - RINT (I) * DAAI (IKJ)
      FCBAR (I) = - RINT (I) * DBAR (IKJ)
      FCBAI (I) = - RINT (I) * DBAI (IKJ)
C
      FAAR (IIL) = FAAR (IIL) - FCAAR (I)
      FAAI (IIL) = FAAI (IIL) - FCAAI (I)
      FBAR (IIL) = FBAR (IIL) - FCBAR (I)
      FBAI (IIL) = FBAI (IIL) - FCBAI (I)
C
      FCAAR (I) = - RINT (I) *  DAAR (IIL) * 3.0D0
      FCAAI (I) = - RINT (I) *  DAAI (IIL)
      FCBAR (I) = - RINT (I) *  DBAR (IIL)
      FCBAI (I) = - RINT (I) *  DBAI (IIL)
C
      FAAR (IKJ) = FAAR (IKJ) - FCAAR (I)
      FAAI (IKJ) = FAAI (IKJ) - FCAAI (I)
      FBAR (IKJ) = FBAR (IKJ) - FCBAR (I)
      FBAI (IKJ) = FBAI (IKJ) - FCBAI (I)
C
      FCAAR (I) = - RINT (I) *  DAAR (IJL) * 3.0D0
      FCAAI (I) =   RINT (I) *  DAAI (IJL)
      FCBAR (I) =   RINT (I) *  DBAR (IJL)
      FCBAI (I) =   RINT (I) *  DBAI (IJL)
C
      FAAR (IIK) = FAAR (IIK) - FCAAR (I)
      FAAI (IIK) = FAAI (IIK) - FCAAI (I)
      FBAR (IIK) = FBAR (IIK) - FCBAR (I)
      FBAI (IIK) = FBAI (IIK) - FCBAI (I)
C
      FCAAR (I) = - RINT (I) *  DAAR (IIK) * 3.0D0
      FCAAI (I) =   RINT (I) *  DAAI (IIK)
      FCBAR (I) =   RINT (I) *  DBAR (IIK)
      FCBAI (I) =   RINT (I) *  DBAI (IIK)
C
      FAAR (IJL) = FAAR (IJL) - FCAAR (I)
      FAAI (IJL) = FAAI (IJL) - FCAAI (I)
      FBAR (IJL) = FBAR (IJL) - FCBAR (I)
      FBAI (IJL) = FBAI (IJL) - FCBAI (I)
C
 1310 CONTINUE
      RETURN
C
C
C=============================================================================
C                    TYPE 14  (I > K > L > J)
C=============================================================================
C
 1400 DO 1410 I = 1, NT
      IKL = KLAB (I) * (KLAB (I) - 1) / 2 + LLAB (I)
      IIJ = ILAB (I) * (ILAB (I) - 1) / 2 + JLAB (I)
      IIL = ILAB (I) * (ILAB (I) - 1) / 2 + LLAB (I)
      IKJ = KLAB (I) * (KLAB (I) - 1) / 2 + JLAB (I)
      IIK = ILAB (I) * (ILAB (I) - 1) / 2 + KLAB (I)
      ILJ = LLAB (I) * (LLAB (I) - 1) / 2 + JLAB (I)
C
      FCAAR (I) = RINT (I) *  DAAR (IKJ) * 3.0D0
      FCAAI (I) = RINT (I) *  DAAI (IKJ)
      FCBAR (I) = RINT (I) *  DBAR (IKJ)
      FCBAI (I) = RINT (I) *  DBAI (IKJ)
C
      FAAR (IIL) = FAAR (IIL) + FCAAR (I)
      FAAI (IIL) = FAAI (IIL) + FCAAI (I)
      FBAR (IIL) = FBAR (IIL) + FCBAR (I)
      FBAI (IIL) = FBAI (IIL) + FCBAI (I)
C
      FCAAR (I) = RINT (I) *  DAAR (IIL) * 3.0D0
      FCAAI (I) = RINT (I) *  DAAI (IIL)
      FCBAR (I) = RINT (I) *  DBAR (IIL)
      FCBAI (I) = RINT (I) *  DBAI (IIL)
C
      FAAR (IKJ) = FAAR (IKJ) + FCAAR (I)
      FAAI (IKJ) = FAAI (IKJ) + FCAAI (I)
      FBAR (IKJ) = FBAR (IKJ) + FCBAR (I)
      FBAI (IKJ) = FBAI (IKJ) + FCBAI (I)
C
      FCAAR (I) = - RINT (I) *  DAAR (ILJ) * 3.0D0
      FCAAI (I) = - RINT (I) *  DAAI (ILJ)
      FCBAR (I) = - RINT (I) *  DBAR (ILJ)
      FCBAI (I) = - RINT (I) *  DBAI (ILJ)
C
      FAAR (IIK) = FAAR (IIK) - FCAAR (I)
      FAAI (IIK) = FAAI (IIK) - FCAAI (I)
      FBAR (IIK) = FBAR (IIK) - FCBAR (I)
      FBAI (IIK) = FBAI (IIK) - FCBAI (I)
C
      FCAAR (I) = RINT (I) *  DAAR (IIK) * 3.0D0
      FCAAI (I) = RINT (I) *  DAAI (IIK)
      FCBAR (I) = RINT (I) *  DBAR (IIK)
      FCBAI (I) = RINT (I) *  DBAI (IIK)
C
      FAAR (ILJ) = FAAR (ILJ) + FCAAR (I)
      FAAI (ILJ) = FAAI (ILJ) + FCAAI (I)
      FBAR (ILJ) = FBAR (ILJ) + FCBAR (I)
      FBAI (ILJ) = FBAI (ILJ) + FCBAI (I)
C
 1410 CONTINUE
      RETURN
C
C
C=============================================================================
C                        END OF CODE FOR DIFFERENT TYPES
C=============================================================================
C
      END
