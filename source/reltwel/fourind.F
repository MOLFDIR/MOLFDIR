      SUBROUTINE FOURIND (IPRIM,JPRIM,KPRIM,LPRIM,
     &                    ICONTR,JCONTR,KCONTR,LCONTR,
     &                    COEFFI,COEFFJ,COEFFK,COEFFL,
     &                    LDI,LDJ,LDK,LDL,
     &                    RUWINT,RAWINT)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C---------------Description--------------------------------------------
C
C     Transforms set of primitive integrals to contracted set
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
      INTEGER IPRIM,JPRIM,KPRIM,LPRIM
      INTEGER ICONTR,JCONTR,KCONTR,LCONTR
      REAL*8 COEFFI(LDI,ICONTR),COEFFJ(LDJ,JCONTR),
     &       COEFFK(LDK,KCONTR),COEFFL(LDL,LCONTR),
     &       RUWINT(*),RAWINT(*)
C
C---------------Common Blocks--------------------------------------
C
C---------------Local variables--------------------------------------
C
#if defined (BIT64)
      PARAMETER (R0=0.0,R1=1.0)
#else
      PARAMETER (R0=0.D0,R1=1.D0)
#endif
C
C---------------Executable code--------------------------------------
C
C
#if defined (BIT64)
      CALL SGEMM ('N','N',LPRIM*KPRIM*JPRIM,ICONTR,IPRIM,R1,
#else
      CALL DGEMM ('N','N',LPRIM*KPRIM*JPRIM,ICONTR,IPRIM,R1,
#endif
     &            RAWINT,LPRIM*KPRIM*JPRIM,COEFFI,LDI,
     &            R0,RUWINT,LPRIM*KPRIM*JPRIM)
C
      DO 110 IJKL = 1, ICONTR * JCONTR * KPRIM * LPRIM
      RAWINT (IJKL) = 0.D0
  110 CONTINUE
      DO 200 JP = 1, JPRIM
      JKLP =  (JP - 1) * KPRIM * LPRIM
      DO 190 JC = 1, JCONTR
      JKLC =  (JC - 1) * KPRIM * LPRIM
      COEFFC = COEFFJ(JP,JC)
      IF (COEFFC.EQ.0.D0) GO TO 190
      DO 180 IC = 1, ICONTR
      IJKLP = (IC - 1) * JPRIM * KPRIM * LPRIM + JKLP
      IJKLC = (IC - 1) * JCONTR * KPRIM * LPRIM + JKLC
#if defined (BIT64)
      CALL SAXPY (KPRIM*LPRIM,COEFFC,RUWINT(IJKLP+1),1,
     &            RAWINT(IJKLC+1),1)
#else
      CALL DAXPY (KPRIM*LPRIM,COEFFC,RUWINT(IJKLP+1),1,
     &            RAWINT(IJKLC+1),1)
#endif
  170 CONTINUE
  180 CONTINUE
  190 CONTINUE
  200 CONTINUE
C
      DO 310 IJKL = 1, ICONTR * JCONTR * KCONTR * LPRIM
      RUWINT (IJKL) = 0.D0
  310 CONTINUE
      DO 400 KP = 1, KPRIM
      KLP = (KP - 1) * LPRIM
      DO 390 KC = 1, KCONTR
      KLC = (KC - 1) * LPRIM
      COEFFC = COEFFK(KP,KC)
      IF (COEFFC.EQ.0.D0) GO TO 390
      DO 380 LP = 1, LPRIM
      IJKLC = KLC + LP
      IJKLP = KLP + LP
#if defined (BIT64)
      CALL SAXPY (ICONTR*JCONTR,COEFFC,RAWINT(IJKLP),KPRIM*LPRIM,
     &            RUWINT(IJKLC),KCONTR*LPRIM)
#else
      CALL DAXPY (ICONTR*JCONTR,COEFFC,RAWINT(IJKLP),KPRIM*LPRIM,
     &            RUWINT(IJKLC),KCONTR*LPRIM)
#endif
  370 CONTINUE
  380 CONTINUE
  390 CONTINUE
  400 CONTINUE
C
#if defined (BIT64)
      CALL SGEMM ('T','N',LCONTR,KCONTR*JCONTR*ICONTR,LPRIM,R1,
#else
      CALL DGEMM ('T','N',LCONTR,KCONTR*JCONTR*ICONTR,LPRIM,R1,
#endif
     &            COEFFL,LDL,RUWINT,LPRIM,
     &            R0,RAWINT,LCONTR)
C
      RETURN
      END
