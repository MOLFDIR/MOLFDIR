
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
C FILE    : ccintm.F
C SOURCE  : $Source: /home/tc/visscher/sources/Molfdir/Molfdir/source/relccsd/ccintm.F,v $
C
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE AINTM(T1,T2,BUF1,BUF2,AZ)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates A intermediate
C
C---------------Routines called----------------------------------------
C
C     GETTAU
C     GETOOOO, GETVVOO, GETVOOO
C     XGEMM, XAXPY
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 T1(*),T2(*)
      REAL*8 AZ(*),BUF1(*),BUF2(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "symm.inc"
      INCLUDE "complex.inc"
C
C---------------Local variables--------------------------------------
C
      real*8 ddot
C---------------Executable code--------------------------------------
C
C--------------------
C A(KL,IJ) = W(KL,IJ)
C--------------------
      CALL GETOOOO (AZ)
      CALL DELINT ('OOOO','DDKK',AZ,0,0,0)
C------------------------------------------------
C A(KL,IJ) = A(KL,IJ) + PIJ ( W(KL,IC) * T(C,J) )
C------------------------------------------------
      CALL GETVOOO (BUF1)
      CALL DELINT ('VOOO','DKDD',BUF1,0,0,0)
      CALL SRT9 (NREP,MULTB,NVO,NV,NO,NOOT,NOOOT,KOOOVT,KKOOOT,
     &           BUF1,BUF2)
      CALL CNTRCT ('N','N',NOOOT,NO,NV,A1,BUF2,T1,A0,BUF1,NREP)
      CALL SRT1T3 (NREP,MULTB,LTR,NOOT,NO,NO,NOOOT,KOOOOT,KKOOOT,
     &             BUF1,BUF2)
      CALL XAXPY (IOOOOTT(NREP+1),A1,BUF2,1,AZ,1)
C---------------------------------------------
C A(KL,IJ) = A(KL,IJ) +  W(KL,CD) * TAU(CD,IJ)
C---------------------------------------------
      CALL GETVVOO (BUF1)
      CALL DELINT ('VVOO','DDDD',BUF1,0,0,0)
      CALL GETTAU (T1,T2,BUF2)
      CALL CNTRCT ('C','N',NOOT,NOOT,NVVT,A1,BUF1,BUF2,A1,AZ,NREP)
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE GOINTM (FVO,T1,BUF1,BUF2,GO)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates GO intermediate
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 FVO(*)
      REAL*8 T1(*)
      REAL*8 GO(*)
      REAL*8 BUF1(*),BUF2(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "symm.inc"
      INCLUDE "complex.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
C----------------------------------
C G(K,I) = H(K,I) + F(K,C) * T(C,I)
C----------------------------------
      CALL XCOPY (NFVO,FVO,1,BUF1,1)
      CALL DELFCK ('VO','DD',BUF1)
      CALL CNTRCT ('C','N',NO,NO,NV,A1,BUF1,T1,A1,GO,NREP)
C-----------------------------------------------------
C G(KI) = G(KI) + W(KL,IC) * T(CL) = G(KI) - W*(CI,KL)
C Order W from CI,KL to CL,KI
C-----------------------------------------------------
      M = MOO(1)
      N = MVO(1)
      IF (N.EQ.0) RETURN
      CALL GETVOOO (BUF1)
      CALL DELINT ('VOOO','DKDD',BUF1,0,0,0)
      CALL SRT26 (NREP,MULTB,LTR,LFA,NV,NO,NO,NO,MVO,JVOOO,JJVO,JJOO,
     &            BUF1,BUF2)
      CALL XGEMV ('C',N,M,-A1,BUF2,N,T1,1,A1,GO,1)
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE GVINTM(FVO,T1,BUF3,NBUF3,BUF2,NBUF2,GV)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates GV intermediate
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 FVO(*)
      REAL*8 T1(*)
      REAL*8 GV(*)
      REAL*8 BUF3(*),BUF2(*)
      INTEGER NBUF3,NBUF2
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "complex.inc"
      INCLUDE "symm.inc"
      INCLUDE "ccpar.inc"
C
C---------------Local variables--------------------------------------
C
      LOGICAL USEDZ,RIGHT
      real*8 ddot
C
C---------------Executable code--------------------------------------
C
C ********************
C ** TASK SPLITTING **
C ********************
C ** only master calculates local contr. to G(A,C) !
C ** all nodes calculate nonlocal contr.

      IF(MYPROC.EQ.MASTER) THEN

C----------------------------------
C G(A,C) = H(A,C) - T(A,K) * F(K,C)
C----------------------------------
        CALL XCOPY (NFVO,FVO,1,BUF2,1)
        CALL DELFCK ('VO','DD',BUF2)
        CALL CNTRCT ('N','C',NV,NV,NO,-A1,T1,BUF2,A1,GV,NREP)
  
      ELSE

        CALL XCOPY(NFVV*RCW,A0,0,GV,1)

      ENDIF
C ***************************
C ** END OF TASK SPLITTING **
C ***************************

C------------------------------------
C G(A,C) = G(A,C) + W(AK,CD) * T(D,K)
C------------------------------------
      M = MVV(1)
      N = MVO(1)
      IF (N.EQ.0) RETURN
      USEDZ = .FALSE.
      RIGHT = .TRUE.
      CALL SRT20D(NREP,MULTB,NVO,NV,NO,NV,NV,MVV,JVVVO,JJVV,
     &            JJVO,BUF2,NBUF2,T1,GV,USEDZ,RIGHT)
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE HINTM(T1,T2,BUF1,BUF2,BUF3,NBUF3,H)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates H intermediate
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
      REAL*8 T1(*),T2(*),H(*)
      REAL*8 BUF1(*),BUF2(*),BUF3(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "complex.inc"
      INCLUDE "symm.inc"
      INCLUDE "eqns.inc"
      INCLUDE "ccpar.inc"
C
C---------------Local variables--------------------------------------
C
      REAL*8 T1VAL(2)
      DATA T1VAL /2*0.0D0/
      LOGICAL DONE,TEQ
      real*8 ddot,xsum
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
         CALL QUIT('Wrong keyword in call to HINTM')
      ENDIF

C ********************
C ** TASK SPLITTING **
C ********************
C ** master calculates local contr. 
C ** - W(AK,CI)  + T(A,L) * W(LK,CI) 
C ** to the H intermediate which is kept
C ** in BUF1 ! ===> If we want to start from a clear H array we have to
C ** clear BUF1 at the extent of the H length.

      IF(MYPROC.EQ.MASTER) THEN
C---------------------------------------------------------------------
C H(AK,IC) = - W(AK,CI)
C We store h initially as H(AK,CI), ordening and minus sign after next
C step !
C---------------------------------------------------------------------
      CALL GETVOVO (H)
      CALL DELINT ('VOVO','KDDK',H,0,0,0)
C----------------------------------------
C H(AK,CI) = H(AK,CI) + T(A,L) * W(LK,CI)
C----------------------------------------
      CALL GETVOOO (BUF1)
      CALL DELINT ('VOOO','DKDD',BUF1,0,0,0)
      CALL SRT1C1 (NREP,NVO,NOOT,BUF1,BUF2)
      CALL SRT1T2 (NREP,MULTB,LFA,NOOT,NO,NO,NVO,LOOVO,LLOVO,BUF2,BUF1)
      CALL CNTRCT ('N','N',NV,NOVO,NO,A1,T1,BUF1,A0,BUF3,NREP)
      CALL SRT1S2 (NREP,MULTB,LTR,NVO,NV,NO,NVO,LVOVO,LLOVO,BUF3,BUF1)
      CALL XAXPY (IVOVO(NREP+1),-A1,H,1,BUF1,1)

      ELSE
    
        CALL XCOPY(NV4,A0,0,BUF1,1)

      ENDIF

C ***************************
C ** END OF TASK SPLITTING **
C ***************************
C ** here now come the distributed VOVV contributions to be calculated
C ** by each node !

C------------------------------------------------------------------
C H(AK,CI) = H(AK,CI) - W(AK,CD) * T(D,I)
C H is kept in BUF1, still ordered VOVO
C------------------------------------------------------------------
      CALL XTIME(28,1,' ** HINTM: VOVV*T             ')
      DO 30 IRP = 1, NREP
         IF (NVO(IRP).EQ.0) GOTO 30
         DONE = .FALSE.
         ISTART = 0
         CCO=0
         MINT = NBUF3/NVO(IRP)

C ** reading from GETVOVV: ISTART is updated with the actual batch #
C ** available on the local node, NINT is updated with the number of
C ** actually read integrals from this batch and is a pure output parameter !
C ** We therefore have to count upwards in case of ISTART.NE.0 !!

         CALL GETVOVV (IRP,ISTART,NINT,DONE,BUF3,MINT)
         CALL DELINT ('VOVV','KDDD',BUF3,IRP,ISTART,NINT)

C ** now start SORTING and contraction loop

         CD = 0
         DO 20 DREP = 1, NREP
           CREP = MULTB(DREP,IRP+NREP,2)
           IF (CREP.LT.DREP) GOTO 20
           DO 15 D = 1, NV(DREP)
              CMIN = 1
              IF (CREP.EQ.DREP) CMIN = D + 1
              DO 10 C = CMIN, NV(CREP)

                 IF(CCO.LT.ISTART) THEN
                   CCO=CCO+1
                   GOTO 10
                 ENDIF

                 CD = CD + 1
                 IF (CD.GT.NINT) THEN
C                   ---------------------------------------
C                   We need the next buffer in this IRREP !
C                   ---------------------------------------
                    ISTART = ISTART + NINT
                    IF(.NOT.DONE) THEN
                      CALL GETVOVV (IRP,ISTART,NINT,DONE,BUF3,MINT)
                      CALL DELINT ('VOVV','KDDD',BUF3,IRP,ISTART,NINT)
                      CD = 1
                      CCO=ISTART
                    ELSE
                      GOTO 30
                    ENDIF
                 ENDIF
                 AKCD = (CD-1)*NVO(IRP)*RCW+1
                 DO I = 1, NO(DREP)
                    CI = IIVO(CREP,DREP)+(I-1)*NV(CREP)+C
                    AKCI = (IVOVO(IRP)+(CI-1)*NVO(IRP))*RCW+1
                    T1DI = (IVO(DREP)+(I-1)*NV(DREP)+D-1) * RCW + 1
                    T1VAL(1) = - T1(T1DI)
                    IF (CARITH) T1VAL(2) = - T1(T1DI+1)
                    CALL XAXPY(NVO(IRP),T1VAL,BUF3(AKCD),1,BUF1(AKCI),1)
                 ENDDO
                 DO I = 1, NO(CREP)
                    DI = IIVO(DREP,CREP)+(I-1)*NV(DREP)+D
                    AKDI = (IVOVO(IRP)+(DI-1)*NVO(IRP))*RCW+1
                    T1CI = (IVO(CREP)+(I-1)*NV(CREP)+C-1) * RCW + 1
                    T1VAL(1) = T1(T1CI)
                    IF (CARITH) T1VAL(2) = T1(T1CI+1)
                    CALL XAXPY(NVO(IRP),T1VAL,BUF3(AKCD),1,BUF1(AKDI),1)
                 ENDDO
 10           CONTINUE
 15        CONTINUE
 20      CONTINUE
 30    CONTINUE
C------------------------------------------------------------------
C Now sort H(AK,CI) to H(AI,CK)  is to be done in any case !!
C The H buffer is not changed any more up to the final XGEMM !
C Every other calculation is done in BUF1 !!
C------------------------------------------------------------------
      CALL SRT16 (NREP,MULTB,LFA,LFA,NV,NO,NV,NO,MVO,JVOVO,JJVO,JJVO,
     &            BUF1,H)
      CALL XTIME(28,2,' ** HINTM: VOVV*T             ')

C------------------------------------------------------------------
C H(AI,CK) = H(AI,CK) + [ 0.5*T(AD,IL) - T(A,L)*T(D,I) ] * W(KL,CD)
C------------------------------------------------------------------
      CALL XCOPY (NDIMT2,T2,1,BUF1,1)
      IF (TEQ) CALL XSCAL (NDIMT2,AP5,BUF1,1)
C----------------------------------------------------------------------
C For the H-intermediate in the lambda equations we should not scale t2
C----------------------------------------------------------------------
      CALL GETTAU (T1,BUF1,BUF2)
      CALL SRT1TT4 (NREP,MULTB,LFA,LFA,NV,NV,NO,NO,MVO,J2VOVO,JJVO,JJVO,
     &              BUF2,BUF1)
C---------------------------------------------------------------
C BUF1 now contains 0.5*T(AD,IL) - T(A,L)*T(D,I) + T(A,I)*T(D,L)
C Subtract T(A,I)*T(D,L)
C---------------------------------------------------------------
      CALL XGEMM ('N','N',NDIMT1,NDIMT1,1,-A1,T1,NDIMT1,T1,1,
     &               A1,BUF1,NDIMT1)
C
      CALL GETVVOO (BUF3)
      CALL DELINT ('VVOO','DDDD',BUF3,0,0,0)
      IF (CARITH) CALL CONJUGA (NV3,BUF3,1)
      CALL SRT1TT4 (NREP,MULTB,LFA,LFA,NV,NV,NO,NO,MVO,J2VOVO,JJVO,JJVO,
     &              BUF3,BUF2)

      CALL XTIME(29,1,' ** HINTM: VVOO cont             ')

C
C --- old nonparallel VOVO contraction
C
C     OFF1 = 1
C     OFF3 = 1
C     DO IREP = 1, NREP
C        JREP = MULTB(IREP+NREP,1+NREP,2)
C        M = MVO(IREP)
C        N = MVO(IREP)
C        K = MVO(JREP)
C        OFF2 = J2VOVO(JREP) * RCW + 1
C        CALL XGEMM ('N','N',M,N,K,A1,BUF1(OFF1),M,BUF2(OFF2),K,
C    &               A1,H(OFF3),M)
C        OFF1 = OFF1 + M * K * RCW
C        OFF3 = OFF3 + M * N * RCW
C     ENDDO
C
 
C
C --- new parallelized VOVO contraction
C
      OFF3 = 1
      DO IREP = 1, NREP
         JREP = MULTB(IREP+NREP,1+NREP,2)
         M    = MVO(IREP)
         N    = MVO(IREP)
         KLD  = MVO(JREP)
         K    = IDIST(1,4,JREP)
         OFF1 = IDIST(2,4,JREP)
         OFF2 = IDIST(3,4,JREP)
         CALL XGEMM ('N','N',M,N,K,A1,BUF1(OFF1),M,BUF2(OFF2),
     &               KLD,A1,H(OFF3),M)
         OFF3 = OFF3 + M * N * RCW
      ENDDO
 
      CALL XTIME(29,2,' ** HINTM: VVOO cont             ')

      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE HOINTM(FOO,T1,T2,BUF1,BUF2,BUF3,HO)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates HO intermediate
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 FOO(*)
      REAL*8 T1(*),T2(*)
      REAL*8 HO(*)
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
      real*8 ddot
C---------------Executable code--------------------------------------
C
C-----------------------------------
C H(K,I) = F(K,I) * (1 - DELTA(K,I))
C-----------------------------------
      CALL XCOPY (NFOO,FOO,1,HO,1)
      CALL DELFCK ('OO','DK',HO)
      II = 1
      DO IRP = 1, NREP
         N = NO(IRP)
         CALL XCOPY(N,A0,0,HO(II),N+1)
         II = II + N * N * RCW
      ENDDO
C----------------------------------------------------------------------
C H(K,I) = H(K,I) + W(KL,CD) * T(CD,IL) = H(K,I) + W*(CDL,K) * T(CDL,I)
C----------------------------------------------------------------------
      CALL GETVVOO (BUF3)
      CALL DELINT ('VVOO','DDDD',BUF3,0,0,0)
      CALL SRT1T3 (NREP,MULTB,LFA,NVVT,NO,NO,NVVOT,KVVOOT,KKVVOT,
     &             BUF3,BUF1)
      CALL GETTAU  (T1,T2,BUF3)

      CALL SRT1T3 (NREP,MULTB,LFA,NVVT,NO,NO,NVVOT,KVVOOT,KKVVOT,
     &             BUF3,BUF2)
      CALL CNTRCT ('C','N',NO,NO,NVVOT,A1,BUF1,BUF2,A1,HO,NREP)
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE HOVINTM(FVO,T1,BUF1,BUF2,HOV)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates HOV intermediate
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 FVO(*)
      REAL*8 T1(*)
      REAL*8 HOV(*)
      REAL*8 BUF1(*),BUF2(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "symm.inc"
      INCLUDE "complex.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
C--------------------------------
C HOV(C,K) = FOV(K,C) = FVO*(C,K)
C--------------------------------
      CALL XCOPY (NFVO,FVO,1,HOV,1)
      CALL DELFCK ('VO','DD',HOV)
      IF (CARITH) CALL CONJUGA (NFVO,HOV,1)
C-------------------------------------------------------------------
C HOV(CK) = HOV(CK) + W(KL,CD) * T(DL) = HOV(CK) + W*(CD,KL) * T(DL)
C-------------------------------------------------------------------
      M = MVO(1)
      K = MVO(1)
      CALL GETVVOO (BUF1)
      CALL DELINT ('VVOO','DDDD',BUF1,0,0,0)
      CALL XCOPY (JVOVO(2),A0,0,BUF2,1)
      CALL SRT1TT4 (NREP,MULTB,LTR,LFA,NV,NV,NO,NO,MVO,JVOVO,JJVO,JJVO,
     &              BUF1,BUF2)
      IF (CARITH) CALL CONJUGA (JVOVO(2),BUF2,1)
      CALL XGEMV ('N',M,K,A1,BUF2,M,T1,1,A1,HOV,1)
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE HVINTM(FVV,T1,T2,BUF1,BUF2,BUF3,HV)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates HV intermediate
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 FVV(*)
      REAL*8 T1(*),T2(*)
      REAL*8 HV(*)
      REAL*8 BUF1(*),BUF2(*),BUF3(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "symm.inc"
      INCLUDE "complex.inc"
C
C---------------Local variables--------------------------------------
      REAL*8 ddot
C
C---------------Executable code--------------------------------------
C
C-----------------------------------
C H(A,C) = F(A,C) * (1 - DELTA(A,C))
C-----------------------------------
      CALL XCOPY (NFVV,FVV,1,HV,1)
      CALL DELFCK ('VV','KD',HV)
      AA = 1
      DO IRP = 1, NREP
         N = NV(IRP)
         CALL XCOPY(N,A0,0,HV(AA),N+1)
         AA = AA + N * N * RCW
      ENDDO
C----------------------------------------------------------------------
C H(A,C) = H(A,C) - W(KL,CD) * T(AD,KL) = H(A,C) - T(A,DKL) * W*(C,DKL)
C----------------------------------------------------------------------
      CALL GETTAU (T1,T2,BUF3)
      CALL SRT1T2 (NREP,MULTB,LFA,NVVT,NV,NV,NOOT,LVVOOT,LLVOOT,
     &             BUF3,BUF1)
      CALL GETVVOO (BUF3)
      CALL DELINT ('VVOO','DDDD',BUF3,0,0,0)
      CALL SRT1T2 (NREP,MULTB,LFA,NVVT,NV,NV,NOOT,LVVOOT,LLVOOT,
     &             BUF3,BUF2)
      CALL CNTRCT ('N','C',NV,NV,NVOOT,-A1,BUF1,BUF2,A1,HV,NREP)
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE BINTM(T1,T2,BUF1,BUF2,NBUF2,BUF3,NBUF3,S2,IW)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates B intermediate and adds it to new T2's
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      INTEGER NBUF2,NBUF3,IW
      REAL*8 T1(*),T2(*)
      REAL*8 S2(*)
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
      LOGICAL DONE,TEQ
      REAL*8 T1MIN(2)
      DATA T1MIN /2*0.0D0/
      REAL*8 ddot
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
         CALL QUIT('Wrong keyword in call to BINTM')
      ENDIF
C---------------------------------------------
C S(AB,CD) =  S(AB,CD) + V(AB,CD) * TAU(CD,IJ)
C---------------------------------------------
      IF (TEQ) THEN
         CALL GETTAU (T1,T2,BUF1)
      ELSE
         CALL XCOPY  (NDIMT2,T2,1,BUF1,1)
      ENDIF

C****************************************************
C**
C**   DEBUGGING PURPOSES:
C**   VERIFYING TAU MATRIX
C**
C     write(*,*) '== IN BINTM: TAU MATRIX: =='
C     OFF1=1
C     DO IRP=1,NREP
C       M=NVVT(IRP)
C       N=NOOT(IRP)
C       write(*,*) 'IRP,M,N:',IRP,M,N
C       write(*,'(A,I5,F30.15)') 'TAU(IREP)',IRP,
C    >  ddot(M*N*RCW,BUF1(OFF1),1,BUF1(OFF1),1)
C       OFF1=OFF1 + M*N*RCW
C     ENDDO
C**
C**
C****************************************************

      OFF1 = 1
      OFF2 = 1
      DO 30 IRP = 1, NREP
         ISTART = 0
         IF (NVVT(IRP).EQ.0) GOTO 30
         MINT = NBUF3/NVVT(IRP)
  10     CALL GETVVVV (IRP,ISTART,NINT,DONE,BUF3,MINT)
         CALL DELINT ('VVVV','KKDD',BUF3,IRP,ISTART,NINT)
         M = NVVT(IRP)
         N = NOOT(IRP)
         K = NINT
         CALL XGEMM ('N','N',M,N,K,A1,BUF3,M,BUF1(OFF1+ISTART*RCW),M,
     &               A1,S2(OFF2),M)
         IF (.NOT.DONE) THEN
            ISTART = ISTART + NINT
            GOTO 10
         ENDIF

 111  format (a,i6,2f30.15)
C     write(iw,111) '------>>> VVVV/IRP contraction in IRP:',IRP,
C    >    ddot(M*N*RCW,S2(OFF2),1,S2(OFF2),1)

         OFF1 = OFF1 + M * N * RCW
         OFF2 = OFF2 + M * N * RCW
 30   CONTINUE

C******************************************************
C**
C**     DEBUGGING PURPOSES
C**     construct absolute norm of VOVV per IRREP
C**
C**
C******************************************************
C     DO 37 IRP = 1, NREP
C        write(iw,*) '=========='
C        write(iw,*) '== IRREP:',IRP
C        write(iw,*) '=========='
C        ISTART=0
C        MINT = NBUF3/NVO(IRP)
C        INX=1
C38      CALL GETVOVV (IRP,ISTART,NINT,DONE,BUF3,MINT)
C        DO IJNV=1,NINT   ! loop over all IJ combinations in this irrep
C          DO IKS=1,NO(IRP)
C           DO IAS=1,NV(IRP)
C            write(iw,'(2I5,E25.16)') INX,IJNV,BUF3(INX)
C            INX=INX+1
C           ENDDO
C          ENDDO
C        ENDDO
C        IF (.NOT.DONE) THEN
C           ISTART = ISTART + NINT
C           GOTO 38
C        ENDIF
C37    CONTINUE
C      call quit('now compare integrals !')

C--------------------------------------------------------------
C S(AB,IJ) =  S(AB,IJ) - PAB [ T(B,K) * W(AK,CD) * TAU(CD,IJ) ]
C--------------------------------------------------------------
      CALL XCOPY (NV2,A0,0,BUF2,1)
C
      OFF1 = 1
      OFF2 = 1
      DO 130 IRP = 1, NREP
         ISTART = 0
         IF (NVVT(IRP).EQ.0.OR.NVO(IRP).EQ.0) GOTO 130
         M = NVO (IRP)
         N = NOOT(IRP)
         KD = NVVT(IRP)
         MINT = NBUF3/NVO(IRP)

 110     CALL GETVOVV (IRP,ISTART,NINT,DONE,BUF3,MINT)
         CALL DELINT ('VOVV','KDDD',BUF3,IRP,ISTART,NINT)

         K = NINT

C ** now we construct the full NVO x NOOT matrix
C ** but the contributions are only from the restricted CD range
C ** the method is additive since ZGEMM works according to
C **      C=A*B + C  ==> all the already computed contributions
C ** will be added in this IRREP.

         CALL XGEMM ('N','N',M,N,K,A1,BUF3,M,BUF1(OFF1+ISTART*RCW),
     &               KD,A1,BUF2(OFF2),M)
         IF (.NOT.DONE) THEN
            ISTART = ISTART + NINT
            GOTO 110
         ENDIF


C******************************************************
C**
C**     DEBUGGING PURPOSES
C
C     write(iw,111) '--------->> VOVV contraction in IRP:',IRP,
C    >    ddot(M*N*RCW,BUF2(OFF2),1,BUF2(OFF2),1)
C**
C**
C******************************************************

         OFF1 = OFF1 + KD * N * RCW
         OFF2 = OFF2 + M * N * RCW
 130  CONTINUE

C ** now the <AK||IJ> contr. are available in memory sorted according
C ** to combined IRREPS (AK) and (IJ).

C ** next sort resorts according to (KA) and (IJ)

      CALL SRT1L1 (NREP,MULTB,LFA,NVO,NV,NO,NOOT,IVOOOT,IIOV,BUF2,BUF3)

C ** next sort resorts according to K,AIJ which means sorted according to
C ** IRREPS K and not combined IRREPS anymore for subsequent contraction

      CALL SRT1S2 (NREP,MULTB,LFA,NVO,NO,NV,NOOT,LOVOOT,LLVOOT,
     &             BUF3,BUF2)
C---------------------------------------------------------------
C Contract with T1 amplitudes instead of L1 for Lambda equations
C---------------------------------------------------------------
      IF (TEQ) THEN
         CALL CNTRCT ('N','N',NV,NVOOT,NO,-A1,T1,BUF2,A0,BUF3,NREP)
      ELSE
         CALL GETAMPT (BUF1,BUF2)
         CALL CNTRCT ('N','N',NV,NVOOT,NO,-A1,BUF1,BUF2,A0,BUF3,NREP)
      ENDIF
      CALL SRT1T2 (NREP,MULTB,LTR,NVVT,NV,NV,NOOT,LVVOOT,LLVOOT,
     &             BUF3,BUF2)
      CALL XAXPY (NDIMT2,A1,BUF2,1,S2,1)
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE GETTAU (T1,T2,TAU)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates Tau from T1 and T2 amplitudes
C
C---------------Routines called----------------------------------------
C
C     XCOPY, XAXPY
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 T1(*),T2(*),TAU(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "symm.inc"
      INCLUDE "complex.inc"
C
C---------------Local variables--------------------------------------
C
      REAL*8 T1MIN(2)
      DATA T1MIN /2*0.0D0/
C
C---------------Executable code--------------------------------------
C
      CALL XCOPY (NDIMT2,T2,1,TAU,1)
C
      ABIJOFF = 0
      DO IJRP = 1, NREP
      DO 10 JRP = 1, NREP
      IRP = MULTB(JRP,IJRP+NREP,2)
      IF (IRP.LT.JRP) GOTO 10
      DO J = 1, NO(JRP)
         IMIN = 1
         IF (IRP.EQ.JRP) IMIN = J + 1
         DO I = IMIN, NO(IRP)
            DO 20 BRP = 1, NREP
            ARP = MULTB(BRP,IJRP+NREP,2)
            IF (ARP.LT.BRP) GOTO 20
C--------------------------------------------------------
C TAU(AB,IJ) = T2(AB,IJ) + T1(A,I) * T1(B,J)
C--------------------------------------------------------
            IF (BRP.EQ.JRP) THEN
               AB = 1
               BJ = (IVO(JRP) + (J-1) * NV(JRP)) * RCW + 1
               DO B = 1, NV(BRP)
                  AMIN = 1
                  IF (ARP.EQ.BRP) AMIN = B + 1
                  AI = (IVO(IRP) + (I-1) * NV(IRP) + AMIN - 1) * RCW + 1
                  NA = NV(ARP) - AMIN + 1
                  CALL XAXPY (NA,T1(BJ),T1(AI),1,TAU(ABIJOFF+AB),1)
                  AB = AB + NA * RCW
                  BJ = BJ + RCW
               ENDDO
            ENDIF
C--------------------------------------------------------
C TAU(AB,IJ) = T2(AB,IJ) - T1(A,J) * T1(B,I)
C--------------------------------------------------------
            IF (ARP.EQ.JRP) THEN
               AB = 1
               BI = (IVO(IRP) + (I-1) * NV(IRP)) * RCW + 1
               DO B = 1, NV(BRP)
                  AMIN = 1
                  IF (ARP.EQ.BRP) AMIN = B + 1
                  AJ = (IVO(JRP) + (J-1) * NV(JRP) + AMIN - 1) * RCW + 1
                  NA = NV(ARP) - AMIN + 1
                  T1MIN(1) = -T1(BI)
                  IF (CARITH) T1MIN(2) = -T1(BI+1)
                  CALL XAXPY (NA,T1MIN,T1(AJ),1,TAU(ABIJOFF+AB),1)
                  AB = AB + NA * RCW
                  BI = BI + RCW
               ENDDO
            ENDIF
C--------------------------------------------------------
C UPDATE OFFSET AND GO TO NEXT IRREP PAIR
C--------------------------------------------------------
            IF (ARP.NE.BRP) THEN
               ABIJOFF = ABIJOFF + NV(ARP) * NV(BRP) * RCW
            ELSE
               ABIJOFF = ABIJOFF + NV(ARP) * (NV(ARP)-1) * RCW / 2
            ENDIF
 20         CONTINUE
         ENDDO
      ENDDO
 10   CONTINUE
      ENDDO
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE GETTAU2 (S1,DL,TAU)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates Hamiltonian matrix from S1 and S2 elements
C     Essentially Slater rules for 2-e determinants !
C
C     General formula (indices are active virtual or active occupied)
C     We apply it to the whole array and extract the relevant part
C     later. Note that TAU should already be initialized with S2.
C
C     H2(AB,IJ) = S1(A,I)*DELTA(B,J) + S1(B,J)*DELTA(A,I)
C               - S1(A,J)*DELTA(B,I) - S1(B,I)*DELTA(A,J)
C               + S2(AB,IJ)
C
C---------------Routines called----------------------------------------
C
C     XCOPY, XAXPY
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 S1(*),DL(*),TAU(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "symm.inc"
      INCLUDE "complex.inc"
      INCLUDE "param.inc"
C
C---------------Local variables--------------------------------------
C
      REAL*8 S1MIN(2)
      DATA S1MIN /2*0.0D0/
C
C---------------Executable code--------------------------------------
C
      CALL XCOPY (NDIMT1,A0,0,DL,1)
      AI = 1
      DO IRP = 1, NREP
      DO I = 1, NO(IRP)
         F = I - NSO(IRP)
         N = I - NIO(IRP)
         DO A = 1, NV(IRP)
             E = A
             M = A - NSV(IRP)
             IF (E.EQ.F) DL(AI) = D1
             IF (M.EQ.N.AND.M.GT.0) DL(AI) = D1
             AI = AI + RCW
         ENDDO
      ENDDO
      ENDDO
C
      ABIJOFF = 0
      DO IJRP = 1, NREP
      DO 10 JRP = 1, NREP
      IRP = MULTB(JRP,IJRP+NREP,2)
      IF (IRP.LT.JRP) GOTO 10
      DO J = 1, NO(JRP)
         IMIN = 1
         IF (IRP.EQ.JRP) IMIN = J + 1
         DO I = IMIN, NO(IRP)
            DO 20 BRP = 1, NREP
            ARP = MULTB(BRP,IJRP+NREP,2)
            IF (ARP.LT.BRP) GOTO 20
C-----------------------------------------------------------------------
C TAU(AB,IJ) = S2(AB,IJ) + S1(A,I) * DELTA (B,J) + DELTA(A,I) * S1 (B,J)
C-----------------------------------------------------------------------
            IF (BRP.EQ.JRP) THEN
               AB = 1
               BJ = (IVO(JRP) + (J-1) * NV(JRP)) * RCW + 1
               DO B = 1, NV(BRP)
                  AMIN = 1
                  IF (ARP.EQ.BRP) AMIN = B + 1
                  AI = (IVO(IRP) + (I-1) * NV(IRP) + AMIN - 1) * RCW + 1
                  NA = NV(ARP) - AMIN + 1
                  CALL XAXPY (NA,DL(BJ),S1(AI),1,TAU(ABIJOFF+AB),1)
                  CALL XAXPY (NA,S1(BJ),DL(AI),1,TAU(ABIJOFF+AB),1)
                  AB = AB + NA * RCW
                  BJ = BJ + RCW
               ENDDO
            ENDIF
C-----------------------------------------------------------------------
C TAU(AB,IJ) = S2(AB,IJ) - S1(A,J) * DELTA (B,I) - DELTA(B,I) * S1 (A,J)
C-----------------------------------------------------------------------
            IF (ARP.EQ.JRP) THEN
               AB = 1
               BI = (IVO(IRP) + (I-1) * NV(IRP)) * RCW + 1
               DO B = 1, NV(BRP)
                  AMIN = 1
                  IF (ARP.EQ.BRP) AMIN = B + 1
                  AJ = (IVO(JRP) + (J-1) * NV(JRP) + AMIN - 1) * RCW + 1
                  NA = NV(ARP) - AMIN + 1
                  S1MIN(1) = -S1(BI)
                  IF (CARITH) S1MIN(2) = -S1(BI+1)
                  CALL XAXPY (NA,S1MIN,DL(AJ),1,TAU(ABIJOFF+AB),1)
                  S1MIN(1) = -DL(BI)
                  IF (CARITH) S1MIN(2) = -DL(BI+1)
                  CALL XAXPY (NA,S1MIN,S1(AJ),1,TAU(ABIJOFF+AB),1)
                  AB = AB + NA * RCW
                  BI = BI + RCW
               ENDDO
            ENDIF
C--------------------------------------------------------
C UPDATE OFFSET AND GO TO NEXT IRREP PAIR
C--------------------------------------------------------
            IF (ARP.NE.BRP) THEN
               ABIJOFF = ABIJOFF + NV(ARP) * NV(BRP) * RCW
            ELSE
               ABIJOFF = ABIJOFF + NV(ARP) * (NV(ARP)-1) * RCW / 2
            ENDIF
 20         CONTINUE
         ENDDO
      ENDDO
 10   CONTINUE
      ENDDO
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE GETTAU3 (T1,T2,TAU)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates Tau3 from T1 and T2 amplitudes
C     Tau3 is used in the -T correction : T3 = T2 + 1/3 PIJ T1.T1
C
C---------------Routines called----------------------------------------
C
C     XCOPY, XAXPY
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 T1(*),T2(*),TAU(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "symm.inc"
      INCLUDE "complex.inc"
      INCLUDE "param.inc"
C
C---------------Local variables--------------------------------------
C
      REAL*8 T1VAL(2)
      DATA T1VAL /2*0.0D0/
C
C---------------Executable code--------------------------------------
C
      CALL XCOPY (NDIMT2,T2,1,TAU,1)
C
      ABIJOFF = 0
      DO IJRP = 1, NREP
      DO 10 JRP = 1, NREP
      IRP = MULTB(JRP,IJRP+NREP,2)
      IF (IRP.LT.JRP) GOTO 10
      DO J = 1, NO(JRP)
         IMIN = 1
         IF (IRP.EQ.JRP) IMIN = J + 1
         DO I = IMIN, NO(IRP)
            DO 20 BRP = 1, NREP
            ARP = MULTB(BRP,IJRP+NREP,2)
            IF (ARP.LT.BRP) GOTO 20
C--------------------------------------------------------
C TAU(AB,IJ) = T2(AB,IJ) + T1(A,I) * T1(B,J)
C--------------------------------------------------------
            IF (BRP.EQ.JRP) THEN
               AB = 1
               BJ = (IVO(JRP) + (J-1) * NV(JRP)) * RCW + 1
               DO B = 1, NV(BRP)
                  AMIN = 1
                  IF (ARP.EQ.BRP) AMIN = B + 1
                  AI = (IVO(IRP) + (I-1) * NV(IRP) + AMIN - 1) * RCW + 1
                  NA = NV(ARP) - AMIN + 1
                  T1VAL(1) = T1(BJ) * ARP3
                  IF (CARITH) T1VAL(2) = T1(BJ+1) * ARP3
                  CALL XAXPY (NA,T1VAL,T1(AI),1,TAU(ABIJOFF+AB),1)
                  AB = AB + NA * RCW
                  BJ = BJ + RCW
               ENDDO
            ENDIF
C--------------------------------------------------------
C TAU(AB,IJ) = T2(AB,IJ) - T1(A,J) * T1(B,I)
C--------------------------------------------------------
            IF (ARP.EQ.JRP) THEN
               AB = 1
               BI = (IVO(IRP) + (I-1) * NV(IRP)) * RCW + 1
               DO B = 1, NV(BRP)
                  AMIN = 1
                  IF (ARP.EQ.BRP) AMIN = B + 1
                  AJ = (IVO(JRP) + (J-1) * NV(JRP) + AMIN - 1) * RCW + 1
                  NA = NV(ARP) - AMIN + 1
                  T1VAL(1) = -T1(BI) * ARP3
                  IF (CARITH) T1VAL(2) = -T1(BI+1) * ARP3
                  CALL XAXPY (NA,T1VAL,T1(AJ),1,TAU(ABIJOFF+AB),1)
                  AB = AB + NA * RCW
                  BI = BI + RCW
               ENDDO
            ENDIF
C--------------------------------------------------------
C UPDATE OFFSET AND GO TO NEXT IRREP PAIR
C--------------------------------------------------------
            IF (ARP.NE.BRP) THEN
               ABIJOFF = ABIJOFF + NV(ARP) * NV(BRP) * RCW
            ELSE
               ABIJOFF = ABIJOFF + NV(ARP) * (NV(ARP)-1) * RCW / 2
            ENDIF
 20         CONTINUE
         ENDDO
      ENDDO
 10   CONTINUE
      ENDDO
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE LAMBD21(T1,L1,S2)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates contribution to L2 equations
C
C---------------Routines called----------------------------------------
C
C     XCOPY, XAXPY
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 L1(*),T1(*),S2(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "symm.inc"
      INCLUDE "complex.inc"
C
C---------------Local variables--------------------------------------
C
      REAL*8 T1MIN(2)
      DATA T1MIN /2*0.0D0/
C
C---------------Executable code--------------------------------------
C
      ABIJOFF = 0
      DO IJRP = 1, NREP
      DO 10 JRP = 1, NREP
      IRP = MULTB(JRP,IJRP+NREP,2)
      IF (IRP.LT.JRP) GOTO 10
      DO J = 1, NO(JRP)
         IMIN = 1
         IF (IRP.EQ.JRP) IMIN = J + 1
         DO I = IMIN, NO(IRP)
            DO 20 BRP = 1, NREP
            ARP = MULTB(BRP,IJRP+NREP,2)
            IF (ARP.LT.BRP) GOTO 20
C--------------------------------------------------------------
C L2(AB,IJ) = L2(AB,IJ) + L1(A,I) * T1(B,J) + T1(A,I) * L1(B,J)
C--------------------------------------------------------------
            IF (BRP.EQ.JRP) THEN
               AB = 1
               BJ = (IVO(JRP) + (J-1) * NV(JRP)) * RCW + 1
               DO B = 1, NV(BRP)
                  AMIN = 1
                  IF (ARP.EQ.BRP) AMIN = B + 1
                  AI = (IVO(IRP) + (I-1) * NV(IRP) + AMIN - 1) * RCW + 1
                  NA = NV(ARP) - AMIN + 1
                  CALL XAXPY (NA,T1(BJ),L1(AI),1,S2(ABIJOFF+AB),1)
                  CALL XAXPY (NA,L1(BJ),T1(AI),1,S2(ABIJOFF+AB),1)
                  AB = AB + NA * RCW
                  BJ = BJ + RCW
               ENDDO
            ENDIF
C--------------------------------------------------------------
C S2(AB,IJ) = S2(AB,IJ) - L1(A,J) * T1(B,I) - T1(A,J) * L1(B,I)
C--------------------------------------------------------------
            IF (ARP.EQ.JRP) THEN
               AB = 1
               BI = (IVO(IRP) + (I-1) * NV(IRP)) * RCW + 1
               DO B = 1, NV(BRP)
                  AMIN = 1
                  IF (ARP.EQ.BRP) AMIN = B + 1
                  AJ = (IVO(JRP) + (J-1) * NV(JRP) + AMIN - 1) * RCW + 1
                  NA = NV(ARP) - AMIN + 1
                  T1MIN(1) = -T1(BI)
                  IF (CARITH) T1MIN(2) = -T1(BI+1)
                  CALL XAXPY (NA,T1MIN,L1(AJ),1,S2(ABIJOFF+AB),1)
                  T1MIN(1) = -L1(BI)
                  IF (CARITH) T1MIN(2) = -L1(BI+1)
                  CALL XAXPY (NA,T1MIN,T1(AJ),1,S2(ABIJOFF+AB),1)
                  AB = AB + NA * RCW
                  BI = BI + RCW
               ENDDO
            ENDIF
C--------------------------------------------------------
C UPDATE OFFSET AND GO TO NEXT IRREP PAIR
C--------------------------------------------------------
            IF (ARP.NE.BRP) THEN
               ABIJOFF = ABIJOFF + NV(ARP) * NV(BRP) * RCW
            ELSE
               ABIJOFF = ABIJOFF + NV(ARP) * (NV(ARP)-1) * RCW / 2
            ENDIF
 20         CONTINUE
         ENDDO
      ENDDO
 10   CONTINUE
      ENDDO
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE CINTM(T1,T2,HOV,AZ,BUF1,BUF2,BUF3,NBUF3,CZ)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C
C     Make modified VOOO integrals for lambda equations
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
      REAL*8 T1(*),T2(*),AZ(*),HOV(*),CZ(*)
      REAL*8 BUF1(*),BUF2(*),BUF3(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "complex.inc"
      INCLUDE "symm.inc"
C
C---------------Local variables--------------------------------------
C
      REAL*8 T1VAL(2)
      DATA T1VAL /2*0.0D0/
      LOGICAL DONE
      real*8 ddot,xsum
C
C---------------Executable code--------------------------------------
C
C---------------------------------------------------------------------
C CZ(AL,IJ) = V(AL,IJ)
C---------------------------------------------------------------------
      CALL GETVOOO (CZ)
C--------------------------------------------------
C CZ(AL,IJ) = CZ(AL,IJ) - T(A,K) * A(KL,IJ)
C--------------------------------------------------
      CALL SRT1T2 (NREP,MULTB,LFA,NOOT,NO,NO,NOOT,LOOOOT,LLOOOT,AZ,BUF1)
      CALL CNTRCT ('N','N',NV,NOOOT,NO,A1,T1,BUF1,A0,BUF2,NREP)
      CALL SRT1S2 (NREP,MULTB,LTR,NVO,NV,NO,NOOT,LVOOOT,LLOOOT,
     &             BUF2,BUF1)
      CALL XAXPY (NV2,-A1,BUF2,1,CZ,1)
C----------------------------------------------------
C CZ(AL,IJ) = CZ(AL,IJ) + V(AL,CD) * TAU(CD,IJ)
C----------------------------------------------------
      CALL GETTAU (T1,T2,BUF1)
      OFF1 = 1
      OFF2 = 1
      DO 30 IRP = 1, NREP
         IF (NVO(IRP).EQ.0) GOTO 30
         DONE = .FALSE.
         ISTART = 0
         MINT = NBUF3/NVO(IRP)
Clv : Check this code : it probably does not work after the
Clv : parallelization. Does not matter because these lambda routines
Clv : were not completed or used anyway. Comments holds also for next
Clv : GETVOVV calls that adddress the distributed integrals
  10     CALL GETVOVV (IRP,ISTART,NINT,DONE,BUF3,MINT)
         M = NVO (IRP)
         N = NOOT(IRP)
         K = NINT
         CALL XGEMM ('N','N',M,N,K,A1,BUF3,M,BUF1(OFF1+ISTART*RCW),
     &               NVVT(IRP),A1,CZ(OFF2),M)
         IF (.NOT.DONE) THEN
            ISTART = ISTART + NINT
            GOTO 10
         ENDIF
         OFF1 = OFF1 + M * N * RCW
         OFF2 = OFF2 + M * N * RCW
 30    CONTINUE
C-------------------------------------------------------
C LVOOO1(CI,KL) = VVOOO(CI,KL) + VVVOO (CD,KL) * T1(D,I)
C-------------------------------------------------------
      CALL GETVVOO (BUF1)
      CALL SRT1C1 (NREP,NVVT,NOOT,BUF1,BUF2)
      CALL SRT1T3 (NREP,MULTB,LFA,NOOT,NV,NV,NOOVT,KOOVVT,KKOOVT,
     &             BUF2,BUF1)
      CALL CNTRCT ('N','N',NOOVT,NO,NV,A1,BUF1,T1,A0,BUF2,NREP)
      CALL SRT1S3 (NREP,MULTB,LTR,NOOT,NV,NO,NOOOT,KOOVOT,KKOOVT,
     &             BUF2,BUF1)
      CALL SRT1C1 (NREP,NOOT,NVO,BUF1,BUF2)
      CALL GETVOOO (BUF1)
      CALL XAXPY (IVOOOT(NREP+1),A1,BUF2,1,BUF1,1)
C------------------------------------------------------------------
C CZ(AL,IJ) = CZ(AL,IJ) + PIJ.[ T(AC,IK) * LVOOO1(KL,CJ) ]
C------------------------------------------------------------------
      IF (CARITH) CALL CONJUGA (NV2,BUF1,1)
      CALL SRT1ST4 (NREP,MULTB,LFA,NV,NO,NO,NO,MVO,JVOOO,JJVO,JJOO,
     &              BUF1,BUF2)
      CALL SRT1TT4 (NREP,MULTB,LFA,LFA,NV,NV,NO,NO,MVO,J2VOVO,JJVO,JJVO,
     &              T2,BUF1)
C     ---------------------------------------------------------
C     Ordered as T(AI,CK) and V(CK,JL) : do the multiplication.
C     ---------------------------------------------------------
      OFF1 = 1
      OFF3 = 1
      DO IREP = 1, NREP
         JREP = MULTB(IREP+NREP,1+NREP,2)
         M = MVO(IREP)
         N = MOO(IREP)
         K = MVO(JREP)
         OFF2 = J2VOOO(JREP) * RCW + 1
         CALL XGEMM ('N','N',M,N,K,A1,BUF1(OFF1),M,BUF2(OFF2),K,
     &               A0,BUF3(OFF3),M)
         OFF1 = OFF1 + M * K * RCW
         OFF3 = OFF3 + M * N * RCW
      ENDDO
C     -----------------------------------------------------------------
C     Sort from order (AI,JL) back to (AL,JI) and add with a minus sign
C     -----------------------------------------------------------------
      CALL SRT26 (NREP,MULTB,LFA,LTR,NV,NO,NO,NO,MVO,JVOOO,JJVO,JJOO,
     &            BUF3,BUF1)
      CALL XAXPY (NV2,-A1,BUF1,1,CZ,1)
C------------------------------------------------------------------
C CZ(AL,IJ) = CZ(AL,IJ) + PIJ.[ V(AL,ID) * T(D,J) ]
C------------------------------------------------------------------
      CALL GETVOVO (BUF1)
      CALL SRT19 (NREP,MULTB,NVO,NV,NO,NVOO,KVOOV,KKVOO,BUF1,BUF2)
      OFF1 = 1
      OFF2 = 1
      OFF3 = 1
      DO IRP =1 , NREP
         M = NVOO(IRP)
         N = NO(IRP)
         K = NV(IRP)
         CALL XGEMM ('N','N',M,N,K,A1,BUF2(OFF1),M,T1(OFF2),K,
     &               A0,BUF1(OFF3),M)
         OFF1 = OFF1 + M * K * RCW
         OFF2 = OFF2 + K * N * RCW
         OFF3 = OFF3 + M * N * RCW
      ENDDO
      CALL SRT1T3 (NREP,MULTB,LTR,NVO,NO,NO,NVOO,KVOOO,KKVOO,BUF1,BUF2)
      CALL XAXPY (NV2,A1,BUF2,1,CZ,1)
C------------------------------------------------------
C CZ(AL,IJ) = CZ(AL,IJ) + HOV(L,C) * T(AC,IJ) ]
C------------------------------------------------------
      CALL SRT1T2 (NREP,MULTB,LFA,NVVT,NV,NV,NOOT,LVVOOT,LLVOOT,T2,BUF1)
      OFF1 = 1
      OFF2 = 1
      OFF3 = 1
      DO IRP = 1, NREP
         M = NO(IRP)
         N = NVOOT(IRP)
         K = NV(IRP)
         CALL XGEMM ('C','N',M,N,K,A1,HOV(OFF1),K,BUF1(OFF2),K,
     &                A0,BUF2(OFF3),M)
         OFF1 = OFF1 + M * K * RCW
         OFF2 = OFF2 + K * N * RCW
         OFF3 = OFF3 + M * N * RCW
      ENDDO
      CALL SRT1S2 (NREP,MULTB,LTR,NOV,NO,NV,NOOT,LOVOOT,LLVOOT,
     &             BUF2,BUF1)
      CALL SRT1L1 (NREP,MULTB,LTR,NOV,NO,NV,NOOT,IVOOOT,IIVO,BUF1,BUF2)
      CALL XAXPY (NV2,A1,BUF2,1,CZ,1)
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE DINTM(T1,T2,HOV,BUF1,BUF2,NBUF2,BUF3,NBUF3,H)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates LVOVV intermediate for lambda equations and write it to
C     file. Note that this is the complex conjugate of the intermediate
C     defined by Gauss & Stanton
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
      REAL*8 T1(*),T2(*),HOV(*)
      REAL*8 BUF1(*),BUF2(*),BUF3(*),H(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "symm.inc"
      INCLUDE "eqns.inc"
      INCLUDE "complex.inc"
C
C---------------Local variables--------------------------------------
C
      REAL*8 T1CON(2)
      DATA T1CON /2*0.0D0/
      LOGICAL DONE
C
C---------------Executable code--------------------------------------
C
C -------------------------------------------------------------
C H1(AK,CJ) = V(AK,CJ) + V(AK,CD) * T(D,J) - V(KL,CD) * T(AD,LJ)
C -------------------------------------------------------------
      CALL DINTM1(T1,T2,BUF1,BUF2,BUF3,NBUF3,H)
      IF (CARITH) CALL CONJUGA (NV4,H,1)
C
      CALL GETTAU (T1,T2,BUF1)
      DO 50 IRP = 1, NREP
         IF (NOV(IRP).EQ.0.OR.NVVT(IRP).EQ.0) GOTO 50
         MINT = MIN0(NBUF3/NVVT(IRP),NBUF2/NVO(IRP))
         ISTART = 0
 10      CALL GETVOVV (IRP,ISTART,NINT,DONE,BUF2,MINT)
C----------------------------------------------
C D(CJ,AB) =  VVOVV(CJ,AB) + T*(J,D) * W(CD,AB)
C----------------------------------------------
         MINT = NBUF3/NVVT(IRP)
         CALL GETVVVV (IRP,ISTART,NINT,DONE,BUF3,MINT)
         IF (CARITH) THEN
            CALL CONJUGA (NINT*NVVT(IRP),BUF3,1)
         ENDIF
C----------------------------------------------
C W(CD,AB) =  V(CD,AB) + V(CD,KL) * TAU*(KL,AB)
C----------------------------------------------
         CALL GETVVOO (T2)
         M = NVVT(IRP)
         N = NINT
         K = NOOT(IRP)
         ABKL = (IVVOOTT(IRP)+ISTART)*RCW+1
         CALL XGEMM ('N','C',M,N,K,A1,T2,M,BUF1(ABKL),NVVT(IRP),
     &               A1,BUF3,M)
         CALL GETAMPT(T1,T2)
         CD = 1
         DO 20 DREP = 1, NREP
            CREP = MULTB(DREP,IRP+NREP,2)
            IF (CREP.LT.DREP) GOTO 20
            DO D = 1, NV(DREP)
               CMIN = 1
               IF (CREP.EQ.DREP) CMIN = D + 1
               DO C = CMIN, NV(CREP)
                  JREP = DREP
                  DO J = 1, NO(JREP)
                     DJ = (IVO(JREP)+(J-1)*NV(DREP)+D-1) * RCW + 1
                     CJ = (IIVO(CREP,JREP)+(J-1)*NV(CREP)+C-1) * RCW + 1
                     T1CON(1) =  T1(DJ)
                     IF (CARITH) T1CON(2) = - T1(DJ+1)
                     CALL XAXPY(NINT,T1CON,BUF3(CD),1,
     &                          BUF2(CJ),NVO(IRP))
                  ENDDO
                  JREP = CREP
                  DO J = 1, NO(JREP)
                     CJ = (IVO(JREP)+(J-1)*NV(CREP)+C-1) * RCW + 1
                     DJ = (IIVO(DREP,JREP)+(J-1)*NV(DREP)+D-1) * RCW + 1
                     T1CON(1) =  - T1(CJ)
                     IF (CARITH) T1CON(2) = T1(CJ+1)
                     CALL XAXPY(NINT,T1CON,BUF3(CD),1,
     &                          BUF2(DJ),NVO(IRP))
                  ENDDO
                  CD = CD + RCW
               ENDDO
            ENDDO
 20      CONTINUE
C----------------------------------------------
C D(CJ,AB) =  D(CJ,AB) + V(CJ,KL) * TAU*(KL,AB)
C----------------------------------------------
         CALL GETVOOO (BUF3)
         M = NVO(IRP)
         N = NINT
         K = NOOT(IRP)
         ABKL = (IVVOOTT(IRP)+ISTART)*RCW+1
         CALL XGEMM ('N','C',M,N,K,A1,BUF3,M,BUF1(ABKL),NVVT(IRP),
     &               A1,BUF2,M)
C----------------------------------------------
C D(CJ,AB) =  D(CJ,AB) - H(C,K) * T2*(KJ,AB)
C----------------------------------------------
         ABKJ = (IVVOOTT(IRP)+ISTART)*RCW+1
         DO 30 JREP = 1, NREP
            KREP = MULTB(JREP,IRP+NREP,2)
            IF (KREP.LT.JREP) GOTO 30
            DO J = 1, NO(JREP)
               KMIN = 1
               IF (KREP.EQ.JREP) KMIN = J + 1
               DO K = KMIN, NO(KREP)
                  CREP = KREP
                  DO C = 1, NV(CREP)
                     CK = (IVO(KREP)+(K-1)*NV(CREP)+C-1) * RCW + 1
                     CJ = (IIVO(CREP,JREP)+(J-1)*NV(CREP)+C-1) * RCW + 1
                     T1CON(1) =  - HOV(CK)
                     IF (CARITH) T1CON(2) = HOV(CK+1)
                     CALL XAXPY(NINT,T1CON,T2(ABKJ),1,
     &                          BUF2(CJ),NVO(IRP))
                  ENDDO
                  CREP = JREP
                  DO C = 1, NV(CREP)
                     CJ = (IVO(JREP)+(J-1)*NV(CREP)+C-1) * RCW + 1
                     CK = (IIVO(CREP,KREP)+(K-1)*NV(CREP)+C-1) * RCW + 1
                     T1CON(1) =  HOV(CJ)
                     IF (CARITH) T1CON(2) = - HOV(CJ+1)
                     CALL XAXPY(NINT,T1CON,T2(ABKJ),1,
     &                          BUF2(CK),NVO(IRP))
                  ENDDO
                  ABKJ = ABKJ + NVVT(IRP) * RCW
               ENDDO
            ENDDO
 30      CONTINUE
C----------------------------------------------
C D(CJ,AB) =  D(CJ,AB) - H1(AK,CJ) * T1*(K,B)
C----------------------------------------------
         AB = 0
         DO 40 BREP = 1, NREP
            AREP = MULTB(BREP,IRP+NREP,2)
            IF (AREP.LT.BREP) GOTO 40
            DO B = 1, NV(BREP)
               AMIN = 1
               IF (AREP.EQ.BREP) AMIN = B + 1
               DO A = AMIN, NV(AREP)
                  IF (AB.LT.ISTART) GOTO 40
                  IF (AB.GE.ISTART+NINT) GOTO 41
                  KREP = BREP
                  AKCJ = (IVOVO(IRP)+IIVO(AREP,KREP)+A-1)
                  DO K = 1, NO(KREP)
                     BK = (IVO(KREP)+(K-1)*NV(BREP)+B-1) * RCW + 1
                     T1CON(1) = - T1(BK)
                     IF (CARITH) T1CON(2) = T1(BK+1)
                     CALL XAXPY (NVO(IRP),T1CON,H(AKCJ),NVO(IRP),
     &                           BUF2(AB*RCW+1),1)
                     AKCJ = AKCJ + NV(AREP) * RCW
                  ENDDO
                  KREP = AREP
                  BKCJ = (IVOVO(IRP)+IIVO(BREP,KREP)+B-1)
                  DO K = 1, NO(KREP)
                     AK = (IVO(KREP)+(K-1)*NV(BREP)+A-1) * RCW + 1
                     T1CON(1) = T1(AK)
                     IF (CARITH) T1CON(2) = - T1(AK+1)
                     CALL XAXPY (NVO(IRP),T1CON,H(BKCJ),NVO(IRP),
     &                           BUF2(AB*RCW+1),1)
                     BKCJ = BKCJ + NV(BREP) * RCW
                  ENDDO
                  AB = AB + 1
               ENDDO
            ENDDO
   40    CONTINUE
   41    CONTINUE
C
         CALL PUTDZ (IRP,ISTART,NINT,DONE,BUF2)
C
         IF (.NOT.DONE) THEN
            ISTART = ISTART + NINT
            GOTO 10
         ENDIF
 50   CONTINUE
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE MKGPP (T2,S2,BUF1,BUF2,GOO,GVV)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates GOO and GVV intermediates for lambda equations.
C     Note that the definition differs from Gauss & Stanton
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 GOO(*),GVV(*)
      REAL*8 T2(*),S2(*)
      REAL*8 BUF1(*),BUF2(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "symm.inc"
      INCLUDE "eqns.inc"
      INCLUDE "complex.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
C----------------------------------------------------------------------
C G(I,J) = G(I,J) - L(CD,IL) * T(CD,JL) = - L(CDL,I) * T(CDL,J)
C----------------------------------------------------------------------
      CALL SRT1T3(NREP,MULTB,LFA,NVVT,NO,NO,NVVOT,KVVOOT,KKVVOT,S2,BUF1)
      CALL SRT1T3(NREP,MULTB,LFA,NVVT,NO,NO,NVVOT,KVVOOT,KKVVOT,T2,BUF2)
      CALL CNTRCT ('C','N',NO,NO,NVVOT,-A1,BUF1,BUF2,A0,GOO,NREP)
C----------------------------------------------------------------------
C G(A,B) = L(AD,KL) * T(BD,KL) = L(A,DKL) * T(B,DKL)
C----------------------------------------------------------------------
      CALL SRT1T2 (NREP,MULTB,LFA,NVVT,NV,NV,NOOT,LVVOOT,LLVOOT,S2,BUF1)
      CALL SRT1T2 (NREP,MULTB,LFA,NVVT,NV,NV,NOOT,LVVOOT,LLVOOT,T2,BUF2)
      CALL CNTRCT ('N','C',NV,NV,NVOOT,A1,BUF1,BUF2,A0,GVV,NREP)
C
      IF (CARITH) THEN
         CALL CONJUGA (NFVV,GVV,1)
      ENDIF
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE CNTRCT (TRANSA,TRANSB,MDIM,NDIM,KDIM,ALPHA,A,B,
     &                   BETA,C,NREP)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Generalization of XGEMM to contract symmetry packed matrices
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      REAL*8 A(*),B(*),C(*)
      COMPLEX*16 ALPHA,BETA
      INTEGER MDIM(NREP),NDIM(NREP),KDIM(NREP)
      CHARACTER*1 TRANSA,TRANSB
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "complex.inc"
C
C---------------Local variables--------------------------------------
C
C---------------Executable code--------------------------------------
C
      OFF1 = 1
      OFF2 = 1
      OFF3 = 1
      DO IRP = 1, NREP
         M = MDIM(IRP)
         N = NDIM(IRP)
         K = KDIM(IRP)
         IF (TRANSA.EQ.'N'.OR.TRANSA.EQ.'n') THEN
            LDA = M
         ELSE
            LDA = K
         ENDIF
         IF (TRANSB.EQ.'N'.OR.TRANSB.EQ.'n') THEN
            LDB = K
         ELSE
            LDB = N
         ENDIF
         LDC = M
         IF (K.EQ.0) THEN
            CALL XSCAL (M*N,BETA,C(OFF3),1)
         ELSE
            CALL XGEMM (TRANSA,TRANSB,M,N,K,ALPHA,A(OFF1),LDA,
     &                  B(OFF2),LDB,BETA,C(OFF3),LDC)
         ENDIF
         OFF1 = OFF1 + M * K * RCW
         OFF2 = OFF2 + K * N * RCW
         OFF3 = OFF3 + M * N * RCW
      ENDDO
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE DINTM1(T1,T2,BUF1,BUF2,BUF3,NBUF3,H)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates H-like intermediate for D intermediate in Lambda equations
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
      REAL*8 T1(*),T2(*),H(*)
      REAL*8 BUF1(*),BUF2(*),BUF3(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "complex.inc"
      INCLUDE "symm.inc"
C
C---------------Local variables--------------------------------------
C
      REAL*8 T1VAL(2)
      DATA T1VAL /2*0.0D0/
      LOGICAL DONE
      real*8 ddot,xsum
C
C---------------Executable code--------------------------------------
C
C---------------------------------------------------------------------
C H(AK,IC) = - W(AK,CI)
C We store h initially as H(AK,CI), ordening and minus sign after next
C step !
C---------------------------------------------------------------------
      CALL GETVOVO (BUF1)
C------------------------------------------------------------------
C H(AK,CI) = H(AK,CI) + W(AK,CD) * T(D,I)
C H is kept in BUF1, still ordered VOVO
C------------------------------------------------------------------
      DO 30 IRP = 1, NREP
         IF (NVO(IRP).EQ.0) GOTO 30
         DONE = .FALSE.
         ISTART = 0
         MINT = NBUF3/NVO(IRP)
  10     CALL GETVOVV (IRP,ISTART,NINT,DONE,BUF3,MINT)
         CD = 0
         DO 20 DREP = 1, NREP
           CREP = MULTB(DREP,IRP+NREP,2)
           IF (CREP.LT.DREP) GOTO 20
           DO D = 1, NV(DREP)
              CMIN = 1
              IF (CREP.EQ.DREP) CMIN = D + 1
              DO C = CMIN, NV(CREP)
                 CD = CD + 1
                 IF (CD.GT.NINT) THEN
C                   -----------------------
C                   We need the next buffer
C                   -----------------------
                    ISTART = ISTART + NINT
                    CALL GETVOVV (IRP,ISTART,NINT,DONE,BUF3,MINT)
                    CD = 1
                 ENDIF
                 AKCD = (CD-1)*NVO(IRP)*RCW+1
                 DO I = 1, NO(DREP)
                    CI = IIVO(CREP,DREP)+(I-1)*NV(CREP)+C
                    AKCI = (IVOVO(IRP)+(CI-1)*NVO(IRP))*RCW+1
                    T1DI = (IVO(DREP)+(I-1)*NV(DREP)+D-1) * RCW + 1
                    T1VAL(1) = T1(T1DI)
                    IF (CARITH) T1VAL(2) = T1(T1DI+1)
                    CALL XAXPY(NVO(IRP),T1VAL,BUF3(AKCD),1,BUF1(AKCI),1)
                 ENDDO
                 DO I = 1, NO(CREP)
                    DI = IIVO(DREP,CREP)+(I-1)*NV(DREP)+D
                    AKDI = (IVOVO(IRP)+(DI-1)*NVO(IRP))*RCW+1
                    T1CI = (IVO(CREP)+(I-1)*NV(CREP)+C-1) * RCW + 1
                    T1VAL(1) = - T1(T1CI)
                    IF (CARITH) T1VAL(2) = - T1(T1CI+1)
                    CALL XAXPY(NVO(IRP),T1VAL,BUF3(AKCD),1,BUF1(AKDI),1)
                 ENDDO
              ENDDO
           ENDDO
 20      CONTINUE
 30    CONTINUE
C------------------------------------------------------------------
C Now sort H(AK,CI) to H(AI,CK)
C------------------------------------------------------------------
      CALL SRT16 (NREP,MULTB,LFA,LFA,NV,NO,NV,NO,MVO,JVOVO,JJVO,JJVO,
     &            BUF1,H)
C------------------------------------------------------------------
C H(AI,CK) = H(AI,CK) + T(AD,IL) * W(KL,CD)
C------------------------------------------------------------------
      CALL SRT1TT4 (NREP,MULTB,LFA,LFA,NV,NV,NO,NO,MVO,J2VOVO,JJVO,JJVO,
     &              T2,BUF1)
      CALL GETVVOO (BUF3)
      IF (CARITH) CALL CONJUGA (NV3,BUF3,1)
      CALL SRT1TT4 (NREP,MULTB,LFA,LFA,NV,NV,NO,NO,MVO,J2VOVO,JJVO,JJVO,
     &              BUF3,BUF2)
      OFF1 = 1
      OFF3 = 1
      DO IREP = 1, NREP
         JREP = MULTB(IREP+NREP,1+NREP,2)
         M = MVO(IREP)
         N = MVO(IREP)
         K = MVO(JREP)
         OFF2 = J2VOVO(JREP) * RCW + 1
         CALL XGEMM ('N','N',M,N,K,A1,BUF1(OFF1),M,BUF2(OFF2),K,
     &               A1,H(OFF3),M)
         OFF1 = OFF1 + M * K * RCW
         OFF3 = OFF3 + M * N * RCW
      ENDDO
C------------------------------------------------------------------
C Sort H(AI,CK) back to H(AK,CI)
C------------------------------------------------------------------
      CALL SRT16 (NREP,MULTB,LFA,LTR,NV,NO,NV,NO,MVO,JVOVO,JJVO,JJVO,
     &            BUF1,H)
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE DINTM2(T1,BUF1,BUF3,NBUF3,H)
C
      IMPLICIT INTEGER (A-Z)
C
C---------------Description--------------------------------------------
C
C     Calculates H-like intermediate in Lambda2 equations
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
      REAL*8 T1(*),H(*)
      REAL*8 BUF1(*),BUF3(*)
C
C---------------Common Blocks--------------------------------------
C
      INCLUDE "param.inc"
      INCLUDE "complex.inc"
      INCLUDE "symm.inc"
C
C---------------Local variables--------------------------------------
C
      REAL*8 T1VAL(2)
      DATA T1VAL /2*0.0D0/
      LOGICAL DONE
      real*8 ddot,xsum
C
C---------------Executable code--------------------------------------
C
C---------------------------------------------------------------------
C Zero the array H (AK,CI)
C We store h initially as H(AK,CI), ordening and minus sign after next
C step !
C---------------------------------------------------------------------
      CALL XCOPY (NV4,A0,0,BUF1,1)
C------------------------------------------------------------------
C H(AK,CI) = H(AK,CI) + W(AK,CD) * T(D,I)
C H is kept in BUF1, still ordered VOVO
C------------------------------------------------------------------
      DO 30 IRP = 1, NREP
         IF (NVO(IRP).EQ.0) GOTO 30
         DONE = .FALSE.
         ISTART = 0
         MINT = NBUF3/NVO(IRP)
  10     CALL GETVOVV (IRP,ISTART,NINT,DONE,BUF3,MINT)
         CD = 0
         DO 20 DREP = 1, NREP
           CREP = MULTB(DREP,IRP+NREP,2)
           IF (CREP.LT.DREP) GOTO 20
           DO D = 1, NV(DREP)
              CMIN = 1
              IF (CREP.EQ.DREP) CMIN = D + 1
              DO C = CMIN, NV(CREP)
                 CD = CD + 1
                 IF (CD.GT.NINT) THEN
C                   -----------------------
C                   We need the next buffer
C                   -----------------------
                    ISTART = ISTART + NINT
                    CALL GETVOVV (IRP,ISTART,NINT,DONE,BUF3,MINT)
                    CD = 1
                 ENDIF
                 AKCD = (CD-1)*NVO(IRP)*RCW+1
                 DO I = 1, NO(DREP)
                    CI = IIVO(CREP,DREP)+(I-1)*NV(CREP)+C
                    AKCI = (IVOVO(IRP)+(CI-1)*NVO(IRP))*RCW+1
                    T1DI = (IVO(DREP)+(I-1)*NV(DREP)+D-1) * RCW + 1
                    T1VAL(1) = T1(T1DI)
                    IF (CARITH) T1VAL(2) = T1(T1DI+1)
                    CALL XAXPY(NVO(IRP),T1VAL,BUF3(AKCD),1,BUF1(AKCI),1)
                 ENDDO
                 DO I = 1, NO(CREP)
                    DI = IIVO(DREP,CREP)+(I-1)*NV(DREP)+D
                    AKDI = (IVOVO(IRP)+(DI-1)*NVO(IRP))*RCW+1
                    T1CI = (IVO(CREP)+(I-1)*NV(CREP)+C-1) * RCW + 1
                    T1VAL(1) = - T1(T1CI)
                    IF (CARITH) T1VAL(2) = - T1(T1CI+1)
                    CALL XAXPY(NVO(IRP),T1VAL,BUF3(AKCD),1,BUF1(AKDI),1)
                 ENDDO
              ENDDO
           ENDDO
 20      CONTINUE
 30    CONTINUE
C------------------------------------------------------------------
C Now sort H(AK,CI) to H(AI,CK)
C------------------------------------------------------------------
      CALL SRT16 (NREP,MULTB,LFA,LFA,NV,NO,NV,NO,MVO,JVOVO,JJVO,JJVO,
     &            BUF1,H)
C
      RETURN
      END
