      SUBROUTINE TRANS2(MDINT,NRJ,LBRCIJ,INDKLP,INDKL,
     & INDRJ,JNDRJ,RJ,RJB,RJC,RK,RLR,RL,REDME,INDK,
     & NSKSIZ1,LBRCSK1,NBCK1,LBCK1,CBCK1,
     & NSKSIZ2,LBRCSK2,NBCK2,LBCK2,CBCK2,
     & NSKSIZ3,LBRCSK3,NBCK3,LBCK3,CBCK3,
     & NSKSIZ4,LBRCSK4,NBCK4,LBCK4,CBCK4)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     Second half-transformation. 
C     Symmetrizes skeleton integrals by transforming to and from
C     reduced matrix elements.
C
      INCLUDE "inpt.inc"
      INCLUDE "vecs.inc"
      INCLUDE "thres.inc"
      INCLUDE "molf.inc"
      INCLUDE "redme.inc"
      INCLUDE "sym.inc"
      INCLUDE "files.inc"
C
      COMPLEX*16 A0,A1
#if defined (BIT64)
      PARAMETER (A0=(0.0,0.0),A1=(1.0,0.0))
#else
      PARAMETER (A0=(0.D0,0.D0),A1=(1.D0,0.D0))
#endif
C
      INTEGER INDKL(*),INDKLP(NSYMRP,NSYMRP,2)
      INTEGER INDRJ(NRJ,2),JNDRJ(NRJ,2),LBRCIJ(0:*)
      COMPLEX*16 RJ(NRJ),RJB(NRJ),RJC(NRJ),REDME(*)
      COMPLEX*16 RK(NSC*NSP),RLR(NSP*NSP),RL(NSP*NSP)
      COMPLEX*16 A,B
      INTEGER LBRCSK1(0:NKR),LBCK1(2,NSKSIZ1,NKR),NBCK1(NSKSIZ1,NKR)
      INTEGER LBRCSK2(0:NKR),LBCK2(2,NSKSIZ2,NKR),NBCK2(NSKSIZ2,NKR)
      INTEGER LBRCSK3(0:NKR),LBCK3(2,NSKSIZ3,NKR),NBCK3(NSKSIZ3,NKR)
      INTEGER LBRCSK4(0:NKR),LBCK4(2,NSKSIZ4,NKR),NBCK4(NSKSIZ4,NKR)
      COMPLEX*16 CBCK1(NSKSIZ1,NKR),CBCK2(NSKSIZ2,NKR)
      COMPLEX*16 CBCK3(NSKSIZ3,NKR),CBCK4(NSKSIZ4,NKR)
      INTEGER INDK(*)
C
      LOGICAL ANYHTI
      LOGICAL GETHTI
      LOGICAL ANYHTBI
      LOGICAL GETHTBI
      LOGICAL CONJUG
C
      IF (IPRNT.GT.10) WRITE (6,1000)
C
C     ------------------------------------------
C     Initialize files for transformed integrals
C     ------------------------------------------
C
      CALL OUTPP0 (1,NKR,NSKSIZ1,LBRCSK1,NBCK1)
      CALL OUTPP0 (2,NKR,NSKSIZ2,LBRCSK2,NBCK2)
      CALL OUTPP0 (3,NKR,NSKSIZ3,LBRCSK3,NBCK3)
      IF (QUATER) CALL OUTPP0 (4,NKR,NSKSIZ4,LBRCSK4,NBCK4)
C
C     -----------------------------------
C     Make index array for gather/scatter 
C     -----------------------------------
C
      CALL SKELIND(NSP,NSYMRP,MOFSET,INDKLP,INDKL)
C
C     ------------------------------------
C     Loop over (multi-dimensional) irreps
C     ------------------------------------
      DO 101 IRA = 1, NREP
      DO 102 IRB = 1, IRA
C     ------------------------------------
C     Loop over spinors in this irrep pair
C     ------------------------------------
        DO 201 MOI = 1, NFIE(IRA)
        DO 202 MOJ = 1, NFIE(IRB)
C     -------------------------------------
C     Zero the reduced matrix element array
C     -------------------------------------
          CALL CZERO (NRME(IRA,IRB),REDME)
C     ---------------------------------
C     Loop over rows in this irrep pair
C     ---------------------------------
          DO 301 IA = IOFFRP(IRA), IOFFRP(IRA+1)-1
          I = MOFSET(IA) + MOI
          DO 302 IB = IOFFRP(IRB), IOFFRP(IRB+1)-1 
          IF (MMATA(IA,IB).EQ.0) GOTO 10
          J = MOFSET(IB) + MOJ
          CALL UNIQUE (NKR,I,J,BREIT,JI,CONJUG,IS)
          IF (.NOT.BREIT) THEN
C
C...........Coulomb type integral
C
C     ----------------------------------
C     Get the half-transformed integrals
C     ----------------------------------
            ANYHTI=GETHTI(JI,RJ,NRJ,MRJ,INDRJ,LBRCIJ)
            IF (.NOT.ANYHTI) GOTO 10
            IF (CONJUG) THEN
               DO 401 INT = 1, MRJ
#if defined (BIT64)
                  RJ(INT) =  CONJG(RJ(INT))
#else
                  RJ(INT) = DCONJG(RJ(INT))
#endif
 401           CONTINUE
            ENDIF
            IF (IS.LT.0) THEN
               DO 402 INT = 1, MRJ
                  RJ(INT) = - RJ(INT)
 402           CONTINUE
            ENDIF
C     -------------------------------
C     Transform to skeleton integrals
C     -------------------------------
            CALL MATMUL3 (RJ,MRJ,INDRJ,NRJ,QA,N22,RK,NSC,NSP,NSC)
#if defined (BIT64)
            CALL CGEMM ('C','N',NSP,NSP,NSC,A1,QA,N22,RK,NSC,A0,RLR,NSP)
#else
            CALL ZGEMM ('C','N',NSP,NSP,NSC,A1,QA,N22,RK,NSC,A0,RLR,NSP)
#endif
            CALL MSUMTIM(RL,NSP,RLR,NSP,NSP,1)
C
         ELSE
C
C...........Breit type integral
C
C     ----------------------------------
C     Get the half-transformed integrals
C     ----------------------------------
            ANYHTBI=GETHTBI(JI,RJ,RJB,RJC,NRJ,MRJ,INDRJ,LBRCIJ)
            IF (.NOT.ANYHTBI) GOTO 10
            IF (CONJUG) THEN
               DO 403 INT = 1, MRJ
#if defined (BIT64)
                  RJ(INT) =  CONJG(RJ(INT))
                  RJB(INT) =  CONJG(RJB(INT))
                  RJC(INT) =  CONJG(RJC(INT))
#else
                  RJ(INT) = DCONJG(RJ(INT))
                  RJB(INT) = DCONJG(RJB(INT))
                  RJC(INT) = DCONJG(RJC(INT))
#endif
 403           CONTINUE
#if defined (BIT64)
               CALL CSWAP (MRJ,RJB,1,RJC,1)
#else
               CALL ZSWAP (MRJ,RJB,1,RJC,1)
#endif
            ENDIF
            IF (IS.LT.0) THEN
               DO 404 INT = 1, MRJ
                  RJ(INT) = - RJ(INT)
                  RJB(INT) = - RJB(INT)
                  RJC(INT) = - RJC(INT)
 404           CONTINUE
            ENDIF
C     -------------------------------
C     Transform to skeleton integrals
C     -------------------------------
            CALL MATMUL3(RJ,MRJ,INDRJ,NRJ,QA,N22,RK,NSC,NSP,NSC)
#if defined (BIT64)
            CALL CGEMM ('C','N',NSP,NSP,NSC,A1,QA,N22,RK,NSC,A0,RLR,NSP)
#else
            CALL ZGEMM ('C','N',NSP,NSP,NSC,A1,QA,N22,RK,NSC,A0,RLR,NSP)
#endif
            CALL MSUMTIM(RL,NSP,RLR,NSP,NSP,-1)
            CALL MATMUL7(RJB,MRJ,INDRJ,NRJ,QB,N22,RK,NSC,NSP,NSC)
#if defined (BIT64)
            CALL CGEMM ('C','N',NSP,NSP,NSC,A1,QA,N22,RK,NSC,A1,RL,NSP)
#else
            CALL ZGEMM ('C','N',NSP,NSP,NSC,A1,QA,N22,RK,NSC,A1,RL,NSP)
#endif
            CALL MATMUL7(RJC,MRJ,INDRJ,NRJ,QA,N22,RK,NSC,NSP,NSC)
#if defined (BIT64)
            CALL CGEMM ('C','N',NSP,NSP,NSC,A1,QB,N22,RK,NSC,A1,RL,NSP)
#else
            CALL ZGEMM ('C','N',NSP,NSP,NSC,A1,QB,N22,RK,NSC,A1,RL,NSP)
#endif
C
            ENDIF
            IF (IPRNT.GT.10) THEN
            LKX = 0
            DO 405 KX = 1, NSP
            DO 406 LX = 1, NSP
               LKX = LKX + 1
#if defined (BIT64)
               IF ( CABS(RL(LKX)).GT.THROUT)
#else
               IF (CDABS(RL(LKX)).GT.THROUT)
#endif
     &         WRITE (*,'(4I4,2F20.16)') LX,KX,J,I,RL(LKX)
 406        CONTINUE
 405        CONTINUE
            ENDIF
C
C           --------------------------------------------
C           Add contributions to reduced matrix elements
C           --------------------------------------------
C
            DO 407 IRME = 1, MMATA(IA,IB)
               NMAT = MMATPA(IRME,IA,IB)
               ITAU = ITAUA(NMAT)
               INDEX = INDTAU(ITAU)
               A = AMAT(NMAT)
               IK = KA(NMAT)
               IL = LA(NMAT)
               NKL = NOC(IK)*NOC(IL)
               KLP = INDKLP(IK,IL,1)
#if defined (BIT64)
               CALL CGTHR(NKL,RL,RJ,INDKL(KLP))
               CALL CAXPY(NKL,A,RJ,1,REDME(INDEX),1)
#else
               CALL ZGTHR(NKL,RL,RJ,INDKL(KLP))
               CALL ZAXPY(NKL,A,RJ,1,REDME(INDEX),1)
#endif
 407        CONTINUE 
C     -----------------------------------------------------
C     All contributions from this row combination are added
C     ------------------------------------------------------
   10       CONTINUE
C     ----------------------------------------
C     End of loops over rows in the irrep pair
C     ----------------------------------------
 302      CONTINUE
 301      CONTINUE
C     ----------------------------------------------
C     The reduced matrix elements are complete
C     Backtransformation to get completed integrals
C     Loop again over rows of the irrep-pair
C     ----------------------------------------------
          DO 501 IA = IOFFRP(IRA), IOFFRP(IRA+1)-1
          I = MOFSET(IA) + MOI
          IKR = IKRMO(I)
          IKRA = ABS(IKR)
          DO 502 IB = IOFFRP(IRB), IOFFRP(IRB+1)-1 
          J = MOFSET(IB) + MOJ
          JKR = IKRMO(J)
          JKRA = ABS(JKR)
          IF (MMATB(IA,IB).EQ.0) GOTO 20
          IF (ABS(IKR).LT.ABS(JKR)) GOTO 20
C     ----------------------------
C     Zero this block of integrals
C     ----------------------------
          CALL CZERO (NSP*NSP,RL)
C     ---------------------------
C     Loop over all contributions
C     ---------------------------
          DO 601 IRME = 1, MMATB(IA,IB)
             NMAT = MMATPB(IRME,IA,IB)
             ITAU = ITAUB(NMAT)
             INDEX = INDTAU(ITAU)
             B = BMAT(NMAT)
             IK = KB(NMAT)
             IL = LB(NMAT)
             NKL = NOC(IK)*NOC(IL)
             KLP = INDKLP(IK,IL,1)
             CALL CZERO (NKL,RJ)
#if defined (BIT64)
             CALL CAXPY(NKL,B,REDME(INDEX),1,RJ,1)
#else
             CALL ZAXPY(NKL,B,REDME(INDEX),1,RJ,1)
#endif
             DO 602 IKL = 1, NKL
                RL(INDKL(KLP+IKL-1)) = RL(INDKL(KLP+IKL-1)) + RJ(IKL)
 602         CONTINUE
 601       CONTINUE
C     -----------------------------------------------------------
C     All contributions are added : write out completed integrals
C     In OUTPP1 the integrals are written as (ij|kl),in OUTPP2
C     the integrals are written as (kl|ij) pre-sorted on k-value
C     Integrals are subdivided in 4 classes
C     -----------------------------------------------------------
           IF (JKR.LT.0) THEN
C............ (ibar, jbar | kbar, lbar)
              CALL OUTPP1 (NKR,NSP,1,INTSKEL+1,THROUT,KRB,KRB,
     &                     IKRA,JKRA,INDK,RL,RLR,NSKEL)
C............ (kbar, lbar | ibar, jbar)
              CALL OUTPP2 (NKR,NSP,1,INTSKEL+5,THROUT,KRB,KRB,
     &                     IKRA,JKRA,RL,NSKSIZ1,LBRCSK1,NBCK1,
     &                     LBCK1,CBCK1,NSKEL)
              IF (QUATER) THEN
C............ (kbar, l    | i   , j   ) = - (ibar, jbar | k   , lbar) *
              CALL CJNVEC(NSP*NSP,RL)
              CALL OUTPP2 (NKR,NSP,4,INTSKEL+8,THROUT,KRA,KRB,
     &                     IKRA,JKRA,RL,NSKSIZ4,LBRCSK4,NBCK4,
     &                     LBCK4,CBCK4,NSKEL)
              CALL CJNVEC(NSP*NSP,RL)
              ENDIF
           ELSE
C............ (ibar, j    | kbar, l   )
              CALL OUTPP1 (NKR,NSP,2,INTSKEL+2,THROUT,KRB,KRA,
     &                     IKRA,JKRA,INDK,RL,RLR,NSKEL)
C............ (kbar, l    | ibar, j   )
              CALL OUTPP2 (NKR,NSP,2,INTSKEL+6,THROUT,KRB,KRA,
     &                     IKRA,JKRA,RL,NSKSIZ2,LBRCSK2,NBCK2,
     &                     LBCK2,CBCK2,NSKEL)
C............ (ibar, j    | k   , lbar)
              CALL OUTPP1 (NKR,NSP,3,INTSKEL+3,THROUT,KRA,KRB,
     &                     IKRA,JKRA,INDK,RL,RLR,NSKEL)
C............ (lbar, k    | j   , ibar) = (ibar, j    | k   , lbar) *
              CALL CJVEC(NSP*NSP,RL)
              CALL OUTPP2 (NKR,NSP,3,INTSKEL+7,THROUT,KRA,KRB,
     &                     IKRA,JKRA,RL,NSKSIZ3,LBRCSK3,NBCK3,
     &                     LBCK3,CBCK3,NSKEL)
              CALL CJVEC(NSP*NSP,RL)
C............ (ibar, j    | k   , l   )
              IF (QUATER) THEN
              CALL OUTPP1 (NKR,NSP,4,INTSKEL+4,THROUT,KRA,KRA,
     &                     IKRA,JKRA,INDK,RL,RLR,NSKEL)
              ENDIF
           ENDIF
   20      CONTINUE
C     ----------------------------------------
C     End of loops over rows in the irrep pair
C     ----------------------------------------
 502     CONTINUE
 501     CONTINUE
C     -------------------------------------------
C     End of loops over spinors in the irrep pair
C     -------------------------------------------
 202    CONTINUE
 201    CONTINUE
C     ---------------------------------
C     End of loops over the irrep pairs
C     ---------------------------------
 102  CONTINUE
 101  CONTINUE
C
C     ----------------------------------------
C     Flush buffers with transformed integrals
C     ----------------------------------------
C
      CALL OUTPP3(NKR,INTSKEL+1,NSKSIZ1,LBRCSK1,NBCK1,LBCK1,CBCK1,NSKEL)
      CALL OUTPP3(NKR,INTSKEL+2,NSKSIZ2,LBRCSK2,NBCK2,LBCK2,CBCK2,NSKEL)
      CALL OUTPP3(NKR,INTSKEL+3,NSKSIZ3,LBRCSK3,NBCK3,LBCK3,CBCK3,NSKEL)
      IF (QUATER)
     &CALL OUTPP3(NKR,INTSKEL+4,NSKSIZ4,LBRCSK4,NBCK4,LBCK4,CBCK4,NSKEL)
C
 1000 FORMAT (/' Molecular Spinor integrals before symmetrization'/)
      RETURN
      END
