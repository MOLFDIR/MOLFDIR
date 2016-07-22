C
      SUBROUTINE CALIDS_UP (NORB1,NORB2,NORB3,NORB4,ISYM,JDET,NVERT,
     &                      ILIND,IRIND,JLIND,JRIND,JVECL,IVWU,
     &                      IVDET,JVDET)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     ======================================================================
C     GIVEN : - EXCITATION A+(NORB1)A(NORB2) = E(NORB1,NORB2)
C             - ABELIAN SYMMETRY CHARACTER OF BRA OR KET DETERMINANTS
C               ISYM>0 KET DETERMINANTS BELONG TO REPRESENTATION ISYM
C               ISYM<0 BRA DETERMINANTS BELONG TO REPRESENTATION ISYM
C     GENERATE : - ADRESSES OF ALL INTERACTING DETERMINANTS
C                  JLIND: TAIL PART OF ADRESSES OF BRA DETERMINANTS
C                  JRIND: TAIL PART OF ADRESSES OF KET DETERMINANTS
C                         PLUS SIGN OF INTERACTION MULTIPLIED BY HEAD PART
C     DIRECTION : WALK FROM BOTTOM TO TOP OF GRAPH    
C     ======================================================================
C
      INCLUDE 'param.inc'
      INCLUDE 'symm.inc'
C
      INTEGER JVECL(2,*)
      INTEGER ILIND(*),IRIND(*),JLIND(*),JRIND(*)
      INTEGER IVDET(16,0:NORB,0:NELEC),JVDET(16,0:NORB,0:NELEC)
      INTEGER IVWU(16,0:NORB,0:NELEC)
      LOGICAL IJARR
C
      JDET = 0
      NVERT = 1
      JVECL(1,1) = 0
      JVECL(2,1) = 0
      JVECL(1,2) = 0
C     -------------------------------------------------------------
C     SET CONSTRAINTS IMPOSED BY THE PREVIOUS (IF ANY) EXCITATION :
C     A+(NORB3)A(NORB4)
C     -------------------------------------------------------------
      ICREA = NORB3 - 1
      IANNI = NORB4 - 1
      IF (IANNI.EQ.ICREA) IANNI = - 1
      NDORB = NORB1 - NORB2
      IF (NDORB.LT.0) THEN
         ITAIL = NORB2
         IHEAD = NORB1
      ELSE
         ITAIL = NORB1
         IHEAD = NORB2
      ENDIF
C
      ITSYM = IRPAMO(ITAIL)
      IHSYM = IRPAMO(IHEAD)
      IF (ISYM*NDORB.GT.0) THEN
         LSYM = MULTBI(IHSYM,ABS(ISYM))
         LSYM = MULTB(ITSYM,LSYM)
      ELSE
         LSYM = ABS(ISYM)
      ENDIF
C     -----------------------------
C     LAST SWITCH MUST END IN JXIND
C     -----------------------------
      IF (MOD(NORB-IHEAD,2).NE.0) THEN
         IJARR = .TRUE.
         JLIND(1) = 1
         JRIND(1) = 1
      ELSE
         IJARR = .FALSE.
         ILIND(1) = 1
         IRIND(1) = 1
      ENDIF
C
      DO 30 I = 0, NORB
      DO 20 J = MAX0(IORBR(I)-1,0), MIN0(NELEC,I+1)
      DO 10 IR = 1, NREP
      IVDET(IR,I,J) = 0
      JVDET(IR,I,J) = 0
   10 CONTINUE
   20 CONTINUE
   30 CONTINUE
C
      IVDET(LSYM,NORB,NELEC) = 0
      JVDET(LSYM,NORB,NELEC) = 1
C     --------------
C     MAKE ALL TAILS
C     --------------
      DO 300 I = NORB-1, ITAIL, -1
      IJARR = (.NOT.IJARR)
      JDET = 0
      JMIN = IORBR(I)
      JMIN = MAX0(NELEC+I-NORB,0)
      JMAX = MIN0(NELEC,I)
C     ----------------------------------------------------------------
C     LOOP OVER VERTICES : CHECK IF BOTH (PARALLEL) PATHS ARE POSSIBLE
C     ----------------------------------------------------------------
      DO 200 J = JMAX, JMIN, -1
      JLMIN = (NREP / 2 * MOD(J+1,2)) + 1
      JLMAX = JLMIN + (NREP / 2) - 1
      DO 100 JLR = JLMIN, JLMAX
      JRR = MULTBI(ITSYM,JLR)
      JRR = MULTB(IHSYM,JRR)
      ILR = MULTB(JLR,IRPAMO(I+1))
      IRR = MULTB(JRR,IRPAMO(I+1))
      IF (IVWU(JLR,I,J).NE.0.AND.IVWU(JRR,I,J).NE.0) THEN
C     -------------------------------------------
C     MAKE DETERMINANTS WHICH PASS THESE VERTICES
C     -------------------------------------------
      IVDET(JLR,I,J) = JDET
      IF (IJARR) THEN
         IF (J.NE.NELEC.AND.I.NE.IANNI) THEN
            DO 60 K = IVDET(ILR,I+1,J+1)+1,
     &                IVDET(ILR,I+1,J+1)+JVDET(ILR,I+1,J+1)
            JDET = JDET + 1
            JLIND(JDET) = ILIND(K) + IVWU(ILR,I,J+1)
            JRIND(JDET) = IRIND(K) + IVWU(IRR,I,J+1)
   60       CONTINUE
         ENDIF
         IF (I.NE.ICREA) THEN
            DO 70 K = IVDET(JLR,I+1,J)+1,
     &                IVDET(JLR,I+1,J)+JVDET(JLR,I+1,J)
            JDET = JDET + 1
            JLIND(JDET) = ILIND(K)
            JRIND(JDET) = IRIND(K)
   70       CONTINUE
         ENDIF
      ELSE
         IF (J.NE.NELEC.AND.I.NE.IANNI) THEN
            DO 61 K = IVDET(ILR,I+1,J+1)+1,
     &                IVDET(ILR,I+1,J+1)+JVDET(ILR,I+1,J+1)
            JDET = JDET + 1
            ILIND(JDET) = JLIND(K) + IVWU(ILR,I,J+1)
            IRIND(JDET) = JRIND(K) + IVWU(IRR,I,J+1)
   61       CONTINUE
         ENDIF
         IF (I.NE.ICREA) THEN
            DO 71 K = IVDET(JLR,I+1,J)+1,
     &                IVDET(JLR,I+1,J)+JVDET(JLR,I+1,J)
            JDET = JDET + 1
            ILIND(JDET) = JLIND(K)
            IRIND(JDET) = JRIND(K)
   71       CONTINUE
         ENDIF
      ENDIF
      JVDET(JLR,I,J) = JDET - IVDET(JLR,I,J)
      ENDIF
  100 CONTINUE
  200 CONTINUE
      IF (JDET.EQ.0) RETURN
  300 CONTINUE
C
      IF (NDORB.EQ.0) GOTO 5000
C
C     -----------------------------
C     SPLIT THE LEFT AND RIGHT PATH
C     -----------------------------
      I = ITAIL - 1
      IJARR = (.NOT.IJARR)   
      JDET = 0
      JMIN = IORBR(I)
      JMIN = MAX0(NELEC+I-NORB,0)
      JMAX = MIN0(NELEC-1,I-1)
C     -------------------------------------------------------
C     LOOP OVER VERTICES : CHECK IF THE SPLITTING IS POSSIBLE
C     -------------------------------------------------------
      DO 2200 J = JMAX, JMIN, -1
      JLMIN = (NREP / 2 * MOD(J+1,2)) + 1
      JLMAX = JLMIN + (NREP / 2) - 1
      DO 2100 JLR = JLMIN, JLMAX
      JRR = MULTB(IHSYM,JLR)
      ILR = MULTB(IRPAMO(I+1),JLR)
      IF (IVWU(JLR,I,J).NE.0.AND.IVWU(JRR,I,J+1).NE.0) THEN
C     -------------------------------------------
C     MAKE DETERMINANTS WHICH PASS THESE VERTICES
C     -------------------------------------------
      IVDET(JLR,I,J) = JDET
      IF (IJARR) THEN
         DO 2060 K = IVDET(ILR,I+1,J+1)+1,
     &               IVDET(ILR,I+1,J+1)+JVDET(ILR,I+1,J+1)
         JDET = JDET + 1
         JLIND(JDET) = ILIND(K) + IVWU(ILR,I,J+1)
         JRIND(JDET) = IABS(IRIND(K))
 2060    CONTINUE
      ELSE
         DO 2061 K = IVDET(ILR,I+1,J+1)+1,
     &               IVDET(ILR,I+1,J+1)+JVDET(ILR,I+1,J+1)
         JDET = JDET + 1
         ILIND(JDET) = JLIND(K) + IVWU(ILR,I,J+1)
         IRIND(JDET) = IABS(JRIND(K))
 2061    CONTINUE
      ENDIF
      JVDET(JLR,I,J) = JDET - IVDET(JLR,I,J)
      ENDIF
 2100 CONTINUE
 2200 CONTINUE
      IF (JDET.EQ.0) RETURN
C
C     -------------------
C     MAKE ALL LOOPBODIES
C     -------------------
      DO 3300 I = ITAIL - 2, IHEAD, - 1
      IJARR = (.NOT.IJARR)      
      JDET = 0
      JMIN = IORBR(I)
      JMIN = MAX0(NELEC+I-NORB,0)
      JMAX = MIN0(NELEC-1,I-1)
C     ----------------------------------------------------------------
C     LOOP OVER VERTICES : CHECK IF BOTH (PARALLEL) PATHS ARE POSSIBLE
C     ----------------------------------------------------------------
      DO 3200 J = JMAX, JMIN, -1
      JLMIN = (NREP / 2 * MOD(J+1,2)) + 1
      JLMAX = JLMIN + (NREP / 2) - 1
      DO 3100 JLR = JLMIN, JLMAX
      JRR = MULTB(IHSYM,JLR)
      ILR = MULTB(IRPAMO(I+1),JLR)
      IRR = MULTB(IRPAMO(I+1),JRR)
      IF (IVWU(JLR,I,J).NE.0.AND.IVWU(JRR,I,J+1).NE.0) THEN
C     -------------------------------------------
C     MAKE DETERMINANTS WHICH PASS THESE VERTICES
C     -------------------------------------------
      IVDET(JLR,I,J) = JDET
      IF (IJARR) THEN
         IF (I.NE.IANNI) THEN
            DO 3060 K = IVDET(ILR,I+1,J+1)+1,
     &                  IVDET(ILR,I+1,J+1)+JVDET(ILR,I+1,J+1)
               JDET = JDET + 1
               JLIND(JDET) = ILIND(K) + IVWU(ILR,I,J+1)
               JRIND(JDET) = -1 * ISIGN(1,IRIND(K)) * 
     &                       (IABS(IRIND(K)) + IVWU(IRR,I,J+2))
 3060       CONTINUE
         ENDIF
         IF (I.NE.ICREA) THEN
            DO 3070 K = IVDET(JLR,I+1,J)+1,
     &                  IVDET(JLR,I+1,J)+JVDET(JLR,I+1,J)
               JDET = JDET + 1
               JLIND(JDET) = ILIND(K)
               JRIND(JDET) = IRIND(K)
 3070       CONTINUE
         ENDIF
      ELSE
         IF (I.NE.IANNI) THEN
            DO 3061 K = IVDET(ILR,I+1,J+1)+1,
     &                  IVDET(ILR,I+1,J+1)+JVDET(ILR,I+1,J+1)
               JDET = JDET + 1
               ILIND(JDET) = JLIND(K) + IVWU(ILR,I,J+1)
               IRIND(JDET) = -1 * ISIGN(1,JRIND(K)) * 
     &                       (IABS(JRIND(K)) + IVWU(IRR,I,J+2))
 3061       CONTINUE
         ENDIF
         IF (I.NE.ICREA) THEN
            DO 3071 K = IVDET(JLR,I+1,J)+1,
     &                  IVDET(JLR,I+1,J)+JVDET(JLR,I+1,J)
               JDET = JDET + 1
               ILIND(JDET) = JLIND(K)
               IRIND(JDET) = JRIND(K)
 3071       CONTINUE
         ENDIF
      ENDIF
      JVDET(JLR,I,J) = JDET - IVDET(JLR,I,J)
      ENDIF
 3100 CONTINUE
 3200 CONTINUE
      IF (JDET.EQ.0) RETURN
 3300 CONTINUE
C     --------------------------------------------------
C     CLOSE THE LOOPS; PUT THE INDICES ON THE RIGHT SIDE
C     --------------------------------------------------
      I = IHEAD - 1
      JDET = 0
      IVERT = 0
      JMIN = IORBR(I)
      JMIN = MAX0(NELEC+I-NORB,0)
      JMAX = MIN0(NELEC,I)
C     -----------------------------------------------------
C     LOOP OVER VERTICES : CHECK IF THE CLOSURE IS POSSIBLE
C     -----------------------------------------------------
      DO 4200 J = JMAX, JMIN, -1
      JLMIN = (NREP / 2 * MOD(J+1,2)) + 1
      JLMAX = JLMIN + (NREP / 2) - 1
      DO 4100 JLR = JLMIN, JLMAX
      IRR = MULTB(IRPAMO(I+1),JLR)
      IF (IVWU(JLR,I,J).NE.0.AND.JVDET(JLR,I+1,J).GT.0) THEN
C     ----------------------------------------
C     MAKE DETERMINANTS WHICH PASS THIS VERTEX
C     ----------------------------------------
      IVERT = IVERT + 1
      JVECL(1,IVERT) = JDET
      JVECL(2,IVERT) = IVWU(JLR,I,J)
      IF (NDORB.GT.0) THEN
      DO 4060 K = IVDET(JLR,I+1,J)+1,
     &               IVDET(JLR,I+1,J)+JVDET(JLR,I+1,J)
         JDET = JDET + 1
         JLIND(JDET) = ILIND(K)
         JRIND(JDET) = ISIGN(1,IRIND(K)) *
     &                   (IABS(IRIND(K)) + IVWU(IRR,I,J+1))
 4060 CONTINUE
      ELSE
      DO 4080 K = IVDET(JLR,I+1,J)+1,
     &               IVDET(JLR,I+1,J)+JVDET(JLR,I+1,J)
         JDET = JDET + 1
         JLIND(JDET) = IABS(IRIND(K)) + IVWU(IRR,I,J+1)
         JRIND(JDET) = ISIGN(1,IRIND(K)) * ILIND(K)
 4080 CONTINUE
      ENDIF
      ENDIF
 4100 CONTINUE
 4200 CONTINUE
C
      IVERT = IVERT + 1
      JVECL(1,IVERT) = JDET
      JVECL(2,IVERT) = 0
      NVERT = IVERT
      RETURN
C
 5000 CONTINUE
C     -------------------------------
C     NO LOOP BODIES IF IHEAD = ITAIL
C     -------------------------------
      I = IHEAD - 1
      JDET = 0
      IVERT = 0
      JMIN = IORBR(I)
      JMIN = MAX0(NELEC+I-NORB,0)
      JMAX = MIN0(NELEC-1,I)
C     -----------------------------------------------------
C     LOOP OVER VERTICES : CHECK IF THE CLOSURE IS POSSIBLE
C     -----------------------------------------------------
      DO 5200 J = JMAX, JMIN, -1
      JLMIN = (NREP / 2 * MOD(J+1,2)) + 1
      JLMAX = JLMIN + (NREP / 2) - 1
      DO 5100 JLR = JLMIN, JLMAX
      ILR = MULTB(IRPAMO(I+1),JLR)
      IF (IVWU(JLR,I,J).NE.0.AND.JVDET(ILR,I+1,J+1).NE.0
     &    .AND.I.NE.IANNI) THEN
C        ----------------------------------------
C        MAKE DETERMINANTS WHICH PASS THIS VERTEX
C        ----------------------------------------
         IVERT = IVERT + 1
         JVECL(1,IVERT) = JDET
         JVECL(2,IVERT) = IVWU(JLR,I,J)
         DO 5060 K = IVDET(ILR,I+1,J+1)+1,
     &               IVDET(ILR,I+1,J+1)+JVDET(ILR,I+1,J+1)
         JDET = JDET + 1
         JLIND(JDET) = ILIND(K) + IVWU(ILR,I,J+1)
         JRIND(JDET) = JLIND(JDET)
 5060    CONTINUE
      ENDIF
 5100 CONTINUE
 5200 CONTINUE
      IVERT = IVERT + 1
      JVECL(1,IVERT) = JDET
      JVECL(2,IVERT) = 0
      NVERT = IVERT
C
      RETURN
      END
