      SUBROUTINE SKELOUT(NSP,NKR,J,I,JI,NSKSIZ,NSKBUF,NBCK,LBCK,
     &                   LBRCSK,RL,CBCK)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     IN RL, THE FOUR-INDEX TRANSFORMED INTEGRALS (LK|JI) WITH JI FIXED;
C     DISTRIBUTE (LK|JI) AND (JI|LK) LARGER THEN THRESHOLD OVER BUCKETS 
C     WHICH ARE LABELED BY JI; FULL BUCKETS ARE WRITTEN TO FILE
C
      INTEGER NBCK(*),LBCK(NSKSIZ,NSKBUF),LBRCSK(0:*)
      COMPLEX*16 CBCK(NSKSIZ,NSKBUF),RL(NSP*NSP)
C
      INCLUDE "vecs.inc"
      INCLUDE "files.inc"
C
      PARAMETER (THRSH=1.0D-16)
C
C     ------------------------------------------------------------------
C     Restore the particle-particle symmetry by storing the integral
C     both as (lk|ji) and as (ji|lk).
C     Because the record index is triangular both in permutation and tr
C     and the batch index is square in both we have to expand
C     the ji indices.
C     ------------------------------------------------------------------
      JISQ = (I-1)*NSP+J
      IJSQ = (J-1)*NSP+I
      ITR = ABS(ITRMO(I))
      JTR = ABS(ITRMO(J))
      JITRSQ = (ITR-1)*NSP+JTR
      IJTRSQ = (JTR-1)*NSP+ITR
      ISIJ = ISIGN(1,ITRMO(I)*ITRMO(J))
      LKSQ = 0
      DO K = 1, NSP
      DO L = 1, NSP
         LKSQ = LKSQ + 1
#if defined (BIT64)
         IF (CABS (RL(LKSQ)).GT.THRSH) THEN
#else
         IF (CDABS(RL(LKSQ)).GT.THRSH) THEN
#endif
            NB=NBCK(JI)+1
            LBCK(NB,JI)=LKSQ
            CBCK(NB,JI)=RL(LKSQ)
            IF (NB.EQ.NSKSIZ) THEN
              LREC=LBRCSK(0)+1
              WRITE(INTSKEL,REC=LREC) NB,LBRCSK(JI),
     +                               (CBCK(IX,JI),IX=1,NB),
     +                               (LBCK(IX,JI),IX=1,NB)
              LBRCSK(JI)=LREC
              LBRCSK(0)=LREC
              NB=0
            ENDIF
            NBCK(JI)=NB
        ENDIF
      ENDDO
      ENDDO
C--> Now the particle-interchanged contributions
      LK = 0
      DO KBAR = 1, 2
      DO KKR = 1, NKR
         IF (KBAR.EQ.1) K=KRA(KKR,1)
         IF (KBAR.EQ.2) K=KRB(KKR,1)
         DO 10 LKR = KKR, NKR
            LK = LK + 1
            L = KRB(LKR,1)
            LKSQ = (K-1)*NSP+L
            KLSQ = (L-1)*NSP+K
            KTR = ABS(ITRMO(K))
            LTR = ABS(ITRMO(L))
            LKTRSQ = (KTR-1)*NSP+LTR
            KLTRSQ = (LTR-1)*NSP+KTR
            ISKL = ISIGN(1,ISIJ*ITRMO(K)*ITRMO(L))
#if defined (BIT64)
            IF (CABS (RL(LKSQ)).GT.THRSH) THEN
#else
            IF (CDABS(RL(LKSQ)).GT.THRSH) THEN
#endif
               NB=NBCK(LK)+1
               LBCK(NB,LK)=JISQ
               CBCK(NB,LK)=RL(LKSQ)
               IF (NB.EQ.NSKSIZ) THEN
                  LREC=LBRCSK(0)+1
                  WRITE(INTSKEL,REC=LREC) NB,LBRCSK(LK),
     +                                    (CBCK(IX,LK),IX=1,NB),
     +                                    (LBCK(IX,LK),IX=1,NB)
                  LBRCSK(LK)=LREC
                  LBRCSK(0)=LREC
                  NB=0
               ENDIF
               NBCK(LK)=NB
            ENDIF
#if defined (BIT64)
            IF (CABS (RL(LKTRSQ)).GT.THRSH) THEN
#else
            IF (CDABS(RL(LKTRSQ)).GT.THRSH) THEN
#endif
               NB=NBCK(LK)+1
               LBCK(NB,LK)=JITRSQ
               CBCK(NB,LK)=DCONJG(RL(LKTRSQ))*ISKL
               IF (NB.EQ.NSKSIZ) THEN
                  LREC=LBRCSK(0)+1
                  WRITE(INTSKEL,REC=LREC) NB,LBRCSK(LK),
     +                                    (CBCK(IX,LK),IX=1,NB),
     +                                    (LBCK(IX,LK),IX=1,NB)
                  LBRCSK(LK)=LREC
                  LBRCSK(0)=LREC
                  NB=0
               ENDIF
               NBCK(LK)=NB
            ENDIF
            IF (IJSQ.EQ.JISQ.OR.IJTRSQ.EQ.JISQ) GOTO 10
#if defined (BIT64)
            IF (CABS (RL(KLSQ)).GT.THRSH) THEN
#else
            IF (CDABS(RL(KLSQ)).GT.THRSH) THEN
#endif
               NB=NBCK(LK)+1
               LBCK(NB,LK)=IJSQ
#if defined (BIT64)
               CBCK(NB,LK)=CONJG(RL(KLSQ))
#else
               CBCK(NB,LK)=DCONJG(RL(KLSQ))
#endif
               IF (NB.EQ.NSKSIZ) THEN
                  LREC=LBRCSK(0)+1
                  WRITE(INTSKEL,REC=LREC) NB,LBRCSK(LK),
     +                                    (CBCK(IX,LK),IX=1,NB),
     +                                    (LBCK(IX,LK),IX=1,NB)
                  LBRCSK(LK)=LREC
                  LBRCSK(0)=LREC
                  NB=0
               ENDIF
               NBCK(LK)=NB
            ENDIF
#if defined (BIT64)
            IF (CABS (RL(KLTRSQ)).GT.THRSH) THEN
#else
            IF (CDABS(RL(KLTRSQ)).GT.THRSH) THEN
#endif
               NB=NBCK(LK)+1
               LBCK(NB,LK)=IJTRSQ
               CBCK(NB,LK)=RL(KLTRSQ)*ISKL
               IF (NB.EQ.NSKSIZ) THEN
                  LREC=LBRCSK(0)+1
                  WRITE(INTSKEL,REC=LREC) NB,LBRCSK(LK),
     +                                    (CBCK(IX,LK),IX=1,NB),
     +                                    (LBCK(IX,LK),IX=1,NB)
                  LBRCSK(LK)=LREC
                  LBRCSK(0)=LREC
                  NB=0
               ENDIF
               NBCK(LK)=NB
            ENDIF
  10     CONTINUE
      ENDDO
      ENDDO
C
      RETURN
      END
