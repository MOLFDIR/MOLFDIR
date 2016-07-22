      SUBROUTINE RESKB (MFDOEMR,MFDOEMI,IREC,NL,NS,NLR,NSR,C,
     &                  AR,AI,BR,BI,CR,CI,TEMPE,E)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     DIAGONALIZE [P^2]SS AND REMOVE SOLUTIONS WITH ZERO E.V.'S
C     THIS PROCEDURE IS INDENTICAL TO USING RESTRICTED KIN. BALANCE
C     WHEN USING UNCONTRACTED BASIS SETS
C 
C     Luuk Visscher, March 1996.
C
#if defined (BIT64)
      PARAMETER (A0=0.0,A1=1.0)
#else
      PARAMETER (A0=0.D0,A1=1.D0)
#endif
      LOGICAL PRRKB
      PARAMETER (PRRKB=.FALSE.)
      REAL*8 C,AR(*),AI(*),BR(*),BI(*),CR(*),CI(*)
C
C     -------------------
C     READ C [@.P] MATRIX
C     -------------------
C
      NBSL = NS*NL
      READ (MFDOEMR, REC = IREC + 6) (CR(I),I=1,NBSL)
      READ (MFDOEMI, REC = IREC + 6) (CI(I),I=1,NBSL)
C
      DO I = 1, NBSL
          CR(I)=CR(I)/C
          CI(I)=CI(I)/C
      ENDDO
C
C     ---------------------------------------
C     TRANSFORM LEFT TO ORTHOGONAL S.C. BASIS
C     ---------------------------------------
C
      READ (MFDOEMR, REC = IREC + 9) (AR(I),I=1,NS*NSR)
      READ (MFDOEMI, REC = IREC + 9) (AI(I),I=1,NS*NSR)
C
#if defined (BIT64)
      CALL SGEMM ('T','N',NSR,NL,NS,A1,AR,NS,CR,NS,A0,BR,NSR)
      CALL SGEMM ('T','N',NSR,NL,NS,A1,AI,NS,CI,NS,A1,BR,NSR)
      CALL SGEMM ('T','N',NSR,NL,NS,A1,AR,NS,CI,NS,A0,BI,NSR)
      CALL SGEMM ('T','N',NSR,NL,NS,-A1,AI,NS,CR,NS,A1,BI,NSR)
#else
      CALL DGEMM ('T','N',NSR,NL,NS,A1,AR,NS,CR,NS,A0,BR,NSR)
      CALL DGEMM ('T','N',NSR,NL,NS,A1,AI,NS,CI,NS,A1,BR,NSR)
      CALL DGEMM ('T','N',NSR,NL,NS,A1,AR,NS,CI,NS,A0,BI,NSR)
      CALL DGEMM ('T','N',NSR,NL,NS,-A1,AI,NS,CR,NS,A1,BI,NSR)
#endif
C
C     ----------------------------------------
C     TRANSFORM RIGHT TO ORTHOGONAL L.C. BASIS
C     ----------------------------------------
C
      READ (MFDOEMR, REC = IREC + 8) (AR(I),I=1,NL*NLR)
      READ (MFDOEMI, REC = IREC + 8) (AI(I),I=1,NL*NLR)
C
#if defined (BIT64)
      CALL SGEMM ('N','N',NSR,NLR,NL,A1,BR,NSR,AR,NL,A0,CR,NSR)
      CALL SGEMM ('N','N',NSR,NLR,NL,-A1,BI,NSR,AI,NL,A1,CR,NSR)
      CALL SGEMM ('N','N',NSR,NLR,NL,A1,BR,NSR,AI,NL,A0,CI,NSR)
      CALL SGEMM ('N','N',NSR,NLR,NL,A1,BI,NSR,AR,NL,A1,CI,NSR)
#else
      CALL DGEMM ('N','N',NSR,NLR,NL,A1,BR,NSR,AR,NL,A0,CR,NSR)
      CALL DGEMM ('N','N',NSR,NLR,NL,-A1,BI,NSR,AI,NL,A1,CR,NSR)
      CALL DGEMM ('N','N',NSR,NLR,NL,A1,BR,NSR,AI,NL,A0,CI,NSR)
      CALL DGEMM ('N','N',NSR,NLR,NL,A1,BI,NSR,AR,NL,A1,CI,NSR)
#endif
C
C     ------------------------
C     MAKE P^2 = [@.P] * [@.P]
C     ------------------------
C
#if defined (BIT64)
      CALL SGEMM ('N','T',NSR,NSR,NLR,A1,CR,NSR,CR,NSR,A0,BR,NSR)
      CALL SGEMM ('N','T',NSR,NSR,NLR,A1,CI,NSR,CI,NSR,A1,BR,NSR)
      CALL SGEMM ('N','T',NSR,NSR,NLR,-A1,CR,NSR,CI,NSR,A0,BI,NSR)
      CALL SGEMM ('N','T',NSR,NSR,NLR,A1,CI,NSR,CR,NSR,A1,BI,NSR)
#else
      CALL DGEMM ('N','T',NSR,NSR,NLR,A1,CR,NSR,CR,NSR,A0,BR,NSR)
      CALL DGEMM ('N','T',NSR,NSR,NLR,A1,CI,NSR,CI,NSR,A1,BR,NSR)
      CALL DGEMM ('N','T',NSR,NSR,NLR,-A1,CR,NSR,CI,NSR,A0,BI,NSR)
      CALL DGEMM ('N','T',NSR,NSR,NLR,A1,CI,NSR,CR,NSR,A1,BI,NSR)
#endif
C
C     ---------------
C     DIAGONALIZE P^2
C     ---------------
C
      CALL CRDIAG (NSR,NSR,BR,BI,AR,CR,CI,TEMPE,TEMPE,E,IFAIL)
C
C     -------------------------------------------------
C     NUMBER OF ZERO EIGENVALUES SHOULD BE EQUAL TO NLR
C     -------------------------------------------------
C
      IF (PRRKB) THEN
         DO I = 1, NSR
            PRINT*,IREC,I,AR(I)
         ENDDO
      ENDIF
C
      NSR2 = NLR
C
C     ------------------------------
C     MAKE NEW TRANSFORMATION MATRIX
C     ------------------------------
C
      READ (MFDOEMR, REC = IREC + 9) (CR(I),I=1,NS*NSR)
      READ (MFDOEMI, REC = IREC + 9) (CI(I),I=1,NS*NSR)
C
      II = NSR * (NSR - NSR2) + 1
#if defined (BIT64)
      CALL SGEMM ('N','N',NS,NSR2,NSR,A1,CR,NS,BR(II),NSR,A0,AR,NS)
      CALL SGEMM ('N','N',NS,NSR2,NSR,-A1,CI,NS,BI(II),NSR,A1,AR,NS)
      CALL SGEMM ('N','N',NS,NSR2,NSR,A1,CR,NS,BI(II),NSR,A0,AI,NS)
      CALL SGEMM ('N','N',NS,NSR2,NSR,A1,CI,NS,BR(II),NSR,A1,AI,NS)
#else
      CALL DGEMM ('N','N',NS,NSR2,NSR,A1,CR,NS,BR(II),NSR,A0,AR,NS)
      CALL DGEMM ('N','N',NS,NSR2,NSR,-A1,CI,NS,BI(II),NSR,A1,AR,NS)
      CALL DGEMM ('N','N',NS,NSR2,NSR,A1,CR,NS,BI(II),NSR,A0,AI,NS)
      CALL DGEMM ('N','N',NS,NSR2,NSR,A1,CI,NS,BR(II),NSR,A1,AI,NS)
#endif
C
      WRITE (MFDOEMR, REC = IREC + 9) (AR(I),I=1,NS*NSR2)
      WRITE (MFDOEMI, REC = IREC + 9) (AI(I),I=1,NS*NSR2)
C
C     -----------------------------------
C     MAKE NEW BACK-TRANSFORMATION MATRIX
C     -----------------------------------
C
      READ (MFDOEMR, REC = IREC + 14) (CR(I),I=1,NS*NSR)
      READ (MFDOEMI, REC = IREC + 14) (CI(I),I=1,NS*NSR)
C
      II = NSR * (NSR - NSR2) + 1
#if defined (BIT64)
      CALL SGEMM ('N','N',NS,NSR2,NSR,A1,CR,NS,BR(II),NSR,A0,AR,NS)
      CALL SGEMM ('N','N',NS,NSR2,NSR,-A1,CI,NS,BI(II),NSR,A1,AR,NS)
      CALL SGEMM ('N','N',NS,NSR2,NSR,A1,CR,NS,BI(II),NSR,A0,AI,NS)
      CALL SGEMM ('N','N',NS,NSR2,NSR,A1,CI,NS,BR(II),NSR,A1,AI,NS)
#else
      CALL DGEMM ('N','N',NS,NSR2,NSR,A1,CR,NS,BR(II),NSR,A0,AR,NS)
      CALL DGEMM ('N','N',NS,NSR2,NSR,-A1,CI,NS,BI(II),NSR,A1,AR,NS)
      CALL DGEMM ('N','N',NS,NSR2,NSR,A1,CR,NS,BI(II),NSR,A0,AI,NS)
      CALL DGEMM ('N','N',NS,NSR2,NSR,A1,CI,NS,BR(II),NSR,A1,AI,NS)
#endif
C
      WRITE (MFDOEMR, REC = IREC + 14) (AR(I),I=1,NS*NSR2)
      WRITE (MFDOEMI, REC = IREC + 14) (AI(I),I=1,NS*NSR2)
C
      NSR = NSR2
C
      RETURN
      END
