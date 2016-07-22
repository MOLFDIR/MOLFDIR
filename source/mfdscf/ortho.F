C
C     ===================================================
      SUBROUTINE ORTHO (FOCKR, FOCKI, NBASL, NBASS, IREC,
     +                  SR, SI, BUFR, BUFI,NLR,NSR,IOPT)
C     ===================================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     TRANSFORM THE MATRIX FOCK* TO AN ORTHOGONAL BASIS (TRANSFORM
C     WITH Q)
C     IF IOPT = 1 WE TRANSFORM THE FOCK MATRIX WITH Q ON RECORDS
C     8 AND 9 OF MFDOEM*
C     IF IOPT = 2 WE TRANSFORM THE DENSITY MATRIX WITH Q ON
C     RECORDS 14 AND 15 OF MFDOEM*
C     MFDOEMR AND MFDOEMI MUST BE OPEN
C     SR, SI, BUFR, BUFI ARE USED AS SCRATCH (OF DIMENSION MSS)
C
      INCLUDE 'paracom.inc'
      INCLUDE 'general.inc'
C
      REAL*8 FOCKR (*), FOCKI (*)
      INTEGER NBASL, NBASS, IREC
      REAL*8 SR (MS, MS), SI (MS, MS)
      REAL*8 BUFR (*), BUFI (*)
C
      NBAS = NBASL + NBASS
      NRT = NLR + NSR
C
C     -----------------------------
C     READ LARGE COMPONENT Q MATRIX
C     -----------------------------
C
      IF (IOPT .EQ. 1) THEN
      IRECTR = 8
      ELSE
      IRECTR = 13
      ENDIF
      READ (MFDOEMR, REC = IREC + IRECTR)
     +     ((SR (I,J), I=1,NBASL), J=1,NLR)
      READ (MFDOEMI, REC = IREC + IRECTR)
     +     ((SI (I,J), I=1,NBASL), J=1,NLR)
C
C     ---------------------------
C     CALCULATE [Q+][F][Q] FOR LL
C     ---------------------------
C
      CALL OMTR (NBASL, NLR, NBASL, NBAS, FOCKR(1),
     +           FOCKI(1), MS, SR, SI, MS, BUFR, BUFI)
      CALL OMTL (NLR, NLR, NBASL, MS, SR, SI, MS, BUFR, BUFI,
     +           NBAS, FOCKR(1), FOCKI(1))
C
      IF (.NOT. TWOC) THEN
C
C     -----------------------
C     CALCULATE [F][Q] FOR SL
C     -----------------------
C
      CALL OMTR (NBASS, NLR, NBASL, NBAS, FOCKR (1 + NBAS * NBASL),
     +           FOCKI (1 + NBAS * NBASL), MS, SR, SI, MS, BUFR,BUFI)
C
C     -----------------------------
C     READ SMALL COMPONENT Q MATRIX
C     -----------------------------
C
      IF (IOPT .EQ. 1) THEN
      IRECTR = 9
      ELSE
      IRECTR = 14
      ENDIF
      READ (MFDOEMR, REC = IREC + IRECTR)
     +     ((SR (I,J), I=1,NBASS), J=1,NSR)
      READ (MFDOEMI, REC = IREC + IRECTR)
     +     ((SI (I,J), I=1,NBASS), J=1,NSR)
C
C     ---------------------------
C     CALCULATE [Q+][F][Q] FOR SL
C     ---------------------------
C
      CALL OMTL (NSR, NLR, NBASS, MS, SR, SI, MS, BUFR, BUFI,
     +           NBAS, FOCKR (1 + NLR),
     +           FOCKI (1 + NLR))
C
C     --------------------------
C     CALCULTE [Q+][F][Q] FOR SS
C     --------------------------
C
      CALL OMTR (NBASS, NSR, NBASS, NBAS,
     +           FOCKR (1 + NBASL * (NBAS + 1)),
     +           FOCKI (1 + NBASL * (NBAS + 1)),
     +           MS, SR, SI, MS, BUFR, BUFI)
      CALL OMTL (NSR, NSR, NBASS, MS, SR, SI, MS, BUFR, BUFI,
     +           NBAS, FOCKR (1 + NLR * (NBAS + 1)),
     +           FOCKI (1 + NLR * (NBAS + 1)))
      ENDIF
      RETURN
      END
