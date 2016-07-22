C
      SUBROUTINE MAKIDS (ILIND,IRIND,IJREC,JLIND,JRIND,
     &                   JVECL,IVWA)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     GENERATE FILE WITH ALL 1-ELECTRON COUPLING COEFFICIENTS
C
      INCLUDE 'param.inc'
      INCLUDE 'files.inc'
      INCLUDE 'symm.inc'
      INCLUDE 'mempoint.inc' 
C
      DIMENSION JVECL(2,*),JLIND(*),JRIND(*),ILIND(*),IRIND(*)
      DIMENSION IJREC(NORB,NORB),IVWA(*)
C
      NSTORE = 0
      NCOUPL = 0
      IREC = 0
      IVERT = 1
      ICOREX = 1
      LNCORE = LNORBC*NORB*NORB
      LEXREC = 512
#if defined (BIT64)
      LENREC = LEXREC * 8
#else
      LENREC = LEXREC * 4
#endif
      IF (IGENEX.EQ.2) OPEN (MRCEXCS,FILE=FNAM(6),ACCESS='DIRECT',
     &                       RECL=LENREC)
      DO 200 I = 1, NORB
         DO 100 J = 1, NORB
            IJREC(I,J) = 0
            CALL GETIDS (I,J,0,0,IREP,ND,NVERT,ILIND,IRIND,
     &                   JLIND(ICOREX),JRIND(ICOREX),
     &                   JVECL(1,IVERT),IVWA(1),
     &                   IVWA(1+IVWI),0)
            NCOUPL = NCOUPL + ND
            IF (ND.LE.LNCORE) THEN
               NSTORE = NSTORE + ND
               JVERT = IVERT + NVERT
               IJREC(I,J) = IVERT
               DO 20 KVERT = IVERT, JVERT - 1
                  JVECL(1,KVERT) = JVECL(1,KVERT) + ICOREX - 1
   20             CONTINUE
               IVERT = JVERT
               ICOREX = ICOREX + ND
               LNCORE = LNCORE - ND
               GOTO 100
            ENDIF
            IF (IGENEX.EQ.1) GOTO 100
            IREC = IREC + 1
            IJREC(I,J) = - IREC
C           IJREC(J,I) = - IREC
            INREC = 0
            NSKIP = 2 * NVERT + 4
            NDWR1 = 1
            NDWR2 = MIN0(ND,NDWR1+(LEXREC-2-NSKIP)/3-1)
            IF (NDWR2.NE.ND) INREC = IREC + 1
            WRITE (MRCEXCS,REC=IREC) INREC,NVERT,NDWR1,NDWR2,
     &            ((JVECL(IP,IV),IP=1,2),IV=IVERT,IVERT+NVERT-1),
     &            (JLIND(ID),JRIND(ID),ID=NDWR1+ICOREX-1,
     &                                    NDWR2+ICOREX-1)
   99       IF (INREC.EQ.0) GOTO 100
            NSKIP = 4
            IREC = IREC + 1
            INREC = 0
            NDWR1 = NDWR2 + 1
            NDWR2 = MIN0(ND,NDWR1+(LEXREC-2-NSKIP)/3-1)
            IF (NDWR2.NE.ND) INREC = IREC + 1
            WRITE (MRCEXCS,REC=IREC) INREC,NVERT,NDWR1,NDWR2,
     &            (JLIND(ID),JRIND(ID),ID=NDWR1+ICOREX-1,
     &                                    NDWR2+ICOREX-1)
            GOTO 99
  100       CONTINUE
  200    CONTINUE
      IF (LNCORE.EQ.0) GOTO 500
      DO 300 I = 1, NORB
         DO 400 J = 1, NORB
            IF (IJREC(I,J).LE.0) THEN
               CALL GETIDS (I,J,0,0,IREP,ND,NVERT,ILIND,IRIND,
     &                      JLIND(ICOREX),JRIND(ICOREX),
     &                      JVECL(1,IVERT),IVWA(1),
     &                      IVWA(1+IVWI),0)
               IF (ND.LE.LNCORE) THEN
                  NSTORE = NSTORE + ND
                  NCOUPL = NCOUPL - ND
                  JVERT = IVERT + NVERT
                  IJREC(I,J) = IVERT
                  DO 30 KVERT = IVERT, JVERT - 1
                     JVECL(1,KVERT) = JVECL(1,KVERT) + ICOREX - 1
   30             CONTINUE
                  IVERT = JVERT
                  ICOREX = ICOREX + ND
                  LNCORE = LNCORE - ND
                  IF (LNCORE.EQ.0) GOTO 500
                  GOTO 400
               ENDIF
            ENDIF
  400    CONTINUE
  300 CONTINUE
  500 NCOREX = ICOREX
      NTVERT = IVERT
      RSTORE = REAL(NSTORE) / REAL(NCOUPL) * 100.0
      WRITE (6,1000) RSTORE
      RETURN
C
 1000 FORMAT (/,F6.2,' Percent of the Coupling ',
     & 'Coefficients are stored.')
C
      END
