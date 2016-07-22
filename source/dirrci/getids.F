c
      SUBROUTINE GETIDS (I,J,K,L,ISYM,ND,NV,ILIND,IRIND,
     &                   JLIND,JRIND,JVECL,
     &                   IVWU,IVWR,IREC)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     GET 1-ELECTRON COEFFICIENTS :
C     CALCULATE IF IGENEX = 1
C     FROM FILE IF IGENEX = 2
C
      INCLUDE 'param.inc'
      INCLUDE 'files.inc'
      INCLUDE 'symm.inc'
      INCLUDE 'mempoint.inc'
C
      DIMENSION ILIND(*),IRIND(*),JLIND(*),JRIND(*)
      DIMENSION JVECL(2,*)
      DIMENSION IVWU(16,0:NORB,0:NELEC),IVWR(*)
C
      INTERN = NORBR(1) + NORBR(2)
      IF (MAXE3.GT.2) INTERN = NORB
      IF (IGENEX.EQ.1.OR.IREC.EQ.0) THEN
         CALL CALIDS_UP (I,J,K,L,ISYM,ND,NV,ILIND,IRIND,JLIND,
     &                   JRIND,JVECL,IVWU,IVWR(1),IVWR(1+IVWI))
         RETURN
      ELSEIF (IGENEX.EQ.2) THEN
C     ------------------------------------------
C     One cannot use the symmetry 
C
C     USE SYMMETRY : <I|E(i,j)|J> = <J|E(j,i)|I>
C
C     if pointgroup symmetry is used
C     ------------------------------------------
      READ (MRCEXCS,REC=IREC) INREC,NVERT,NDWR1,NDWR2,
     &     ((JVECL(IP,IV),IP=1,2),IV=1,NVERT),
     &     (JLIND(ID),JRIND(ID),ID=NDWR1,NDWR2)
  201 IF (INREC.EQ.0) RETURN
      IREC = INREC
      READ (MRCEXCS,REC=IREC) INREC,NVERT,NDWR1,NDWR2,
     &     (JLIND(ID),JRIND(ID),ID=NDWR1,NDWR2)
      GOTO 201
      ENDIF
      RETURN
      END
