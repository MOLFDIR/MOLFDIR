C
      SUBROUTINE DENSMT (DR,DI,VECTR,VECTI,ILIND,IRIND,
     &                   JLIND,JRIND,JVECL,IJREC,
     &                   DENSR,DENSI,IVWA,IVEC,DD)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     =====================================
C     CALCULATES FIRST ORDER DENSITY MATRIX
C     =====================================
C
      INCLUDE 'param.inc'
      INCLUDE 'general.inc'
      INCLUDE 'symm.inc'
      INCLUDE 'files.inc'
      INCLUDE 'mempoint.inc'
C
      DIMENSION DENSR(NORB,NORB),DENSI(NORB,NORB),DD(NORB)
      DIMENSION ILIND(*),IRIND(*),JLIND(*),JRIND(*),IVWA(*)
      DIMENSION DR(MDET,*),DI(MDET,*),VECTR(NDET,*),VECTI(NDET,*)
      DIMENSION JVECL(2,*),IJREC(NORB,NORB)
C
C     -----------------------------------------
C     LOOP OVER SINGLE PARTICLE REPRESENTATIONS
C     -----------------------------------------
      DO 1200 I = 1, NORB
         DO 1100 J = 1, I   
            DENSR(I,J) = 0.D0
            IF (.NOT.REALAR) DENSI(I,J) = 0.D0
            IF (IRPAMO(J).NE.IRPAMO(I)) GOTO 1100
            IREC = 0
            IF (IGENEX.EQ.2.AND.IJREC(I,J).LT.0) IREC = -IJREC(I,J)
            CALL GETIDS (I,J,0,0,IREP,ND,NV,ILIND,IRIND,
     &                   JLIND(1),JRIND(1),JVECL(1,1),
     &                   IVWA(1),IVWA(1+IVWI),IREC)
            IVERT = 0
C     --------------------------------
C     START "WHILE LOOP" OVER VERTICES
C     --------------------------------
   30      IVERT = IVERT + 1
           JD1 = JVECL(1,IVERT) + 1 
           JD2 = JVECL(1,IVERT+1) 
           NDH = IABS(JVECL(2,IVERT))
           IF (NDH.EQ.0) GOTO 1100
C$DIR FORCE_PARALLEL
           DO 500 ID = JD1, JD2
              IS = ISIGN(1,JRIND(ID))
              ILD = JLIND(ID) - 1
              IRD = IABS(JRIND(ID)) - 1
              IF (REALAR) THEN
                 DO 398 IDH = 1, NDH
                    IL = ILD + IDH
                    IR = IRD + IDH
                    DENSR(I,J) = DENSR(I,J) +
     &              VECTR(IL,IVEC) * IS * VECTR(IR,IVEC) 
  398            CONTINUE
              ELSE
                 DO 400 IDH = 1, NDH
                    IL = ILD + IDH
                    IR = IRD + IDH
                    DENSR(I,J) = DENSR(I,J) +
     &              VECTR(IL,IVEC) * IS * VECTR(IR,IVEC) +
     &              VECTI(IL,IVEC) * IS * VECTI(IR,IVEC)
                    DENSI(I,J) = DENSI(I,J) +
     &              VECTR(IL,IVEC) * IS * VECTI(IR,IVEC) -
     &              VECTI(IL,IVEC) * IS * VECTR(IR,IVEC)
  400            CONTINUE
              ENDIF
  500         CONTINUE
C             ----------------
C             GOTO NEXT VERTEX
C             ----------------
           GOTO 30
 1100    CONTINUE
 1200 CONTINUE
      DO 1220 I = 1, NORB
         DO 1210 J = 1, I - 1
            DENSR(J,I) = DENSR(I,J)
 1210       CONTINUE
 1220    CONTINUE
      IF (.NOT.REALAR) THEN
         DO 1221 I = 1, NORB
            DO 1211 J = 1, I - 1
               DENSI(J,I) = - DENSI(I,J)
 1211       CONTINUE
 1221    CONTINUE
      ENDIF
      DO 1230 I = 1, NORB
 1230    DD(I) = DENSR(I,I)
      MVEC = NVEC
      RETURN
      END
