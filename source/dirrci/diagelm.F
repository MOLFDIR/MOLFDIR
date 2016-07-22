C
      SUBROUTINE DIAGELM (DR,DGEL,TINTR,TINTI,IJEX,GMATR,GMATI,
     &                    ILIND,IRIND,JLIND,JRIND,JVECL,IVWA,
     &                    IJREC)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     =========================================
C     CALCULATES DIAGONAL ELEMENTS OF CI MATRIX
C     =========================================
C
      INCLUDE 'param.inc'
      INCLUDE 'general.inc'
      INCLUDE 'files.inc'
      INCLUDE 'symm.inc'
      INCLUDE 'mempoint.inc'
C
      DIMENSION DGEL(*),DR(*),TINTR(*),TINTI(*),GMATR(*),GMATI(*)
      DIMENSION IVWA(*),IJREC(NORB,NORB)
      DIMENSION IJEX(*),ILIND(*),IRIND(*),JLIND(*),JRIND(*),JVECL(2,*)
      LOGICAL ENDFIL
C
      REWIND (MRCTWOE)
      CALL ZZERO(NDET,DGEL)
C     -------------------------------------------
C     THE COULOMB TYPE OF CONTRIBUTIONS : (II|JJ)
C     -------------------------------------------
 3000 CALL TWOLIN (ENDFIL,TINTR,TINTI,IJEX,IJ,NIJ)
      IF (ENDFIL) GOTO 3001
        I = (IJ-1)/NORB + 1
        J = MOD(IJ-1,NORB) + 1
        if (i.eq.j) then
           open (88,file='Progress_Dirrci',form='formatted')
           write (88,*) 'Coulomb',i
           close (88)
        endif
        IF (I.NE.J) GOTO 3000
          CALL ZZERO (MDET,DR(1))
          IF (IJREC(I,J).GT.0) THEN
             IVERT = IJREC(I,J) - 1
             ICOREX = 0
          ELSE
             IREC = -IJREC(I,J)
             CALL GETIDS (I,J,0,0,IREP,ND,NV,ILIND,IRIND,
     &                    JLIND(NCOREX),JRIND(NCOREX),
     &                    JVECL(1,NTVERT),IVWA(1),IVWA(1+IVWI),
     &                    IREC)
             IVERT = NTVERT - 1
             ICOREX = NCOREX - 1
          ENDIF
C         --------------------------------
C         START "WHILE LOOP" OVER VERTICES
C         --------------------------------
   30       IVERT = IVERT + 1
            JD1 = JVECL(1,IVERT) + 1 + ICOREX
            JD2 = JVECL(1,IVERT+1) + ICOREX
            NDH = IABS(JVECL(2,IVERT))
            IF (NDH.EQ.0) GOTO 1001
            DO 400 ID = JD1, JD2
               ILD = JLIND(ID) - 1
               DO 300 IDH = 1, NDH
                  IL = ILD + IDH
                  DGEL(IL) = DGEL(IL) + GMATR(IJ)
                  DR(IL) = 1.D0
  300             CONTINUE
  400          CONTINUE
C           --------------
C           DO NEXT VERTEX
C           --------------
            GOTO 30
C
 1001     DO 2000 IX = 1, NIJ
            KL = IJEX(IX)
            K = (KL-1)/NORB + 1
            L = MOD(KL-1,NORB) + 1
            IF (K.EQ.L) THEN
               IF (IJREC(K,L).GT.0) THEN
                  IVERT = IJREC(K,L) - 1
                  ICOREX = 0
               ELSE
                  IREC = -IJREC(K,L)
                  CALL GETIDS (K,L,I,J,IREP,ND,NV,ILIND,IRIND,
     &                         JLIND(NCOREX),JRIND(NCOREX),
     &                         JVECL(1,NTVERT),IVWA(1),IVWA(1+IVWI),
     &                         IREC)
                  IVERT = NTVERT - 1
                  ICOREX = NCOREX - 1
               ENDIF
 1030          IVERT = IVERT + 1
               JD1 = JVECL(1,IVERT) + 1 + ICOREX
               JD2 = JVECL(1,IVERT+1) + ICOREX
               NDH = IABS(JVECL(2,IVERT))
               IF (NDH.EQ.0) GOTO 2000
                DO 1400 ID = JD1, JD2
                   ILD = JLIND(ID) - 1
                   DO 1300 IDH = 1, NDH
                      IL = ILD + IDH
                      DGEL(IL) = DGEL(IL) + TINTR(IX) * DR(IL)
 1300                 CONTINUE
 1400              CONTINUE
C               --------------
C               DO NEXT VERTEX
C               --------------
                GOTO 1030
C            -------------------
C            END IF BLOCK K.EQ.L
C            -------------------
            ENDIF
 2000       CONTINUE
C     -----------------------
C     GET NEXT INTEGRAL BLOCK
C     -----------------------
      GOTO 3000
C
 3001 REWIND (MRCTWOE)
C
C     -------------------------------------------
C     THE EXCHANGE TYPE OF CONTRIBUTIONS : (IJ|JI)
C     -------------------------------------------
C
 5000 CALL TWOLIN (ENDFIL,TINTR,TINTI,IJEX,IJ,NIJ)
      IF (ENDFIL) GOTO 5001
      I = (IJ-1)/NORB + 1
      J = MOD(IJ-1,NORB) + 1
      if (i.eq.j) then
         open (88,file='Progress_Dirrci',form='formatted')
         write (88,*) 'Exchange',i
         close (88)
      endif
      JI = (J-1) * NORB + I
      IF (I.EQ.J) GOTO 5000
      DO 4000 IX = 1, NIJ
         IF (IJEX(IX).EQ.JI) THEN
           IF (IJREC(I,J).GT.0) THEN
              IVERT = IJREC(I,J) - 1
              ICOREX = 0
           ELSE
              IREC = -IJREC(I,J)
              CALL GETIDS (I,J,0,0,IREP,ND,NV,ILIND,IRIND,
     &                     JLIND(NCOREX),JRIND(NCOREX),
     &                     JVECL(1,NTVERT),IVWA(1),IVWA(1+IVWI),
     &                     IREC)
              IVERT = NTVERT - 1
              ICOREX = NCOREX - 1
           ENDIF
C             --------------------------------
C             START "WHILE LOOP" OVER VERTICES
C             --------------------------------
 2030        IVERT = IVERT + 1
             JD1 = JVECL(1,IVERT) + 1 + ICOREX
             JD2 = JVECL(1,IVERT+1) + ICOREX
             NDH = IABS(JVECL(2,IVERT))
             IF (NDH.EQ.0) GOTO 5000
             DO 2400 ID = JD1, JD2
                IRD = IABS(JRIND(ID)) - 1
                DO 2300 IDH = 1, NDH
                   IR = IRD + IDH
                   DGEL(IR) = DGEL(IR) + TINTR(IX)
 2300              CONTINUE
 2400           CONTINUE
C            --------------
C            DO NEXT VERTEX
C            --------------
             GOTO 2030
C        ---------------------
C        END IF BLOCK KL.EQ.JI
C        ---------------------
         ENDIF
 4000    CONTINUE
C     -----------------------
C     GET NEXT INTEGRAL BLOCK
C     -----------------------
      GOTO 5000
 5001 CONTINUE
C
      RETURN
      END
