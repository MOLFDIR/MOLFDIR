C
      SUBROUTINE EXPAND(DR,DI,VECTR,VECTI,SIGMAR,SIGMAI,
     &                  ILIND,IRIND,JLIND,JRIND,JVECL,
     &                  TINTR,TINTI,IJEX,GMATR,GMATI,
     &                  IVWU,IVWA,IJREC)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     =======================
C     CALCULATES SIGMA VECTOR
C     =======================
C
      INCLUDE 'param.inc'
      INCLUDE 'general.inc'
      INCLUDE 'symm.inc'
      INCLUDE 'files.inc'
      INCLUDE 'mempoint.inc'
C
      DIMENSION ILIND(*),IRIND(*),JLIND(*),JRIND(*)
      DIMENSION JVECL(2,*),IVWA(*),IJREC(NORB,NORB),IJEX(*)
      DIMENSION IVWU(16,0:NORB,0:NELEC)
      DIMENSION DR(MDET,*),DI(MDET,*),VECTR(NDET,*),VECTI(NDET,*)
      DIMENSION SIGMAR(NDET,*),SIGMAI(NDET,*),TINTR(*),TINTI(*)
      DIMENSION GMATR(*),GMATI(*)
      real*8  cpui
      LOGICAL ENDFIL
C
      INTERN = NORBR(1) + NORBR(2)
      IF (MAXE3.GT.2) INTERN = NORB
      REWIND (MRCTWOE)
      DO 10 IVEC = 1, NVEC - MVEC
         CALL ZZERO (NDET,SIGMAR(1,IVEC))
         IF (.NOT.REALAR) CALL ZZERO (NDET,SIGMAI(1,IVEC))
   10    CONTINUE
C     -------------------------------------------------------------------
C     LOOP OVER THE ONE-ELECTRON INTEGRALS FOR IRPAMO(I).EQ.IRPAMO(J)
C     LOOP OVER THE TWO-ELECTRON INTEGRALS : BLOCKS (KL|IJ) WITH IJ FIXED
C     -------------------------------------------------------------------
 5000 CALL TWOLIN (ENDFIL,TINTR,TINTI,IJEX,IJ,NIJ)
      IF (ENDFIL) GOTO 5001
      I = (IJ-1)/NORB + 1
      J = MOD(IJ-1,NORB) + 1
      DO 20 IVEC = 1, NVEC - MVEC
         CALL ZZERO (MDET,DR(1,IVEC))
         IF (.NOT.REALAR) CALL ZZERO (MDET,DI(1,IVEC))
   20    CONTINUE
      IF (IJREC(I,J).GT.0) THEN
         IVERT = IJREC(I,J) - 1
         ICOREX = 0
      ELSE
         IREC = -IJREC(I,J)
         CALL GETIDS (I,J,0,0,IREP,ND,NV,ILIND,IRIND,
     &                JLIND(NCOREX),JRIND(NCOREX),
     &                JVECL(1,NTVERT),IVWU,
     &                IVWA(1),IREC)
         IVERT = NTVERT - 1
         ICOREX = NCOREX - 1
      ENDIF
C     --------------------------------
C     START "WHILE LOOP" OVER VERTICES
C     --------------------------------
   40   IVERT = IVERT + 1
        JD1 = JVECL(1,IVERT) + 1 + ICOREX
        JD2 = JVECL(1,IVERT+1) + ICOREX
        NDH = IABS(JVECL(2,IVERT))
        IF (NDH.EQ.0) GOTO 1001
C$DIR FORCE_PARALLEL
        DO 1600 IVEC = 1, NVEC - MVEC
           DO 1500 ID = JD1, JD2
              IS = ISIGN(1,JRIND(ID))
              ILD = JLIND(ID) - 1
              IRD = IABS(JRIND(ID)) - 1
              DO IDH = 1, NDH
                 IL = ILD + IDH
                 IR = IRD + IDH
                 DR(IL,IVEC) = VECTR(IR,IVEC) * DBLE(IS)
              ENDDO
              IF (.NOT.REALAR) THEN
                 DO IDH = 1, NDH
                    IL = ILD + IDH
                    IR = IRD + IDH
                    DI(IL,IVEC) = VECTI(IR,IVEC) * DBLE(IS)
                 ENDDO
              ENDIF
              IF (IRPAMO(I).EQ.IRPAMO(J)) THEN
                 DO IDH = 1 , NDH 
                    IL = ILD + IDH
                    SIGMAR(IL,IVEC) = SIGMAR(IL,IVEC) +
     &              DR(IL,IVEC) * GMATR(IJ) 
                    IF (.NOT.REALAR) THEN
                       SIGMAR(IL,IVEC) = SIGMAR(IL,IVEC) -
     &                 DI(IL,IVEC) * GMATI(IJ)
                       SIGMAI(IL,IVEC) = SIGMAI(IL,IVEC) +
     &                 DR(IL,IVEC) * GMATI(IJ) +
     &                 DI(IL,IVEC) * GMATR(IJ)
                    ENDIF
                 ENDDO
              ENDIF
 1500      CONTINUE
 1600   CONTINUE
C       ----------------
C       GOTO NEXT VERTEX
C       ----------------
        GOTO 40
C     -------------------------------------------------------------------
C     LOOP OVER THE SECOND EXCITATIONS : TWO-ELECTRON PART OF HAMILTONIAN
C     -------------------------------------------------------------------
 1001 DO 3000 IX = 1, NIJ
         KL = IJEX(IX)
         K = (KL-1)/NORB + 1
         L = MOD(KL-1,NORB) + 1
C        -------------------------------------------------------------
C        FIND OUT WHICH DETERMINANTS INTERACT :
C        1) SPECIAL CASE OF 3 OR MORE EXTERNAL LABELS : ONLY 1 VERTEX
C           K=>I>INTERN; L>INTERN, I<>L
C           J,K,L>INTERN L=<INTERN
C        2) ADDRESSES ARE IN CORE MEMORY
C        3) ADDRESSES ARE ON FILE OR NEED TO BE CALCULATED
C        -------------------------------------------------------------
         IF (I.GT.INTERN.AND.L.GT.INTERN.AND.I.NE.L) THEN
            ICOREX = NCOREX - 1
            IVERT = NTVERT - 1
            IOSYM = IRPAMO(I)
            KOSYM = IRPAMO(K)
            LOSYM = IRPAMO(L)
            IV1SYM = MULTBI(KOSYM,IREP)
            KREP = MULTB(LOSYM,IV1SYM)
            IVSYM = MULTBI(IOSYM,IV1SYM)
            IV2SYM = MULTB(LOSYM,IVSYM)
            JVECL(1,NTVERT) = 0
            JVECL(1,NTVERT+1) = 1
            JVECL(2,NTVERT) = IVWU(IVSYM,INTERN,NELEC-2)
            IF (K.EQ.I) JVECL(2,NTVERT) = 0
            JVECL(2,NTVERT+1) = 0
            JRIND(NCOREX) = ISIGN(1,L-I) * (IVWU(IREP,K-1,NELEC) 
     &                      + IVWU(IV1SYM,I-1,NELEC-1)+1)
            IF (I.LT.L) THEN
               JLIND(NCOREX) = IVWU(KREP,L-1,NELEC) +
     &                    IVWU(IV1SYM,I-1,NELEC-1) + 1
            ELSE
               JLIND(NCOREX) = IVWU(KREP,I-1,NELEC) +
     &                    IVWU(IV2SYM,L-1,NELEC-1) + 1
            ENDIF
         ELSE IF (I.LE.INTERN.AND.J.GT.INTERN.AND.
     &            K.GT.INTERN.AND.L.GT.INTERN) THEN
            ICOREX = NCOREX - 1
            IVERT = NTVERT - 1
            IOSYM = IRPAMO(I)
            KOSYM = IRPAMO(K)
            LOSYM = IRPAMO(L)
            IVSYM = MULTBI(KOSYM,IREP)
            KREP = MULTB(LOSYM,IVSYM)
            JVECL(1,NTVERT) = 0
            JVECL(1,NTVERT+1) = 1
            JVECL(2,NTVERT) = IVWU(IVSYM,INTERN,NELEC-1)
            JVECL(2,NTVERT+1) = 0
            JRIND(NCOREX) = IVWU(IREP,K-1,NELEC) + 1
            JLIND(NCOREX) = IVWU(KREP,L-1,NELEC) + 1
         ELSE IF (IJREC(L,K).GT.0) THEN
            IVERT = IJREC(L,K) - 1
            ICOREX = 0
         ELSE
            IREC = -IJREC(L,K)
            CALL GETIDS (L,K,I,J,IREP,ND,NV,ILIND,IRIND,
     &                   JLIND(NCOREX),JRIND(NCOREX),
     &                   JVECL(1,NTVERT),IVWU,
     &                   IVWA(1),IREC)
            IVERT = NTVERT - 1
            ICOREX = NCOREX - 1
         ENDIF
C        --------------------------------
C        START "WHILE LOOP" OVER VERTICES
C        --------------------------------
 1030      IVERT = IVERT + 1
           JD1 = JVECL(1,IVERT) + 1 + ICOREX
           JD2 = JVECL(1,IVERT+1) + ICOREX
           NDH = IABS(JVECL(2,IVERT))
           IF (NDH.EQ.0) GOTO 3000
C$DIR FORCE_PARALLEL
           DO 2600 IVEC = 1, NVEC - MVEC
              DO 2500 ID = JD1, JD2
                 IS = ISIGN(1,JRIND(ID))
                 ILD = IABS(JRIND(ID)) - 1
                 IRD = JLIND(ID) - 1
                 DO IDH = 1, NDH
                    IL = ILD + IDH
                    IR = IRD + IDH
                    SIGMAR(IL,IVEC) = SIGMAR(IL,IVEC) +
     &              DR(IR,IVEC) * DBLE(IS) * TINTR(IX) 
                 ENDDO
                 IF (.NOT.REALAR) THEN
                    DO IDH = 1, NDH
                       IL = ILD + IDH
                       IR = IRD + IDH
                       SIGMAR(IL,IVEC) = SIGMAR(IL,IVEC) -
     &                 DI(IR,IVEC) * DBLE(IS) * TINTI(IX)
                       SIGMAI(IL,IVEC) = SIGMAI(IL,IVEC) +
     &                 DR(IR,IVEC) * DBLE(IS) * TINTI(IX) +
     &                 DI(IR,IVEC) * DBLE(IS) * TINTR(IX)
                    ENDDO
                 ENDIF
 2500         CONTINUE
 2600      CONTINUE
C          ----------------
C          GOTO NEXT VERTEX
C          ----------------
           GOTO 1030
 3000 CONTINUE
      open (88,file='Progress_Dirrci',form='formatted')
      write (88,*) 'Expand in iteration:',iter,", i and j:",i,j
      call cpuused(cpui)
      write (88,'(A10,F10.2)') 'Total CPU ',cpui
      close (88)
C     -----------------------
C     GET NEXT INTEGRAL BLOCK
C     -----------------------
      GOTO 5000
 5001 CONTINUE
C
      DO IVEC = 1, NVEC - MVEC
      CALL PUTSIG (MVEC+IVEC,SIGMAR(1,IVEC),SIGMAI(1,IVEC))
      ENDDO
C
      RETURN
      END
