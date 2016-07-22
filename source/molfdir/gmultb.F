C
C     =================
      SUBROUTINE GMULTB
C     =================
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INCLUDE 'paracom.inc'
#if defined (PARALLEL)
      INCLUDE 'mpidef.inc'
#else
      PARAMETER (mpirnk = 0)
#endif
C
      COMPLEX*16 TW
      PARAMETER (TW=(2.0,0.0))
C
      COMPLEX*16 REPC(16,16),REPCDP(16)
      DIMENSION MULTB(16,16)
C
       MSYMOP = NOPER(IGRID(1))
       IGAB = IGRID(2)
       MSYMRP = ISYGEN(IGAB)
C
C     INITIALIZE CHARACTER ARRAYS
C
      DO 20 L = 1, MSYMRP
        REPC(1,L) = (1.,0.)
        REPC(1+MSYMOP,L) = (-1.,0.)
   20   CONTINUE
      IND = INDCHAR(IGAB) - 1
      DO 40 LOP = 2, MSYMOP
        DO 30 L = 1, MSYMRP
          IND = IND + 1
          REPC(LOP,L) = CHARACT(IND) / TW
          REPC(LOP+MSYMOP,L) = - CHARACT(IND) / TW
   30     CONTINUE
   40   CONTINUE
C
      DO 45 L = MSYMRP + 1, 2 * MSYMRP
         REPC(1,L) = (1.,0.)
         REPC(1+MSYMOP,L) = (1.,0.)
   45    CONTINUE
      JND = JNDCHAR(IGAB) - 1
      DO 60 LOP = 2, MSYMOP
        DO 50 L = MSYMRP + 1, 2 * MSYMRP
          JND = JND + 1
          REPC(LOP,L) = CHARACS(JND) / TW
          REPC(LOP+MSYMOP,L) = CHARACS(JND) / TW
   50     CONTINUE
   60   CONTINUE
C
C     MAKE DIRECT PRODUCT TABLE
C
      DO 200 L = 1, MSYMRP * 2
        DO 190 M = 1, L
          DO 100 LOP = 1, MSYMOP * 2
            REPCDP(LOP) = REPC(LOP,L) * REPC(LOP,M)
  100       CONTINUE
          DO 120 MM =  1, 2 * MSYMRP
            DO 110 LOP = 1, MSYMOP * 2
#if defined (BIT64)
              IF (CABS(REPC(LOP,MM)-REPCDP(LOP)).GT.1.D-10) GOTO 120
#else
              IF (CDABS(REPC(LOP,MM)-REPCDP(LOP)).GT.1.D-10) GOTO 120
#endif
  110         CONTINUE
            MULTB(L,M) = MM
            MULTB(M,L) = MM
            GOTO 130
  120       CONTINUE
          CALL Q2JOB(6,'DIRECT PRODUCT TABLE',' ',0)
  130     CONTINUE
  190     CONTINUE
  200   CONTINUE
C
      IF ((PRNT.GE.2).and.(mpirnk.eq.0)) THEN
        WRITE (6,1000)
        WRITE (6,1010) '   ',
     &        (GREP(IND),IND=INDREP(IGAB),INDREP(IGAB)+MSYMRP-1),
     &        (GREP1(JND),JND=JNDREP(IGAB),JNDREP(IGAB)+MSYMRP-1)
        WRITE (6,1020) ('-----',I=1,2*MSYMRP+1)
        DO 210 L = 1, MSYMRP
          WRITE (6,1010) GREP(INDREP(IGAB)+L-1),
     &    (GREP1(JNDREP(IGAB)+MULTB(L,M)-MSYMRP-1),M=1,MSYMRP),
     &    (GREP(INDREP(IGAB)+MULTB(L,M)-1),M=MSYMRP+1,2*MSYMRP)
  210     CONTINUE
        DO 220 L = MSYMRP + 1, 2 * MSYMRP
          WRITE (6,1010) GREP1(JNDREP(IGAB)+L-MSYMRP-1),
     &  (GREP(INDREP(IGAB)+MULTB(L,M)-1),M=1,MSYMRP),
     &  (GREP1(JNDREP(IGAB)+MULTB(L,M)-MSYMRP-1),M=MSYMRP+1,2*MSYMRP)
  220     CONTINUE
        WRITE (6,*)
      ENDIF
C
      WRITE (15) MSYMRP,(GREP(INDREP(IGAB)+M-1),M=1,MSYMRP),
     &           (GREP1(JNDREP(IGAB)+M-1),M=1,MSYMRP)
      WRITE (15) ((MULTB(L,M),L=1,2*MSYMRP),M=1,2*MSYMRP)
      WRITE (15) ((REPC(LOP,L),LOP=1,2*MSYMOP),L=1,2*MSYMRP)
C
 1000 FORMAT(//' DIRECT PRODUCT TABLE OF THE ABELIAN SUBGROUP :'/)
 1010 FORMAT (A4,'|',16(1X,A4))
 1020 FORMAT (18A5)
      RETURN
      END
