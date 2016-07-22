C
C     ===============================
      SUBROUTINE EXPMOS (NHERP)
C     Expand MO's in primitive basis.
C     ===============================
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE "molf.inc"
      INCLUDE "files.inc"
      INCLUDE "sym.inc"
      INCLUDE "inpt.inc"
      INCLUDE "vecs.inc"
      DIMENSION INDHER(N1)
#if defined (BIT64)
      PARAMETER (A0=(0.0,0.0),A1=(1.0,0.0))
#else
      PARAMETER (A0=(0.D0,0.D0),A1=(1.D0,0.D0))
#endif
C
      OPEN (MFDHER,FILE=FNAME(4),FORM='FORMATTED')
 1000 FORMAT (I10)
      READ (MFDHER,1000) NMOLP
      READ (MFDHER,1000) NHERP
      IF (NMOLP.GT.N1) THEN
         print*,'INCREASE N1',NMOLP
         STOP
      ENDIF
      IF (NHERP.GT.N2) THEN
         print*,'INCREASE N2',NHERP
         STOP
      ENDIF
      DO I = 1, NMOLP
         READ (MFDHER,1000) INDHER(I)
      ENDDO
      CLOSE (MFDHER)
C
      DO IMO = 1, NSP
         DO I = 1, NHERP
            UA(I,IMO) = A0
            UB(I,IMO) = A0
         ENDDO
      ENDDO
C
      IP = 0
      DO IM = 1, NEQUIV
         ITA = KCOMB(IM)
         JND = INDCOM(ITA) + 1
         KND = INDCOM(ITA+1)
         DO IND = JND, KND
             II = NEM(IM) + IND - JND + 1
             DO I = INDBAS(IM) + 1, INDBAS(IM+1)
                IP = IP + 1
                JP = INDHER(IP)
                DO IMO = 1, NSP
                UA(JP,IMO) = UA(JP,IMO) + QA(II,IMO) * COEFF(I)
                UB(JP,IMO) = UB(JP,IMO) + QB(II,IMO) * COEFF(I)
                ENDDO
             ENDDO
         ENDDO
      ENDDO
      
C
      IF (IPRNT.GE.3) THEN
         WRITE (6,2000)
          DO IMO = 1, NSP
            WRITE (6,2001) IMO
             DO II = 1, NHERP
               WRITE (6,2002) II,UA(II,IMO),UB(II,IMO)
            ENDDO
         ENDDO
      ENDIF
C
 2000 FORMAT (//' Spinors expanded in primitive scalar cartesian basis')
 2001 FORMAT (/' Spinor #',I4
     &/' Basisfn. Real Alpha  Imag. Alpha  Real Beta Imag. Beta')
 2002 FORMAT (I9,4F12.10)
C
      RETURN
      END
