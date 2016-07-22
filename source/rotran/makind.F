      SUBROUTINE MAKIND(NPASS,INDPQ,INDPQU,INDPQS,IPQMIN,IPQMAX,
     &                  IPSYM,NPQBUF)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE "molf.inc"
      INCLUDE "inpt.inc"
      INTEGER INDPQ(*),INDPQU(*),INDPQS(*),IPQMIN(*),IPQMAX(*)
      INTEGER IPSYM(NSC,*)
C
C     First determine unique list of PQ blocks
C     For each pq combination determine the symmetry operation
C     that transforms it into the unique block.
C     Make also the array IPSYM which is the extended version of ICOMOP
C
      IP=0
      IPQ=0
      DO IM = 1, NEQUIV
         IT = KCOMB(IM)
         INDI = INDCOM(IT)+1
         KNDI = INDCOM(IT+1)
         DO I = INDI, KNDI
            IP = IP + 1
            DO ISYMOP = 1, NSYMOP
               IPT = ABS(ICOMOP(I,ISYMOP)) + NEM(IM) - INDI + 1
               IPSYM(IP,ISYMOP) = SIGN(IPT,ICOMOP(I,ISYMOP))
            ENDDO
            IQ = 0
            DO KM = 1, IM
               KT = KCOMB(KM)
               INDK = INDCOM(KT)+1
               KNDK = INDCOM(KT+1)
               IF (KM.EQ.IM) KNDK = I
               DO K = INDK, KNDK
                  IQ = IQ + 1
                  IPQ = IPQ + 1
                  IPQTMX = 0
                  IPQTOP = 0
                  DO ISYMOP = 1, NSYMOP
                     IPT = ABS(ICOMOP(I,ISYMOP)) + NEM(IM) - INDI + 1
                     KPT = ABS(ICOMOP(K,ISYMOP)) + NEM(KM) - INDK + 1
                     IPMT = MAX(IPT,KPT)
                     IQMT = MIN(IPT,KPT)
                     IPQT = IPMT*(IPMT-1)/2+IQMT
                     IPQS = SIGN(1,ICOMOP(I,ISYMOP)*ICOMOP(K,ISYMOP))
                     IF (IPQT.GT.IPQTMX) THEN
                        IPQTMX = IPQT
                        IPQTOP = ISYMOP * IPQS
                     ENDIF
                  ENDDO
                  INDPQU(IPQ) = IPQTMX
                  INDPQS(IPQ) = IPQTOP
               ENDDO
            ENDDO
         ENDDO
      ENDDO
C
      IPQ=0
      DO IP=1,NSC
         DO IQ=1,IP
            IPQ=IPQ+1
            INDPQ(IPQ)=0
         ENDDO
      ENDDO
C
      IND=0
      JND=0
      NPASS=1
      IPQ=0
      IF (.NOT.BREIT) THEN
         DO 10 IP=1,NSC
           DO 20 IQ=1,IP
             IPQ=IPQ+1
C            No SL blocks for Coulomb interaction
             IF ((IP.GT.NSC1).AND.(IQ.LE.NSC1)) GOTO 20
C            SS blocks only when transforming SSSS contribution
             IF (IP.GT.NSC1.AND.MFIL.LT.3) GOTO 20
C            We do only the unique blocks
             IF (IPQ.NE.INDPQU(IPQ)) GOTO 20
             IF (JND.EQ.0) IPQMIN(NPASS)=IPQ
             IND=IND+1
             JND=JND+1
             INDPQ(IPQ)=IND
             IF (JND.EQ.NPQBUF) THEN
                IPQMAX(NPASS)=IPQ
                NPASS=NPASS+1
                JND=0
             ENDIF
 20          CONTINUE
 10        CONTINUE
      ELSE
        DO 30 IP = NSC1 + 1, NSC
           DO 40 IQ = 1, NSC1
             IPQ = IP*(IP-1)/2 + IQ
C            We do only the unique blocks
             IF (IPQ.NE.INDPQU(IPQ)) GOTO 40
             IF (JND.EQ.0) IPQMIN(NPASS)=IPQ
             IND=IND+1
             JND=JND+1
             INDPQ(IPQ)=IND
             IF (JND.EQ.NPQBUF) THEN
                IPQMAX(NPASS)=IPQ
                NPASS=NPASS+1
                JND=0
             ENDIF
  40         CONTINUE
  30       CONTINUE
      ENDIF
C
      IPQMAX(NPASS)=IPQ
      WRITE (6,1000) IND,NPASS
      NPQBUF = MIN0(NPQBUF,IND)
C
 1000 Format (
     &/' Number of blocks to be transformed:',T40,I8
     &/' Number of passes :',T40,I8/)
      END
