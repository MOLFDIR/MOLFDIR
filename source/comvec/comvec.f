      PROGRAM COMVEC
C
      IMPLICIT REAL*8 (A-H, O-Z)
      REAL*8 EW(300),COPCOF(300),OCC(300)
      CHARACTER*80 ,TXT1,TXT
      CHARACTER TSYM*26,LOGICS*8
      CHARACTER*20 FILE1,FILE2,FILNEW
      LOGICAL SWAPPED
      CHARACTER*80 FORM11,FORM12,FORM21
C             large    small
      INTEGER IPRL(16,2),IPRS(16,2)
      COMPLEX*16 VEC(300,1000), VECIN(300,1000)
C
      DO 1 I=1,300
         DO 1 J=1,600
            VECIN(I,J)=(0.0D0,0.0D0)
            VEC(I,J)=(0.0D0,0.0D0)
  1   CONTINUE
C
      WRITE(*,*) 'Give name first vector file :'
      READ (5,*) FILE1
      WRITE(*,*) 'Give name second vector file :'
      READ (5,*) FILE2
      WRITE(*,*) 'Give name OUTPUT vector file :'
      READ (5,*) FILNEW
      OPEN(11,FILE=FILE1,FORM='FORMATTED',STATUS='OLD',
     +             ERR=30)
      OPEN(12,FILE=FILE2,FORM='FORMATTED',STATUS='OLD',
     +             ERR=30)
      OPEN(21,FILE=FILNEW,FORM='FORMATTED')
      REWIND(11)
      REWIND(12)
      REWIND(21)
C
      READ (11,1000,END=30,ERR=30) TXT1
      READ (12,1000,END=30,ERR=30) TXT
      WRITE (21,1000) TXT1
      READ (11,1000,END=30,ERR=30) TXT
      READ (12,1000,END=30,ERR=30) TXT
      WRITE (21,1000) TXT
      READ (11,'(I4,8A)',END=30,ERR=30) ICYCLS,LOGICS
      READ (11,1007,END=30,ERR=30) ETOT1,EPREV1,
     +                                  EMDIF1,WDIFP1,TOTDIF
C
      READ (12,'(I4,8A)',END=30,ERR=30) ICYCLS,LOGICS
      READ (12,1007,END=30,ERR=30) ETOT2,EPREV2,
     +                             EMDIF,WDIFP,TOTDIF
      WRITE (21,'(I4,8A)') 0,LOGICS
      WRITE (21,1007) ETOT1+ETOT2,EPREV1+EPREV2,
     +                EMDIF,WDIFP,TOTDIF
C
      READ (11, 1001, END=30,ERR=30) FORM11
      READ (12, 1001, END=30,ERR=30) FORM12
      IF (FORM11.EQ.' ') FORM11='(6F22.16)'
      IF (FORM12.EQ.' ') FORM12='(6F22.16)'
      FORM21='(6F22.16)'
      WRITE (21,1001) FORM21
C
      READ (11,'(I4)',ERR=30,END=30) NSYMRP
      READ (11,'(16I4)',ERR=30,END=30) (IPRL(I,1),I=1,NSYMRP)
      READ (11,'(16I4)',ERR=30,END=30) (IPRS(I,1),I=1,NSYMRP)
      READ (12,'(I4)',ERR=30,END=30) NSYMRP
      READ (12,'(16I4)',ERR=30,END=30) (IPRL(I,2),I=1,NSYMRP)
      READ (12,'(16I4)',ERR=30,END=30) (IPRS(I,2),I=1,NSYMRP)          
C
      WRITE (21,'(I4)') NSYMRP
      WRITE (21,'(16I4)') (IPRL(I,1)+IPRL(I,2),I=1,NSYMRP)
      WRITE (21,'(16I4)') (IPRS(I,1)+IPRS(I,2),I=1,NSYMRP)
C      
      DO 80 IRP=1,NSYMRP
        IF (IPRL(IRP+1,1).NE.IPRL(IRP,1)) THEN
          NRLA=IPRL(IRP,1)+IPRL(IRP,2)
          NRSM=IPRS(IRP,1)+IPRS(IRP,2)
          READ (11, 1008) NSYM,TSYM,IDUM,IDUM
          READ (12, 1008) NSYM,TSYM,IDUM,IDUM
          WRITE (21,1008) NSYM,TSYM,NRLA,NRLA+NRSM
          DO 70 I=1,IPRL(IRP,1) 
            READ (11,1010,ERR=30) EW(I),OCC(I),COPCOF(I)
            READ (11,FORM11) (VECIN(I,J),J=1,IPRL(IRP,1)+IPRS(IRP,1))
   70     CONTINUE
          DO 75 I=1+IPRL(IRP,1),NRLA
            READ (12,1010,ERR=30) EW(I),OCC(I),COPCOF(I)
            READ (12,FORM12) (VECIN(I,J),J=1,IPRL(IRP,2)+IPRS(IRP,2))
   75     CONTINUE
C
C PUT LARGE AND SMALL COMPONENT IN CORRECT PLACE 
C
          DO 78 I=1,IPRL(IRP,1)
             DO 77 J=1,IPRL(IRP,1)
                VEC(I,J)=VECIN(I,J)          
  77         CONTINUE
             DO 76 J=1,IPRS(IRP,1)
                VEC(I,J+NRLA)=VECIN(I,J+IPRL(IRP,1))
  76         CONTINUE
  78      CONTINUE
          DO 87 I=1+IPRL(IRP,1),NRLA
             DO 87 J=1,IPRL(IRP,2)
                VEC(I,J+IPRL(IRP,1))=VECIN(I,J)          
  87         CONTINUE
             DO 86 J=1,IPRS(IRP,1)
                VEC(I,J+NRLA+IPRS(IRP,1))=VECIN(I,J+IPRL(IRP,2))
  86         CONTINUE
  88      CONTINUE
C
C SORT AND WRITE HALF OF IT TO DISK
C
   60     SWAPPED=.FALSE.
          DO 50 I=1,NRLA-1
            IF (OCC(I).LT.OCC(I+1)) THEN
              CALL SWAP(EW,OCC,COPCOF,VEC,I,NRLA+NRSM)
              SWAPPED=.TRUE.
            ELSE 
              IF (OCC(I).EQ.OCC(I+1)) THEN
                IF (EW(I).GT.EW(I+1)) THEN
                  CALL SWAP(EW,OCC,COPCOF,VEC,I,NRLA+NRSM)
                  SWAPPED=.TRUE.
                ENDIF
              ENDIF
            ENDIF 
   50     CONTINUE
          IF (SWAPPED) GOTO 60
          DO 40,I=1,NRLA
            WRITE(21,1009) I,EW(I),OCC(I),COPCOF(I)
            WRITE(21,FORM21) (VEC(I,J),J=1,NRLA+NRSM)
   40     CONTINUE
        ENDIF
   80 CONTINUE
C
   30 CLOSE(16)
      CLOSE(17)
      CLOSE(18)
 1000 FORMAT(A)
 1001 FORMAT(20A)
 1004 FORMAT(10X,G20.10)
 1005 FORMAT(2A10)
 1007 FORMAT(5G20.10)
 1008 FORMAT(I4,1X,A25,I4,I4)
 1009 FORMAT(1X,'MO:',I2,' EW:',G20.10,' OCC:',G15.10,' COPCOF:',
     +       G20.10)
 1010 FORMAT(10X,G20.10,5X,G15.10,8X,G20.10)
     
      END
C
C
      SUBROUTINE SWAP(EIGEN,OCCU,CCOF,VECT,NR,NMO)      
C     
      COMPLEX*16 VECT(300,1000)
      REAL*8 EIGEN(300),OCCU(300),CCOF(300)
      REAL*8 DE,DO,DC,DV(1000)
      INTEGER NR,I,NMO
C
      DE = EIGEN(NR+1)
      DO = OCCU(NR+1)
      DC = CCOF(NR+1)
      DO 41 I=1,NMO
        DV(I) = VECT(NR+1,I)
   41 CONTINUE
      EIGEN(NR+1) = EIGEN(NR)
      OCCU(NR+1) = OCCU(NR)
      CCOF(NR+1) = CCOF(NR)
      DO 42,I=1,NMO
      VECT(NR+1,I)=VECT(NR,I)  
   42 CONTINUE
      EIGEN(NR) = DE
      OCCU(NR) = DO
      CCOF(NR) = DC
      DO 43 I=1,NMO
        VECT(NR,I)=DV(I)
   43 CONTINUE
      END
      
