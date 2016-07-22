      PROGRAM ROTRAN
C     ====================================================== 
C     FIRST VERSION WRITTEN BY OLIVIER VISSER
C     THEORETICAL CHEMISTRY DEPARTMENT,
C     UNIVERSITY OF GRONINGEN (RUG),
C     NIJENBORGH 16, 9747 AG GRONINGEN, THE NETHERLANDS
C     MODIFIED AND OPTIMIZED JUNE-JULY 1994
C     LUUK VISSCHER
C     NASA AMES RESEARCH CENTER
C     MOFFETT FIELD
C     CA 94035-1000
C     ======================================================
      IMPLICIT INTEGER (A-Z)
      EXTERNAL INITC
C
#if defined (BIT64)
      PARAMETER (NITR=1,NRTC=2)
#else
      PARAMETER (NITR=2,NRTC=2)
#endif
      PARAMETER (MAXPASS=10)
      PARAMETER (NWORK=16 000 000)
C
C Instead of increasing NWORK one can also reduce NPQBUF
C
      REAL*8 RW(NWORK)
      CHARACTER*10 DATEX,TIMEX*8
      LOGICAL DOBREIT
      REAL*8 SEC0,SEC1
C
      INCLUDE "inpt.inc"
      INCLUDE "molf.inc"
      INCLUDE "redme.inc"
      INCLUDE "files.inc"
      INCLUDE "thres.inc"
C
C     Input and Initialization section
C
      CALL DAYTIME (DATEX,TIMEX)
      WRITE (6, 1000) VERSION, DATEX, TIMEX  
      CALL CPUUSED (SEC0)
      CALL INIT
      CALL READIN 
      CALL GETMOS
      CALL COMSYM
      CALL CPUUSED (SEC1)
      WRITE (6,1010) 'Input',SEC1-SEC0
      IF (BREIT) THEN 
         IF (MFIL.NE.0) THEN
            BREIT = .FALSE.
            DOBREIT = .TRUE.
         ELSE
            BREIT = .TRUE.
            DOBREIT = .FALSE.
         ENDIF
      ELSE
         DOBREIT = .FALSE.
      ENDIF
   1  CONTINUE
      IF (.NOT.BREIT) WRITE (6,1040) 'Coulomb'
      IF (BREIT) WRITE (6,1040) 'Breit'
C
C     Initialize counters
C
      NHALFT = 0
      NHALFT0 = 0
      NSKEL = 0
      NFINAL = 0
      NFINAL1 = 0
      NFINAL2 = 0
      NFINAL3 = 0
      NFINAL4 = 0
C
C     Calculate array dimensions
C     Allocate memory for the LBRCIJ which is used in step1 and step2
C
      NSCTR = NSC*(NSC+1)/2
      NSPTR = NKR*(NKR+1)
      NSPSQ = NSP*NSP
      INTBUF = 12288 ! RELTWEL INTEGRAL BUFFER SIZE
      NG = NSCTR ! SORTED INTEGRAL BUFFER SIZE
      NPQBUF = MIN0(NSCTR,16384)
      NPQSIZ = 512
      NIJBUF = NSPTR
      LBRCIJ = 1
      INDPQ = LBRCIJ + ((NSPTR+1)+1)/NITR
      INDPQU = INDPQ + (NSCTR+1)/NITR
      INDPQS = INDPQU + (NSCTR+1)/NITR
      IPQMIN = INDPQS + (NSCTR+1)/NITR
      IPQMAX = IPQMIN + (MAXPASS+1)/NITR
      IPSYM = IPQMAX + (MAXPASS+1)/NITR
      SEC0 = SEC1
C
C     Calculate how many passes are required to transform the integrals
C     The integrals are first reordered to have only blocks with
C     symmetry unique pq's. MAKIND will reduce NPQBUF if there are
C     less unique blocks
C
      CALL MAKIND(NPASS,RW(INDPQ),RW(INDPQU),RW(INDPQS),
     &            RW(IPQMIN),RW(IPQMAX),RW(IPSYM),NPQBUF)
      IF (NPASS.GT.MAXPASS) THEN
         WRITE (6,1030) NPASS
         STOP "NPASS > MAXPASS"
      ENDIF
C
C     Size of the blocks in which the half-transf integrals are sorted
C
      NIJSIZ = MIN0(NPQBUF,16384)
C
C     Calculate if there is enough memory available or if the buffer
C     sizes need to be reduced
C
C     The first half transformation
C
      TOP = IPSYM + (NSC*NSYMOP+1)/NITR
      LBRCPQ = TOP
      LABEL1 = LBRCPQ + ((NPQBUF+1)+1)/NITR
      LABEL2 = LABEL1 + (INTBUF+1)/NITR
      NBUCK = LABEL2 + (INTBUF+1)/NITR
      LBUCK = NBUCK + (NPQBUF+1)/NITR
      RINT = LBUCK + (NPQBUF*NPQSIZ+1)/NITR
      RBUCK = RINT + INTBUF
      TOP = RBUCK + NPQBUF*NPQSIZ
      IF (TOP.GT.NWORK) THEN
         PRINT*, "TOP, NWORK",TOP, NWORK
         CALL Q2JOB (4,'Sort1',' ',0)
      ENDIF
  110 TOP = LABEL1
      NBCK = TOP
      IF (.NOT.BREIT) THEN
          LBCK = NBCK + (NIJBUF+1)/NITR
          CBCK = LBCK + (NIJBUF*NIJSIZ+1)/NITR
          INDG = CBCK + NIJBUF*NIJSIZ*NRTC
      ELSE
          LBCK = NBCK + 2*(NIJBUF+1)/NITR
          CBCK = LBCK + (NIJBUF*NIJSIZ+1)/NITR
          INDG = CBCK + 6*NIJBUF*NIJSIZ
      ENDIF
      G = INDG + NG*2
      HA = G + NG
      RI = HA + NSC*NSP*NRTC
      RIA = RI + NSPSQ*NRTC
      RIB = RIA + NSPSQ*NRTC
      TOP = RIB + NSPSQ*NRTC
      IF (TOP.GT.NWORK) THEN
         IF (NIJSIZ.LE.128) THEN
            PRINT*, "TOP, NWORK",TOP, NWORK
            CALL Q2JOB (4,'Trans1',' ',0)
         ELSE
            NIJSIZ=MAX0(NIJSIZ-128,128)
            GOTO 110
         ENDIF
      ENDIF
      WRITE (6,'(/I10,A)') 8*TOP/1024+1,
     & " kB memory is to be used in the first half transformation"
C
C     The second half transformation
C
      TOP = LBRCIJ + ((NSPTR+1)+1)/NITR
      LBRCSK1 = TOP
      LBRCSK2 = LBRCSK1 + (NKR+2)/NITR
      LBRCSK3 = LBRCSK2 + (NKR+2)/NITR
      LBRCSK4 = LBRCSK3 + (NKR+2)/NITR
      INDRJ = LBRCSK4 + (NKR+2)/NITR
      JNDRJ = INDRJ + (NSCTR*2+1)/NITR
      INDKLP = JNDRJ + (NSPSQ+1)/NITR
      INDKL = INDKLP + (NSYMRP*NSYMRP*2+1)/NITR
      REDME = INDKL + (NSPSQ*2+1)/NITR
      RJ = REDME + NRMES*NRTC
      RJB = RJ + NSCTR*NRTC
      RJC = RJB + NSCTR*NRTC
      RK = RJC + NSCTR*NRTC
      RLR = RK + NSP*NSC*NRTC
      RL = RLR + NSPSQ*NRTC
      INDK = RL + NSPSQ*NRTC
      NBCK1   = INDK + (NSPSQ+1)/NITR
      NBCK2   = NBCK1   + (NKR+2)/NITR
      NBCK3   = NBCK2   + (NKR+2)/NITR
      NBCK4   = NBCK3   + (NKR+2)/NITR
      NSKSIZ1 = MIN0(NKR*NKR,16384)
      NSKSIZ2 = MIN0(NSPTR/2,16384)
      NSKSIZ3 = MIN0(NSPTR/2,16384)
      NSKSIZ4 = MIN0(NKR*NKR,16384)
      IF (QUATER) NSKSIZ4 = 0
      LBCK1 = NBCK4 + (NKR+2)/NITR
      LBCK2 = LBCK1 + (2*NKR*NSKSIZ1)/NITR
      LBCK3 = LBCK2 + (2*NKR*NSKSIZ2)/NITR
      LBCK4 = LBCK3 + (2*NKR*NSKSIZ3)/NITR
      CBCK1 = LBCK4 + (2*NKR*NSKSIZ4)/NITR
      CBCK2 = CBCK1 + NKR*NSKSIZ1*NRTC
      CBCK3 = CBCK2 + NKR*NSKSIZ2*NRTC
      CBCK4 = CBCK3 + NKR*NSKSIZ3*NRTC
      TOP =   CBCK4 + NKR*NSKSIZ4*NRTC
      IF (TOP.GT.NWORK) THEN
         PRINT*, "TOP, NWORK",TOP, NWORK
         CALL Q2JOB (4,'Trans2',' ',0)
      ENDIF
      WRITE (6,'(I10,A)') 8*TOP/1024+1,
     & " kB memory is to be used in the second transformation"
C
C     Restoration of particle-particle symmetry
C
      NSKSIZ = MAX(NSKSIZ1,NSKSIZ2,NSKSIZ3,NSKSIZ4)
      TOP = LBRCSK4 + (NKR+2)/NITR
      JKRIND = TOP
      ICIND = JKRIND + (NKR+1)/NITR
      INDKL2 = ICIND + (NKR+1)/NITR
      INDK2 = INDKL2 + (NSPSQ+1)/NITR
      INDL2 = INDK2 + (NSPSQ+1)/NITR
      RKL2 = INDL2 + (NSPSQ+1)/NITR
      LBCKS = RKL2 + NSPSQ*NRTC
      CBCKS = LBCKS + (2*NSKSIZ)/NITR
      TOP = CBCKS + NSKSIZ*NRTC
      FREE = NWORK - TOP
      NC2 = FREE / (NKR * NKR * NRTC)
      IF (NC2.LT.1) NC2 = 1
      IF (NC2.GT.NKR) NC2 = NKR
      CBCKS2 = TOP
      TOP = CBCKS2 + NC2*NKR*NKR*NRTC
      IF (TOP.GT.NWORK) THEN
         PRINT*, "TOP, NWORK",TOP, NWORK
         CALL Q2JOB (4,'PPsym',' ',0)
      ENDIF
      WRITE (6,'(I10,A//)') 8*TOP/1024+1,
     & " kB memory is to be used in restoration of pp-symmetry"
#if defined (CRAY) || defined (LINUX) || defined (SUN)
      CALL FLUSH (6)
#endif
#if defined (IBM) 
      CALL FLUSH_(6)
#endif
C
C     Do first step of the transformation : intermediary integrals
C     are sorted and written to file.
C
      CALL SR2INI(NIJBUF,NIJSIZ,RW(LBRCIJ))
      DO IPASS = 1, NPASS
         CALL SORT1(IPASS,RW(INDPQ),RW(INDPQU),RW(INDPQS),
     &     RW(IPQMIN),RW(IPQMAX),RW(IPSYM),
     &     NPQBUF,NPQSIZ,INTBUF,RW(LBRCPQ),RW(LABEL1),RW(LABEL2),
     &     RW(NBUCK),RW(LBUCK),RW(RINT),RW(RBUCK))
         CALL TRANS1(IPASS,RW(IPQMIN),RW(IPQMAX),RW(INDPQ),
     &     NG,NIJSIZ,NIJBUF,RW(LBRCPQ),RW(LBRCIJ),RW(INDG),
     &     RW(NBCK),RW(LBCK),RW(G),RW(HA),RW(RI),RW(RIA),
     &     RW(RIB),RW(CBCK))
         CLOSE (INTSRT,STATUS='DELETE')
         IF (.NOT.BREIT) THEN
         CALL SR2FLSH(NIJBUF,NIJSIZ,RW(LBRCIJ),RW(NBCK),RW(LBCK),
     &     RW(CBCK))
         ELSE
         CALL SR2FLSB(NIJBUF,NIJSIZ,RW(LBRCIJ),RW(NBCK),RW(LBCK),
     &     RW(CBCK))
         ENDIF
         WRITE (6,1014) IPASS,NHALFT-NHALFT0
         NHALFT0 = NHALFT
#if defined (CRAY) || defined (LINUX) || defined (SUN)
      CALL FLUSH (6)
#endif
#if defined (IBM) 
      CALL FLUSH_(6)
#endif
      ENDDO
      IF (DELSRT) CALL SRTDEL
      CALL CPUUSED (SEC1)
      WRITE (6,1010) 'First transformation',SEC1-SEC0
C
C     Check whether the records are filled     
C
      NBYTE = NHALFTR * NHALFTB / (1024 * 1024) + 1
      WRITE (6,1012) 'half-transformed',NHALFT,NHALFTR,NIJSIZ,NBYTE
#if defined (CRAY) || defined (LINUX) || defined (SUN)
      CALL FLUSH (6)
#endif
#if defined (IBM) 
      CALL FLUSH_(6)
#endif
C
C     Do second step of the transformation
C     integrals are symmetrized and written to file
C     After this step we are only missing the particle-particle symmetry
C
      SEC0 = SEC1
      CALL TRANS2 (MDINT,NSCTR,RW(LBRCIJ),RW(INDKLP),
     &     RW(INDKL),RW(INDRJ),RW(JNDRJ),RW(RJ),RW(RJB),RW(RJC),RW(RK),
     &     RW(RLR),RW(RL),RW(REDME),RW(INDK),
     &     NSKSIZ1,RW(LBRCSK1),RW(NBCK1),RW(LBCK1),RW(CBCK1),
     &     NSKSIZ2,RW(LBRCSK2),RW(NBCK2),RW(LBCK2),RW(CBCK2),
     &     NSKSIZ3,RW(LBRCSK3),RW(NBCK3),RW(LBCK3),RW(CBCK3),
     &     NSKSIZ4,RW(LBRCSK4),RW(NBCK4),RW(LBCK4),RW(CBCK4))
      CALL CPUUSED (SEC1)
      WRITE (6,1010) 'Second transformation',SEC1-SEC0
      WRITE (6,1013) 'transformed',NSKEL
      CLOSE (INTTWO,STATUS='DELETE')
C
#if defined (CRAY) || defined (LINUX) || defined (SUN)
      CALL FLUSH (6)
#endif
#if defined (IBM) 
      CALL FLUSH_(6)
#endif
C
C     Restore the particle-particle symmetry
C
      SEC0 = SEC1
      CALL INITTWOE (IPRNT,BREIT,NKR,NSP,MDINT)
      CALL PPSYM (INTSKEL,MDINT,1,-1,-1,-1,-1,NKR,RW(INDKL2),
     &     RW(INDK2),RW(INDL2),RW(RKL2),NSKSIZ1,RW(LBRCSK1),
     &     RW(LBCKS),RW(CBCKS),RW(CBCKS2),NC2,RW(JKRIND),RW(ICIND),
     &     IPRNT,THROUT,NFINAL1)
      CLOSE(INTSKEL+1,STATUS='DELETE')
      CLOSE(INTSKEL+5,STATUS='DELETE')
      CALL PPSYM (INTSKEL,MDINT,2,-1, 1,-1, 1,NKR,RW(INDKL2),
     &     RW(INDK2),RW(INDL2),RW(RKL2),NSKSIZ2,RW(LBRCSK2),
     &     RW(LBCKS),RW(CBCKS),RW(CBCKS2),NC2,RW(JKRIND),RW(ICIND),
     &     IPRNT,THROUT,NFINAL2)
      CLOSE(INTSKEL+2,STATUS='DELETE')
      CLOSE(INTSKEL+6,STATUS='DELETE')
      CALL PPSYM (INTSKEL,MDINT,3,-1, 1, 1,-1,NKR,RW(INDKL2),
     &     RW(INDK2),RW(INDL2),RW(RKL2),NSKSIZ3,RW(LBRCSK3),
     &     RW(LBCKS),RW(CBCKS),RW(CBCKS2),NC2,RW(JKRIND),RW(ICIND),
     &     IPRNT,THROUT,NFINAL3)
      CLOSE(INTSKEL+3,STATUS='DELETE')
      CLOSE(INTSKEL+7,STATUS='DELETE')
      IF (QUATER) THEN
      CALL PPSYM (INTSKEL,MDINT,4,-1, 1, 1, 1,NKR,RW(INDKL2),
     &     RW(INDK2),RW(INDL2),RW(RKL2),NSKSIZ4,RW(LBRCSK4),
     &     RW(LBCKS),RW(CBCKS),RW(CBCKS2),NC2,RW(JKRIND),RW(ICIND),
     &     IPRNT,THROUT,NFINAL4)
      CLOSE(INTSKEL+4,STATUS='DELETE')
      CLOSE(INTSKEL+8,STATUS='DELETE')
      ENDIF
      NFINAL = NFINAL1 + NFINAL2 + NFINAL3 + NFINAL4
      CALL EXITTWOE (MDINT)
      CALL CPUUSED (SEC1)
      WRITE (6,1010) 'Particle-Particle symmetry',SEC1-SEC0
C
#if defined (CRAY) || defined (LINUX) || defined (SUN)
      CALL FLUSH (6)
#endif
#if defined (IBM) 
      CALL FLUSH_(6)
#endif
C
      WRITE (6,1013) 'final',NFINAL
      WRITE (6,1015) NFINAL1,NFINAL2,NFINAL3,NFINAL4
      IF (DOBREIT) THEN 
         BREIT = .TRUE.
         DOBREIT = .FALSE.
         GOTO 1
      ENDIF
      CALL DAYTIME (DATEX,TIMEX)
      WRITE (6,1011) SEC0
      WRITE (6,1020) DATEX,TIMEX
 1000 FORMAT (' ***********************************'/
     +        ' *                                 *'/
     +        ' *  R O T R A N    VS ',A10,   '   *'/
     +        ' *                                 *'/
     +        ' *  First version written by :     *'/
     +        ' *  Olivier Visser                 *'/
     +        ' *  University of Groningen        *'/
     +        ' *                                 *'/
     +        ' *  This version written by:       *'/
     +        ' *  Luuk Visscher                  *'/
     +        ' *  NASA Ames Research Center      *'/
     +        ' *                                 *'/
     +        ' ***********************************'///
     +        ' Program start: ',A10,2X,A8/)
 1010 FORMAT (/3x,a,' step completed in ',f10.4,' seconds')
 1011 FORMAT (/3x,'Total CPU time for this run ',f10.4,' seconds')
 1012 FORMAT (/3x,'Total number of ',A,' integrals :',I10
     &/3x,'Written in ',I6,' records of ',I6,' integrals'
     &/3x,'Used',I5,' mB on disk for this class')
 1013 FORMAT (/3x,'Total number of ',A,' integrals :',I10)
 1014 FORMAT (/3x,'Pass number',I4,' completed ',
     & I10,' integrals written')
 1015 FORMAT (/3x,'Integrals were written as :'/
     &/3x,'(ibar, jbar | kbar lbar) (i=>j) (ij=>kl) (ij=>lk)',T60,I10
     &/3x,'(ibar, j    | kbar l   ) (i=>j) (k=>l) (ij=>kl)',T60,I10
     &/3x,'(ibar, j    | k    lbar) (i=>j) (k=>l) (ij=>kl)',T60,I10
     &/3x,'(ibar, j    | k    l   ) (i=>j)',T60,I10)
 1020 FORMAT (/' Program end  : ',A10,2X,A8)
 1030 FORMAT (/' This run would require ',i5,' passes through the',
     &' integral file !'/' If this is really what you want : change',
     &' MAXPASS in rotran and continue.')
 1040 FORMAT (/' Transforming ',A,' integrals'/)
      END
