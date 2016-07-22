C
C     ==================
      SUBROUTINE MOLECUL
C     Write interface information to get 1-electron integrals from
C     HERMIT. Created DIRAC and HERMIT input files.
C     ==================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'paracom.inc'
      PARAMETER (N5A=10) ! Number of exponent blocks in one l-type
      PARAMETER (N15A=30000)
      DIMENSION NTONC(N1*8,2),NPONC(0:N5A,N5,N1*8,2),IPONC(N5,N1*8,2),
     &          NBONC(N5,N1*8,2)
      DIMENSION IPINF(N15A,6),ITYPHER(N6),IDEG(N5)
      CHARACTER*80 LINE, COMMENT*79
      NAMELIST/DIRAC/COMMENT,MAXPRI
C
      DATA ITYPHER /
     S     1,2,3,4,5,8,9,6,10,7,
     F     11,14,15,16,20,18,12,17,19,13,
     G     21,24,25,30,33,31,26,34,35,28,22,27,32,29,23,
     H     36,39,40,45,51,46,47,54,56,49,41,52,55,53,43,37,
     H     42,48,50,44,38/
C
C     Degeneracy of Cartesian gaussian functions with l-value.
C
      DATA IDEG / 1,3,6,10,15,21/
C
C     Check first if they want to compute properties
C
      IF (.NOT.PROPERTY) RETURN
C
C     Copy the dirac input file to DIRAC.INP
C     If no namelist DIRAC is found : write default .
C
      OPEN (87,FILE='DIRAC.INP',FORM='FORMATTED')
      COMMENT = ' DIRAC input copied from MOLFDIR input file'
      MAXPRI = 10
      CALL LOCATE('DIRAC',5,ITEST)
      IF (ITEST.GT.0) THEN
         READ (5,DIRAC,ERR=10)
      ELSE
         GOTO 10
      ENDIF
      LINE(1:80) = '#'//COMMENT
      WRITE (87,1000) LINE
    1 READ (5,1000) LINE
      WRITE (87,1000) LINE
      IF (LINE(1:4).NE.'*END') GOTO 1
      GOTO 11
   10 CONTINUE
      LINE(1:80) = '# Default DIRAC input used'
      WRITE (87,1000) LINE
      LINE(1:80) = '**DIRAC'
      WRITE (87,1000) LINE
      LINE(1:80) = '.PROPER'
      WRITE (87,1000) LINE
      LINE(1:80) = '.INTGRL'
      WRITE (87,1000) LINE
      LINE(1:80) = '**GENERAL'
      WRITE (87,1000) LINE
      LINE(1:80) = '**PROPERTIES'
      WRITE (87,1000) LINE
      LINE(1:80) = '.DIPOLE'
      WRITE (87,1000) LINE
      LINE(1:80) = '*END OF'
      WRITE (87,1000) LINE
   11 CLOSE (87,STATUS='KEEP')
C
C     -- WRITE MOLECULE INPUT FILE FOR USE IN HERMIT/DIRAC --
C
C     Gather data about number of functions per l-value
C     Make an index array to the positions of the primitives in HERMIT
C
      DO ICOMP = 1, 2
         DO IC = 1, NNUCL
           NTONC(IC,ICOMP) = 0
           DO IT = 1, N5
              NPONC(0,IT,IC,ICOMP) = 0
              IPONC(IT,IC,ICOMP) = 0
           ENDDO
        ENDDO
      ENDDO
C
      IPFIE = 0
      DO IM = 1, NEQ(2)
         IF (IM.LE.NEQ(1)) THEN
            ICOMP = 1
         ELSE
            ICOMP = 2
         ENDIF
         NBAS = INDBAS(IM+1) - INDBAS(IM)
         ITA = KCOMB(IM)
         JND = INDCOM(ITA) + 1
         KND = INDCOM(ITA+1)
         DO IND = JND, KND
            IC = ICENT(IND)
            ITYP = ITYPE(IND)
            IT = NX(ITYP) + NY(ITYP) + NZ(ITYP) + 1
            NTONC(IC,ICOMP) = MAX0(IT,NTONC(IC,ICOMP))
            NPONC(0,IT,IC,ICOMP) = NBAS
            IPONC(IT,IC,ICOMP) = INDBAS(IM)
C
C           Characterize the MOLFDIR functions 
C
            DO I = 1, NBAS
               IPFIE = IPFIE + 1
               IF (IPFIE.GT.N15A) STOP 'INCREASE N15A IN MOLECUL'
               IPINF(IPFIE,1) = ICOMP
               IPINF(IPFIE,2) = IC
               IPINF(IPFIE,3) = ITYP
               IPINF(IPFIE,4) = I
               IPINF(IPFIE,5) = INDBAS(IM) + I
            ENDDO
C
C Determine number of blocks for this l-value
C Be ware that HERMIT reorders exponents in a block form large to small
C Break list at exponents that violate this ordering to avoid problems
C Also break when the number of exponents in a block exceeds MAXPRI
C
            NBLOCK = 1
            NINBL = 1
            DO I = 2, NBAS
               IOFF = INDBAS(IM) + I
               IF (ALPHA(IOFF).GT.ALPHA(IOFF-1).OR.NINBL.GE.MAXPRI) THEN
                  NPONC(NBLOCK,IT,IC,ICOMP) = NINBL
                  NBLOCK = NBLOCK + 1
                  IF (NBLOCK.GT.N5A) THEN
                     PRINT*,'TOO MANY BLOCKS IN MOLECULE INPUT'
                     PRINT*,'CHECK MOLFDIR EXPONENT ORDERING'
                     STOP 'CHECK OR INCREASE N5A'
                  ENDIF
                  NINBL = 1
               ELSE
                  NINBL = NINBL + 1
               ENDIF
            ENDDO
            NPONC(NBLOCK,IT,IC,ICOMP) = NINBL
            NBONC(IT,IC,ICOMP) = NBLOCK
         ENDDO
      ENDDO
C
      NPFIE = IPFIE
C
C    Write the MOLECULE input (DIRAC style).
C
      KINB = 3
      IF (TWOC) KINB = 0
      OPEN (88,FILE='MOLECULE.INP',FORM='FORMATTED')
      WRITE (88,'(A6)') 'INTGRL'
      WRITE (88,1010) 
      WRITE (88,1000) NAME
      WRITE (88,1001) 'C',NEQNUC,0
      DO NUC=1,NEQNUC
        IC = INDNUC(NUC) + 1
        WRITE (88,1002) CHARGE(IC),INDNUC(NUC+1)-INDNUC(NUC),RNUC(IC)
        DO IC = INDNUC(NUC) + 1, INDNUC(NUC+1)
           WRITE (88,1003) CENTRN(IC),IC,XCORD(IC),YCORD(IC),ZCORD(IC)
        ENDDO
        IC = INDNUC(NUC) + 1
        WRITE (88,1004) 'LARGE',NTONC(IC,1),
     &                  (NBONC(IT,IC,1),IT=1,NTONC(IC,1))
        DO IT = 1, NTONC(IC,1)
          JND = IPONC(IT,IC,1)
          DO IB = 1, NBONC(IT,IC,1)
             WRITE (88,1005) NPONC(IB,IT,IC,1),0,KINB
             DO I = 1,NPONC(IB,IT,IC,1)
                JND = JND + 1
                WRITE (88,1006) ALPHA(JND)
             ENDDO
          ENDDO
        ENDDO
      ENDDO
      WRITE (88,1011) 
      CLOSE (88,STATUS='KEEP')
C
C     Regenerate function list in order done by HERMIT/DIRAC and write 
C     the mapping.
C     First match large component : this can be done by comparing
C     indices only
C
      JPFIE = 0
      DO IC=1,NNUCL
        DO IT = 1, NTONC(IC,1)
          DO I = 1, NPONC(0,IT,IC,1)
            DO IDIR = 1, IDEG(IT)
               JPFIE = JPFIE + 1
               JTYP = IDIR + (IT-1) * IT * (IT+1) / 6
               ITYP = ITYPHER(JTYP)
               DO IPFIE = 1, NPFIE
                  IF (1   .EQ.IPINF(IPFIE,1).AND.
     &                IC  .EQ.IPINF(IPFIE,2).AND.
     &                ITYP.EQ.IPINF(IPFIE,3).AND.
     &                I   .EQ.IPINF(IPFIE,4))    THEN
                      IPINF(IPFIE,6) = JPFIE
                  ENDIF
               ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
C     Now match small component : here we need to compare exponents
C     because we may have used a family basis
C
      IF (.NOT.TWOC) THEN
      DO IC=1,NNUCL
        DO IT = 1, NTONC(IC,2)
          IF (IT.GT.1) THEN
            DO JND1 = 1, NPONC(0,IT-1,IC,1)
              ALPHA1 = ALPHA(IPONC(IT-1,IC,1)+JND1)
              DO IDIR = 1, IDEG(IT)
                 JPFIE = JPFIE + 1
                 JTYP = IDIR + (IT-1) * IT * (IT+1) / 6
                 ITYP = ITYPHER(JTYP)
                 DO IPFIE = 1, NPFIE
                    JND2 = IPINF(IPFIE,5)
                    ALPHA2 = ALPHA(JND2)
                    IF (2   .EQ.IPINF(IPFIE,1).AND.
     &                  IC  .EQ.IPINF(IPFIE,2).AND.
     &                  ITYP.EQ.IPINF(IPFIE,3).AND.
     &                  ALPHA1.EQ.ALPHA2)          THEN
                       IPINF(IPFIE,6) = JPFIE
                     ENDIF
                 ENDDO
              ENDDO
            ENDDO
          ENDIF
          IF (IT.LT.NTONC(IC,1)+1) THEN
            DO JND1 = 1, NPONC(0,IT+1,IC,1)
              ALPHA1 = ALPHA(IPONC(IT+1,IC,1)+JND1)
              DO IDIR = 1, IDEG(IT)
                 JPFIE = JPFIE + 1
                 JTYP = IDIR + (IT-1) * IT * (IT+1) / 6
                 ITYP = ITYPHER(JTYP)
                 DO IPFIE = 1, NPFIE
                    JND2 = IPINF(IPFIE,5)
                    ALPHA2 = ALPHA(JND2)
                    IF (2   .EQ.IPINF(IPFIE,1).AND.
     &                  IC  .EQ.IPINF(IPFIE,2).AND.
     &                  ITYP.EQ.IPINF(IPFIE,3).AND.
     &                  ALPHA1.EQ.ALPHA2)          THEN
                       IPINF(IPFIE,6) = JPFIE
                    ENDIF
                 ENDDO
              ENDDO
            ENDDO
          ENDIF
        ENDDO
      ENDDO
      ENDIF
C
      OPEN (89,FILE='MFDHERM',FORM='FORMATTED')
      WRITE (89,'(I10)') NPFIE
      WRITE (89,'(I10)') JPFIE
      DO I = 1, NPFIE
      WRITE (89,'(I10)') IPINF(I,6)
      ENDDO
      CLOSE (89,STATUS='KEEP')
C
 1000 FORMAT (A80)
 1001 FORMAT (A1,I4,I5)
 1002 FORMAT (F10.0,I5,F20.5)
 1003 FORMAT (A2,I2,3F20.8)
 1004 FORMAT (A5,12I5)
 1005 FORMAT ('H',I4,2I5)
 1006 FORMAT (F20.10)
 1010 FORMAT ('MOLECULE input automatically generated by MOLFDIR')
 1011 FORMAT ('FINISH')
      RETURN
      END

