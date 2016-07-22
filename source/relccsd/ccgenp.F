C
C...   Copyright (c) 1998 by the authors of Dirac (see below).
C...   All Rights Reserved.
C...
C...   The source code in this file is part of 
C...   "Dirac, a relativistic ab initio electronic structure program, 
C...   Release 3.1 (1998), written by T. Saue, T. Enevoldsen, T. Helgaker,
C...   H. J. Aa. Jensen, J. Laerdahl, K. Ruud, J. Thyssen, and L. Visscher"
C...
C...   This source code is provided under a written licence and may be 
C...   used, copied, transmitted, or stored only in accord with that
C...   written licence.
C...
C...   In particular, no part of the source code or compiled modules may 
C...   be distributed outside the research group of the licence holder. 
C...   This means also that persons (e.g. post-docs) leaving the research
C...   group of the licence holder may not take any part of Dirac,
C...   including modified files, with him/her, unless that person has
C...   obtained his/her own licence. 
C...
C...   For questions concerning this copyright write to:
C...      dirac-admin@dirac.chem.ou.dk
C...
C...   For information on how to get a licence see:
C...      http://dirac.chem.ou.dk/Dirac
C
C
C VERSION : $Revision: 1.3 $
C DATE    : $Date: 2001/09/13 00:05:04 $
C FILE    : ccgenp.F
C SOURCE  : $Source: /home/tc/visscher/sources/Molfdir/Molfdir/source/relccsd/ccgenp.F,v $
C
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      INTEGER FUNCTION SEC2I(N)
C
C     BUFF in the common block IOBUFF should have a length of
C     at least two times the value used here.
C
      SEC2I=512*N
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE XTIMRD (TIMERN,ELTIM)
C
C  purpose:  read out a specific timer previously set by XTIME
C

      IMPLICIT INTEGER (A-Z)
      REAL*8 ELTIM

      INCLUDE "timtbl.inc"
      INCLUDE "param.inc"
      INCLUDE "files.inc"
      
      IF(TIMERN.LT.1.OR.TIMERN.GT.MXTIMF) THEN
        WRITE(IW,10)
        ELTIM=D0
      ELSE
        ELTIM=CPUTF(TIMERN)
      ENDIF

 10   FORMAT(/' **WARNING** Timer # out of range'/,
     &        ' ** zero returned')

      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE XTIME (TIMERN,FUNCT,NAME)

      IMPLICIT INTEGER (A-Z)
      CHARACTER*30 NAME

      INCLUDE "timtbl.inc"
      INCLUDE "param.inc"
      INCLUDE "files.inc"
      INCLUDE "ccpar.inc"
#if defined (VAR_MPI)
      INCLUDE 'mpif.h'
      DIMENSION ISTAT(MPI_STATUS_SIZE)
      PARAMETER(TITAG=12)
#endif

      REAL*8 A
C     REAL*8 SUMT

C ** check requested function

      IF(TIMERN.LT.1.OR.TIMERN.GT.MXTIMF) THEN
        WRITE(IW,130) MXTIMF
        RETURN
      ENDIF

      IF(FUNCT.EQ.TCLEAR) THEN
        DO I=1,MXTIMF
          CPUTF(I)=D0
          CPUTFT(I)=D0
          CPUTAV(I)=D0
          CPUTMI(I)=D0
          CPUTMA(I)=D0
          CPUTT(I)='                    '
        ENDDO
      ELSE IF (FUNCT.EQ.TSTART) THEN
        CALL CPUUSED(A)
        CPUTF(TIMERN)=A
        CPUTT(TIMERN)=NAME
      ELSE IF (FUNCT.EQ.TSTOP) THEN
        CALL CPUUSED(A)
        CPUTF(TIMERN)=A-CPUTF(TIMERN)
        CPUTFT(TIMERN)=CPUTFT(TIMERN)+CPUTF(TIMERN)
      ELSE IF (FUNCT.EQ.TPRNTI) THEN
        IF(CPUTF(TIMERN).NE.D0) THEN
          WRITE(IW,222) CPUTT(TIMERN),CPUTF(TIMERN)
        ELSE
          WRITE(IW,224) TIMERN
        ENDIF
      ELSE IF (FUNCT.EQ.TPRNTA) THEN
#if defined (VAR_MPI)
        IF(NMPROC.EQ.1) THEN
          WRITE(IW,220)
          DO I=1,MXTIMF
            IF(CPUTFT(I).NE.D0) WRITE(IW,222) CPUTT(I),CPUTFT(I)
          ENDDO
C         WRITE(IW,223) SUMT(CPUTFT,MXTIMF)
        ELSE
          IF(MYPROC.EQ.MASTER) THEN
            DO I=1,MXTIMF
              CPUTMI(I)=CPUTFT(I)
              CPUTMA(I)=CPUTFT(I)
              CPUTAV(I)=CPUTFT(I)
            ENDDO
            WRITE(IW,300)
            WRITE(IW,310) MASTER
            DO I=1,MXTIMF
              IF(CPUTFT(I).NE.D0) WRITE(IW,222) CPUTT(I),CPUTFT(I)
            ENDDO

            DO ND=1,NMPROC-1
              CALL MPI_RECV(CPUTFT,MXTIMF,MPI_DOUBLE_PRECISION,
     &           ND,TITAG,MPI_COMM_WORLD,ISTAT,IERR)
              IF(IERR.NE.MPI_SUCCESS) CALL 
     &          QUIT('MPI ERROR IN XTIME')
              WRITE(IW,310) ND
              DO I=1,MXTIMF
                IF(CPUTFT(I).LT.CPUTMI(I)) CPUTMI(I)=CPUTFT(I)
                IF(CPUTFT(I).GT.CPUTMA(I)) CPUTMA(I)=CPUTFT(I)
                CPUTAV(I)=CPUTAV(I)+CPUTFT(I)
                IF(CPUTFT(I).NE.D0) WRITE(IW,222) CPUTT(I),CPUTFT(I)
              ENDDO
C             WRITE(IW,223) SUMT(CPUTFT,MXTIMF)
            ENDDO

            WRITE(IW,330)
            DO I=1,MXTIMF
              IF(CPUTAV(I).NE.D0) WRITE(IW,340) CPUTT(I),CPUTMI(I),
     &          CPUTMA(I),CPUTAV(I)/DBLE(NMPROC)
            ENDDO
          ELSE
            CALL MPI_SEND(CPUTFT,MXTIMF,MPI_DOUBLE_PRECISION,
     &          MASTER,TITAG,MPI_COMM_WORLD,IERR)
            IF(IERR.NE.0) CALL QUIT('MPI ERROR in XTIME')
          ENDIF
        ENDIF
        if (myproc.eq.master) WRITE(IW,350)
#else
        WRITE(IW,220)
        DO I=1,MXTIMF
          IF(CPUTFT(I).NE.D0) WRITE(IW,222) CPUTT(I),CPUTFT(I)
        ENDDO
C       WRITE(IW,223) SUMT(CPUTFT,MXTIMF)
#endif
      ELSE
        WRITE(IW,135)
      ENDIF
 130  FORMAT(/' **WARNING** Timer # out of range'/,
     &        ' ** no action taken')
 135  FORMAT(/' **WARNING** Unknown timer function'/,
     &        ' ** no action taken')
 220  FORMAT(/'########################################'/,
     &        '###'/'### Timing results:'/'###'/,
     &        '########################################'/)
 222  FORMAT(' Time in ',A30,T33,F12.3,' seconds')
 223  FORMAT(T37,'-------------',/,T25,'Sum: ',F12.3,' seconds',
     &       /,T37,'-------------')
 224  FORMAT(' Timer ',I3,' not used for measurements')
 300  FORMAT(/'########################################'/,
     &        '###'/'### Timing results for all nodes'/'###'/,
     &        '########################################'/)
 310  FORMAT(/,T10,'Results node ',I3,/,T10,'----------------'/)
 330  FORMAT(//,T30,'Summary with min/max/average'/,
     &          T30,'----------------------------'//,
     &          T2,'Timer',T33,'Minimum',T43,'Maximum',
     &          T53,'Average'/)
 340  FORMAT(T2,A30,T33,F12.3,T43,F12.3,T53,F12.3)
 350  FORMAT(/'########################################'/,
     &        '###'/'### End of timing report'/'###'/,
     &        '########################################'/)
     
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      FUNCTION SUMT(A,N)
      REAL*8 SUMT,A(*)
      INTEGER N

      INCLUDE "param.inc"

      SUMT=D0
      DO I=1,N
        SUMT=SUMT+A(I)
      ENDDO
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE RSAVE(ITAPE,BLOCKS)
C
C FLUSH THE FILE
C
      IMPLICIT INTEGER (A-Z)
C
      JCODE = 3
      CALL RCLOSE (ITAPE,JCODE)
      CALL RFILE  (ITAPE,BLOCKS)
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE RFILE(ITAPE,BLOCKS)
C
C USE THIS FOR ALL UNFORMATTED FILES
C
      IMPLICIT INTEGER (A-Z)
#if defined (VAR_MPI)
      INCLUDE "ccpar.inc"
#endif
C
      CHARACTER*4 DN, FNAM*8, FTFILN*9
      INTEGER FTFILL,IZLL
C
      COMMON /POINTR/ POS(100)
      COMMON /SECT/ SECTOR,MAXLN
      COMMON/ FLNAME/ DN(100) 
C
      IF (ITAPE.GT.99.OR.ITAPE.LT.1) THEN
      WRITE(6,*) ' INVALID TAPE NUMBER GIVEN: ITAPE=' ,ITAPE
      CALL QUIT('ccgenp.F line 251')
      ENDIF
      SECTOR = SEC2I(1)
      MAXLN = SECTOR*1024*1024
C
C !! ** attention ** !! for cray systems the file name
C !! definition has yet to be checked. (WOPEN)
C !! no complications if all processes share same file system ???
C
C for the moment we keep the definitions for the parallel MPI run as they
C were before and change for non CRAY systems only
C

#if defined (VAR_MPI)
#if defined (CRAY) || defined (T3D)
      IF (BLOCKS.EQ.0) BLOCKS = 20
      IF (BLOCKS.EQ.1) BLOCKS = 2
      ISTATS = 1
      CALL WOPEN(ITAPE,BLOCKS,ISTATS,IERR)
      IF(IERR .NE. 0) THEN
          WRITE(6,*) ' PROBLEMS OPENING FILE ',ITAPE,' IERR =',IERR
          CALL QUIT('ccgenp line 262')
      END IF
#else
      FNAM = DN(ITAPE)   ! contains a string out of 'ft01' ... 'ft99'
      IF (MYPROC .LT. 10) THEN    !  MPI ID has one digit
         WRITE (FTFILN,'(A4,A1,I1)') FNAM,'.',MYPROC
         FTFILL=6
      ELSEIF (MYPROC .LT. 100) THEN  ! MPI ID has two digits
         WRITE (FTFILN,'(A4,A1,I2)') FNAM,'.',MYPROC
         FTFILL=7
      ELSEIF (MYPROC .LT. 1000) THEN  ! MPI ID has three digits
         WRITE (FTFILN,'(A4,A1,I3)') FNAM,'.',MYPROC
         FTFILL=8
      ELSE
         CALL QUIT("NMPROC.GT.1000! EXTEND SUBR. RFILE IN CCGENP.F")
      ENDIF

      IZLL=FTFILL+1
      FTFILN(IZLL:IZLL)=CHAR(0)  ! char(0) marks the string end in C!

      SIZE = 0
      STATUS = 1
      CALL OPENC(ITAPE,FTFILN,SIZE,STATUS)
#endif
#else
#if defined (CRAY) || defined (T3D)
      IF (BLOCKS.EQ.0) BLOCKS = 20
      IF (BLOCKS.EQ.1) BLOCKS = 2
      ISTATS = 1
      CALL WOPEN(ITAPE,BLOCKS,ISTATS,IERR)
      IF(IERR .NE. 0) THEN
          WRITE(6,*) ' PROBLEMS OPENING FILE ',ITAPE,' IERR =',IERR
          CALL QUIT('ccgenp line 308')
      END IF
#else
      FNAM = DN(ITAPE)
      FNAM(5:5)=CHAR(0)
      SIZE = 0
      STATUS = 1
      CALL OPENC(ITAPE,FNAM,SIZE,STATUS)
#endif
#endif

C
C PLACE THE POINTER OF ITAPE AT THE BEGINNING
C
      POS(ITAPE) = 1
C
C RETURN
C
      RETURN
      END
C
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE RREAD(ITAPE,ARRAY,NLEN,IREC)
C
C THIS ROUTINE READS NLEN INTEGER WORDS INTO ARRAY STARTING AT
C SECTOR IREC.
C
      IMPLICIT INTEGER (A-Z)
C
      COMMON /POINTR/ POS(100)
      COMMON /SECT/ SECTOR,MAXLN
      COMMON /IOBUFF/ BUFF(1024)
C
      DIMENSION ARRAY(NLEN)
C
      IF (NLEN.EQ.0) RETURN
      FWORD = (IREC-1)*SECTOR + 1
      NGRP = NLEN/MAXLN
      IF(MAXLN*NGRP.NE.NLEN) NGRP = NGRP + 1
C
      IF(NGRP.EQ.1) THEN
C
      NW = (NLEN/SECTOR)*SECTOR
      OFF = FWORD - 1
#if defined (CRAY) || defined (T3D)
      IERR = 0
      IF(NW.GT.0) CALL GETWA(ITAPE,ARRAY,FWORD,NW,IERR)
      IF(IERR.NE.0) THEN
         WRITE(*,*) ' ERROR IN RREAD FOR FILE ',ITAPE,FWORD,NW
         WRITE(*,*) ' IERR = ',IERR
         CALL QUIT('ccgenp.F line 341')
      END IF
#else
      IF(NW.GT.0) CALL RDABSF(ITAPE,ARRAY,NW,OFF)
#endif
C
      EXTRA = MOD(NLEN,SECTOR)
      IF(EXTRA.NE.0) THEN
      FWD = FWORD + NW
      OFF = FWD - 1
#if defined (CRAY) || defined (T3D)
      CALL GETWA(ITAPE,BUFF,FWD,SECTOR,IERR)
      IF(IERR.NE.0) THEN
         WRITE(*,*) ' ERROR IN RREAD FOR FILE ',ITAPE,FWD,NW
         WRITE(*,*) ' IERR = ',IERR
         CALL QUIT('ccgenp.F line 356')
      END IF
#else
      CALL RDABSF(ITAPE,BUFF,SECTOR,OFF)
#endif
      DO 12 I = 1,EXTRA
      ARRAY(NW+I) = BUFF(I)
   12 CONTINUE
      END IF
C
      ELSE
C
      LLEN = MOD(NLEN,MAXLN)
      IF(LLEN.EQ.0) LLEN = MAXLN
      FWD = FWORD
C
      DO 11 IGRP = 1,NGRP-1
      IPT = (IGRP-1)*MAXLN + 1
      OFF = FWD - 1
#if defined (CRAY) || defined (T3D)
      CALL GETWA(ITAPE,ARRAY(IPT),FWD,MAXLN,IERR)
      IF(IERR.NE.0) THEN
         WRITE(*,*) ' ERROR IN RREAD FOR FILE ',ITAPE
         WRITE(*,*) ' IERR = ',IERR
         CALL QUIT('ccgenp.F line 380')
      END IF
#else
      CALL RDABSF(ITAPE,ARRAY(IPT),MAXLN,OFF)
#endif
      FWD = FWD + MAXLN
  11  CONTINUE
C
      NW = (LLEN/SECTOR)*SECTOR
      IPT = (NGRP-1)*MAXLN + 1
      OFF = FWD - 1
#if defined (CRAY) || defined (T3D)
      IF(NW.GT.0) CALL GETWA(ITAPE,ARRAY(IPT),FWD,NW,IERR)
      IF(IERR.NE.0) THEN
         WRITE(*,*) ' ERROR IN RREAD FOR FILE ',ITAPE
         WRITE(*,*) ' IERR = ',IERR
         CALL QUIT('ccgenp.F line 396')
      END IF
#else
      IF(NW.GT.0) CALL RDABSF(ITAPE,ARRAY(IPT),NW,OFF)
#endif
C
      EXTRA = MOD(LLEN,SECTOR)
      IF(EXTRA.NE.0) THEN
      FWD = FWD + NW
      OFF = FWD - 1
#if defined (CRAY) || defined (T3D)
      CALL GETWA(ITAPE,BUFF,FWD,SECTOR,IERR)
      IF(IERR.NE.0) THEN
         WRITE(*,*) ' ERROR IN RREAD FOR FILE ',ITAPE
         WRITE(*,*) ' IERR = ',IERR
         CALL QUIT('ccgenp.F line 411')
      END IF
#else
      CALL RDABSF(ITAPE,BUFF,SECTOR,OFF)
#endif
      IPT = IPT + NW - 1
      DO 13 I = 1,EXTRA
      ARRAY(IPT+I) = BUFF(I)
   13 CONTINUE
      END IF
C
      END IF      
C
C WE MUST POSITION THE FILE POINTER AT THE BEGINNING OF THE NEXT SECTOR
C
      TEST = NLEN/SECTOR
      IF(SECTOR*TEST .NE. NLEN) TEST = TEST + 1
      TESTL = SECTOR*TEST
      POS(ITAPE) = FWORD + TESTL
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE RWRIT(ITAPE,ARRAY,NLEN,IREC)
C
C THIS ROUTINE WRITES NLEN INTEGER WORDS FROM ARRAY TO FILE ITAPE
C STARTING AT SECTOR IREC.
C
      IMPLICIT INTEGER (A-Z)
C
      COMMON /POINTR/ POS(100)
      COMMON /SECT/ SECTOR,MAXLN
      COMMON /IOBUFF/ BUFF(1024)
C
      DIMENSION ARRAY(NLEN)
C
      IF (NLEN.EQ.0) RETURN
      FWORD = (IREC-1)*SECTOR + 1
      NGRP = NLEN/MAXLN
      IF(MAXLN*NGRP.NE.NLEN) NGRP = NGRP + 1
C
      IF(NGRP.EQ.1) THEN
      NW = (NLEN/SECTOR)*SECTOR
      OFF = FWORD - 1
#if defined (CRAY) || defined (T3D)
      IERR = 0
      IF(NW.GT.0) CALL PUTWA(ITAPE,ARRAY,FWORD,NW,IERR)
      IF(IERR.NE.0) THEN
         WRITE(*,*) ' ERROR IN RWRIT FOR FILE ',ITAPE
         WRITE(*,*) ' IERR = ',IERR
         CALL QUIT('ccgenp.F line 461')
      END IF
#else
      CALL WRABSF(ITAPE,ARRAY,NW,OFF)
#endif
C
      EXTRA = MOD(NLEN,SECTOR)
      IF(EXTRA.NE.0) THEN
      DO 12 I = 1,EXTRA
      BUFF(I) = ARRAY(NW+I)
   12 CONTINUE
      FWD = FWORD + NW
      OFF = FWD - 1
#if defined (CRAY) || defined (T3D)
      CALL PUTWA(ITAPE,BUFF,FWD,SECTOR,IERR)
      IF(IERR.NE.0) THEN
         WRITE(*,*) ' ERROR IN RWRIT FOR FILE ',ITAPE
         WRITE(*,*) ' IERR = ',IERR
         CALL QUIT('ccgenp.F line 479')
      END IF
#else
      CALL WRABSF(ITAPE,BUFF,SECTOR,OFF)
#endif
      END IF
C
      ELSE
C
      LLEN = MOD(NLEN,MAXLN)
      IF(LLEN.EQ.0) LLEN = MAXLN
      FWD = FWORD
C
      DO 11 IGRP = 1,NGRP-1
      IPT = (IGRP-1)*MAXLN + 1
      OFF = FWD - 1
#if defined (CRAY) || defined (T3D)
      CALL PUTWA(ITAPE,ARRAY(IPT),FWD,MAXLN,IERR)
      IF(IERR.NE.0) THEN
         WRITE(*,*) ' ERROR IN RWRIT FOR FILE ',ITAPE
         WRITE(*,*) ' IERR = ',IERR
         CALL QUIT('ccgenp.F line 500')
      END IF
#else
      CALL WRABSF(ITAPE,ARRAY(IPT),MAXLN,OFF)
#endif
      FWD = FWD + MAXLN
  11  CONTINUE
C
      NW = (LLEN/SECTOR)*SECTOR
      IPT = (NGRP-1)*MAXLN + 1
      OFF = FWD - 1
#if defined (CRAY) || defined (T3D)
      IF(NW.GT.0) CALL PUTWA(ITAPE,ARRAY(IPT),FWD,NW,IERR)
      IF(IERR.NE.0) THEN
         WRITE(*,*) ' ERROR IN RWRIT FOR FILE ',ITAPE
         WRITE(*,*) ' IERR = ',IERR
         CALL QUIT('ccgenp.F line 516')
      END IF
#else
      IF(NW.GT.0) CALL WRABSF(ITAPE,ARRAY(IPT),NW,OFF)
#endif
C
      EXTRA = MOD(LLEN,SECTOR)
      IF(EXTRA.NE.0) THEN
      IPT = IPT + NW - 1
      DO 13 I = 1,EXTRA
      BUFF(I) = ARRAY(IPT+I)
   13 CONTINUE
      FWD = FWD + NW
      OFF = FWD - 1
#if defined (CRAY) || defined (T3D)
      CALL PUTWA(ITAPE,BUFF,FWD,SECTOR,IERR)
      IF(IERR.NE.0) THEN
         WRITE(*,*) ' ERROR IN RWRIT FOR FILE ',ITAPE
         WRITE(*,*) ' IERR = ',IERR
         CALL QUIT('ccgenp.F line 535')
      END IF
#else
      CALL WRABSF(ITAPE,BUFF,SECTOR,OFF)
#endif
      END IF
C
      END IF      
C
C WE MUST POSITION THE FILE POINTER AT THE BEGINNING OF THE NEXT SECTOR
C
      TEST = NLEN/SECTOR
      IF(SECTOR*TEST .NE. NLEN) TEST = TEST + 1
      TESTL = SECTOR*TEST
      POS(ITAPE) = FWORD + TESTL
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE RCLOSE(ITAPE,JCODE)
C
      IMPLICIT INTEGER (A-Z)

#if defined (VAR_MPI)
      INCLUDE "ccpar.inc"
#endif

      CHARACTER*8 FNAM
      CHARACTER*9 FTFILN
      CHARACTER*4 DN
C
      COMMON /POINTR/ POS(100)
      common /flname/ dn(100)
C
C JCODE = 4     CLOSE AND DELETE FILE
C JCODE = 3     CLOSE AND SAVE FILE
C
      IF(JCODE.NE.3.AND.JCODE.NE.4) THEN
         WRITE(*,*) ' INVALID JCODE IN RCLOSE,  JCODE = ',JCODE
         WRITE(*,*) ' FILE ',ITAPE,'  CLOSED AND SAVED.'
         JCODE = 3
      END IF
C
      IF(ITAPE.EQ.6) CALL QUIT(' YOU CANNOT CLOSE A FILE ON UNIT 6')
#if defined (CRAY) || defined (T3D)
      IERR = 0
      CALL WCLOSE(ITAPE,IERR)
      IF(IERR.NE.0) THEN
         WRITE(*,*) ' ERROR ENCOUNTERED CLOSING FILE',ITAPE
         WRITE(*,*) ' IERR,JCODE = ',IERR,JCODE
      END IF
#else
      CALL CLOSEC(ITAPE)
#endif
C
#if defined (VAR_MPI)
      IF(JCODE.EQ.4) THEN
       FNAM = DN(ITAPE)
       IF (MYPROC .LT. 10) THEN    !  MPI ID has one digit
        WRITE (FTFILN,'(A4,A1,I1)') FNAM,'.',MYPROC
        FTFILL=6
       ELSEIF (MYPROC .LT. 100) THEN  ! MPI ID has two digits
        WRITE (FTFILN,'(A4,A1,I2)') FNAM,'.',MYPROC
        FTFILL=7
       ELSEIF (MYPROC .LT. 1000) THEN  ! MPI ID has three digits
        WRITE (FTFILN,'(A4,A1,I3)') FNAM,'.',MYPROC
        FTFILL=8
       ELSE
        CALL QUIT("NMPROC.GT.1000! EXTEND SUBR. RFILE IN CCGENP.F")
       ENDIF
       IZLL=FTFILL+1
       FTFILN(IZLL:IZLL)=CHAR(0)  ! char(0) marks the string end in C!

#if defined (CRAY) || defined (T3D)
        IF (ITAPE.LT.10) WRITE (FNAM,'(A5,I1,A1)') 'fort.',ITAPE,char(0)
        IF (ITAPE.GT.10) WRITE (FNAM,'(A5,I2,A1)') 'fort.',ITAPE,char(0)
        CALL PXFUNLINK(FTFILN,0,STAT)
#else
        STAT = UNLINK(FTFILN)
#endif
        IF(STAT.NE.0) THEN
          WRITE(6,*) '  PROBLEMS DELETING FILE ',ITAPE,'  STAT = ',STAT
          WRITE(6,*) ' DN = ',DN(ITAPE)
        END IF
      END IF
#else
      IF(JCODE.EQ.4) THEN
        FNAM = DN(ITAPE)
        FNAM(5:5)=CHAR(0)
#if defined (CRAY) || defined (T3D)
        IF (ITAPE.LT.10) WRITE (FNAM,'(A5,I1,A1)') 'fort.',ITAPE,char(0)
        IF (ITAPE.GT.10) WRITE (FNAM,'(A5,I2,A1)') 'fort.',ITAPE,char(0)
        CALL PXFUNLINK(FNAM,0,STAT)
#else
        STAT = UNLINK(FNAM)
#endif
        IF(STAT.NE.0) THEN
          WRITE(6,*) '  PROBLEMS DELETING FILE ',ITAPE,'  STAT = ',STAT
          WRITE(6,*) ' DN = ',DN(ITAPE)
        END IF
      END IF
#endif
C
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      BLOCK DATA FLNAMES
      CHARACTER*4 DN
      COMMON/ FLNAME/ DN(100) 
      DATA DN /'ft01','ft02','ft03','ft04','ft05','ft06','ft07','ft08',
     &  'ft09',
     &  'ft10','ft11','ft12','ft13','ft14','ft15','ft16','ft17','ft18',
     &  'ft19','ft20','ft21','ft22','ft23','ft24','ft25','ft26','ft27',
     &  'ft28','ft29','ft30','ft31','ft32','ft33','ft34','ft35','ft36',
     &  'ft37','ft38','ft39','ft40','ft41','ft42','ft43','ft44','ft45',
     &  'ft46','ft47','ft48','ft49','ft50','ft51','ft52','ft53','ft54',
     &  'ft55','ft56','ft57','ft58','ft59','ft60','ft61','ft62','ft63',
     &  'ft64','ft65','ft66','ft67','ft68','ft69','ft70','ft71','ft72',
     &  'ft73','ft74','ft75','ft76','ft77','ft78','ft79','ft80','ft81',
     &  'ft82','ft83','ft84','ft85','ft86','ft87','ft88','ft89','ft90',
     &  'ft91','ft92','ft93','ft94','ft95','ft96','ft97','ft98','ft99',
     &  'help'/
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE FLIN(A,IDIM,IN,IM,DET)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     LINEAR SIMULTANEOUS EQUATION
C
C     A(IN*IN) * X(IN*IM) = B(IN*IM)
C
C     A & B SHOULD BE STORED ON A(IN*(IN+IM))
C     SOLUTION X WILL BE STORED ON B PART IN DIMENSION A.
C
      DIMENSION A(IDIM,*)
#if defined (BIT64)
      PARAMETER (ZERO=0.0,ONE=1.0)
#else
      PARAMETER (ZERO=0.D0,ONE=1.D0)
#endif
C
      N=IN
      NR=IM
      JMAX=N+NR
      SIGN=ONE
C M IS THE STAGE OF ELIMINATION
      DO 49 M=1,N
      TEMP=ZERO
      DO 41 I=M,N
      IF(M.GT.1)A(I,M)=A(I,M)-DOTT(A(I,1),IDIM,A(1,M),1,M-1)
      AVAL=A(I,M)
#if defined (BIT64)
      IF( ABS(AVAL).LE.TEMP)GOTO 41
      TEMP= ABS(AVAL)
#else
      IF(ABS(AVAL).LE.TEMP)GOTO 41
      TEMP=ABS(AVAL)
#endif
      IMAX=I
 41   CONTINUE
      IF(TEMP.LE.ZERO)GOTO 999
      IF(IMAX.EQ.M)GOTO 45
      SIGN=-SIGN
      DO 44 J=1,JMAX
      STOR=A(M,J)
      A(M,J)=A(IMAX,J)
      A(IMAX,J)=STOR
 44   CONTINUE
 45   CONTINUE
      JJ=M+1
      IF(JJ.GT.JMAX)GOTO 49
      IF(M.GT.1)GOTO 47
      DO 46 J=JJ,JMAX
      A(1,J)=A(1,J)/A(1,1)
 46   CONTINUE
      D=A(1,1)
      GOTO 49
 47   CONTINUE
      DO 48 J=JJ,JMAX
      A(M,J)=(A(M,J)-DOTT(A(M,1),IDIM,A(1,J),1,M-1))/A(M,M)
 48   CONTINUE
      D=D*A(M,M)
 49   CONTINUE
      IF(NR.EQ.0) RETURN
      DO 59 I=1,NR
      NPI=N+I
      DO 58 K=2,N
      J=N+1-K
      A(J,NPI)=A(J,NPI)-DOTT(A(J,J+1),IDIM,A(J+1,NPI),1,K-1)
 58   CONTINUE
 59   CONTINUE
C***  IF(ABS(D).GE.1.0D+36) D=1.0D+36
C***  IF(ABS(D).LE.1.0D-36) D=1.0D-36
      DET=D*SIGN
      RETURN
C ON ZERO PIVOT, SET DET=0.AND RETURN TO CALLING PROGRAM NOV 1972
 999  DET=ZERO
      RETURN
      END
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      FUNCTION DOTT(A,NA,B,NB,N)
      IMPLICIT REAL*8 (A-H,O-Z)

      DIMENSION A(*),B(*)
#if defined (BIT64)
      PARAMETER (ZERO=0.0,ONE=1.0)
#else
      PARAMETER (ZERO=0.D0,ONE=1.D0)
#endif
C
      IAPT=1
      IBPT=1
      D   =ZERO
      DO 10 I=1,N
        D   = D+A(IAPT)*B(IBPT)
        IAPT= IAPT+NA
        IBPT= IBPT+NB
 10   CONTINUE
      DOTT=D
      RETURN
      END
