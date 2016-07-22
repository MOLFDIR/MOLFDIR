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
C VERSION : $Revision: 1.1 $
C DATE    : $Date: 2001/09/11 12:57:32 $
C FILE    : mempkg.F
C SOURCE  : $Source: /home/tc/visscher/sources/Molfdir/Molfdir/source/relccsd/mempkg.F,v $
C
C
#ifdef UNDEF
/* Comdeck memlog */
910110-hjaaj MEMREL: removed non-standard char. concat. in CALL MEMCHK
901004-hjaaj: NWNMEM introduced, no final check on NWNMEM (yet)
900420-hjaaj: MEMPKG.U: corrected errors for IRAT=1 (e.g. Cray)
891208-hjaaj:
 MEMINI: new routine to define output units in MEM* routines
         (makes MEM* routines indep. of sirius, uses LUWMEM not LUWSIR)
891114-hjaaj:
 MEMREL: new routine to release memory
 MEMGET: warning if memid(2) .ne. 0 for work(kfree)
890429-hjaaj-k10d
 corrected code error in MEMGET for KFREE=1
#endif
C  /* Deck memini */
      SUBROUTINE MEMINI(LUWME1,LUEME1)
C
C Copyright 8-Dec-1989,4-Oct-1990 Hans Joergen Aa. Jensen
C
C     Define output units for memory allocation routines
C     Initialize mempkg warning count NWNMEM to zero
C
C
      EXTERNAL MEMBKD
      COMMON /MEMCB1/ LUWMEM, LUEMEM, NWNMEM
C
      LUWMEM = LUWME1
      LUEMEM = LUEME1
      NWNMEM = 0
      RETURN
      END
C  /* Deck membkd */
      BLOCK DATA MEMBKD
      COMMON /MEMCB1/ LUWMEM, LUEMEM, NWNMEM
      DATA LUWMEM,LUEMEM /6,6/
      END
C  /* Deck memget */
      SUBROUTINE MEMGET(TYPE,KBASE,LENGTH,WORK,KFREE,LFREE)
C
C Copyright 9-Jan-1988/27-Jul-1993 Hans Joergen Aa. Jensen
C
C l.r. 980824-hjaaj: QENTER/QEXIT calls use too much time because MEMGET
C is called often. Now only call QENTER/QEXIT if warning or error.
C Restructured code (e.g. changed errors from IF () THEN to IF () GO TO)
C to get fewer logical tests and fewer jumps after logical tests.
C
C     Memory allocation
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) TYPE
      DIMENSION     WORK(*)
C
C     IRAT  = (real word length) / (integer word length)
C     IRAT2 = (real word length) / (half-integer word length)
C             if available and used, otherwise IRAT2 = IRAT
C     LRAT  = (real word length) / (logical word length)
#if defined (CRAY) || defined (T3D)
      PARAMETER (IRAT = 1, IRAT2 = 1, LRAT = 1)
#else
      PARAMETER (IRAT = 2, IRAT2 = 2, LRAT = 2)
#endif
      COMMON /MEMCB1/ LUWMEM, LUEMEM, NWNMEM
C
      EXTERNAL MEMBKD
C
      PARAMETER ( LENID = 1/IRAT + 1)
      DIMENSION     WMEMID(2), WMEMCK(2)
      INTEGER       MEMID(2),  MEMCK(2)
      EQUIVALENCE  (MEMID, WMEMID), (MEMCK, WMEMCK)
      SAVE          MEMID
      DATA          MEMID(1) /1234567890/
C
      IF (TYPE(1:4) .EQ. 'REAL') THEN
         LREAL = LENGTH
      ELSE IF (TYPE(1:4) .EQ. 'INTE') THEN
         LREAL = (LENGTH-1)/IRAT + 1
      ELSE IF (TYPE(1:4) .EQ. 'LOGI') THEN
         LREAL = (LENGTH-1)/LRAT + 1
      ELSE IF (TYPE(1:4) .EQ. 'INT2') THEN
         LREAL = (LENGTH-1)/IRAT2 + 1
      ELSE IF (TYPE(1:4) .EQ. 'WORK') THEN
         LENGTH = LFREE - 2*LENID
C                         ^- make sure OK for KFREE .eq. 1
         LREAL  = LENGTH
      ELSE
         WRITE (LUWMEM,'(/2A)')
     *      ' MEMGET ERROR, illegal type :',TYPE
         IF (LUEMEM .NE. LUWMEM) THEN
            WRITE (LUEMEM,'(/2A)')
     *         ' MEMGET ERROR, illegal type :',TYPE
         END IF
         CALL QUIT('MEMGET ERROR, illegal type')
      END IF
C
C
      MEMID(2) = LREAL
      IF (KFREE .NE. 1) THEN
C
C        error and warning checks:
C
         WMEMCK(1) = WORK(KFREE-LENID)
         IF (LENID .EQ. 2) WMEMCK(2) = WORK(KFREE-1)
C        error check:
         IF (LREAL+LENID .GT. LFREE) GO TO 8001
         IF (MEMCK(1) .NE. MEMID(1)) GO TO 8002
C        warning check:
         IF (MEMCK(2) .NE. 0) GO TO 8003
C        ... go issue warning and continue at 8013
C
         WORK(KFREE-LENID) = WMEMID(1)
         IF (LENID .EQ. 2) WORK(KFREE-1) = WMEMID(2)
      ELSE
         IF (LREAL+2*LENID .GT. LFREE) GO TO 8001
         WORK(KFREE) = WMEMID(1)
         IF (LENID .EQ. 2) WORK(KFREE+1) = WMEMID(2)
         KFREE = KFREE + LENID
         LFREE = LFREE - LENID
      END IF
 8013 KBASE  = KFREE
      KFREE  = KBASE + (LREAL + LENID)
      LFREE  = LFREE - (LREAL + LENID)
      MEMID(2) = 0
      WORK(KFREE-LENID) = WMEMID(1)
      IF (LENID .EQ. 2) WORK(KFREE-1) = WMEMID(2)
      RETURN
C
C     error branches:
C
C        error branch  IF (LREAL+LENIDS .GT. LFREE) :
C
 8001    CONTINUE
         IF (KFREE .GT. 1) THEN
            LENIDS = LENID
            WRITE (LUWMEM,1010) LREAL+LENIDS,LFREE
            CALL MEMCHK('MEMGET ERROR (insuff. memory)',WORK,1)
         ELSE
            LENIDS = 2*LENID
            WRITE (LUWMEM,1010) LREAL+LENIDS,LFREE
         END IF
         IF (LUEMEM .NE. LUWMEM) THEN
            WRITE (LUEMEM,1010) LREAL+LENIDS,LFREE
         END IF
         CALL QUIT('MEMGET ERROR, insufficient work space in memory')
 1010 FORMAT(
     &   /' MEMGET ERROR, insufficient free space for next allocation',
     &   /T16,'( Need:',I10,', available (LFREE):',I10,' )')
C
C        error branch IF (MEMCK(1) .NE. MEMID(1)) :
C
 8002    CONTINUE
         WRITE (LUWMEM,'(/A/A,I10,2(/A,I25))')
     *      ' MEMGET ERROR, not a valid memget id in work(kfree)',
     *      ' KFREE =',KFREE,
     *      ' found memory check :',MEMCK(1),
     *      ' expected           :',MEMID(1)
         IF (LUEMEM.NE.LUWMEM) THEN
            WRITE (LUEMEM,'(/A/A)')
     *      ' MEMGET ERROR, not a valid memget id in work(kfree)',
     *      ' ---> see output file'
         END IF
         CALL QUIT('MEMGET ERROR, not a valid memget '//
     &             'id in work(kfree)')
C
C        warning branch IF (MEMCK(2) .NE. 0) :
C
 8003    NWNMEM = NWNMEM + 1
         WRITE (LUWMEM,'(/A/A,I10,/A,I25)')
     *      ' MEMGET WARNING, nonzero allocation work(kfree)',
     *      ' KFREE =',KFREE,
     *      ' found memory allocation :',MEMCK(2)
         IF (LUEMEM.NE.LUWMEM) THEN
            WRITE (LUEMEM,'(/A/A)')
     *      ' MEMGET WARNING, nonzero allocation work(kfree)',
     *      ' ---> see output file'
         END IF
C        Warning issued, do the assignment now and continue
         WORK(KFREE-LENID) = WMEMID(1)
         IF (LENID .EQ. 2) WORK(KFREE-1) = WMEMID(2)
         GO TO 8013
C
C     end of MEMGET
C
      END
C  /* Deck memrel */
      SUBROUTINE MEMREL(TEXT,WORK,KFIRST,KREL,KFREE,LFREE)
C
C 14-Mar-1989 Hans Joergen Aa. Jensen
C
C Check memory allocation from WORK(KFIRST)
C then free memory from WORK(KREL)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION     WORK(*)
      CHARACTER*(*) TEXT
#if defined (CRAY) || defined (T3D)
      PARAMETER (IRAT = 1, IRAT2 = 1, LRAT = 1)
#else
      PARAMETER (IRAT = 2, IRAT2 = 2, LRAT = 2)
#endif
C
      COMMON /MEMCB1/ LUWMEM, LUEMEM, NWNMEM
C
      CHARACTER     TMPTXT*71
      PARAMETER ( LENID = 1/IRAT + 1)
      DIMENSION     WMEMID(2), WMEMCK(2)
      INTEGER       MEMID(2),  MEMCK(2)
      EQUIVALENCE  (MEMID, WMEMID), (MEMCK, WMEMCK)
      SAVE          MEMID
      DATA          MEMID(1) /1234567890/
C
C
C     First check if memory allocation is intact
C     (check if anything has been out of bounds)
C
      LENTXT = LEN(TEXT)
      LENTXT = MIN(LENTXT,50)
      TMPTXT = TEXT(1:LENTXT) // ' (called from MEMREL)'
      LENTXT = LENTXT + 21
      CALL MEMCHK(TMPTXT(1:LENTXT),WORK,KFIRST)
C
C     Check if WORK(krel) has legal memid
C
      IF (KREL .GT. KFREE) GO TO 8001
      IF (KREL .NE. 1) THEN
         KFREE1 = KREL
      ELSE
         KFREE1 = 1 + LENID
      END IF
      WMEMCK(1) = WORK(KFREE1-LENID)
      IF (MEMCK(1) .NE. MEMID(1)) GO TO 8002
C
C     release ...
C
      MEMID(2) = 0
      WORK(KFREE1-LENID) = WMEMID(1)
      IF (LENID .EQ. 2) WORK(KFREE1-1) = WMEMID(2)
      LFREE = LFREE + (KFREE-KREL)
      KFREE = KREL
C
      RETURN
C
C     error branch IF (KREL .GT. KFREE) THEN
C
 8001    WRITE (LUWMEM,'(/A/2A/A,3I10)')
     *      ' MEMREL ERROR, krel .gt. kfree',
     *      ' Text from calling routine : ',TEXT,
     *      ' KFIRST,KREL,KFREE =',KFIRST,KREL,KFREE
         IF (LUEMEM.NE.LUWMEM) THEN
            WRITE (LUEMEM,'(/A/A)')
     *      ' MEMREL ERROR, krel .gt. kfree',
     *      ' ---> see output file'
         END IF
         CALL QUIT('MEMREL ERROR, krel .gt. kfree')
C
C     error branch IF (MEMCK(1) .NE. MEMID(1)) THEN
C
 8002    WRITE (LUWMEM,'(/A/2A/A,2I10,2(/A,I25))')
     *      ' MEMREL ERROR, not a valid memget id in work(krel)',
     *      ' Text from calling routine : ',TEXT,
     *      ' KFIRST,KREL =',KFIRST,KREL,
     *      ' found memory check :',MEMCK(1),
     *      ' expected           :',MEMID(1)
         IF (LUEMEM.NE.LUWMEM) THEN
            WRITE (LUEMEM,'(/A/A)')
     *      ' MEMREL ERROR, not a valid memget id in work(kfree)',
     *      ' ---> see output file'
         END IF
         CALL QUIT('MEMREL ERROR, not a valid memget id in work(kfree)')
C
C     end of MEMREL
C
      END
C  /* Deck memchk */
      SUBROUTINE MEMCHK(TEXT,WORK,KFIRST)
C
C 17-Mar-1989 Hans Joergen Aa. Jensen
C
C Check if memory allocation from WORK(KFIRST) is intact
C (check if anything has been out of bounds).
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION     WORK(*)
      CHARACTER*(*) TEXT
#if defined (CRAY) || defined (T3D)
      PARAMETER (IRAT = 1, IRAT2 = 1, LRAT = 1)
#else
      PARAMETER (IRAT = 2, IRAT2 = 2, LRAT = 2)
#endif
C
      COMMON /MEMCB1/ LUWMEM, LUEMEM, NWNMEM
C
      PARAMETER ( LENID = 1/IRAT + 1)
      DIMENSION     WMEMID(2), WMEMCK(2)
      INTEGER       MEMID(2),  MEMCK(2)
      EQUIVALENCE  (MEMID, WMEMID), (MEMCK, WMEMCK)
      SAVE          MEMID
      DATA          MEMID(1) /1234567890/
C
C
      KFREE = KFIRST
      IF (KFREE .EQ. 1) KFREE = 1 + LENID
C
      IALLOC = 0
  100 CONTINUE
         IALLOC = IALLOC + 1
         WMEMCK(1) = WORK(KFREE-LENID)
         IF (LENID .EQ. 2) WMEMCK(2) = WORK(KFREE-1)
         IF (MEMCK(1) .NE. MEMID(1)) GO TO 8001
         LREAL = MEMCK(2)
         IF (LREAL .LT. 0) GO TO 8002
         IF (LREAL .GT. 0) THEN
            KFREE = KFREE + LENID + LREAL
            GO TO 100
         END IF
C
      RETURN
C
C        error branch IF (MEMCK(1) .NE. MEMID(1)) THEN
C
 8001    WRITE (LUWMEM,'(/A/2A/A,3I10,2(/A,I25))')
     *      ' MEMCHK ERROR, not a valid memget id in work(kfree-1)',
     *      ' Text from calling routine : ',TEXT,
     *      ' KFIRST,KFREE,IALLOC =',KFIRST,KFREE,IALLOC,
     *      ' found memory check :',MEMCK(1),
     *      ' expected           :',MEMID(1)
         WRITE (LUWMEM,'(/A/A)')
     &      ' Dump of allocations before invalid memget id:',
     &      ' IALLOC #    KBASE         Length'
         KFREE = KFIRST
         IF (KFREE .EQ. 1) KFREE = 1 + LENID
         DO I = 1, IALLOC-1
            WMEMCK(1) = WORK(KFREE-LENID)
            IF (LENID .EQ. 2) WMEMCK(2) = WORK(KFREE-1)
            LREAL = MEMCK(2)
            WRITE (LUWMEM,'(I5,2I15)') I,KFREE,LREAL
            KFREE = KFREE + LENID + LREAL
         END DO
         IF (LUEMEM.NE.LUWMEM) THEN
            WRITE (LUEMEM,'(/A/A)')
     *      ' MEMCHK ERROR, not a valid memget id in work(kfree-1)',
     *      ' ---> see output file'
         END IF
         CALL QUIT('MEMCHK ERROR, not a valid memget '//
     &             'id in work(kfree-1)')
C
C        error branch IF (LREAL .LT. 0) THEN
C
 8002    WRITE (LUWMEM,'(/A/2A/A,4I10)')
     *      ' MEMCHK ERROR, next allocation LENGTH is negative',
     *      ' Text from calling routine : ',TEXT,
     *      ' KFIRST,KFREE,IALLOC,LENGTH =',KFIRST,KFREE,IALLOC,LREAL
         IF (LUEMEM.NE.LUWMEM) THEN
            WRITE (LUEMEM,'(/A/A)')
     *      ' MEMCHK ERROR, next allocation LENGTH is negative',
     *      ' ---> see output file'
         END IF
         CALL QUIT('MEMCHK ERROR, next '//
     &             'allocation has negative length.')
C
C     end of MEMCHK
C
      END
C  /* Deck memopt */
      SUBROUTINE MEMOPT(TYPE,KBASE,LENGTH,WORK,KFREE,LFREE,ALLOC)
C
C     Almost indentical to MEMGET. Luuk Visscher 21-10-1997.
C     Updated to optimized version. J. Thyssen Aug 25 1998
C
C     Memory allocation
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) TYPE
      DIMENSION     WORK(*)
      LOGICAL       ALLOC
C
#if defined (CRAY) || defined (T3D)
      PARAMETER (IRAT = 1, IRAT2 = 1, LRAT = 1)
#else
      PARAMETER (IRAT = 2, IRAT2 = 2, LRAT = 2)
#endif
      COMMON /MEMCB1/ LUWMEM, LUEMEM, NWNMEM
C
      EXTERNAL MEMBKD
C
      PARAMETER ( LENID = 1/IRAT + 1)
      DIMENSION     WMEMID(2), WMEMCK(2)
      INTEGER       MEMID(2),  MEMCK(2)
      EQUIVALENCE  (MEMID, WMEMID), (MEMCK, WMEMCK)
      SAVE          MEMID
      DATA          MEMID(1) /1234567890/
C
      IF (TYPE(1:4) .EQ. 'REAL') THEN
         LREAL = LENGTH
      ELSE IF (TYPE(1:4) .EQ. 'INTE') THEN
         LREAL = (LENGTH-1)/IRAT + 1
      ELSE IF (TYPE(1:4) .EQ. 'LOGI') THEN
         LREAL = (LENGTH-1)/LRAT + 1
      ELSE IF (TYPE(1:4) .EQ. 'INT2') THEN
         LREAL = (LENGTH-1)/IRAT2 + 1
      ELSE IF (TYPE(1:4) .EQ. 'WORK') THEN
         LENGTH = LFREE - 2*LENID
C                         ^- make sure OK for KFREE .eq. 1
         LREAL  = LENGTH
      ELSE
         WRITE (LUWMEM,'(/2A)')
     *      ' MEMOPT ERROR, illegal type :',TYPE
         IF (LUEMEM .NE. LUWMEM) THEN
            WRITE (LUEMEM,'(/2A)')
     *         ' MEMOPT ERROR, illegal type :',TYPE
         END IF
         CALL QUIT('MEMOPT ERROR, illegal type')
      END IF
C
C
      IF (ALLOC) THEN
         MEMID(2) = LREAL
         IF (KFREE .NE. 1) THEN
C
C           error and warning checks:
C
            WMEMCK(1) = WORK(KFREE-LENID)
            IF (LENID .EQ. 2) WMEMCK(2) = WORK(KFREE-1)
            IF (LREAL+LENID .GT. LFREE) GO TO 8001
            IF (MEMCK(1) .NE. MEMID(1)) GO TO 8002
            IF (MEMCK(2) .NE. 0) GO TO 8003
C
 8013       WORK(KFREE-LENID) = WMEMID(1)
            IF (LENID .EQ. 2) WORK(KFREE-1) = WMEMID(2)
         ELSE
            IF (LREAL+2*LENID .GT. LFREE) GO TO 8001
            WORK(KFREE) = WMEMID(1)
            IF (LENID .EQ. 2) WORK(KFREE+1) = WMEMID(2)
            KFREE = KFREE + LENID
            LFREE = LFREE - LENID
         END IF
      ELSE
         IF (KFREE .EQ. 1) THEN
            KFREE = KFREE + LENID
            LFREE = LFREE - LENID
         END IF
      END IF
      KBASE  = KFREE
      KFREE  = KBASE + (LREAL + LENID)
      LFREE  = LFREE - (LREAL + LENID)
      MEMID(2) = 0
      IF (ALLOC) WORK(KFREE-LENID) = WMEMID(1)
      IF (LENID .EQ. 2 .AND. ALLOC) WORK(KFREE-1) = WMEMID(2)
      RETURN
C
C     error branches:
C
C        error branch  IF (LREAL+LENIDS .GT. LFREE) :
C
 8001    CONTINUE
         IF (KFREE .GT. 1 .AND. ALLOC) THEN
            LENIDS = LENID
            WRITE (LUWMEM,1010) LREAL+LENIDS,LFREE
            CALL MEMCHK('MEMOPT ERROR (insuff. memory)',WORK,1)
         ELSE
            LENIDS = 2*LENID
            WRITE (LUWMEM,1010) LREAL+LENIDS,LFREE
         END IF
         IF (LUEMEM .NE. LUWMEM) THEN
            WRITE (LUEMEM,1010) LREAL+LENIDS,LFREE
         END IF
         CALL QUIT('MEMOPT ERROR, insufficient work space in memory')
 1010 FORMAT(
     &   /' MEMOPT ERROR, insufficient free space for next allocation',
     &   /T16,'( Need:',I10,', available (LFREE):',I10,' )')
C
C        error branch IF (MEMCK(1) .NE. MEMID(1)) :
C
 8002    CONTINUE
         WRITE (LUWMEM,'(/A/A,I10,2(/A,I25))')
     *      ' MEMOPT ERROR, not a valid MEMOPT id in work(kfree)',
     *      ' KFREE =',KFREE,
     *      ' found memory check :',MEMCK(1),
     *      ' expected           :',MEMID(1)
         IF (LUEMEM.NE.LUWMEM) THEN
            WRITE (LUEMEM,'(/A/A)')
     *      ' MEMOPT ERROR, not a valid MEMOPT id in work(kfree)',
     *      ' ---> see output file'
         END IF
         CALL QUIT('MEMOPT ERROR, not a valid MEMOPT '//
     &             'id in work(kfree)')
C
C        warning branch IF (MEMCK(2) .NE. 0) :
C
 8003    NWNMEM = NWNMEM + 1
         WRITE (LUWMEM,'(/A/A,I10,/A,I25)')
     *      ' MEMOPT WARNING, nonzero allocation work(kfree)',
     *      ' KFREE =',KFREE,
     *      ' found memory allocation :',MEMCK(2)
         IF (LUEMEM.NE.LUWMEM) THEN
            WRITE (LUEMEM,'(/A/A)')
     *      ' MEMOPT WARNING, nonzero allocation work(kfree)',
     *      ' ---> see output file'
         END IF
         GO TO 8013
C
C     end of MEMOPT
C
      END
