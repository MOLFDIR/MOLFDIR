C     =================
      SUBROUTINE MFOPEN
C     =================
      IMPLICIT REAL*8 (A-H, O-Z)
C
      INCLUDE 'paracom.inc'
      INCLUDE 'general.inc'
      INCLUDE 'memory.inc'
C
      LOGICAL LBTOBE,INTOBE
C
      BREXIST = .FALSE.
      DO 101 I = MFIL, 1, -1
         INQUIRE (FILE = FNAMES(I), EXIST = INTOBE)
         INQUIRE (FILE = FNAMES(I+4), EXIST = LBTOBE)
         IF (.NOT.INTOBE) GOTO 102
         IF (.NOT.LBTOBE) GOTO 103
         OPEN ( INTFILE (I) , FILE = FNAMES (I) ,
     +         FORM = 'UNFORMATTED', ERR = 102)
         OPEN ( LBLFILE (I) , FILE = FNAMES (I+4) ,
     +         FORM = 'UNFORMATTED', ERR = 103)
         GOTO 101
  102    PRINT*,'INTEGRAL FILE ',FNAMES(I),' CANNOT BE READ'
         STOP 'ERROR IN INTEGRAL FILE !'
  103    PRINT*,'LABEL FILE ',FNAMES(I+4),' CANNOT BE READ'
         STOP 'ERROR IN LABEL FILE !'
  101 CONTINUE
      IF (.NOT.TWOC) THEN
         INQUIRE (FILE = FNAMES(4), EXIST = INTOBE)
         INQUIRE (FILE = FNAMES(8), EXIST = LBTOBE)
         IF (INTOBE.AND.LBTOBE) THEN
            OPEN ( INTFILE (4) , FILE = FNAMES (4) ,
     +            FORM = 'UNFORMATTED', ERR = 106)
            OPEN ( LBLFILE (4) , FILE = FNAMES (8) ,
     +            FORM = 'UNFORMATTED', ERR = 106)
            BREXIST = .TRUE.
  106       IF (BREIT.AND..NOT.BREXIST) THEN
               PRINT*,'BREIT LABEL OR INTEGRAL FILE CANNOT ',
     +         'BE READ'
               STOP 'ERROR IN SRT BREIT FILES'
            ENDIF
         ENDIF
      ENDIF
C
      RETURN
C
      END
