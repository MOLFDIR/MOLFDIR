C     ========================================
      SUBROUTINE Q2JOB(ICODE,STRING,IPAR,IVAL)
C     ========================================
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     THIS ROUTINE (QUIT TO JOB) FINISHES THE CALLING PROGRAM AFTER
C     ISSUING AN APPROPRIATE ERROR-MESSAGE. THE FOLLOWING OPTIONS
C     ARE AVAILABLE :
C     1---END OF FILE ENCOUNTERED
C         STRING---DEFENITION OF THE RECORD EXPECTED
C     2---USER ERROR (AS INCONSISTENT INPUT)
C         STRING---INDICATION OF THE ERROR
C     3---DIMENSION BOUND EXCEEDED BECAUSE PROGRAM PARAMETERS ARE
C         TOO SMALL TO RUN WITH THE INPUT-DATA
C         STRING---INDICATION OF SECTION WHERE ERROR WAS DETECTED
C         IPAR  ---NAME OF THE PARAMETER TO BE CHANGED
C         IVAL  ---INDICATION OF THE LEAST VALUE TO ASSIGN TO PARAMETER
C     4---DIMENSION BOUND EXCEEDED BECAUSE OF INTERNAL PROGRAM ERRORS
C         STRING---INDICATION OF THE LOCATION WHERE ERROR WAS DETECTED
C     5---NON MATCHING PARAMETERS
C         STRING---PLACE ERROR OCCURRED
C         IPAR  ---PARAMETER IN ERROR
C         IVAL  ---PREVIOUS VALUE FOR PARAMETER
C     6---ANY PROGRAM ERROR
C         STRING---INDICATION/LOCATION OF ERROR
C
      CHARACTER STRING*(*),AST*4,IPAR*3
      DATA AST/' ** '/
      PRINT 100,AST
      GOTO (10,20,30,40,50,60) ICODE
   10 PRINT 400,AST,'>>>>>EOF ENCOUNTERED : ',STRING,' EXPECTED <<<<<'
      PRINT 200,AST
      STOP  '--CHECK INPUT DECK FOR MISSING DATA--'
   20 PRINT 200,AST,STRING,AST
      PRINT 200,AST
      STOP '** USER ERROR--PROGRAM ABORTED **'
   30 PRINT 200,AST,'DIMENSION BOUND EXCEEDED :',AST
      PRINT 200,AST//'ERROR OCCURRED AT ',STRING,AST
      PRINT 200,AST,'PARAMETER TO BE CHANGED : '//IPAR,AST
      PRINT 300,AST,'TO AT LEAST ',IVAL,AST
      PRINT 200,AST
      STOP '-- ENLARGE PARAMETER  --'
   40 PRINT 200,AST,'DIMENSION BOUND EXCEEDED :',AST
      PRINT 200,AST//'ERROR OCCURRED AT ',STRING,AST
      PRINT 200,AST,'PROGRAM ERROR',AST
      PRINT 200,AST
      STOP '-- CORRECT PROGRAM --'
   50 PRINT 400,AST,'NON MATCHING PARAMETER AT ',STRING,AST
      PRINT 300,AST,'PARAMETER '//IPAR//' SHOULD BE AT LEAST',IVAL,AST
      PRINT 200,AST
      STOP '--NON MATCHING PARAMETERS--'
   60 PRINT 200,AST
      PRINT 200,AST,STRING,AST
      PRINT 200,AST,'PROGRAM ERROR',AST
      PRINT 200,AST
      STOP'--PROGRAM ERROR--'
  100 FORMAT(/A)
  200 FORMAT(2A,T55,A)
  300 FORMAT(2A,I10,T55,A)
  400 FORMAT(4A)
      END
