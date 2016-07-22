C
C     DEFINITION VARIABLES FOR MPI
C
      PARAMETER (MAXNOD=999)
C
      COMMON/MPIDATA/mpisiz,mpirnk
C
      INTEGER mpisiz,mpirnk
C
C     ***********************************
C     definition of the used mpitags !
C     ***********************************

      integer M_BELL_T
      integer M_IMHE_T
      integer M_DAAV_T
      integer M_SEOK_T
      integer M_AFDA_T
      integer M_GIBL_T
      integer M_GLBL_T
      integer M_CIBL_T
      integer M_CLBL_T
      integer M_BIBL_T
      integer M_BLBL_T
      integer M_NDAV_T
      integer M_ACDA_T
      integer M_JODO_T
      integer M_IJKL_T
      integer M_CONT_T
      integer M_IRTS_T
      integer M_ERRS_T

C **   ring the bell, are you there ?
      data M_BELL_T /'80'X/

C  **  I am here !
      data M_IMHE_T /'81'X/

C **   data available
      data M_DAAV_T /'82'X/

C **   now do your send
      data M_SEOK_T /'83'X/

C **   ask for data
      data M_AFDA_T /'84'X/

C **   Coulomb integral block
      data M_CIBL_T /'85'X/

C **   Coulomb label block
      data M_CLBL_T /'86'X/

C **   Breit integral block
      data M_BIBL_T /'87'X/

C **   Breit label block
      data M_BLBL_T /'88'X/

C **   node available
      data M_NDAV_T /'89'X/

C **   actual  data
      data M_ACDA_T /'90'X/

C **   job done
      data M_JODO_T /'91'X/

C **   ijkl integral stream
      data M_IJKL_T /'92'X/

C **   continue execution
      data M_CONT_T /'93'X/

C **   get integral block
      data M_GIBL_T /'94'X/

C **   get label block
      data M_GLBL_T /'95'X/

C **   return status
      data M_IRTS_T /'96'X/

C **   error statur
      data M_ERRS_T /'97'X/
