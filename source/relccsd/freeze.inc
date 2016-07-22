C
C VERSION : $Revision: 1.2 $
C DATE    : $Date: 2001/09/11 12:53:16 $
C FILE    : freeze.inc
C SOURCE  : $Source: /home/tc/visscher/sources/Molfdir/Molfdir/source/relccsd/freeze.inc,v $
C
      PARAMETER (MAXFROZ=100)
      COMMON/FREEZE/NFROZEN,IFROZ(MAXFROZ,32)
      INTEGER NFROZEN,IFROZ
