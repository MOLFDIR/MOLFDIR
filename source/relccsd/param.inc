C
C VERSION : $Revision: 1.2 $
C DATE    : $Date: 2001/09/11 12:53:19 $
C FILE    : param.inc
C SOURCE  : $Source: /home/tc/visscher/sources/Molfdir/Molfdir/source/relccsd/param.inc,v $
C
      REAL*8 ACCUR
      PARAMETER (ACCUR=1.D-15)
      REAL*8 AR0,AR1,ARP3
      PARAMETER (AR0=0.0D00,AR1=1.0D00,ARP3=1.0D00/3.0D00)
      COMPLEX*16 A0,A1,A2,AP5,A1I
      PARAMETER (A0=(0.0D00,0.0D00),
     &           A1=(1.0D00,0.0D00),
     &           AP5=(0.50D00,0.0D00))
      PARAMETER (A2=(2.0D00,0.0D00))
      PARAMETER (A1I=(0.0D00,1.0D00))
      LOGICAL LTR,LFA
      PARAMETER (LTR=.TRUE.,LFA=.FALSE.)
      REAL*8 D0, D1, DP5
      PARAMETER ( D0 = 0.0D00 , D1 = 1.0D00, DP5 = 0.50D00 )
