C
      FUNCTION OVLAP (L,M,A,B,T)
      IMPLICIT REAL*8 (A-H, O-Z)
C
C     ==================================================================
C     PURPOSE : FUNCTION OVLAP RETURNS THE VALUE OF THE OVERLAPINTEGRAL
C
C               ('PI'*T)**-0.5 * [(X+A)**L * (X+B)**M * EXP(-X**2/T) DX]
C
C     ==================================================================
C
      LL=L+1
      MM=M+1
      IF (MM.GT.LL) THEN
         ITEMP=LL
         LL=MM
         MM=ITEMP
         TEMP=A
         A=B
         B=TEMP
      ENDIF
      GO TO (100,101,102,103,104,105,106,107),LL
      CALL Q2JOB(6,' ARGUMENTS L,M TOO LARGE IN P.OVLAP ',' ',0)
  100 GO TO (110),MM
  101 GO TO (120,121),MM
  102 GO TO (130,131,132),MM
  103 GO TO (140,141,142,143),MM
  104 GO TO (150,151,152,153,154),MM
  105 GO TO (160,161,162,163,164,165),MM
  106 GO TO (170,171,172,173,174,175,176),MM
  107 GO TO (180,181,182,183,184,185),MM
      CALL Q2JOB(6,' ARGUMENTS L,M TOO LARGE IN P.OVLAP ',' ',0)
C......00
  110 OVLAP=1.
      GO TO 300
C......10
  120 OVLAP=A
      GO TO 300
C......11
  121 OVLAP=A*B+0.5*T
      GO TO 300
C......20
  130 OVLAP=A*A+0.5*T
      GO TO 300
C......21
  131 OVLAP=A*A*B+T*(A+0.5*B)
      GO TO 300
C......22
  132 OVLAP=A*A*B*B+T*(0.5*(A*A+B*B)+2.*A*B+0.75*T)
      GO TO 300
C......30
  140 OVLAP=A*(A*A+1.5*T)
      GO TO 300
C......31
  141 OVLAP=A*(A*(A*B+1.5*T)+1.5*B*T)+0.75*T*T
      GO TO 300
C......32
  142 B2=B*B
      T2=T*T
      OVLAP=A*(A*(A*(B2+0.5*T)+3.*B*T)+1.5*B2*T+2.25*T2)+1.5*B*T2
      GO TO 300
C.....33
  143 AB=A*B
      ABY=A*A+3.*AB+B*B
      OVLAP=AB*(AB*AB+1.5*T*ABY)+T*T*(2.25*ABY+1.875*T)
      GO TO 300
C......40
  150 OVLAP=A*A*(A*A+3.*T)+0.75*T*T
      GO TO 300
C......41
  151 OVLAP=A*(A*(A*(A*B+2.*T)+3.*B*T)+3.*T*T)+0.75*B*T*T
      GO TO 300
C......42
  152 B2=B*B
      T2=T*T
      OVLAP=A*(A*(A*(A*(B2+0.5*T)+4.*B*T)+3.*B2*T+4.5*T2)+6.*B*T2)+
     1T2*(0.75*B2+1.875*T)
      GO TO 300
C.....43
  153 T2=T*T
      B2=B*B
      OVLAP=A*(A*(A*(A*(B*(B2+1.5*T))+3.*T*(2.*B2+T))+3.*B*T*(B2+4.5*T))
     1+T2*(9.*B2+7.5*T))+B*T2*(0.75*B2+5.625*T)
      GO TO 300
C.....44
  154 A2=A*A
      B2=B*B
      AB=A*B
      OVLAP=0.75*T*T*(T*(8.75*T+15.*(A2+B2)+40.*AB)+A2*A2+B2*B2+16.*AB*
     $(A2+B2)+36.*A2*B2) + A2*B2*(((A2+B2)*3.+8.*AB)*T+A2*B2)
      GO TO 300
C.....50
  160 A2=A*A
      OVLAP=A*(5.*T*(0.75*T+A2)+A2*A2)
      GO TO 300
C.....51
  161 T2=T*T
      OVLAP=A*(A*(A*(A*(A*B+2.5*T)+5.*B*T)+7.5*T2)+3.75*B*T2)+1.875*T*T2
      GO TO 300
C.....52
  162 T2=T*T
      B2=B*B
      OVLAP=A*(A*(A*(A*(A*(0.5*T+B2)+5.*B*T)+5.*B2*T+7.5*T2)+
     115.*B*T2)+T2*(3.75*B2+9.375*T))+3.75*B*T*T2
      GO TO 300
C.....53
  163 U=T/2.
      U2=U*U
      A2=A*A
      A4=A2*A2
      B2=B*B
      AB=A*B
      OVLAP=AB*A4*B2+U*(3.*AB*A4+15.*A4*B2+10.*A2*B2*AB)+3.*U2*(
     15.*A4+30.*A2*AB+30.*A2*B2+5.*AB*B2)+15.*U*U2*(10.*A2+15.*AB+
     23.*B2)+105.*U2*U2
      GOTO 300
C.....54
  164 A2=A*A
      B2=B*B
      OVLAP=3.75*T*T*(T*(7.*T*(1.25*A+B)+2.*B*B2+15.*B2*A+20.*A2*B+5.
     $*A2*A)+A*B2*B2+8.*A2*B2*B+12.*A2*A*B2+4.*A2*A2*B+0.2*A2*A2*A)
     $+A*A2*B2*(T*(5.*B2+10.*A*B+3.*A2)+A2*B2)
      GOTO 300
C.....55
  165 A2=A*A
      A4=A2*A2
      B2=B*B
      B4=B2*B2
      AB=A*B
      ABX=AB*(A2+B2+2.*AB)
      ABY=A2+B2+2.5*AB
      OVLAP=T*T*T*(65.625*T*(0.45*T+ABY)+9.375*(A4+B4)+93.75*
     $ABX)+AB*(T*(3.75*T*(A4+B4+10.*ABX)+5.*AB*AB*ABY)+A4*B4)
      GOTO 300
C.....60
  170 A2=A*A
      OVLAP=T*T*(1.875*T+11.25*A2)+A2*A2*(7.5*T+A2)
      GOTO 300
C.....61
  171 A2=A*A
      OVLAP=T*T*(1.875*B*T+A*(11.25*(T+A*B)+15.*A2))
     $+A2*A2*(3.*A*T+7.5*B*T+A2*B)
      GOTO 300
C.....62
  172 A2=A*A
      B2=B*B
      OVLAP=T*T*(T*(6.5625*T+28.125*A2+22.5*A*B+1.875*B2)
     $+A2*(30.*A*B+11.25*(A2+B2)))+A2*A2*(T*(0.5*A2+6.*A*B+7.5*B2)
     $+A2*B2)
      GOTO 300
C.....63
  173 A2=A*A
      B2=B*B
      OVLAP=T*T*(T*(T*(39.375*A+19.6875*B)+37.5*A2*A+84.375*A2*B
     $+33.75*A*B2+1.875*B2*B)+A2*(4.5*A2*A+33.75*A2*B+45.*A*B2+
     $11.25*B*B2))+A2*A2*B*(T*(1.5*A2+9.*A*B+7.5*B2)+A2*B2)
      GOTO 300
C.....64
  174 A2=A*A
      A4=A2*A2
      B2=B*B
      B4=B2*B2
      OVLAP=T*T*(T*(T*(29.53125*T+98.4375*A2+157.5*A*B+39.375*B2)
     $+28.125*A4+150.*A2*A*B+168.75*A2*B2+45.*A*B2*B+1.875*B4)
     $+A4*(0.75*A2+18.*A*B+67.5*B2)+60.*A2*A*B2*B+11.25*A2*B4)
     $+A4*B2*(T*(3.*A2+12.*A*B+7.5*B2)+A2*B2)
      GOTO 300
C.....65
  175 A2=A*A
      A4=A2*A2
      B2=B*B
      B4=B2*B2
      OVLAP=T*T*(T*(T*((177.1875*A+147.65625*B)*T+131.25*A2*A
     &+492.1875*A2*B+393.75*A*B2+65.625*B2*B)+11.25*A4*A+140.625*
     &A4*B+375.*A2*B2*A+281.25*A2*B2*B+56.25*A*B4+1.875*B4*B)+
     &3.75*A4*A2*B+45.0*A4*A*B2+112.5*A4*B2*B+75.0*A2*A*B4+
     &11.25*A2*B4*B)+T*(5.0*A4*A2*B2*B+15.0*A4*B4*A+7.5*A4*B4*B)
     &+A4*A2*B4*B
      GOTO 300
C.....66
  176 A2=A*A
      A4=A2*A2
      A5=A4*A
      B2=B*B
      B4=B2*B2
      B5=B4*B
      OVLAP=T*T*(T*(T*(T*(162.421875*T+442.96875*(A2+B2)+1063.125*
     &A*B)+98.4375*(A4+B4)+787.5*(B*(A2*A+A*B2))+1476.5625*A2*B2)+
     &1.875*(A4*A2+B4*B2)+67.5*(A5*B+B5*A)+421.875*(A4*B2+A2*B4)+
     &750.0*A2*A*B2*B)+11.25*(A5*A*B2+A2*B5*B)+90.0*(A5*B2*B+A2*A*
     &B5)+168.75*A4*B4)+A4*B4*((7.5*(A2+B2)+18.0*(A*B))*T+A2*B2)
      GOTO 300
C.....70
  180 A2=A*A
      A5=A2*A2*A
      OVLAP=T*(T*(A*(13.125*T+A2*26.25))+10.5*A5)+A5*A2
      GOTO 300
C.....71
  181 A2=A*A
      A4=A2*A2
      AB=A*B
      OVLAP=T*(T*(T*(6.5625*T+13.125*(3.0*A2+AB))+26.25*(A4+A2*AB))
     &+3.5*(A4*A2+3.0*A4*AB))+AB*A4*A2
      GOTO 300
C.....72
  182 A2=A*A
      A4=A2*A2
      A6=A4*A2
      AB=A*B
      AB2=AB*B
      OVLAP=T*(T*(T*(T*(45.9375*A+13.125*B)+65.625*A2*A+78.75*A2*B+
     &13.125*AB2)+15.75*A4*A+52.5*A4*B+26.25*A2*AB2)+0.5*A6*A+   
     &7.0*A6*B+10.5*A4*AB2)+A6*AB2
      GOTO 300
C.....73
  183 A2=A*A
      B2=B*B
      A4=A2*A2
      AB=A*B
      OVLAP=T*(T*(T*(T*(29.53125*T+137.8125*(A2+AB)+19.6875*B2)+
     &65.625*A4+196.875*A2*AB+118.125*A2*B2+13.125*AB*B2)+5.25*A4*
     &A2+47.25*A4*AB+78.75*A4*B2+26.25*A2*B2*AB)+A2*(1.5*A4*AB+
     &10.5*(A4*B2+A2*AB*B2)))+A4*A2*B2*AB
      GOTO 300
C.....74
  184 A2=A*A
      A4=A2*A2
      B2=B*B
      B4=B2*B2
      AB=A*B  
      OVLAP=T*(T*(T*(T*(T*(206.71875*A+118.125*B)+229.6875*A2*A+
     &275.625*(2.0*A2*B+A*B2)+26.25*B2*B)+13.125*(3.0*A4*A+12.0*
     &A2*B2*B+A*B2*B2)+131.25*(2.0*A4*B+3.0*AB*A2*B))+0.75*A4*A2*A
     &+5.25*(4.0*A4*AB*A+20.0*A4*B2*B+5.0*A2*AB*B2*B)+94.5*A4*AB*B)
     &+3.0*A4*A2*A*B2+14.0*A4*A2*B2*B+10.5*A4*AB*B2*B)+A4*A2*A*B4
      GOTO 300
C.....75
  185 A2=A*A
      A4=A2*A2
      A6=A4*A2
      B2=B*B
      B4=B2*B2
      AB=A*B
      OVLAP=T*(T*(T*(T*(T*(162.421875*T+620.15625*A2+1033.59375*
     &AB+295.3125*B2)+229.6875*A4+1148.4375*A2*AB+459.375*(3.0*
     &A2*B2+AB*B2)+32.8125*B4)+13.125*(A6+AB*B4)+196.875*(A4*AB+
     &A2*B4)+625.25*(A4*B2+AB*A2*B2))+3.75*A6*AB+52.5*A6*B2+
     &157.5*A4*B2*AB+131.25*A4*B4+26.25*A2*AB*B4)+5.0*A6*AB*B2+
     &17.5*A6*B4+10.5*A4*B4*AB)+A6*AB*B4
      GOTO 300
  300 CONTINUE
C     --------------------------------------------------------
C     IF A AND B WERE INTERCHANGED, INTERCHANGE AGAIN TO LEAVE
C     WITH CORRECT VALUES
C     --------------------------------------------------------
      IF (M.GT.L) THEN
         TEMP=A
         A=B
         B=TEMP
      ENDIF
      RETURN
      END
