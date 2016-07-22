      SUBROUTINE HLFUNS (ABXYZ,ALPHAI,ALPHAJ,IPRIM,JPRIM,IJPRIM,
     & AR,BR,NI,NJ,LSUM,
     & P,T,DSAB,DISTAB,GAB,HL,PA,PB,PA2,PB2,PA4,PB4,GG,GG2,GG3,
     & FI0,FI1,FI2,FI3,FI4,FI5,FI6,FI7,FI8,FI9)
C
      IMPLICIT REAL*8 (A-H, O-Z)
C
C---------------Description--------------------------------------------
C
C---------------Routines called----------------------------------------
C
C---------------Last modified------------------------------------------
C
C     Author : Luuk Visscher
C
C---------------Calling variables--------------------------------------
C
      LOGICAL ABXYZ
      INTEGER NI(3),NJ(3),LSUM(4)
      REAL*8 AR(3),BR(3)
      REAL*8 ALPHAI(IPRIM),ALPHAJ(JPRIM)
      REAL*8 P(IJPRIM,3),HL(IJPRIM,0:12),GAB(IJPRIM),T(IJPRIM),
     &       DISTAB(IJPRIM)
      REAL*8 PA(IJPRIM),PB(IJPRIM),PA2(IJPRIM),PB2(IJPRIM),
     &       PA4(IJPRIM),PB4(IJPRIM),GG(IJPRIM),GG2(IJPRIM),
     &       GG3(IJPRIM),FI0(IJPRIM),FI1(IJPRIM),FI2(IJPRIM),
     &       FI3(IJPRIM),FI4(IJPRIM),FI5(IJPRIM),FI6(IJPRIM),
     &       FI7(IJPRIM),FI8(IJPRIM),FI9(IJPRIM)
C
C---------------Common Blocks--------------------------------------
C
C---------------Local variables--------------------------------------
C
C
C---------------Executable code--------------------------------------
C
C
      IF (ABXYZ) THEN
C     -----------------
C     ONE CENTER CASE :
C     -----------------
      IJ = 0
      DO 20 II = 1, IPRIM
      DO 10 JJ = 1, JPRIM
      IJ = IJ + 1
      T (IJ) = ALPHAI (II) + ALPHAJ (JJ)
      GAB(IJ) = 1.D0 / T (IJ)
      DISTAB(IJ) = ALPHAI(II) * ALPHAJ(JJ) * DSAB * GAB(IJ)
      GAB(IJ) = GAB(IJ) * 0.25D0
      P (IJ,1) = AR (1)
      P (IJ,2) = AR (2)
      P (IJ,3) = AR (3)
   10 CONTINUE
   20 CONTINUE
C
      LSUM(1) = 0
      DO 200 KT = 1, 3
      NIJ = NI(KT) + NJ(KT) + 1
      LSKT = LSUM(KT)
      GOTO (100,110,120,130,140,150,160,170,180,185,190) NIJ
      CALL Q2JOB (4,'NIJ OUT OF RANGE IN HLFUNS',' ',0)
C.....00
  100 DO 101 IJ = 1, IJPRIM
      HL(IJ,0 + LSKT) = 1.D0
  101 CONTINUE
      GO TO 198
C.....10,01
  110 DO 111 IJ = 1, IJPRIM
      HL(IJ,0 + LSKT) = 0.D0
      HL(IJ,1 + LSKT) = GAB(IJ)
  111 CONTINUE
      GO TO 198
C.....11,20,02
  120 DO 121 IJ = 1, IJPRIM
      HL(IJ,0 + LSKT) = 2.D0 * GAB (IJ)
      HL(IJ,1 + LSKT) = 0.D0
      HL(IJ,2 + LSKT) = GAB(IJ) * GAB(IJ)
  121 CONTINUE
      GOTO198
C.....21,12,30,03
  130 DO 131 IJ = 1, IJPRIM
      GG(IJ) = GAB(IJ) * GAB (IJ)
      HL(IJ,0 + LSKT) = 0.D0
      HL(IJ,1 + LSKT) = 6.D0 * GG(IJ)
      HL(IJ,2 + LSKT) = 0.D0
      HL(IJ,3 + LSKT) = GAB(IJ) * GG(IJ)
  131 CONTINUE
      GO TO 198
C.....22,31,13,40,04
  140 DO 141 IJ = 1, IJPRIM
      GG(IJ) = GAB(IJ) * GAB(IJ)
      HL(IJ,0 + LSKT) = 12.D0 * GG(IJ)
      HL(IJ,1 + LSKT) = 0.D0
      HL(IJ,2 + LSKT) = 12.D0 * GG(IJ) * GAB(IJ)
      HL(IJ,3 + LSKT) = 0.D0
      HL(IJ,4 + LSKT) = GG(IJ) * GG(IJ)
  141 CONTINUE
      GO TO 198
C.....32,23,41,14,50,05
  150 DO 151 IJ = 1, IJPRIM
      GG(IJ) = GAB (IJ) * GAB (IJ)
      GG2(IJ)= GG(IJ) * GG(IJ)
      HL(IJ,0 + LSKT) = 0.D0
      HL(IJ,1 + LSKT) = 60.D0 * GAB(IJ) * GG (IJ)
      HL(IJ,2 + LSKT) = 0.D0
      HL(IJ,3 + LSKT) = 20.D0 * GG2 (IJ)
      HL(IJ,4 + LSKT) = 0.D0
      HL(IJ,5 + LSKT) = GAB (IJ) * GG2 (IJ)
  151 CONTINUE
      GO TO 198
C.....33,42,24,51,15
  160 DO 161 IJ = 1, IJPRIM
      GG(IJ) = GAB (IJ) * GAB (IJ)
      GG2(IJ) = GG(IJ) * GG(IJ)
      HL(IJ,0 + LSKT) = 120.D0 * GAB(IJ) * GG(IJ)
      HL(IJ,1 + LSKT) = 0.D0
      HL(IJ,2 + LSKT) = 180.D0 * GG2(IJ)
      HL(IJ,3 + LSKT) = 0.D0
      HL(IJ,4 + LSKT) = 30.D0 * GAB(IJ) * GG2(IJ)
      HL(IJ,5 + LSKT) = 0.D0
      HL(IJ,6 + LSKT) = GG(IJ) * GG2(IJ)
  161 CONTINUE
      GO TO 198
C.....43,34,52,25
  170 DO 171 IJ = 1,IJPRIM
      GG(IJ) = GAB(IJ) * GAB(IJ)
      GG2(IJ) = GG(IJ) * GG(IJ)
      HL(IJ,0 + LSKT) = 0.D0
      HL(IJ,1 + LSKT) = 840.D0 * GG2(IJ)
      HL(IJ,2 + LSKT) = 0.D0
      HL(IJ,3 + LSKT) = 420.D0 * GG2(IJ) * GAB(IJ)
      HL(IJ,4 + LSKT) = 0.D0
      HL(IJ,5 + LSKT) = 42.D0 * GG2(IJ) * GG(IJ)
      HL(IJ,6 + LSKT) = 0.D0
      HL(IJ,7 + LSKT) = GG2(IJ) * GG(IJ) * GAB(IJ)
  171 CONTINUE
      GO TO 198
C.....44,53,35
  180 DO 181 IJ = 1, IJPRIM
      GG(IJ) = GAB(IJ) * GAB(IJ)
      GG2(IJ) = GG(IJ) * GG(IJ)
      GG3(IJ) = GG2(IJ) * GG(IJ)
      HL(IJ,0 + LSKT) = 1680.D0 * GG2(IJ)
      HL(IJ,1 + LSKT) = 0.D0
      HL(IJ,2 + LSKT) = 3360.D0 * GG2(IJ) * GAB(IJ)
      HL(IJ,3 + LSKT) = 0.D0
      HL(IJ,4 + LSKT) = 840.D0 * GG3(IJ)
      HL(IJ,5 + LSKT) = 0.D0
      HL(IJ,6 + LSKT) = 56.D0 * GG3(IJ) * GAB(IJ)
      HL(IJ,7 + LSKT) = 0.D0
      HL(IJ,8 + LSKT) = GG2(IJ) * GG2(IJ)
  181 CONTINUE
      GOTO 198
C.....54,45
  185 DO 186 IJ = 1, IJPRIM
      GG(IJ) = GAB(IJ) * GAB(IJ)
      GG2(IJ) = GG(IJ) * GG(IJ)
      GG3(IJ) = GG2(IJ) * GG(IJ)
      HL(IJ,0 + LSKT) = 0.D0
      HL(IJ,1 + LSKT) = 15120.D0 * GG2(IJ) * GAB(IJ)
      HL(IJ,2 + LSKT) = 0.D0
      HL(IJ,3 + LSKT) = 10080.D0 * GG3(IJ)
      HL(IJ,4 + LSKT) = 0.D0
      HL(IJ,5 + LSKT) = 1512.D0 * GG3(IJ) * GAB(IJ)
      HL(IJ,6 + LSKT) = 0.D0
      HL(IJ,7 + LSKT) = 72.D0 * GG3(IJ) * GG(IJ)
      HL(IJ,8 + LSKT) = 0.D0
      HL(IJ,9 + LSKT) = GG3(IJ) * GG(IJ) * GAB(IJ)
  186 CONTINUE
      GOTO 198
C.....55
  190 DO 191 IJ = 1, IJPRIM
      GG(IJ) = GAB(IJ) * GAB(IJ)
      GG2(IJ) = GG(IJ) * GG(IJ)
      GG3(IJ) = GG2(IJ) * GG(IJ)
      HL(IJ,0 + LSKT) = 30240.D0 * GG2(IJ) * GAB(IJ)
      HL(IJ,1 + LSKT) = 0.D0
      HL(IJ,2 + LSKT) = 75600.D0 * GG3(IJ)
      HL(IJ,3 + LSKT) = 0.D0
      HL(IJ,4 + LSKT) = 25200.D0 * GG3(IJ) * GAB(IJ)
      HL(IJ,5 + LSKT) = 0.D0
      HL(IJ,6 + LSKT) = 2520.D0 * GG3(IJ) * GG(IJ)
      HL(IJ,7 + LSKT) = 0.D0
      HL(IJ,8 + LSKT) = 90.D0 * GG3(IJ) * GG(IJ) * GAB(IJ)
      HL(IJ,9 + LSKT) = 0.D0
      HL(IJ,10 +LSKT) = GG3(IJ) * GG2(IJ)
  191 CONTINUE
      GO TO 198
  198 CONTINUE
      LSUM(KT+1) = LSUM(KT) + NIJ
  200 CONTINUE
C     -----------------------
C     TWO DIFFERENT CENTERS :
C     -----------------------
      ELSE
      IJ = 0
      DO 220 II = 1, IPRIM
      DO 210 JJ = 1, JPRIM
      IJ = IJ + 1
      T (IJ) = ALPHAI (II) + ALPHAJ (JJ)
      GAB(IJ) = 1.D0 / T (IJ)
      DISTAB(IJ) = ALPHAI(II) * ALPHAJ(JJ) * DSAB * GAB(IJ)
      P(IJ,1) = (ALPHAI(II) * AR(1) + ALPHAJ(JJ) * BR(1)) * GAB(IJ)
      P(IJ,2) = (ALPHAI(II) * AR(2) + ALPHAJ(JJ) * BR(2)) * GAB(IJ)
      P(IJ,3) = (ALPHAI(II) * AR(3) + ALPHAJ(JJ) * BR(3)) * GAB(IJ)
      GAB(IJ) = GAB(IJ) * 0.25D0
  210 CONTINUE
  220 CONTINUE
C
      LSUM(1) = 0
      DO 400 KT = 1, 3
      LSKT = LSUM(KT)
      NIKT = NI(KT) + 1
      NJKT = NJ(KT) + 1
      NIJ = NIKT + NJKT - 1
      IF (NIKT.GE.NJKT) THEN
         DO 230 IJ = 1, IJPRIM
         PA(IJ) = P(IJ,KT) - AR(KT)
         PB(IJ) = P(IJ,KT) - BR(KT)
  230    CONTINUE
      ELSE
         DO 240 IJ = 1, IJPRIM
         PA(IJ) = P(IJ,KT) - BR(KT)
         PB(IJ) = P(IJ,KT) - AR(KT)
  240    CONTINUE
      ENDIF
C
      GO TO (290,291,292,293,294,295), MAX0(NIKT,NJKT)
      CALL Q2JOB (4,'NIKT OUT OF RANGE IN HLFUNS',' ',0)
  290 GO TO (300), MIN0(NIKT,NJKT)
  291 GO TO (310,311), MIN0(NIKT,NJKT)
  292 GO TO (320,321,322), MIN0(NIKT,NJKT)
  293 GO TO (330,331,332,333), MIN0(NIKT,NJKT)
  294 GO TO (340,341,342,343,344), MIN0(NIKT,NJKT)
  295 GO TO (350,351,352,353,354,355), MIN0(NIKT,NJKT)
      CALL Q2JOB (4,'NJKT OUT OF RANGE IN HLFUNS',' ',0)
C.....00
  300 DO 3001 IJ = 1, IJPRIM
      HL(IJ,0+LSKT) = 1.D0
 3001 CONTINUE
      GO TO 398
C.....10
  310 DO 3101 IJ = 1, IJPRIM
      HL(IJ,0+LSKT) = PA(IJ)
      HL(IJ,1+LSKT) = GAB(IJ)
 3101 CONTINUE
      GO TO 398
C.....11
  311 DO 3111 IJ = 1, IJPRIM
      HL(IJ,0+LSKT) = PA(IJ) * PB(IJ) + 2.D0 * GAB(IJ)
      HL(IJ,1+LSKT) = GAB(IJ) * (PA(IJ) + PB(IJ))
      HL(IJ,2+LSKT) = GAB(IJ) * GAB(IJ)
 3111 CONTINUE
      GO TO 398
C.....20
  320 DO 3201 IJ = 1, IJPRIM
      HL(IJ,0+LSKT) = PA(IJ) * PA(IJ) + 2.D0 * GAB(IJ)
      HL(IJ,1+LSKT) = GAB(IJ) * PA(IJ) * 2.D0
      HL(IJ,2+LSKT) = GAB(IJ) * GAB(IJ)
 3201 CONTINUE
      GO TO 398
C.....21
  321 DO 3211 IJ = 1, IJPRIM
      FI3(IJ) = 1.D0
      FI2(IJ) = PA(IJ)
      FI1(IJ) = FI2(IJ) * PA(IJ)
      FI0(IJ) = FI1(IJ) * PB(IJ)
      FI2(IJ) = 2.D0 * FI2(IJ)
      FI1(IJ) = FI1(IJ) + FI2(IJ) * PB(IJ)
      FI2(IJ) = FI2(IJ) + PB(IJ)
 3211 CONTINUE
      GO TO 360
C.....22
  322 DO 3221 IJ = 1, IJPRIM
      PB2(IJ) = PB(IJ) * PB(IJ)
      FI4(IJ) = 1.D0
      FI3(IJ) = PA(IJ)
      FI2(IJ) = FI3(IJ) * PA(IJ)
      FI1(IJ) = FI2(IJ) * PB(IJ)
      FI0(IJ) = FI1(IJ) * PB(IJ)
      FI1(IJ) = 2.D0 * (FI1(IJ) + PA(IJ) * PB2(IJ))
      FI2(IJ) = FI2(IJ) + PB2(IJ) + 4.D0 * PA(IJ) * PB(IJ)
      FI3(IJ) = 2.D0 * (FI3(IJ) + PB(IJ))
 3221 CONTINUE
      GO TO 360
C.....30
  330 DO 3301 IJ = 1, IJPRIM
      FI3(IJ) = 1.D0
      FI2(IJ) = PA(IJ)
      FI1(IJ) = FI2(IJ) * PA(IJ)
      FI0(IJ) = FI1(IJ) * PA(IJ)
      FI1(IJ) = FI1(IJ) * 3.D0
      FI2(IJ) = FI2(IJ) * 3.D0
 3301 CONTINUE
      GO TO 360
C.....31
  331 DO 3311 IJ = 1, IJPRIM
      FI4(IJ) = 1.D0
      FI3(IJ) = PA(IJ)
      FI2(IJ) = FI3(IJ) * PA(IJ)
      FI1(IJ) = FI2(IJ) * PA(IJ)
      FI0(IJ) = FI1(IJ) * PB(IJ)
      FI2(IJ) = 3.D0 * FI2(IJ)
      FI1(IJ) = FI1(IJ) + FI2(IJ) * PB(IJ)
      FI3(IJ) = FI3(IJ) * 3.D0
      FI2(IJ) = FI2(IJ) + FI3(IJ) * PB(IJ)
      FI3(IJ) = FI3(IJ) + PB(IJ)
 3311 CONTINUE
      GO TO 360
C.....32
  332 DO 3321 IJ = 1, IJPRIM
      PB2(IJ) = PB(IJ) * PB(IJ)
      FI5(IJ) = 1.D0
      FI4(IJ) = PA(IJ)
      FI3(IJ) = FI4(IJ) * PA(IJ)
      FI2(IJ) = FI3(IJ) * PA(IJ)
      FI1(IJ) = FI2(IJ) * PB(IJ)
      FI0(IJ) = FI1(IJ) * PB(IJ)
      FI1(IJ) = 2.D0 * FI1(IJ) + 3.D0 * FI3(IJ) * PB2(IJ)
      FI2(IJ) = FI2(IJ) + 6.D0*FI3(IJ)*PB(IJ) + 3.D0*PA(IJ)*PB2(IJ)
      FI3(IJ) = 3.D0*FI3(IJ) + 6.D0*PA(IJ)*PB(IJ) + PB2(IJ)
      FI4(IJ) = 3.D0*FI4(IJ) + 2.D0*PB(IJ)
 3321 CONTINUE
      GO TO 360
C.....33
  333 DO 3331 IJ = 1, IJPRIM
      PA2(IJ) = PA(IJ) * PA(IJ)
      PB2(IJ) = PB(IJ) * PB(IJ)
      FI6(IJ) = 1.D0
      FI5(IJ) = PA(IJ)
      FI4(IJ) = FI5(IJ) * PB(IJ)
      FI3(IJ) = FI4(IJ) * PA(IJ)
      FI2(IJ) = FI3(IJ) * PB(IJ)
      FI1(IJ) = FI2(IJ) * PA(IJ)
      FI0(IJ) = FI1(IJ) * PB(IJ)
      FI1(IJ) = 3.D0 * (FI1(IJ) + FI2(IJ)*PB(IJ))
      FI2(IJ) = 9.D0*FI2(IJ) + 3.D0*(FI3(IJ)*PA(IJ) + FI4(IJ)*PB2(IJ))
      FI3(IJ) = 9.D0*(FI3(IJ)+FI4(IJ)*PB(IJ))
      FI3(IJ) = FI3(IJ) + PA(IJ)*PA2(IJ) + PB(IJ)*PB2(IJ)
      FI4(IJ) = 9.D0*FI4(IJ) + 3.D0*(PA2(IJ)+PB2(IJ))
      FI5(IJ) = 3.D0 *(FI5(IJ) + PB(IJ))
 3331 CONTINUE
      GO TO 360
C.....40
  340 DO 3401 IJ = 1, IJPRIM
      FI4(IJ) = 1.D0
      FI3(IJ) = PA(IJ)
      FI2(IJ) = FI3(IJ) * PA(IJ)
      FI1(IJ) = FI2(IJ) * PA(IJ)
      FI0(IJ) = FI1(IJ) * PA(IJ)
      FI1(IJ) = FI1(IJ) * 4.D0
      FI2(IJ) = FI2(IJ) * 6.D0
      FI3(IJ) = FI3(IJ) * 4.D0
 3401 CONTINUE
      GO TO 360
C.....41
  341 DO 3411 IJ = 1, IJPRIM
      FI5(IJ) = 1.D0
      FI4(IJ) = PA(IJ)
      FI3(IJ) = FI4(IJ) * PA(IJ)
      FI2(IJ) = FI3(IJ) * PA(IJ)
      FI1(IJ) = FI2(IJ) * PA(IJ)
      FI0(IJ) = FI1(IJ) * PB(IJ)
      FI2(IJ) = 4.D0 * FI2(IJ)
      FI1(IJ) = FI1(IJ) + FI2(IJ) * PB(IJ)
      FI3(IJ) = FI3(IJ) * 6.D0
      FI2(IJ) = FI2(IJ) + FI3(IJ) * PB(IJ)
      FI4(IJ) = FI4(IJ) * 4.D0
      FI3(IJ) = FI3(IJ) + FI4(IJ) * PB(IJ)
      FI4(IJ) = FI4(IJ) + PB(IJ)
 3411 CONTINUE
      GO TO 360
C.....42
  342 DO 3421 IJ = 1, IJPRIM
      PB2(IJ) = PB(IJ) * PB(IJ)
      FI6(IJ) = 1.D0
      FI5(IJ) = PA(IJ)
      FI4(IJ) = FI5(IJ) * PA(IJ)
      FI3(IJ) = FI4(IJ) * PB(IJ)
      FI2(IJ) = FI3(IJ) * PA(IJ)
      FI1(IJ) = FI2(IJ) * PA(IJ)
      FI0(IJ) = FI1(IJ) * PB(IJ)
      FI1(IJ) = 2.D0 * FI1(IJ) + 4.D0 * FI2(IJ) * PB(IJ)
      FI2(IJ) = 8.D0 * FI2(IJ) + 6.D0 * FI3(IJ) * PB(IJ) +
     &          FI4(IJ) * FI4(IJ)
      FI3(IJ) = 12.D0 * FI3(IJ) +
     &          4.D0 * (FI4(IJ) * PA(IJ) + FI5(IJ) * PB2(IJ))
      FI4(IJ) = 6.D0 * FI4(IJ) + 8.D0 * PA(IJ) * PB(IJ) +
     &          PB2(IJ)
      FI5(IJ) = 4.D0 * FI5(IJ) + 2.D0 * PB(IJ)
 3421 CONTINUE
      GO TO 360
C.....43
  343 DO 3431 IJ = 1, IJPRIM
      PA2(IJ) = PA(IJ) * PA(IJ)
      PB2(IJ) = PB(IJ) * PB(IJ)
      FI7(IJ) = 1.D0
      FI6(IJ) = PA(IJ)
      FI5(IJ) = FI6(IJ) * PA(IJ)
      FI4(IJ) = FI5(IJ) * PB(IJ)
      FI3(IJ) = FI4(IJ) * PA(IJ)
      FI2(IJ) = FI3(IJ) * PB(IJ)
      FI1(IJ) = FI2(IJ) * PA(IJ)
      FI0(IJ) = FI1(IJ) * PB(IJ)
      FI1(IJ) = 3.D0 * FI1(IJ) + 4.D0 * FI2(IJ) * PB(IJ)
      FI2(IJ) = 12.D0 * FI2(IJ) + 3.D0 * FI3(IJ) * PA(IJ) +
     &          6.D0 * FI4(IJ) * PB2(IJ)
      FI6(IJ) = 4.D0 * FI6(IJ)
      FI3(IJ) = 12.D0 * FI3(IJ) + PA2(IJ)*(FI5(IJ) + 18.D0 * PB2(IJ))
     &        +  FI6(IJ) * PB(IJ) * PB2(IJ)
      FI4(IJ) = 18.D0 * FI4(IJ) + PA(IJ)*(4.D0*PA2(IJ)+12.D0*PB2(IJ)) +
     &          PB2(IJ) * PB(IJ)
      FI5(IJ) = 6.D0 * FI5(IJ) + 12.D0*PA(IJ)*PB(IJ) + 3.D0*PB2(IJ)
      FI6(IJ) = FI6(IJ) + 3.D0 * PB(IJ)
 3431 CONTINUE
      GO TO 360
C.....44
  344 DO 3441 IJ = 1, IJPRIM
      PA2(IJ) = PA(IJ) * PA(IJ)
      PB2(IJ) = PB(IJ) * PB(IJ)
      FI8(IJ) = 1.D0
      FI7(IJ) = PA(IJ)
      FI6(IJ) = FI7(IJ) * PB(IJ)
      FI5(IJ) = FI6(IJ) * PA(IJ)
      FI4(IJ) = FI5(IJ) * PB(IJ)
      FI3(IJ) = FI4(IJ) * PA(IJ)
      FI2(IJ) = FI3(IJ) * PB(IJ)
      FI1(IJ) = FI2(IJ) * PA(IJ)
      FI0(IJ) = FI1(IJ) * PB(IJ)
      FI1(IJ) = 4.D0 * (FI1(IJ) + FI2(IJ) * PB(IJ))
      FI2(IJ) = 16.D0*FI2(IJ) + 6.D0*FI4(IJ)*(PA2(IJ) + PB2(IJ))
      FI3(IJ) = 24.D0*(FI3(IJ)+FI4(IJ)*PB(IJ)) +
     &          4.D0*(FI5(IJ)*PA2(IJ)+FI6(IJ)*PB2(IJ)*PB(IJ))
      FI4(IJ) = 36.D0*FI4(IJ) + 16.D0*(FI5(IJ)*PA(IJ)+FI6(IJ)*PB2(IJ))
     &          + PA2(IJ)*PA2(IJ) + PB2(IJ)*PB2(IJ)
      FI5(IJ) = 24.D0*(FI5(IJ)+FI6(IJ)*PB(IJ)) +
     &          4.D0*(PA(IJ)*PA2(IJ)+PB(IJ)*PB2(IJ))
      FI6(IJ) = 16.D0 * FI6(IJ) + 6.D0 * (PA2(IJ) + PB2(IJ))
      FI7(IJ) = 4.D0 * (PA(IJ) + PB(IJ))
 3441 CONTINUE
      GO TO 360
C.....50
  350 DO 3501 IJ = 1, IJPRIM
      FI5(IJ) = 1.D0
      FI4(IJ) = PA(IJ)
      FI3(IJ) = FI4(IJ) * PA(IJ)
      FI2(IJ) = FI3(IJ) * PA(IJ)
      FI1(IJ) = FI2(IJ) * PA(IJ)
      FI0(IJ) = FI1(IJ) * PA(IJ) 
      FI4(IJ) = FI4(IJ) * 5.D0
      FI3(IJ) = FI3(IJ) * 10.D0
      FI2(IJ) = FI2(IJ) * 10.D0
      FI1(IJ) = FI1(IJ) * 5.D0
 3501 CONTINUE
      GO TO 360
C.....51
  351 DO 3511 IJ = 1, IJPRIM
      FI6(IJ) = 1.D0
      FI5(IJ) = PA(IJ)
      FI4(IJ) = FI5(IJ) * PA(IJ)
      FI3(IJ) = FI4(IJ) * PA(IJ)
      FI2(IJ) = FI3(IJ) * PA(IJ)
      FI1(IJ) = FI2(IJ) * PA(IJ)
      FI0(IJ) = FI1(IJ) * PB(IJ)
      FI2(IJ) = FI2(IJ) * 5.D0
      FI1(IJ) = FI1(IJ) + FI2(IJ) * PB(IJ)
      FI3(IJ) = FI3(IJ) * 10.D0
      FI2(IJ) = FI2(IJ) + FI3(IJ) * PB(IJ)
      FI4(IJ) = FI4(IJ) * 10.D0 
      FI3(IJ) = FI3(IJ) + FI4(IJ) * PB(IJ)
      FI5(IJ) = FI5(IJ) * 5.D0
      FI4(IJ) = FI4(IJ) + FI5(IJ) * PB(IJ)
      FI5(IJ) = FI5(IJ) + PB(IJ)
 3511 CONTINUE
      GO TO 360
C.....52
  352 DO 3521 IJ = 1, IJPRIM
      PB2(IJ) = PB(IJ) * PB(IJ)
      FI7(IJ) = 1.D0
      FI6(IJ) = PA(IJ)
      FI5(IJ) = FI6(IJ) * PA(IJ)
      FI4(IJ) = FI5(IJ) * PB(IJ)
      FI3(IJ) = FI4(IJ) * PA(IJ)
      FI2(IJ) = FI3(IJ) * PA(IJ)
      FI1(IJ) = FI2(IJ) * PA(IJ)
      FI0(IJ) = FI1(IJ) * PB(IJ)
      FI1(IJ) = 2.D0*FI1(IJ) + 5.D0*FI2(IJ)*PB(IJ)
      FI2(IJ) = 10.D0*FI2(IJ) + PA(IJ)*FI5(IJ)*FI5(IJ) +
     &          10.D0*FI3(IJ)*PB(IJ)
      FI3(IJ) = 20.D0*FI3(IJ) + 5.D0*FI5(IJ)*FI5(IJ) + 
     &          10.D0*FI4(IJ)*PB(IJ)
      FI5(IJ) = 10.D0*FI5(IJ)
      FI6(IJ) = 5.D0*FI6(IJ)
      FI4(IJ) = 20.D0*FI4(IJ) + FI5(IJ)*PA(IJ) + FI6(IJ)*PB2(IJ)
      FI5(IJ) = FI5(IJ) + 2.D0*FI6(IJ)*PB(IJ) + PB2(IJ)
      FI6(IJ) = FI6(IJ) + 2.D0*PB(IJ)
 3521 CONTINUE
      GO TO 360
C.....53
  353 DO 3531 IJ = 1, IJPRIM
      PA2(IJ) = PA(IJ) * PA(IJ)
      PB2(IJ) = PB(IJ) * PB(IJ)
      FI8(IJ) = 1.D0
      FI7(IJ) = PA(IJ)
      FI6(IJ) = FI7(IJ) * PB(IJ) 
      FI5(IJ) = FI6(IJ) * PA(IJ)
      FI4(IJ) = FI5(IJ) * PA(IJ)
      FI3(IJ) = FI4(IJ) * PB(IJ)
      FI2(IJ) = FI3(IJ) * PA(IJ)
      FI1(IJ) = FI2(IJ) * PA(IJ)
      FI0(IJ) = FI1(IJ) * PB(IJ)
      FI1(IJ) = 3.D0*FI1(IJ) + 5.D0*FI2(IJ)*PB(IJ)
      FI2(IJ) = 15.D0*FI2(IJ) + 10.D0*FI3(IJ)*PB(IJ) +
     &          3.D0*FI4(IJ)*PA2(IJ)
      FI3(IJ) = 30.D0*FI3(IJ) + PA2(IJ)*PA2(IJ)*PA(IJ) + 
     &          10.D0*FI5(IJ)*PB2(IJ) + 15.D0*FI4(IJ)*PA(IJ)
      FI4(IJ) = 30.D0*FI4(IJ) + 30.D0*PB(IJ)*FI5(IJ)+
     &          5.D0*PA2(IJ)*PA2(IJ) + 5.D0*FI6(IJ)*PB2(IJ)
      FI5(IJ) = 30.D0*FI5(IJ) + 15.D0*FI6(IJ)*PB(IJ) +
     &          10.D0*PA2(IJ)*PA(IJ) + PB2(IJ)*PB(IJ)
      FI6(IJ) = 15.D0*FI6(IJ) + 10.D0*PA2(IJ) + 3.D0*PB2(IJ)
      FI7(IJ) = 5.D0*FI7(IJ) + 3.D0*PB(IJ)
 3531 CONTINUE
      GO TO 360
C.....54
  354 DO 3541 IJ = 1, IJPRIM
      PA2(IJ) = PA(IJ) * PA(IJ)
      PB2(IJ) = PB(IJ) * PB(IJ)
      FI9(IJ) = 1.D0
      FI8(IJ) = PA(IJ)
      FI7(IJ) = FI8(IJ) * PA(IJ)
      FI6(IJ) = FI7(IJ) * PB(IJ)
      FI5(IJ) = FI6(IJ) * PA(IJ)
      FI4(IJ) = FI5(IJ) * PB(IJ)
      FI3(IJ) = FI4(IJ) * PA(IJ)
      FI2(IJ) = FI3(IJ) * PB(IJ)
      FI1(IJ) = FI2(IJ) * PA(IJ)
      FI0(IJ) = FI1(IJ) * PB(IJ)
      FI1(IJ) = 4.D0*FI1(IJ) + 5.D0*FI2(IJ)*PB(IJ)
      FI2(IJ) = 20.D0*FI2(IJ) + FI4(IJ)*(6.D0*PA2(IJ)+10.D0*PB2(IJ))
      FI3(IJ) = 30.D0*FI3(IJ) + 4.D0*FI5(IJ)*PA2(IJ)+
     &          40.D0*FI4(IJ)*PB(IJ) + 10.D0*FI7(IJ)*PB2(IJ)*PB2(IJ)
      FI4(IJ) = 60.D0*FI4(IJ)+FI6(IJ)*(20.D0*PA2(IJ)+40.D0*PB2(IJ))+
     &          PA2(IJ)*PA2(IJ)*PA(IJ) + 5.D0*PB2(IJ)*PB2(IJ)*PA(IJ)
      FI5(IJ) = 40.D0*FI5(IJ)+5.D0*PA2(IJ)*PA2(IJ)+60.D0*FI6(IJ)*PB(IJ)
     &          +20.D0*PA(IJ)*PB2(IJ)*PB(IJ) + PB2(IJ)*PB2(IJ)
      FI6(IJ) = 40.D0*FI6(IJ) + 10.D0*PA2(IJ)*PA(IJ) +
     &          30.D0*PA(IJ)*PB2(IJ) + 4.D0*PB2(IJ)*PB(IJ)
      FI7(IJ) = 10.D0*FI7(IJ) + 20.D0*PA(IJ)*PB(IJ) + 6.D0*PB2(IJ)
      FI8(IJ) = 5.D0*FI8(IJ) + 4.D0*PB(IJ)
 3541 CONTINUE
      GOTO 360
C.....55
  355 DO 3551 IJ = 1, IJPRIM
      PA2(IJ) = PA(IJ) * PA(IJ)
      PB2(IJ) = PB(IJ) * PB(IJ)
      PA4(IJ) = PA2(IJ) * PA2(IJ)
      PB4(IJ) = PB2(IJ) * PB2(IJ)
      FI9(IJ) = PA(IJ)
      FI8(IJ) = FI9(IJ) * PB(IJ)
      FI7(IJ) = FI8(IJ) * PA(IJ)
      FI6(IJ) = FI7(IJ) * PB(IJ)
      FI5(IJ) = FI6(IJ) * PA(IJ)
      FI4(IJ) = FI5(IJ) * PB(IJ)
      FI3(IJ) = FI4(IJ) * PA(IJ)
      FI2(IJ) = FI3(IJ) * PB(IJ)
      FI1(IJ) = FI2(IJ) * PA(IJ)
      FI0(IJ) = FI1(IJ) * PB(IJ)
      FI1(IJ) = 5.D0*(FI1(IJ)+FI2(IJ)*PB(IJ))
      FI2(IJ) = 25.D0*FI2(IJ) + 10.D0*FI3(IJ)*PA(IJ) +
     &          10.D0*FI4(IJ)*PB2(IJ)
      FI3(IJ) = 50.D0*FI3(IJ)+10.D0*FI5(IJ)*(PA2(IJ)+5.D0*PB2(IJ))+
     &          10.D0*FI7(IJ)*PB4(IJ)
      FI4(IJ) = 100.D0*FI4(IJ) + 5.D0*FI8(IJ)*(PA4(IJ)+PB4(IJ)) +
     &          50.D0*FI6(IJ)*(PA2(IJ)+PB2(IJ))
      FI5(IJ) = 100.D0*(FI5(IJ)+FI6(IJ)*PB(IJ)) + PA4(IJ)*PA(IJ) +
     &          25.D0*(FI7(IJ)*PA2(IJ)+PA(IJ)*PB4(IJ)) +
     &          PB4(IJ)*PB(IJ)
      FI6(IJ) = 100.D0*FI6(IJ) + 5.D0*(PA4(IJ)+PB4(IJ)) +
     &          50.D0*FI8(IJ)*(PA2(IJ)+PB2(IJ))
      FI7(IJ) = 50.D0*(FI7(IJ)+FI8(IJ)*PB(IJ)) +
     &          10.D0*(PA2(IJ)*PA(IJ)+PB2(IJ)*PB(IJ))
      FI8(IJ) = 25.D0*FI8(IJ) + 10.D0*(PA2(IJ)+PB2(IJ))
      FI9(IJ) = 5.D0*(FI9(IJ)+PB(IJ))
 3551 CONTINUE
  360 CONTINUE
      DO 370 IJ = 1, IJPRIM
      HL(IJ,0 + LSKT) = FI0(IJ)
  370 CONTINUE
      IF (NIJ-1) 398,398,371
  371 DO 3711 IJ = 1, IJPRIM
      HL(IJ,1 + LSKT) = FI1(IJ) * GAB(IJ)
 3711 CONTINUE
      IF (NIJ-2) 398,398,372
  372 DO 3721 IJ = 1, IJPRIM
      GG (IJ) = GAB(IJ) * GAB(IJ)
 3721 CONTINUE
      DO 3722 IJ = 1, IJPRIM
      HL(IJ,0 + LSKT) = HL(IJ,0 + LSKT) + FI2(IJ) * GAB(IJ) * 2.D0
      HL(IJ,2 + LSKT) = FI2(IJ) * GG(IJ)
 3722 CONTINUE
      IF (NIJ-3) 398,398,373
  373 DO 3731 IJ = 1, IJPRIM
      HL(IJ,1 + LSKT) = HL(IJ,1 + LSKT) + FI3(IJ) * GG(IJ) * 6.D0
      HL(IJ,3 + LSKT) = FI3(IJ) * GG(IJ) * GAB(IJ)
 3731 CONTINUE
      IF (NIJ-4) 398,398,374
  374 DO 3741 IJ = 1, IJPRIM
      GG2(IJ) = GG(IJ) * GG (IJ)
 3741 CONTINUE
      DO 3742 IJ = 1, IJPRIM
      HL(IJ,0 + LSKT) = HL(IJ,0 + LSKT) + FI4(IJ) * GG(IJ) * 12.D0
      HL(IJ,2 + LSKT) = HL(IJ,2 + LSKT) + FI4(IJ)*GAB(IJ)*GG(IJ)*12.D0
      HL(IJ,4 + LSKT) = FI4(IJ) * GG2(IJ)
 3742 CONTINUE
      IF (NIJ-5) 398,398,375
  375 DO 3751 IJ = 1, IJPRIM
      HL(IJ,1+LSKT) = HL(IJ,1+LSKT) + FI5(IJ)*GG(IJ)*GAB(IJ)*60.D0
      HL(IJ,3+LSKT) = HL(IJ,3+LSKT) + FI5(IJ)*GG2(IJ)*20.D0
      HL(IJ,5+LSKT) = FI5(IJ) * GG2(IJ) * GAB(IJ)
 3751 CONTINUE
      IF (NIJ-6) 398,398,376
  376 DO 3761 IJ = 1, IJPRIM
      HL(IJ,0+LSKT) = HL(IJ,0+LSKT) + FI6(IJ)*GG(IJ)*GAB(IJ)*120.D0
      HL(IJ,2+LSKT) = HL(IJ,2+LSKT) + FI6(IJ)*GG2(IJ)*180.D0
      HL(IJ,4+LSKT) = HL(IJ,4+LSKT) + FI6(IJ)*GG2(IJ)*GAB(IJ)*30.D0
      HL(IJ,6+LSKT) = FI6(IJ) * GG2(IJ) * GG(IJ)
 3761 CONTINUE
      IF (NIJ-7) 398,398,377
  377 DO 3771 IJ = 1, IJPRIM
      GG3(IJ) = GG2(IJ) * GG(IJ)
 3771 CONTINUE
      DO 3772 IJ = 1, IJPRIM
      HL(IJ,1+LSKT) = HL(IJ,1+LSKT) + FI7(IJ)*GG2(IJ)*840.D0
      HL(IJ,3+LSKT) = HL(IJ,3+LSKT) + FI7(IJ)*GG2(IJ)*GAB(IJ)*420.D0
      HL(IJ,5+LSKT) = HL(IJ,5+LSKT) + FI7(IJ)*GG3(IJ)*42.D0
      HL(IJ,7+LSKT) = FI7(IJ)*GG3(IJ)*GAB(IJ)
 3772 CONTINUE
      IF (NIJ-8) 398,398,378
  378 DO 3781 IJ = 1, IJPRIM
      HL(IJ,0+LSKT) = HL(IJ,0+LSKT) + FI8(IJ)*GG2(IJ)*1680.D0
      HL(IJ,2+LSKT) = HL(IJ,2+LSKT) + FI8(IJ)*GG2(IJ)*GAB(IJ)*3360.D0
      HL(IJ,4+LSKT) = HL(IJ,4+LSKT) + FI8(IJ)*GG3(IJ)*840.D0
      HL(IJ,6+LSKT) = HL(IJ,6+LSKT) + FI8(IJ)*GG3(IJ)*GAB(IJ)*56.D0
      HL(IJ,8+LSKT) = FI8(IJ)*GG3(IJ)*GG(IJ)
 3781 CONTINUE
      IF (NIJ-9) 398,398,379
  379 DO 3791 IJ = 1, IJPRIM
      HL(IJ,1+LSKT) = HL(IJ,1+LSKT) + FI9(IJ)*15120.D0*GG2(IJ)*GAB(IJ)
      HL(IJ,3+LSKT) = HL(IJ,3+LSKT) + FI9(IJ)*10080.D0*GG3(IJ)
      HL(IJ,5+LSKT) = HL(IJ,5+LSKT) + FI9(IJ)*1512.D0*GG3(IJ)*GAB(IJ)
      HL(IJ,7+LSKT) = HL(IJ,7+LSKT) + FI9(IJ)*72.D0*GG3(IJ)*GG(IJ)
      HL(IJ,9+LSKT) = GG3(IJ)*GG(IJ)*GAB(IJ)
 3791 CONTINUE
      IF (NIJ-10) 398,398,380
  380 DO 3801 IJ = 1, IJPRIM
      HL(IJ,0+LSKT) = HL(IJ,0+LSKT) + 30240.D0*GG2(IJ)*GAB(IJ)
      HL(IJ,2+LSKT) = HL(IJ,2+LSKT) + 75600.D0*GG3(IJ)
      HL(IJ,4+LSKT) = HL(IJ,4+LSKT) + 25200.D0*GG3(IJ)*GAB(IJ)
      HL(IJ,6+LSKT) = HL(IJ,6+LSKT) + 2520.D0*GG3(IJ)*GG(IJ)
      HL(IJ,8+LSKT) = HL(IJ,8+LSKT) + 90.D0*GG3(IJ)*GG(IJ)*GAB(IJ)
      HL(IJ,10+LSKT)= GG3(IJ)*GG2(IJ)
 3801 CONTINUE
C
  398 CONTINUE
      LSUM(KT+1) = LSUM(KT) + NIJ
  400 CONTINUE
      ENDIF
      RETURN
      END
