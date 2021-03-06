C
C     DEFINITION PARAMETERS RELTWEL:
C
C     N1  NUMBER OF NUCLEI
C     N2  BUFFER-LENGTH FOR TEMPORARY INTEGRAL AND LABEL STORAGE
C     N3  NUMBER OF SINGLE GAUSSIANS PER CONTRACTED FUNCTION (MAXIMUM 32)
C     N5  NUMBER OF DIFFERENT L-VALUES FOR BASISFUNCTIONS
C     N6  NUMBER OF DIFFERENT L-VALUES TIMES MULTIPLICITY OF L
C     N7  SUM OVER UNIQUE NUCLEI TIMES NUMBER OF DIFF.FUNC. TYPES
C     N14 LENGTH OF LIST OF CONTRACTED BASISFUNCTIONS
C     N15 LENGTH OF LIST OF EXPONENTS AND COEFFICIENS
C     LMAX 4*L+1 WHERE L IS THE MAXIMUM L-VALUE FOR WHICH TWO-ELEC-
C         TRON INTEGRAL ROUTINES ARE AVAILABLE (CHANGE WITH CARE !!)
C     NCOMB     NUMBER OF ACTUAL FUNCTION-TYPE/ATOMIC-CENTER COMBINATIONS
C     NQPLE     NUMBER OF ACTUALLY OCCURRING UNIQUE INDCOM QUADRUPLES
C     NIBTR     LENGTH OF FUNCTION/INDCOM TRANSLATION LIST
C     IPQINV(2,N14) INVERSE ARRAY  TO GET INDCOM RANGE FOR BASFC LABEL
C     NPQBUF   MAXIMUM NUMBER OF BUCKETS CONTAINING THE INTS/LABELS
C     NPQSIZ   SIZE OF ONE BUCKET
C
      PARAMETER (N1=100,N2=12288,N3=45)
      PARAMETER (N5=6,N6=N5*(N5+1)*(N5+2)/6)
      PARAMETER (N7=550,N8=20000,N9=2*N7,N10=40000,N12=4000)
      PARAMETER (N14=700,N15=15500)
      PARAMETER (ACC=1.D-14)
      PARAMETER (JMAX=26)
      PARAMETER (NPQBUF=25000,NPQSIZ=256)
C
C     DEFINITION COMMON BLOCKS :
C
      COMMON/GEOM/XCORD(N1),YCORD(N1),ZCORD(N1)
      COMMON/BAS/INDCOM(N7+1),INDBAS(N14+1),INDSYM(N14+1),INDORB(N12+1),
     $KCOMB(N14),NBSYM1(16),NBSYM2(16),NEM(N14),NCORE(N14,2),
     $NBSYM(17),ICENT(N7),ITYPE(N7),ICOMOP(N7,48),INDTWO(16)
      COMMON/BAS2/ KCBINV(N7,2)
      COMMON/NLM/NX(N6),NY(N6),NZ(N6)
      COMMON/OEC/ALPHA(N15),COEFF(N15)
      COMMON/GAMINC/XD(0:JMAX),XU(0:JMAX),STR(0:120,0:JMAX)
      COMMON/CHAR/NAME,T,D,VERSION,FN(8)
      COMMON/REST/ICR,ICMX,IR,KR,LR,NR,IBLK,ICORE
      COMMON/CON/PI,PIROOT,TWOPI,PITERM,DNORM,ENORM(4)
      COMMON/CNT/NEQ1,NEQUIV,NSYMOP,NNUCL
      COMMON/FGAM/FACT(0:6)
      COMMON/OPTNS/TWOC,COUNT,ONECEN
      COMMON/SYS/UN(8),ID
      COMMON/NEWPAR/NEQCO1,NEQCOM,NCOMB,NQPLE,NIBTR
C
      LOGICAL TWOC,COUNT,ONECEN
      INTEGER UN
      CHARACTER*8 T,D*10,VERSION,NAME*80,FN*10
