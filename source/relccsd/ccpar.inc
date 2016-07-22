C
C VERSION : $Revision: 1.1 $
C DATE    : $Date: 2001/09/11 12:57:32 $
C FILE    : inpt.inc
C SOURCE  : $Source: /home/tc/visscher/sources/Molfdir/Molfdir/source/relccsd/ccpar.inc,v $
C
C MPI information for RELCCSD
C
C     General MPI information
C
      COMMON/CCPAR/ MASTER,MYPROC,NMPROC
C
C     MASTER: contains node number of master (0 in general)
C     MYPROC:  -"-     own process number. According to MPI
C             counting starts at zero.....# proc -1 !
C     NMPROC:  -"-     total number of processes (including master !)
C

C Information for integral distribution
C
C     Distribution of two-electron integrals
C     First index : start, stop of block on node
C     Second index : integral type (VOVV,VVVV)
C     Third index : irrep
C
      COMMON/CCPARV/ NV6PAR,NV5PAR,IDIST(6,10,32)
