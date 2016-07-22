      Subroutine CpuUsed (Sec)
C
      Real*8 Sec
#if defined (DEC)
      Real*8 Runsec
#endif
C
#if defined (CRAY)
      Sec = Second()
#endif
#if defined (HP9) || defined (SUN) || defined (DEC) || defined (LINUX)
      Sec = RUNSEC()
#endif
#if defined (IBM) || defined (SGI)
      Sec = MCLOCK()/100.D0
#endif
C
      Return
      End
C
      Real*8 Function CpuDelta()
C
      Real*8 Sec1,Sec0
      Common/CPU/Sec0,Sec1
      Data Sec0 /0.d0/
C
      Call CPUused(Sec1)
      CPUdelta = Sec1 - Sec0
      Sec0 = Sec1
C
      Return
      End

