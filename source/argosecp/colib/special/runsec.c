#define _HPUX_SOURCE
#include <sys/times.h>
#include <unistd.h>

double runsec()

{
  double cpu;
  double cpuu;
  double cpus;
  double ticks;
  struct tms first;
  times(&first);
  ticks = sysconf(_SC_CLK_TCK);
  cpuu = first.tms_utime;
  cpus = first.tms_stime;
  cpu = (cpuu + cpus) / ticks;
  return cpu;
}
