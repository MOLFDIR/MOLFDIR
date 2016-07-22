/*
  Fortran interface to the standard C time() function.
  27-apr-92 -ron shepard

  Usage:

      real*8   secnds
      real*8   fwtime ! external function.
      external fwtime 

      secnds = fwtime() ! assigns seconds since the first call.

  This function is required in the COLUMBUS Program System for the
  following machines:
  IBM RS6000

*/

#include "extsymbols.h"  /* Define the external symbol: */

#include <time.h>

#ifndef NULL
#define NULL    0
#endif

double EXTERNAL_FWTIME()
{
  static time_t time0 ;     /* internal reference time. */
  static int    first = 0 ; /* internal initialization flag */
  double        tdiff ;     /* returned value, wall-clock time, usually
			       accurate to 1 sec. */

  if ( first == 0 ) {
    /* execute the first time only */
    time0 = time( (time_t *)NULL ) ;
    tdiff = 0. ;
    first = 1 ;
  } else {
    tdiff = (double) (time( (time_t *)NULL ) - time0 ) ;
  }
  return( tdiff ) ;
}
