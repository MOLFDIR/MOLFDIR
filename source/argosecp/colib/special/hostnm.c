/*
  Fortran interface to the standard C gethostname()/uname() function.
  15-may-92 POSIX code courtesy of roger edberg, anusf.
  27-apr-92 -ron shepard

  Usage:

       integer ierr
       character*LEN hostname ! returned value, truncated if necessary.
       integer  hostnm  ! external function.
       external hostnm

       ierr = hostnm( hostname )
       if ( ierr .eq. 0 ) then
          ! hostname contains the host name.
       else
          ! hostname is undefined.
       endif

  This function is required in the COLUMBUS Program System for the
  following machines:
  IBM RS6000
  Fujitsu VP2000

*/

#include "extsymbols.h"  /* Define the external symbol: */

#include <sys/param.h>

/* If UNAME is defined, then uname() function will be used below.
   If UNAME is undefined, then gethostname() will be used. 
   Some machines support only one convention; most support both. */

#if defined(_POSIX_SOURCE) || defined(__uxp__) || (SUN)
#define UNAME 
#endif

#ifdef UNAME
#include <sys/utsname.h>
#endif

#if defined(CRAY)

/* fortran character strings are passed by descriptor */
#include <fortran.h>
int EXTERNAL_HOSTNM( fdesc )
_fcd fdesc ; /* fortran character descriptor */

#else

/* fortran string and length are expanded automatically */
int EXTERNAL_HOSTNM( fstring, flength )
char *fstring ; /* string address */
int  flength  ; /* string length  */

#endif /* _CRAY */
{
  char *name ;  /* pointer to the fortran string */
  int lenname ; /* fortran-generated length value */
#if defined (INT64)
  long i,ierr;
#else
  int i, ierr ;
#endif 

#ifdef UNAME
  struct utsname xlocal ; /* local structure containing the C string */
#else
  char localname[MAXHOSTNAMELEN] ; /* local C string */
#endif

  /* localize the string address and argument length */

#if defined(CRAY)
  /* _fcdtocp() and _fcdlen() are macros that extract the string address
     bits and string length respectively from fdesc */
  name    = _fcdtocp( fdesc ) ;
  lenname = _fcdlen( fdesc ) ;
#else
  name    = fstring ;
  lenname = flength ;
#endif /* _CRAY */
    
  /* initialize the return string */
  for (i = 0; i < lenname; i++ ) 
    *(name + i) = ' ' ;

#ifdef UNAME

  ierr = uname( &xlocal ) ;

  if ( ierr >= 0 ) {
    /* uname() call was ok; reset ierr for return */
    ierr = 0 ;
    /* copy the result, truncate if necessary */
    i = 0;
    while ( ( i < lenname ) && ( xlocal.nodename[i] != '\0' ) )
      {
	*(name + i) = xlocal.nodename[i];
	i++ ;
      }
  }

#else

  ierr = gethostname( localname, MAXHOSTNAMELEN ) ;

  if ( ierr == 0 ) {
    /* gethostname() call was ok */
    /* copy the result, truncate if necessary */
    i = 0 ;
    while( (i < lenname) && (localname[i] != '\0') ) {
      *(name + i) = localname[i] ;
      i++ ;
    }
  }

#endif /* UNAME */

  return( ierr ) ;
}
