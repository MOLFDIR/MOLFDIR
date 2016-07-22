/* 04-May-92 written by Ron Shepard */

/* Define the external symbols.  These must be defined so that */
/* the Fortran interface is portable.  This machine-dependency */
/* is localized in this file in order to simplify the C code.  */


/* #elif is not recognized by all C preprocessors, so punt. -rls */

#if defined(CRAY) || defined(ardent)

  /* Fortran symbols are converted to upper case with no trailing "_". */
#define EXTERNAL_FALLOC FALLOC
#define EXTERNAL_FDATE  FDATE
#define EXTERNAL_HOSTNM HOSTNM
#define EXTERNAL_FWTIME FWTIME

#else

#if !(defined(SUN) || defined (SGI) || defined (LINUX))
  /* AIX requires underscores if compiled with -qextname */ 
  /* Fortran symbols are converted to lower case with no trailing "_". */
#define EXTERNAL_FALLOC falloc
#define EXTERNAL_FDATE  fdate
#define EXTERNAL_HOSTNM hostnm
#define EXTERNAL_FWTIME fwtime
#define EXTERNAL_FLUSHSTDOUT flushstdout

#else
  /* AIX requires underscores if compiled with -qextname */ 
  /* Fortran symbols are converted to lower case with no trailing "_". */
#define EXTERNAL_FALLOC falloc_
#define EXTERNAL_FDATE  fdate_
#define EXTERNAL_HOSTNM hostnm_
#define EXTERNAL_FWTIME fwtime_
#define EXTERNAL_FLUSHSTDOUT flushstdout_

#endif /*   */
#endif /* _CRAY */
