#if defined (IBM) || defined (IBMSP2)
void fmalloc(int*icore,int *iptr)
{
  *iptr=malloc(*icore);
}
#endif
#if defined (LINUX)
void fmalloc_(int*icore,int *iptr)
{
  *iptr=malloc(*icore);
}
#endif

