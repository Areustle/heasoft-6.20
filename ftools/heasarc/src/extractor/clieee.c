/* Clear the ieee flags which select_region may or may not of set on the sun */

#if defined(vms)
#define clieee_ clieee
#endif

clieee_()
{

#ifdef SUN
  char *out;

  ieee_flags("clear","exception","inexact",&out);
  ieee_flags("clear","exception","underflow",&out);

#endif

}
