/*
 * $Id: fitsdefs.h,v 1.1 1996/06/13 14:02:29 oneel Exp $
 *
 *     These are the FITSIO routines
 */

#if defined(__cplusplus)
extern "C"
{
#endif

  void fcadef (int, int, int, const int*, const char* const x[], int, int*);
  void fcbdef (int, int, const char* const x[], int, int, int*);
  void fcgcvs (int, int, int, int, int, const char*, char**, int*, int*) ;
  void fcgcl (int, int, int, int, int, int*, int*) ;
  void fcgcvd (int, int, int, int, int, double, double*, int*, int*) ;
  void fcclos (int, int*);
  void fccmsg (void) ;
  void fccrhd (int, int*);
  void fcddef (int, int, int*);
  void fcgerr (int, char*);
  void fcgkyj (int, char*, int*, char*, int*);
  void fcgmsg (char*);
  void fcgtbs (int, int, int, int, char*, int*);
  void fcinit (int, char*, int,int*);
  void fcirow (int, int, int, int*);
  void fcmahd (int, int, int*, int*);
  void fcmkyd (int, char*, double, int, char*, int*);
  void fcmkyj (int, char*, int, char*, int*);
  void fcmkyl (int, char*, int, char*, int*);
  void fcmkys (int, char*, char*, char*, int*);
  void fcmrhd (int, int, int*, int*);
  void fcopen (int, char*, int, int*, int*);
  void fcpcls (int, int, int, int, int, char**, int*) ;
  void fcpclj (int, int, int, int, int, int*, int*) ;
  void fcpcll (int, int, int, int, int, int*, int*) ;
  void fcpcld (int, int, int, int, int, const double*, int*) ;
  void fcpcom (int, const char*, int*);
  void fcpdat (int, int*);
  void fcpdef (int, int, int, int*, int, int, int*);
  void fcphpr (int, int, int, int, int*, int, int, int, int*);
  void fcphtb (int, int, int, int, const char* const x[], const int*,
	       const char* const y[], const char* const z[], const char*, int*);
  void fcphbn (int, int, int, const char* const x[], const char* const y[],
	       const char* const z[], const char*, int, int*);
  void fcpkyd (int, char*, double, int, char*, int*);
  void fcpkyf (int, char*, double, int, char*, int*);
  void fcpkyj (int, char*, int, char*, int*);
  void fcpkyl (int, char*, int, char*, int*);
  void fcpkys (int, char*, char*, char*, int*);
  void fcptbs (int, int, int, int, char*, int*);

#if defined(__cplusplus)
}
#endif

int prtfiostat (int) ;
int prtfiomsg () ;
