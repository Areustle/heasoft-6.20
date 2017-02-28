/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 22:50:06 1999 by E. Miyata*/
/* add some declarations of function
 * Jul  21 2005 by H. Nakajima*/
int bnk2fits_event (fitsfile *fitsd, COLUMN_INF *column_inf,
		    int column_num, long icol, int maxsize);
