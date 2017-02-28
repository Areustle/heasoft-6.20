/* parse.c - Parsing and string functions for ftimgcalc */

#include <fitsio.h>
#include <ctype.h>
#include <string.h>

#include "ftimgcalc.h"

/* 
 * Find a given "variable" in an expression.  Case-insensitive check
 * for substring, with a check that the substring is bounded on either
 * side by non-identifier characters. 
 *
 * char *expr - expression to be searched
 * char *subexpr - target subexpression
 * 
 * RETURN: pointer to the subexpression
 */
char *find_var_expr(char *expr, char *subexpr)
{
  char *s, *p, *q;


  for (s = expr; *s; s++) {
    for (p=s, q=subexpr; *p && *q; p++, q++) {
      if (toupper(*p) != toupper(*q)) break;
    }
    if (*q == 0) {
      /* Check identifier boundaries */
      if ( (s == expr || ! isident(s[-1])) &&
	   (*p == 0   || ! isident(*p)) ) return s;
    }
  }

  return 0;
}

/* 
 * Re-write the expression by transforming from various coordinate
 * expressions into column names.  Note that order is important since
 * coord[i].name is a substring of coord[i].altname.
 *
 * RETURNS: 0
 */
int rewrite_expr(char *expr, struct coord_struct *coord, int naxis) 
{
  char newexpr[MAX_EXPR_LEN];
  int nrep = 0;
  int i;

  for (i=0; i<naxis; i++) {
    nrep += repstr(newexpr,    expr, coord[i].altname, coord[i].newname);
    nrep += repstr(   expr, newexpr, coord[i].name,    coord[i].newname);
    nrep += repstr(newexpr,    expr, coord[i].pixname, coord[i].newpixname);
    nrep += repstr(   expr, newexpr, coord[i].intname, coord[i].newintname);
    nrep += repstr(newexpr,    expr, coord[i].natname, coord[i].newnatname);
    strcpy(expr, newexpr);
  }
  return 0;
}

/* 
 * repstr - search for and replace occurrences of substring with
 *          another substring
 *
 * Example: repstr(result, "Hit the cat with a bat", "at", "ar")
 *  would produce result "Hit the car with a bar"
 * 
 * char *result - output string (must be large enough!)
 * char *haystack - input string
 * char *needle - target substring to be replaced
 * char *newneedle - replacement string
 * 
 * RETURNS: number of targets replaced
 */
int repstr(char *result, char *haystack, char *needle, char *newneedle)
{
  char *s, *src, *dest;
  int l1, l2, n;
  int nrep = 0;

  if (newneedle == 0 || newneedle[0] == 0 ||
      needle    == 0 || needle[0]    == 0) {
    strcpy(result, haystack);
    return 0;
  }

  l1 = strlen(needle);
  l2 = strlen(newneedle);
  src = haystack;
  dest = result;
  dest[0] = '\0';

  while ((s = find_var_expr(src, needle))) {
    n = (s-src);
    strncat(dest, src, n);
    dest += n;
    src  += n;
    dest[0] = '\0';
    strcat(dest, newneedle);
    dest += l2;
    src  += l1;
    nrep ++;
  }
  strcat(dest, src);

  return nrep;
}
