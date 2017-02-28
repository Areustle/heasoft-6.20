/*
 * $Source: /headas/headas/attitude/tasks/prefilter/tleio.c,v $
 * $Revision: 1.7 $
 * $Date: 2005/09/14 21:41:12 $
 *
 * $Log: tleio.c,v $
 * Revision 1.7  2005/09/14 21:41:12  rwiegand
 * Deleted local copy of report.[ch] and updated report calling sequence.
 * Pruned unused mean alignment structure.
 *
 * Revision 1.6  2003/07/21 15:33:19  rwiegand
 * Apply rotation from TEME (SGP coordinate system) to TOD J2000 in
 * tle_to_position.  Allow text TLE files to have blanks in international
 * designator, second derivative of motion and bstar fields.
 *
 * Revision 1.5  2003/02/03 15:24:06  rwiegand
 * Fixed orbit mode string constants to match parameter file.  Reworked loading
 * of text TLEs so individual invalid records cause warnings instead of errors.
 * Indicate which record(s) in TLE file are invalid.
 *
 * Revision 1.4  2002/12/06 20:14:21  rwiegand
 * Added Filter object to pass function pointers for iteration, initialization,
 * status checking.  Broke derive.c into separate files for FORTRAN calling
 * routines, iteration, initialization.  Made compare mode less XTE-centric.
 * Added parameters for pointing axis and boresight.  Allow loading two line
 * elements from FITS or text files.
 *
 * Revision 1.3  2002/11/26 20:13:19  rwiegand
 * Corrected problem with duplicate final record in output.  Made TLE input
 * more robust.
 *
 * Revision 1.2  2002/01/28 16:10:31  rwiegand
 * Removed underscore from TLE types
 *
 * Revision 1.1  2001/11/06 16:31:34  rwiegand
 * Initial revision
 *
 */

#include <math.h>
#include <errno.h>
#include <string.h>

#include "report.h"
#include "tle.h"
#include "tleio.h"


#define MAX_LINE_LENGTH 2048
#define SECONDS_PER_DAY 86400



#undef SPACE
#undef DOT
#undef ONE
#undef TWO
#undef MINUS
#undef TYPE
#undef CLASS
#undef SIGN
#undef DIGIT
#undef LETTER

#define SPACE  0x0001
#define DOT    0x0002
#define ONE    0x0004
#define TWO    0x0008
#define MINUS  0x0010
#define TYPE   0x0020
#define CLASS  0x0040
#define SIGN   0x0080
#define DIGIT  0x0100
#define LETTER 0x0200


/* if length > 1, dest will be null terminated */
static int get_tle_field(char *dest, int length, const char *src, int offset,
        int valid, int *perror)
{
  char buffer[TLE_LINE_LENGTH];
  char vstring[TLE_LINE_LENGTH];
  size_t count;

  memcpy(buffer, src + offset, length);
  buffer[length] = 0;

  /* build string containing all valid characters for field */
  vstring[0] = 0;
  if (valid & SPACE)
    strcat(vstring, " ");
  if (valid & DOT)
    strcat(vstring, ".");
  if (valid & ONE)
    strcat(vstring, "1");
  if (valid & TWO)
    strcat(vstring, "2");
  if (valid & MINUS)
    strcat(vstring, "+-");
  if (valid & SIGN)
    strcat(vstring, "+- ");
  if (valid & TYPE)
    strcat(vstring, "0");  /* NORAD uses "12345" internally */
  if (valid & CLASS)
    strcat(vstring, "UC");
  if (valid & DIGIT)
    strcat(vstring, "0123456789");
  if (valid & LETTER)
    strcat(vstring, "ABCDEFGHIJKLMNOPQRSTUVWXYZ");

  /* check number of valid characters */
  count = strspn(buffer, vstring);
  if (count == length) {
    if (length == 1)
      *dest = src[offset];
    else
      strcpy(dest, buffer);
  }
  else {
    *perror = TLE_BAD_FORMAT;
    /* report src and offset? */
    report_warning("did not find %d characters from '%s' in '%s' [offset %d]\n",
          length, vstring, buffer, offset);
  }

  return *perror;
}


int is_spaces (const char * p)
{
  int space = 1;
  while (*p)
    if (*p++ != ' ')
      space = 0;
  return space;
}


#undef WHITESPACE
#define WHITESPACE 0x0001


int get_tle_number(int *pnumber, const char *buffer, int offset, int length,
	int flags, int *perror)
{
  char c;
  int number = 0;
  const char *p, *end;

  end = buffer + offset + length;
  buffer += offset;
  p = buffer;

  if (flags & WHITESPACE) {
    while (*p == ' ')
      ++p;
  }

  while ((c = *p) && (p != end) && (c >= '0') && (c <= '9')) {
    number *= 10;
    number += c - '0';
    ++p;
  }

  if (*p && (p != end)) {
    *perror = TLE_BAD_FORMAT;
    report_warning("bad number character '%c' in %s\n", *p, buffer);
  }
  else
    *pnumber = number;

  return *perror;
}


char tle_checksum_char (const char *buffer, int length)
{
  int checksum;
  int i;
  char checkchar;

  checksum = 0;
  for (i = 0; i < length; ++i) {
    char c = buffer[i];
    if ((c >= '0') && (c <= '9')) {
      checksum += c - '0';
    }
    else if (c == '-') {
      checksum += 1;
    }
  }
  checkchar = (char) ((checksum % 10) + '0');
  return checkchar;
}


static void get_trimmed_line(FILE *fp, char *dest, size_t limit, int *out)
{
  char buffer[MAX_LINE_LENGTH];

  dest[0] = 0;

  if (*out == TLE_NOT_FOUND) {
    /* don't attempt read */
  }
  else if (fgets(buffer, MAX_LINE_LENGTH, fp)) {
    char c;
    int length = strlen(buffer);
    while ((length > 0) && (c = buffer[length-1])
           && ((c == '\r') || (c == '\n') || (c == ' ')))
      --length;
    buffer[length] = 0;
    if (length < limit) {
      strcpy(dest, buffer);
    }
    else {
      *out = TLE_BAD_FORMAT;
      report_warning("bad line length %d [limit %d]",
            length, limit);
    }
  }
  else if (feof(fp))
      *out = TLE_NOT_FOUND;
  else
      *out = TLE_BAD_FORMAT;
}


int read_tle_raw (FILE *fp, tleraw_t *raw, int flags)
{
  int error;
  int length;
  int debug = flags & TLE_DEBUG;

  error = 0;
  raw->ok = 0;

  if (flags & TLE_UNNAMED)
    raw->name0[0] = 0;
  else
    get_trimmed_line(fp, raw->name0, sizeof(raw->name0), &error);

  get_trimmed_line(fp, raw->line1, sizeof(raw->line1), &error);
  get_trimmed_line(fp, raw->line2, sizeof(raw->line2), &error);

  length = strlen(raw->line1);
  if (error)
    ; /* stuck */
  else if (length == TLE_LINE_LENGTH) {
    char dummy[sizeof(tleraw_t)];
    char *pline1 = raw->line1;
    char checkchar = tle_checksum_char(raw->line1, TLE_LINE_LENGTH-1);

    /* validate and grab the fields from the first element */
    get_tle_field(&raw->number1,    1, pline1,  0, ONE,             &error);
    get_tle_field(dummy,            1, pline1,  1, SPACE,           &error);
    get_tle_field(raw->satellite1,  5, pline1,  2, DIGIT,           &error);
    get_tle_field(&raw->classification,1,pline1,7, CLASS,           &error);
    get_tle_field(dummy,            1, pline1,  8, SPACE,           &error);
    get_tle_field(raw->designator,  8, pline1,  9, SPACE|LETTER|DIGIT,&error);
    if (!is_spaces(raw->designator)) {
      get_tle_field(dummy,          2, pline1,  9, DIGIT,           &error);
      get_tle_field(dummy,          3, pline1, 11, DIGIT,           &error);
      get_tle_field(dummy,          3, pline1, 14, SPACE|LETTER,    &error);
    }
    get_tle_field(dummy,            1, pline1, 17, SPACE,           &error);
    get_tle_field(raw->epochyear,   2, pline1, 18, DIGIT,           &error);
    get_tle_field(raw->epochdoy,    3, pline1, 20, DIGIT,           &error);
    get_tle_field(dummy,            1, pline1, 23, DOT,             &error);
    get_tle_field(raw->epochfrac,   8, pline1, 24, DIGIT,           &error);
    get_tle_field(dummy,            1, pline1, 32, SPACE,           &error);
    get_tle_field(raw->motion2dt,  10, pline1, 33, SIGN|DOT|DIGIT,  &error);
    get_tle_field(dummy,            1, pline1, 33, SIGN,            &error);
    get_tle_field(dummy,            1, pline1, 34, DOT,             &error);
    get_tle_field(dummy,            8, pline1, 35, DIGIT,           &error);
    get_tle_field(dummy,            1, pline1, 43, SPACE,           &error);
    get_tle_field(raw->motion6d2t,  8, pline1, 44, SPACE|SIGN|DIGIT,&error);
    if (!is_spaces(raw->motion6d2t)) {
      get_tle_field(dummy,          1, pline1, 44, SIGN,            &error);
      get_tle_field(dummy,          5, pline1, 45, DIGIT,           &error);
      get_tle_field(dummy,          1, pline1, 50, MINUS,           &error);
      get_tle_field(dummy,          1, pline1, 51, DIGIT,           &error);
    }
    get_tle_field(dummy,            1, pline1, 52, SPACE,           &error);
    get_tle_field(raw->bstar,       8, pline1, 53, SPACE|SIGN|DIGIT,&error);
    if (!is_spaces(raw->bstar)) {
      get_tle_field(dummy,          1, pline1, 53, SIGN,            &error);
      get_tle_field(dummy,          5, pline1, 54, DIGIT,           &error);
      get_tle_field(dummy,          1, pline1, 59, MINUS,           &error);
      get_tle_field(dummy,          1, pline1, 60, DIGIT,           &error);
    }
    get_tle_field(dummy,            1, pline1, 61, SPACE,           &error);
    get_tle_field(&raw->type,       1, pline1, 62, SPACE|TYPE,      &error);
    get_tle_field(dummy,            1, pline1, 63, SPACE,           &error);
    get_tle_field(raw->element,     4, pline1, 64, SPACE|DIGIT,     &error);
    get_tle_field(&raw->checksum1,  1, pline1, 68, DIGIT,           &error);

    if (!(flags & TLE_NO_CHECKSUM))
      if (checkchar != raw->checksum1) {
        error = TLE_BAD_FORMAT;
        report_warning("bad line 1 checksum %c != %c\n",
              checkchar, raw->checksum1);
      }

    if (error) {
      report_warning("validation of line 1 elements failed\n");
    }
  }
  else {
    error = TLE_BAD_FORMAT;
    report_warning("invalid first element length %d\n", length);
  }

  if (!error && debug) {
    report_verbose("line 1 strings\n"
            "\tnumber      '%c'\n"
            "\tsatellite   '%s'\n"
            "\tclassificat '%c'\n"
            "\tdesignator  '%s'\n"
            "\tepochyear   '%s'\n"
            "\tepochdoy    '%s'\n"
            "\tepochfrac   '%s'\n"
            "\tmotion2dt   '%s'\n"
            "\tmotion6dt2  '%s'\n"
            "\tbstar       '%s'\n"
            "\ttype        '%c'\n"
            "\telement     '%s'\n"
            "\tchecksum    '%c'\n",
            raw->number1,
            raw->satellite1,
            raw->classification,
            raw->designator,
            raw->epochyear,
            raw->epochdoy,
            raw->epochfrac,
            raw->motion2dt,
            raw->motion6d2t,
            raw->bstar,
            raw->type,
            raw->element,
            raw->checksum1);
  }

  length = strlen(raw->line2);
  if (error)
    ; /* stuck */
  else if (length == TLE_LINE_LENGTH) {
    char *pline2;
    char dummy[sizeof(tleraw_t)];
    char checkchar = tle_checksum_char(raw->line2, TLE_LINE_LENGTH-1);
    pline2 = raw->line2;

    /* grab fields */
    get_tle_field(&raw->number2,    1, pline2,  0, TWO,             &error);
    get_tle_field(dummy,            1, pline2,  1, SPACE,           &error);
    get_tle_field(raw->satellite2,  5, pline2,  2, DIGIT,           &error);
    get_tle_field(dummy,            1, pline2,  7, SPACE,           &error);
    get_tle_field(raw->inclination, 8, pline2,  8, SPACE|DOT|DIGIT, &error);
    get_tle_field(dummy,            1, pline2, 11, DOT,             &error);
    get_tle_field(dummy,            4, pline2, 12, DIGIT,           &error);
    get_tle_field(dummy,            1, pline2, 16, SPACE,           &error);
    get_tle_field(raw->raan,        8, pline2, 17, SPACE|DOT|DIGIT, &error);
    get_tle_field(dummy,            1, pline2, 20, DOT,             &error);
    get_tle_field(dummy,            4, pline2, 21, DIGIT,           &error);
    get_tle_field(dummy,            1, pline2, 25, SPACE,           &error);
    get_tle_field(raw->eccentricity,7, pline2, 26, DIGIT,           &error);
    get_tle_field(dummy,            1, pline2, 33, SPACE,           &error);
    get_tle_field(raw->argperigee,  8, pline2, 34, SPACE|DOT|DIGIT, &error);
    get_tle_field(dummy,            1, pline2, 37, DOT,             &error);
    get_tle_field(dummy,            4, pline2, 38, DIGIT,           &error);
    get_tle_field(dummy,            1, pline2, 42, SPACE,           &error);
    get_tle_field(raw->meananomaly, 8, pline2, 43, SPACE|DOT|DIGIT, &error);
    get_tle_field(dummy,            1, pline2, 46, DOT,             &error);
    get_tle_field(dummy,            4, pline2, 47, DIGIT,           &error);
    get_tle_field(dummy,            1, pline2, 51, SPACE,           &error);
    get_tle_field(raw->meanmotion, 11, pline2, 52, SPACE|DOT|DIGIT, &error);
    get_tle_field(dummy,            1, pline2, 54, DOT,             &error);
    get_tle_field(dummy,            8, pline2, 55, DIGIT,           &error);
    get_tle_field(raw->revolution,  5, pline2, 63, SPACE|DIGIT,     &error);
    get_tle_field(&raw->checksum2,  1, pline2, 68, DIGIT,           &error);

    if (!(flags & TLE_NO_CHECKSUM))
      if (checkchar != raw->checksum2) {
        error = TLE_BAD_FORMAT;
        report_warning("bad line 2 checksum %c != %c\n",
              checkchar, raw->checksum2);
    }

    /* satellites on both lines must match */
    if (strcmp(raw->satellite1, raw->satellite2)) {
      error = TLE_BAD_FORMAT;
      report_warning("satellite in first record [%s] does not match second [%s]\n",
          raw->satellite1, raw->satellite2);
    }

    if (error) {
      report_warning("validation of line 2 elements failed\n");
    }
  }
  else {
    error = TLE_BAD_FORMAT;
    report_warning("invalid second element length %d\n", length);
  }

  if (!error && debug) {
    report_verbose(
            "line 2 strings\n"
            "\tnumber      '%c'\n"
            "\tsatellite   '%s'\n"
            "\tinclination '%s'\n"
            "\traan        '%s'\n"
            "\teccentricity'%s'\n"
            "\targperigee  '%s'\n"
            "\tmeananomaly '%s'\n"
            "\tmeanmotion  '%s'\n"
            "\trevolution  '%s'\n"
            "\tchecksum    '%c'\n",
            raw->number2,
            raw->satellite2,
            raw->inclination,
            raw->raan,
            raw->eccentricity,
            raw->argperigee,
            raw->meananomaly,
            raw->meanmotion,
            raw->revolution,
            raw->checksum2);
  }

  if (!error)
    raw->ok = 1;

  return error;
}



/*
  TODO: check angle ranges
 */
int tle_convert (const tleraw_t *raw, tleconv_t *conv, int flags)
{
static double invpow10[] = {
        1e-00, 1e-01, 1e-02, 1e-03, 1e-04, 1e-05, 1e-06, 1e-07, 1e-08, 1e-09,
        1e-10, 1e-11, 1e-12, 1e-13, 1e-14, 1e-15, 1e-16, 1e-17, 1e-18, 1e-19,
        };

  int error = 0;

  if (!error) {
      /* parse line 1 fields */
      int x, year, doy, fraction, exponent;
      double sign;

      get_tle_number(&x, raw->satellite1, 0, 5, 0, &error);
      conv->satellite = x;

      conv->classified = (raw->classification == 'C') ? 1 : 0;

      get_tle_number(&year, raw->epochyear, 0, 2, 0, &error);
      get_tle_number(&doy, raw->epochdoy, 0, 3, 0, &error);
      get_tle_number(&fraction, raw->epochfrac, 0, 8, 0, &error);

      conv->epoch = year * 1000. + doy + fraction * invpow10[8];

      sign = (raw->motion2dt[0] == '-') ? -1 : 1;
      get_tle_number(&fraction, raw->motion2dt, 2, 8, 0, &error);
      conv->motion2dt = sign * fraction * invpow10[8];

      /* blank motion6d2t or bstar field indicates value 0 */
      if (!is_spaces(raw->motion6d2t)) {
        sign = (raw->motion6d2t[0] == '-') ? -1 : 1;
        get_tle_number(&fraction, raw->motion6d2t, 1, 5, 0, &error);
        get_tle_number(&exponent, raw->motion6d2t, 7, 1, 0, &error);
        conv->motion6d2t = sign * fraction * invpow10[exponent + 5];
      }
      else
        conv->motion6d2t = 0;

      if (!is_spaces(raw->bstar)) {
        sign = (raw->bstar[0] == '-') ? -1 : 1;
        get_tle_number(&fraction, raw->bstar, 1, 5, 0, &error);
        get_tle_number(&exponent, raw->bstar, 7, 1, 0, &error);
        conv->bstar = sign * fraction * invpow10[exponent + 5];
      }
      else
        conv->bstar = 0;

      if (raw->type == ' ' || raw->type == '0')
        conv->type = 0;
      else
        conv->type = raw->type - '0';

      get_tle_number(&x, raw->element, 0, 4, WHITESPACE, &error);
      conv->element = x;

      conv->checksum1 = (raw->checksum1 - '0');

      if (error) {
        report_warning("unable to convert line 1 elements\n");
      }
  }

  if (!error) {
      /* parse line 2 fields */
      int x, number, fraction;
      double sign;

      sign = 1;

      get_tle_number(&x, raw->satellite1, 0, 5, 0, &error);

      get_tle_number(&number, raw->inclination, 0, 3, WHITESPACE, &error);
      get_tle_number(&fraction, raw->inclination, 4, 4, 0, &error);
      conv->inclination = sign * number + fraction * invpow10[4];

      get_tle_number(&number, raw->raan, 0, 3, WHITESPACE, &error);
      get_tle_number(&fraction, raw->raan, 4, 4, 0, &error);
      conv->raan = sign * number + fraction * invpow10[4];

      get_tle_number(&fraction, raw->eccentricity, 0, 7, 0, &error);
      conv->eccentricity = fraction * invpow10[7];

      get_tle_number(&number, raw->argperigee, 0, 3, WHITESPACE, &error);
      get_tle_number(&fraction, raw->argperigee, 4, 4, 0, &error);
      conv->argperigee = sign * number + fraction * invpow10[4];

      get_tle_number(&number, raw->meananomaly, 0, 3, WHITESPACE, &error);
      get_tle_number(&fraction, raw->meananomaly, 4, 4, 0, &error);
      conv->meananomaly = sign * number + fraction * invpow10[4];

      get_tle_number(&number, raw->meanmotion, 0, 2, WHITESPACE, &error);
      get_tle_number(&fraction, raw->meanmotion, 3, 8, 0, &error);
      conv->meanmotion = sign * number + fraction * invpow10[8];

      get_tle_number(&x, raw->revolution, 0, 5, WHITESPACE, &error);
      conv->revolution = x;
      conv->checksum2 = (raw->checksum2 - '0');

      if (error)
        report_warning("unable to convert line 2 elements\n");
  }

  if (!error) {
    conv->ok = 1;
  }

  return error;
}


int load_tle_from_filename (
        tlesgp_t *tle, const char *filename, const char *key, int flags)
{
  int error;
  int debug = flags & TLE_DEBUG;
  int verbose = flags & TLE_VERBOSE;

  FILE *fp = 0;
  int found = 0;
  tleraw_t raw;
  tleconv_t conv;

  error = 0;

  fp = fopen(filename, "rt");
  if (!fp) {
    error = TLE_BAD_FILE;
    report_warning("unable to open %s [%s]\n",
            filename, strerror(errno));
  }

  while (!error && !found) {
    error = read_tle_raw(fp, &raw, flags);
    if (raw.ok) {
      if (!strcmp(raw.name0, key)) {
        found = 1;

        error = tle_convert(&raw, &conv, flags);
        if (!error) {
          tle->raw = 1;
          tle->deepspace = 0;

          tle->epoch   = conv.epoch;
          tle->xndt2o  = conv.motion2dt;
          tle->xndd6o  = conv.motion6d2t;
          tle->bstar   = conv.bstar;
          tle->xincl   = conv.inclination;
          tle->xnodeo  = conv.raan;
          tle->eo      = conv.eccentricity;
          tle->omegao  = conv.argperigee;
          tle->xmo     = conv.meananomaly;
          tle->xno     = conv.meanmotion;
        }
      }
    }
  }

  if (fp)
    fclose(fp);

  if (!error && verbose) {
    print_tle(stdout, tle, "raw elements");
  }

  if (debug) {
  }

  return error;
}



void print_tle (FILE *fp, const tlesgp_t *tle, const char *header)
{
  if (tle->raw) {
    fprintf(fp, "%s\n"
             "\tepoch    %014.8f (YYDDD.FRACTION)\n"
             "\txndt2o   %e (mean motion / dt / 2, revolutions / day^2)\n"
             "\txndd6o   %e (mean motion / ddt / 6, revolutions / day^3)\n"
             "\tbstar    %e (drag coefficient, (earth radii)^-1\n"
             "\txincl    %f (inclination, degrees)\n"
             "\txnodeo   %f (RAAN, degrees)\n"
             "\teo       %e (eccentricity)\n"
             "\tomegao   %f (argument of perigee, degrees)\n"
             "\txmo      %f (mean anomaly, degrees)\n"
             "\txno      %f (mean motion, revolutions / day)\n",
             header,
             tle->epoch,
             tle->xndt2o,
             tle->xndd6o,
             tle->bstar,
             tle->xincl,
             tle->xnodeo,
             tle->eo,
             tle->omegao,
             tle->xmo,
             tle->xno
             );
  }
  else {
    fprintf(fp, "%s\n"
             "\tepoch    %.8f (YYDDD.FRACTION)\n"
             "\txndt2o   %e (mean motion / dt / 2, radians / minute^2)\n"
             "\txndd6o   %e (mean motion / ddt / 6, radians / minute^3)\n"
             "\tbstar    %e\n"
             "\txincl    %f (inclination, radians)\n"
             "\txnodeo   %f (RAAN, radians)\n"
             "\teo       %e (eccentricity)\n"
             "\tomegao   %f (argument of perigee, radians)\n"
             "\txmo      %f (mean anomaly, radians)\n"
             "\txno      %f (mean motion, radians / minute)\n",
             header,
             tle->epoch,
             tle->xndt2o,
             tle->xndd6o,
             tle->bstar,
             tle->xincl,
             tle->xnodeo,
             tle->eo,
             tle->omegao,
             tle->xmo,
             tle->xno
             );
  }
}

