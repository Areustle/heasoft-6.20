/* $Id: cli_cwrapper.c,v 1.13 2007-02-02 16:52:29 ishisaki Exp $
   cli_cwrapper.c	C wrapper routines for CLI

	1996/09/11 Y.ISHISAKI
	2005/02/16 Y.ISHSIAKI, bug fix in CLimem(), add CLsleep()
	2005/02/17 Y.ISHSIAKI, add CLstrdwc(), CLstricmp(), CLstrnicmp()
	2005/02/19 Y.ISHSIAKI, add CLgetlun(), CLfreelun()
	2005/02/19 Y.ISHSIAKI, add CLrhis(), CLwhis(), CLphis()
	2005/02/19 Y.ISHSIAKI, change CLiopt(char*,int) -> CLiopt(char*,int*)
	2005/02/23 Y.ISHSIAKI, add CLstar(), CLmarklun()
	2005/02/26 Y.ISHSIAKI, add CLlast(), CLilvl(), CLflas(), CLflag()
	2005/02/26 Y.ISHSIAKI, add CLsetpath(), CLfindpath()
	2005/03/02 Y.ISHSIAKI, add extern ...() declarations for gcc warnings
	2005/11/12 Y.ISHSIAKI, do not initialize text in CL[int|flt|fdp|hex]rdX
	2005/11/12 Y.ISHSIAKI, add new macro, fill_all_spaces()
	2005/11/12 Y.ISHSIAKI, use fill_all_spaces() for output only text
	2007/02/02 Y.ISHSIAKI,
		add CLsyso(), CLfgetc(), CLfputc(), CLfseek(), CLftell(), CLfnum(),
		CLchdir(), CLgetwd(), CLtempnam(), CLgets()
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cli.h"

static void
truncate_tail_spaces(string, size)
  char *string;
  int size;
{
	if ( 0 < size-- ) {
		string[size] = '\0';
	}
	while ( 0 < size-- && ' ' == string[size] ) {
		string[size] = '\0';
	}
}

static void
fill_tail_spaces(string, size)
  char *string;
  int size;
{
	int i;
	for (i = 0; i < size && string[i]; i++) {
		;
	}
	while ( i < size ) {
		string[i++] = ' ';
	}
}

#define fill_all_spaces(string, size)	memset(string, ' ', size)

static char *
convert_to_fortran_string_array(n, string_array, array_size)
  int n;
  char **string_array;
  int *array_size;
{
	char *p;
	int i, len, maxlen;
	maxlen = 0;
	for (i = 0; i < n; i++) {
		len = strlen(string_array[i]);
		if ( maxlen < len ) maxlen = len;
	}
	p = malloc(maxlen*n);
	if ( NULL == p ) {
		return NULL;
	}
	for (i = 0; i < n; i++) {
		len = strlen(string_array[i]);
		memcpy(p+i*maxlen, string_array[i], len);
		memset(p+i*maxlen+len, ' ', maxlen-len);
	}
	*array_size = maxlen;
	return p;
}

void
CLvers()
{
	extern void clvers_();
	clvers_();
}

void
CLintrd(prompt, value)
  char *prompt;
  int *value;
{
	extern void intrd_();
	intrd_(prompt, value, strlen(prompt));
}

void
CLintrdL(prompt, value, lower, upper)
  char *prompt;
  int *value;
  int lower;
  int upper;
{
	extern void intrdl_();
	intrdl_(prompt, value, &lower, &upper, strlen(prompt));
}

void
CLintrdX(prompt, value, text, text_size)
  char *prompt;
  int *value;
  char *text;
  int text_size;
{
	extern void intrdx_();
	intrdx_(prompt, value, text, strlen(prompt), text_size-1);
	truncate_tail_spaces(text, text_size);
}

void
CLhexrd(prompt, value)
  char *prompt;
  int *value;
{
	extern void hexrd_();
	hexrd_(prompt, value, strlen(prompt));
}

void
CLhexrdL(prompt, value, lower, upper)
  char *prompt;
  int *value;
  int lower;
  int upper;
{
	extern void hexrdl_();
	hexrdl_(prompt, value, &lower, &upper, strlen(prompt));
}

void
CLhexrdX(prompt, value, text, text_size)
  char *prompt;
  int *value;
  char *text;
  int text_size;
{
	extern void hexrdx_();
	hexrdx_(prompt, value, text, strlen(prompt), text_size-1);
	truncate_tail_spaces(text, text_size);
}

void
CLfltrd(prompt, value)
  char *prompt;
  float *value;
{
	extern void fltrd_();
	fltrd_(prompt, value, strlen(prompt));
}

void
CLfltrdL(prompt, value, lo, hi)
  char *prompt;
  float *value;
  double lo;
  double hi;
{
	extern void fltrdl_();
	fltrdl_(prompt, value, &lo, &hi, strlen(prompt));
}

void
CLfltrdX(prompt, value, text, text_size)
  char *prompt;
  float *value;
  char *text;
  int text_size;
{
	extern void fltrdx_();
	fltrdx_(prompt, value, text, strlen(prompt), text_size-1);
	truncate_tail_spaces(text, text_size);
}

void
CLfdprd(prompt, value)
  char *prompt;
  double *value;
{
	extern void fdprd_();
	fdprd_(prompt, value, strlen(prompt));
}

void
CLfdprdL(prompt, value, lower, upper)
  char *prompt;
  double *value;
  double lower;
  double upper;
{
	extern void fdprdl_();
	fdprdl_(prompt, value, &lower, &upper, strlen(prompt));
}

void
CLfdprdX(prompt, value, text, text_size)
  char *prompt;
  double *value;
  char *text;
  int text_size;
{
	extern void fdprdx_();
	fdprdx_(prompt, value, text, strlen(prompt), text_size-1);
	truncate_tail_spaces(text, text_size);
}

void
CLtxtrd(prompt, text, text_size)
  char *prompt;
  char *text;
  int text_size;
{
	extern void txtrd_();
	fill_tail_spaces(text, text_size);
	txtrd_(prompt, text, strlen(prompt), text_size-1);
	truncate_tail_spaces(text, text_size);
}

void
CLtitrd(prompt, text, text_size)
  char *prompt;
  char *text;
  int text_size;
{
	extern void titrd_();
	fill_tail_spaces(text, text_size);
	titrd_(prompt, text, strlen(prompt), text_size-1);
	truncate_tail_spaces(text, text_size);
}

void
CLlogrd(prompt, value)
  char *prompt;
  int *value;
{
	extern void logrd_();
	logrd_(prompt, value, strlen(prompt));
}

void
CLkeyrd(mode, prompt, word, table, ntable, choice, word_size)
  int mode;
  char *prompt;
  char *word;
  char **table;
  int ntable;
  int *choice;
  int word_size;
{
	extern void keyrd_();
	char *new_table;
	int table_size;
	fill_tail_spaces(word, word_size);
	new_table = convert_to_fortran_string_array(ntable, table, &table_size);
	if ( NULL == new_table ) {
		*choice = 1;
		return;
	}
	keyrd_(&mode, prompt, word, new_table, &ntable, choice,
		   strlen(prompt), word_size-1, table_size);
	truncate_tail_spaces(word, word_size);
	free(new_table);
}

int
CLopnrd(file)
  char *file;
{
	extern int opnrd_();
	return opnrd_(file, strlen(file));
}

int
CLlunrd(lun)
  int lun;
{
	extern int lunrd_();
	return lunrd_(&lun);
}

int
CLiopen(lun, fn, ext, mode)
  int lun;
  char *fn;
  char *ext;
  char *mode;
{
	extern int iopen_();
	return iopen_(&lun, fn, ext, mode, strlen(fn), strlen(ext), strlen(mode));
}

void
CLopen(fn, lun, mode, error, ext)
  char *fn;
  int lun;
  char *mode;
  int *error;
  char *ext;
{
	extern void clopen_();
	clopen_(fn, &lun, mode, error, ext, strlen(fn), strlen(mode), strlen(ext));
}

int
CLqopen(prompt, lun, file, mode, form, stat, file_size)
  char *prompt;
  int lun;
  char *file;
  char *mode;
  char *form;
  char *stat;
  int file_size;
{
	extern int qopen_();
	int status;
	fill_tail_spaces(file, file_size);
	status = qopen_(prompt, &lun, file, mode, form, stat,
		strlen(prompt), file_size-1, strlen(mode), strlen(form), strlen(stat));
	truncate_tail_spaces(file, file_size);
	return status;
}

int
CLxopen(lun, file, mode, form, stat, file_size)
  int lun;
  char *file;
  char *mode;
  char *form;
  char *stat;
  int file_size;
{
	extern int xopen_();
	int status;
	fill_tail_spaces(file, file_size);
	status = xopen_(&lun, file, mode, form, stat,
		file_size-1, strlen(mode), strlen(form), strlen(stat));
	truncate_tail_spaces(file, file_size);
	return status;
}

void
CLaffirm(prompt, value)
  char *prompt;
  int *value;
{
	extern void affirm_();
	affirm_(prompt, value, strlen(prompt));
}

void
CLatof(string, value)
  char *string;
  float *value;
{
	extern void clatof_();
	clatof_(string, value, strlen(string));
}

void
CLatod(string, value)
  char *string;
  double *value;
{
	extern void clatod_();
	clatod_(string, value, strlen(string));
}

void
CLatoi(string, value)
  char *string;
  int *value;
{
	extern void clatoi_();
	clatoi_(string, value, strlen(string));
}

void
CLftoa(value, string, string_size)
  double value;
  char *string;
  int string_size;
{
	extern void clftoa_();
	float v = value;
	fill_all_spaces(string, string_size);
	clftoa_(&v, string, string_size-1);
	truncate_tail_spaces(string, string_size);
}

void
CLdtoa(value, string, string_size)
  double value;
  char *string;
  int string_size;
{
	extern void cldtoa_();
	fill_all_spaces(string, string_size);
	cldtoa_(&value, string, string_size-1);
	truncate_tail_spaces(string, string_size);
}

void
CLitoa(value, string, string_size)
  int value;
  char *string;
  int string_size;
{
	extern void clitoa_();
	fill_all_spaces(string, string_size);
	clitoa_(&value, string, string_size-1);
	truncate_tail_spaces(string, string_size);
}

void
CLxtoa(value, nd, string, string_size)
  double value;
  int nd;
  char *string;
  int string_size;
{
	extern void clxtoa_();
	fill_all_spaces(string, string_size);
	clxtoa_(&value, &nd, string, string_size-1);
	truncate_tail_spaces(string, string_size);
}

void
CLstrupc(string)
  char *string;
{
	extern void clstrupc_();
	int len = strlen(string);
	clstrupc_(&len, string, len);
}

void
CLstrdwc(string)
  char *string;
{
	extern void clstrdwc_();
	int len = strlen(string);
	clstrdwc_(&len, string, len);
}

int
CLstricmp(s1, s2)
  char *s1, *s2;
{
	extern int clstricmp_();
	int l1, l2;
	l1 = strlen(s1);
	l2 = strlen(s2);
	return clstricmp_(s1, s2, l1, l2);
}

int
CLstrnicmp(s1, s2, len)
  char *s1, *s2;
  int len;
{
	extern int clstricmp_();
	int l1, l2;
	l1 = strlen(s1);
	if ( len < l1 ) {
		l1 = len;
	}
	l2 = strlen(s2);
	if ( len < l2 ) {
		l2 = len;
	}
	return clstricmp_(s1, s2, l1, l2);
}

void
CLword(string, delimiter, nwords, words, word_size)
  char *string;
  char *delimiter;
  int *nwords;
  char *words;	/* [nwords][word_size] */
  int word_size;
{
	extern void clword_();
	int i;
	clword_(string, delimiter, nwords, words,
		strlen(string), strlen(delimiter), word_size, *nwords);
	for (i = 0; i < *nwords; i++) {
		if ( ' ' == words[i*word_size + word_size - 1] ) {
			truncate_tail_spaces(&words[i*word_size], word_size);
		}
	}
}

void
CLpart(string, np, word, word_size)
  char *string;
  int np;
  char *word;
  int word_size;
{
	extern void clpart_();
	fill_all_spaces(word, word_size);
	clpart_(string, &np, word, strlen(string), word_size-1);
	truncate_tail_spaces(word, word_size);
}

void
CLpart2(string, np, word, len, word_size)
  char *string;
  int np;
  char *word;
  int *len;
  int word_size;
{
	extern void clpart2_();
	fill_all_spaces(word, word_size);
	clpart2_(string, &np, word, len, strlen(string), word_size-1);
	word[*len] = '\0';
}

void
CLsubp(string, np, index)
  char *string;
  int np;
  int *index;
{
	extern void clsubp_();
	clsubp_(string, &np, index, strlen(string));
}

void
CLsubp2(string, np, index, index2)
  char *string;
  int np;
  int *index;
  int *index2;
{
	extern void clsubp2_();
	clsubp2_(string, &np, index, index2, strlen(string));
}

void
CLugetrd(string, length)
  char *string;
  int length;
{
	extern void ugetrd_();
	ugetrd_(string, &length, length);
}

void
CLalii(alias, command)
  char *alias;
  char *command;
{
	extern void clalii_();
	clalii_(alias, command, strlen(alias), strlen(command));
}

void
CLalip()
{
	extern void clalip_();
	clalip_();
}

void
CLgetv(input, output, output_size, return_length)
  char *input;
  char *output;
  int output_size;
  int *return_length;
{
	extern void clgetv_();
	fill_all_spaces(output, output_size);
	clgetv_(input, output, return_length, strlen(input), output_size-1);
	truncate_tail_spaces(output, output_size);
}

void
CLsetv(var, value)
  char *var;
  char *value;
{
	extern void clsetv_();
	clsetv_(var, value, strlen(var), strlen(value));
}

void
CLseti(var, value)
  char *var;
  int value;
{
	extern void clseti_();
	clseti_(var, &value, strlen(var));
}

void
CLsetf(var, value)
  char *var;
  double value;
{
	extern void clsetf_();
	float v = value;
	clsetf_(var, &v, strlen(var));
}

void
CLsetd(var, value)
  char *var;
  double value;
{
	extern void clsetd_();
	clsetd_(var, &value, strlen(var));
}

void
CLexec(command, in, out)
  char *command;
  char *in;
  char *out;
{
	extern void clexec_();
	clexec_(command, in, out, strlen(command), strlen(in), strlen(out));
}

void
CLfdel(file)
  char *file;
{
	extern void clfdel_();
	clfdel_(file, strlen(file));
}

void
CLstar(icom)
  int icom;
{
	extern void clstar_();
	clstar_(&icom);
}

void
CLiopt(chopt, ival)
  char *chopt;
  int *ival;
{
	extern void cliopt_();
	cliopt_(chopt, ival, strlen(chopt));
}

void
CLecho(string)
  char *string;
{
	extern void clecho_();
	clecho_(string, strlen(string));
}

void
CLprom(prompt)
  char *prompt;
{
	extern void clprom_();
	clprom_(prompt, strlen(prompt));
}

void
CLputl(lun, string)
  int lun;
  char *string;
{
	extern void clputl_();
	clputl_(&lun, string, strlen(string));
}

int
CLgets(lun, string, string_size, length)
  int lun;
  char *string;
  int string_size;
  int *length;
{
	extern int clgets_();
	int istat;
	istat = clgets_(&lun, string, length, string_size-1);
	if ( 0 == istat && 0 <= *length && *length < string_size ) {
		string[*length] = '\0';
	}
	return istat;
}

void
CLclos(lun, ierr)
  int lun;
  int *ierr;
{
	extern void clclos_();
	clclos_(&lun, ierr);
}

void
CLsusp()
{
	extern void clsusp_();
	clsusp_();
}

void
CLstop()
{
	extern void clstop_();
	clstop_();
}

void
CLresm()
{
	extern void clresm_();
	clresm_();
}

void
CLcurl(lunp, linumb, linbuf, linbuf_size)
  int *lunp;
  int *linumb;
  char *linbuf;
  int linbuf_size;
{
	extern void clcurl_();
	fill_all_spaces(linbuf, linbuf_size);
	clcurl_(lunp, linumb, linbuf, linbuf_size-1);
	truncate_tail_spaces(linbuf, linbuf_size);
}

void
CLerok()
{
	extern void clerok_();
	clerok_();
}

void
CLlast(length)
  int *length;
{
	extern void cllast_();
	cllast_(length);
}

int
CLilvl(void)
{
	extern int clilvl_();
	return clilvl_();
}

void
CLilun(lun)
  int *lun;
{
	extern void clilun_();
	clilun_(lun);
}

void
CLflas(IFLQ, IFCR)
  int IFLQ;
  int IFCR;
{
	extern void clflas_();
	clflas_(&IFLQ, &IFCR);
}

void
CLflag(IFLQ, IFCR)
  int *IFLQ;
  int *IFCR;
{
	extern void clflag_();
	clflag_(IFLQ, IFCR);
}

void
CLierr(level, message)
  int level;
  char *message;
{
	extern void clierr_();
	clierr_(&level, message, strlen(message));
}

int
CLilog(sw)
  int sw;
{
	extern int clilog_();
	return clilog_(&sw);
}

void
CLsleep(sec)
  double sec;
{
	extern void clsleep_();
	clsleep_(&sec);
}

void
CLgetlun(lun)
  int *lun;
{
	extern void clgetlun_();
	clgetlun_(lun);
}

void
CLfreelun(lun)
  int lun;
{
	extern void clfreelun_();
	clfreelun_(&lun);
}

void
CLmarklun(lun)
  int lun;
{
	extern void clmarklun_();
	clmarklun_(&lun);
}

void
CLrhis(file, error)
  char *file;
  int *error;
{
	extern void clrhis_();
	clrhis_(file, error, strlen(file));
}

void
CLwhis(file, lines, error)
  char *file;
  int lines;
  int *error;
{
	extern void clwhis_();
	clwhis_(file, &lines, error, strlen(file));
}

void
CLphis(file)
  char *file;
{
	extern void clphis_();
	int len;
	if ( NULL == file ) {
		len = 0;
    } else {
		len = strlen(file);
    }
	clphis_(file, len);
}

int
CLexitcode()
{
	extern int clexitcode_();
	return clexitcode_();
}

void
CLsetpath(path)
  char *path;
{
	extern void clsetpath_();
	clsetpath_(path, strlen(path));
}

void
CLfindpath(file, ext, path, lpath)
  char *file;
  char *ext;
  char *path;
  int *lpath;
{
	extern void clfindpath_();
	clfindpath_(file, ext, path, lpath, strlen(file), strlen(ext), *lpath);
}

void
CLsyso(lun, file, mode, error)
  int lun;
  char *file;
  char *mode;
  int *error;
{
	extern void clsyso_();
	clsyso_(&lun, file, mode, &error, strlen(file), strlen(mode));
}

int
CLfgetc(lun, c)
  int lun;
  char *c;
{
	extern int clfgetc_();
	return clfgetc_(&lun, c, 1);
}

int
CLfputc(lun, c)
  int lun;
  int c;
{
	extern int clfputc_();
	char cc = (char)c;
	return clfputc_(&lun, &cc, 1);
}

void
CLfseek(lun, offset, whence)
  int lun;
  int offset;
  int whence;
{
	extern int clfseek_();
	clfseek_(&lun, &offset, &whence);
}

int
CLftell(lun)
  int lun;
{
	extern int clftell_();
	return clftell_(&lun);
}

int
CLfnum(lun)
  int lun;
{
	extern int clfnum_();
	return clfnum_(&lun);
}

void
CLflush(lun)
  int lun;
{
	extern int clflush_();
	clflush_(&lun);
}

void
CLchdir(path)
  char *path;
{
	extern void clchdir_();
	clchdir_(path, strlen(path));
}

void
CLgetwd(path, path_size)
  char *path;
  int path_size;
{
	extern void clgetwd_();
	clgetwd_(path, path_size-1);
	truncate_tail_spaces(path, path_size);
}

void
CLtempnam(dir, prefix, path, path_size)
  char *dir;
  char *prefix;
  char *path;
  int path_size;
{
	extern void cltempnam_();
	cltempnam_(dir, prefix, path, strlen(dir), strlen(prefix), path_size-1);
	truncate_tail_spaces(path, path_size);
}
