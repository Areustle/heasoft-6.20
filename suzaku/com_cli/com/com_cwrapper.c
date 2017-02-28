/* $Id: com_cwrapper.c,v 1.3 2005-03-07 00:53:22 ishisaki Exp $
   com_cwrapper.c	C wrapper routines for COM

	1996/09/11 Y.ISHISAKI
	2005/03/05 Y.ISHISAKI, add extern ...() declarations for gcc warnings
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
CMchval(quest, nval, names, help, vartyp, array)
  char *quest;
  int nval;
  char *names[];
  char *help[];
  char *vartyp;
  void *array;
{
	extern void chval_();
	char *fnames, *fhelp;
	int fnames_size, fhelp_size;
	fnames = convert_to_fortran_string_array(nval, names, &fnames_size);
	if ( NULL == help || help == names ) {
		fhelp = fnames;
		fhelp_size = fnames_size;
    } else {
		fhelp = convert_to_fortran_string_array(nval, help, &fhelp_size);
    }
	chval_(quest, &nval, fnames, fhelp, vartyp, array,
		   strlen(quest), fnames_size, fhelp_size, strlen(vartyp));
	if ( fhelp != fnames) free(fhelp);
	free(fnames);
}

void
CMinicom(prompt)
  char *prompt;
{
	extern void inicom_();
	inicom_(prompt, strlen(prompt));
}

void
CMcrange(quest, nval, names, help, vartyp, array)
  char *quest;
  int nval;
  char *names[];
  char *help[];
  char *vartyp;
  void *array;
{
	extern void crange_();
	char *fnames, *fhelp;
	int fnames_size, fhelp_size;
	fnames = convert_to_fortran_string_array(nval, names, &fnames_size);
	if ( NULL == help || help == names ) {
		fhelp = fnames;
		fhelp_size = fnames_size;
    } else {
		fhelp = convert_to_fortran_string_array(nval, help, &fhelp_size);
    }
	crange_(quest, &nval, fnames, fhelp, vartyp, array,
		   strlen(quest), fnames_size, fhelp_size, strlen(vartyp));
	if ( fhelp != fnames) free(fhelp);
	free(fnames);
}

void
CMinquir(quest, nval, names, help, nreply, lord)
  char *quest;
  int nval;
  char *names[];
  char *help[];
  int nreply;
  int lord[/*nreply*/];
{
	extern void inquire_();
	char *fnames, *fhelp;
	int fnames_size, fhelp_size;
	fnames = convert_to_fortran_string_array(nval, names, &fnames_size);
	if ( NULL == help || help == names ) {
		fhelp = fnames;
		fhelp_size = fnames_size;
    } else {
		fhelp = convert_to_fortran_string_array(nval, help, &fhelp_size);
    }
	inquire_(quest, &nval, fnames, fhelp, &nreply, lord,
		   strlen(quest), fnames_size, fhelp_size);
	if ( fhelp != fnames) free(fhelp);
	free(fnames);
}

void
CMmodbit(quest, nbits, names, help, bits)
  char *quest;
  int nbits;
  char *names[];
  char *help[];
  int *bits;
{
	extern void modbit_();
	char *fnames, *fhelp;
	int fnames_size, fhelp_size;
	fnames = convert_to_fortran_string_array(nbits, names, &fnames_size);
	if ( NULL == help || help == names ) {
		fhelp = fnames;
		fhelp_size = fnames_size;
    } else {
		fhelp = convert_to_fortran_string_array(nbits, help, &fhelp_size);
    }
	modbit_(quest, &nbits, fnames, fhelp, bits,
			strlen(quest), fnames_size, fhelp_size);
	if ( fhelp != fnames) free(fhelp);
	free(fnames);
}

void
CMmodval(quest, nval, names, help, vartyp, array)
  char *quest;
  int nval;
  char *names[];
  char *help[];
  char *vartyp;
  void *array;
{
	extern void modval_();
	char *fnames, *fhelp;
	int fnames_size, fhelp_size;
	fnames = convert_to_fortran_string_array(nval, names, &fnames_size);
	if ( NULL == help || help == names ) {
		fhelp = fnames;
		fhelp_size = fnames_size;
    } else {
		fhelp = convert_to_fortran_string_array(nval, help, &fhelp_size);
    }
	modval_(quest, &nval, fnames, fhelp, vartyp, array,
			strlen(quest), fnames_size, fhelp_size, strlen(vartyp));
	if ( fhelp != fnames) free(fhelp);
	free(fnames);
}

void
CMshowit(quest, nval, names, help)
  char *quest;
  int nval;
  char *names[];
  char *help[];
{
	extern void showit_();
	char *fnames, *fhelp;
	int fnames_size, fhelp_size;
	fnames = convert_to_fortran_string_array(nval, names, &fnames_size);
	if ( NULL == help || help == names ) {
		fhelp = fnames;
		fhelp_size = fnames_size;
    } else {
		fhelp = convert_to_fortran_string_array(nval, help, &fhelp_size);
    }
	showit_(quest, &nval, fnames, fhelp,
			strlen(quest), fnames_size, fhelp_size);
	if ( fhelp != fnames) free(fhelp);
	free(fnames);
}

void
CMswitch(quest, nval, names, help, vartyp, array)
  char *quest;
  int nval;
  char *names[];
  char *help[];
  char *vartyp;
  int array[];
{
	extern void switch_();
	char *fnames, *fhelp;
	int fnames_size, fhelp_size;
	fnames = convert_to_fortran_string_array(nval, names, &fnames_size);
	if ( NULL == help || help == names ) {
		fhelp = fnames;
		fhelp_size = fnames_size;
    } else {
		fhelp = convert_to_fortran_string_array(nval, help, &fhelp_size);
    }
	switch_(quest, &nval, fnames, fhelp, vartyp, array,
			strlen(quest), fnames_size, fhelp_size, strlen(vartyp), array);
	if ( fhelp != fnames) free(fhelp);
	free(fnames);
}
