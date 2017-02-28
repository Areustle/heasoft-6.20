/* $Id: XRSarfRoot.c,v 1.4 2006/11/01 00:27:38 ishisaki Exp $ */
/****************************************************

 XRSarfRoot.c

  2003-02-16	version 1.00	Y.ISHISAKI

  2003-09-08	version 1.10	Y.ISHISAKI
	Change BNK keyword, "XRSarf" -> "ASTEARF"
	Change enum pos_type -> xrs_pos_type
	Copy TELESCOP & INSTRUME keywords from teldef file

  2003-09-30	version 1.20	Y.ISHISAKI
	modified for HEADAS

  2006-04-24 version 1.3	Y.ISHISAKI
	change parameter names: teldef_file -> teldef

  2006-07-24 version 1.4	Y.ISHISAKI
	add telescop, instrume parameters for CALDB
	support for CALDB
	change "Astro-E2" -> "SUZAKU"

****************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "pil.h"
#include "atFunctions.h"
#include "aste_coord.h"
#include "aste_caldb.h"
#include "fitsio.h"
#include "xrsarf.h"

static char pname[] = "XRSarfRoot";
char XRSarfRoot_version[] = "version 1.4";

/* XRSarfRoot parameters */
static struct {
	char telescop[PIL_LINESIZE];
	char instrume[PIL_LINESIZE];
	char *teldef_file, o_teldef_file[PIL_LINESIZE];
	char rmf_file[PIL_LINESIZE];
	char arf_file[PIL_LINESIZE];
	int clobber;
	struct {
		enum xrs_pos_type type;
		double xrs_xmm, xrs_ymm;
	} pos;
	char pixel_select[32];
	char pixel_text[PIL_LINESIZE];
	char pixel_view[6][20];
	fitsfile *fp;
	TELDEF *teldef;
	int irow, nrow;
	double *energ_lo, *energ_hi;
} com = {
	"none",			/* telescop */
	"none",			/* instrume */
	NULL, "none",	/* teldef_file, o_teldef_file */
	"none",			/* rmf_file */
	"none",			/* arf_file */
	0,				/* clobber */
	{
		PT_POINT_XRSXY,					/* pos.type */
		0.0, 0.0						/* pos.xrs_xmm, xrs_ymm */
	},
	{1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,	/* pixel_select */
	 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},
	"",									/* pixel_text */
	{"","","","","",""},				/* pixel_view */
	NULL,		/* fp */
	NULL,		/* teldef */
	0, 0,		/* irow, nrow */
	NULL, NULL	/* energ_lo, energ_hi */
};

static void
MSG(char *format, ...)
{
	FILE *fp = stdout;
	va_list args;
	va_start(args, format);
	if ( '!' == *format ) {
		vfprintf(fp, format+1, args);
	} else {
		vfprintf(fp, format, args);
		fputc('\n', fp);
	}
	va_end(args);
	if ( isatty(fileno(fp)) ) fflush(fp);
}

static void
parse_pixel_text(char *text, char select[32])
{
	int i, cont, pixel_id, last_select;
	char *p = text;

	for (i = 0; i < 32; i++) {
		select[i] = 0;
	}

	cont = 0;
	last_select = 999;
	while ( *p ) {
		if ( '0' <= *p && *p <= '9' ) {
			pixel_id = atoi(p);
			if ( 0 <= pixel_id && pixel_id < 32 ) {
				select[pixel_id] = 1;
			}
			if ( cont ) {
				for (i = last_select; i < pixel_id; i++) {
					if ( 0 <= i && i < 32 ) {
						select[i] = 1;
					}
					select[i] = 1;
				}
				cont = 0;
			}
			last_select = pixel_id;
			while ( '0' <= *p && *p <= '9' ) {
				p++;
			}
		} else if ( '-' == *p ) {
			cont = 1;
			p++;
		} else {
			p++;
		}
	}
}

static void
make_pixel_text(char select[32], char text[80], char view[6][20])
{
	static char pixel_map[6][6] = {
		{ -1, 13, 15,  6,  4, -1 },
		{ 11, 12, 14,  5,  3, -1 },
		{  9, 10,  8,  7,  1,  0 },
		{ 16, 17, 23, 24, 26, 25 },
		{ 18, 19, 21, 30, 28, 27 },
		{ -1, 20, 22, 31, 29, -1 }
	};
	int i, ix, iy, pixel_id, last_select, cont;

	text[0] = '\0';
	last_select = -999;
	cont = 0;
	for (i = 0; i < 32; i++) {
		if ( select[i] ) {
			if ( i-1 == last_select ) {
				last_select = i;
				cont = 1;
			} else {
				sprintf(text + strlen(text), ",%d", i);
				cont = 0;
				last_select = i;
			}
		} else {
			if ( i-1 == last_select && cont ) {
				sprintf(text + strlen(text), "-%d", last_select);
				cont = 0;
			}
		}
	}
	if ( i-1 == last_select && cont ) {
		sprintf(text + strlen(text), "-%d", last_select);
	}
	if ( ',' == text[0] || '-' == text[0] ) {
		strcpy(text, text+1);
	}

	for (iy = 0; iy < 6; iy++) {
		view[iy][0] = '\0';
		for (ix = 0; ix < 6; ix++) {
			pixel_id = pixel_map[iy][ix];
			if ( pixel_id < 0 ) {
				strcat(view[iy], "  ");
			} else if ( select[pixel_id] ) {
				strcat(view[iy], " o");
			} else {
				strcat(view[iy], " x");
			}
		}
	}

	if ( select[2] ) {
		strcat(view[0], "    (o)");
	} else {
		strcat(view[0], "    (x)");
	}
}

static void
show_parameter(char *title)
{
	MSG("");
	MSG(title, pname);
	MSG("");
	MSG("%4s%-20s'%s'%s", "", "TELDEF", com.teldef_file,
		(com.teldef_file == com.o_teldef_file) ? "" : " (CALDB)");
	MSG("%4s%-20s'%s'", "", "RMFFILE", com.rmf_file);
	MSG("%4s%-20s'%s'", "", "ARFFILE", com.arf_file);
	MSG("%4s%-20s%s", "", "CLOBBER", com.clobber ? "YES" : "NO");
	switch (com.pos.type) {
	case PT_POINT_XRSXY:
		MSG("%4s%-20s%s", "", "POSITION_TYPE", "POINT_XRSXY");
		MSG("%4s%-20s%.6f (mm)", "", "    XRS_XMM", com.pos.xrs_xmm);
		MSG("%4s%-20s%.6f (mm)", "", "    XRS_YMM", com.pos.xrs_ymm);
		break;
	default:
		MSG("%4s%-20s%s", "", "POSITION_TYPE", "(UNKNOWN)");
	}
	make_pixel_text(com.pixel_select, com.pixel_text, com.pixel_view);
	MSG("%4s%-20s[%s]", "", "PIXEL_SELECT", com.pixel_text);
	MSG("%4s%-20s%s", "", "", com.pixel_view[0]);
	MSG("%4s%-20s%s", "", "", com.pixel_view[1]);
	MSG("%4s%-20s%s", "", "", com.pixel_view[2]);
	MSG("%4s%-20s%s", "", "", com.pixel_view[3]);
	MSG("%4s%-20s%s", "", "", com.pixel_view[4]);
	MSG("%4s%-20s%s", "", "", com.pixel_view[5]);
}

static int
write_fits_header(fitsfile *fp)
{
	int i;
	char history[FLEN_FILENAME+FLEN_KEYWORD];
	int status = 0;

	sprintf(history, "%s %s", pname, XRSarfRoot_version);
	fits_write_history(fp, history, &status);
	sprintf(history, "  teldef=%s%s", com.teldef_file,
		(com.teldef_file == com.o_teldef_file) ? "" : " (CALDB)");
	fits_write_history(fp, history, &status);
	sprintf(history, "  rmffile=%s", com.rmf_file);
	fits_write_history(fp, history, &status);
	sprintf(history, "  outfile=%s", com.arf_file);
	fits_write_history(fp, history, &status);
	sprintf(history, "  clobber=%s", com.clobber ? "yes" : "no");
	fits_write_history(fp, history, &status);
	switch (com.pos.type) {
	case PT_POINT_XRSXY:
		sprintf(history, "  position_type=POINT_XRSXY");
		fits_write_history(fp, history, &status);
		sprintf(history, "  pos_xcen=%.6f (mm)", com.pos.xrs_xmm);
		fits_write_history(fp, history, &status);
		sprintf(history, "  pos_ycen=%.6f (mm)", com.pos.xrs_ymm);
		fits_write_history(fp, history, &status);
		break;
	}
	make_pixel_text(com.pixel_select, com.pixel_text, com.pixel_view);
	sprintf(history, "  pixel_select=[%s]", com.pixel_text);
	fits_write_history(fp, history, &status);
	for (i = 0; i < 6; i++) {
		sprintf(history, "               %s", com.pixel_view[i]);
		fits_write_history(fp, history, &status);
	}

	return status;
}

static int
read_energy_lo_hi(char *rmf_file)
{
	int status, hdutype;
	fitsfile *fp;

	status = 0;
	fits_open_file(&fp, rmf_file, READONLY, &status);
	if ( status ) {
		anl_msg_error("\
%s: XRS RMF file '%s' open failed\n", pname, rmf_file);
		return -1;
	}

	for (;;) {
		char extname[32], comment[80];

		status = 0;
		fits_movrel_hdu(fp, 1, &hdutype, &status);
		if ( status ) {
			fits_close_file(fp, &status);
			anl_msg_error("\
%s: no SPECRESP extension in '%s'\n", pname, rmf_file);
			return -1;
		}
		fits_read_key_str(fp, "EXTNAME", extname, comment, &status);
		if ( 0 == strcmp("MATRIX", extname) ||
			 0 == strcmp("SPECRESP MATRIX", extname) ) {
			int ny, col_lo, col_hi, anynul;
			int exact = 1;

			fits_read_key(fp, TINT, "NAXIS2", &ny, comment, &status);
			ffgcno(fp, exact, "ENERG_LO", &col_lo, &status);
			ffgcno(fp, exact, "ENERG_HI", &col_hi, &status);
			com.energ_lo = malloc(2 * sizeof(double) * ny);
			com.energ_hi = com.energ_lo + ny;
			if ( NULL == com.energ_lo ) {
				anl_msg_error("\
%s: ENERG_LO/HI malloc failed for '%s'\n", pname, rmf_file);
				return -1;
			}
			ffgcvd(fp, col_lo, 1, 1, ny, 0.0, com.energ_lo, &anynul, &status);
			ffgcvd(fp, col_hi, 1, 1, ny, 0.0, com.energ_hi, &anynul, &status);
			fits_close_file(fp, &status);
			if ( status ) {
				anl_msg_error("\
%s: ENERG_LO/HI column read error for '%s'\n", pname, rmf_file);
				return -1;
			}
			return ny;
		}
	}
}

static int
create_wmap(fitsfile *fp, TELDEF *teldef)
{
	static struct {
		char *key, *value, *comment;
	} *p, keys[] = {
 { "TELESCOP",	"SUZAKU",	"mission/satellite name" },
 { "INSTRUME",	"XRS",		"instrument/detector name" },
 { "CTYPE1",	"DETX",		"X axis definition" },
 { "CTYPE2",	"DETY",		"Y axis definition" },
 { "HDUCLASS",	"OGIP",		"format conforms to OGIP standard" },
 { "HDUCLAS1",	"IMAGE",	"dataset relates to spectral response" },
 { "HDUVERS1",	"1.0.0",	"Version of family of formats" },
 { "HDUCLAS2",	"WMAP",		"dataset contains Weighted Map Image" },
 { "HDUVERS2",	"1.0.0",	"Version of format" },
};
	int status = 0;
	int naxis = 2;
	long naxes[2];
	int ikey, nkey = sizeof(keys) / sizeof(*keys);

	naxes[0] = teldef->mission.aste->det.xsiz;
	naxes[1] = teldef->mission.aste->det.ysiz;
	fits_create_img(fp, FLOAT_IMG, naxis, naxes, &status);

	keys[0].value = com.teldef->telescop;
	keys[1].value = com.teldef->instrume;

	for (ikey = 0; ikey < nkey; ikey++) {
		p = &keys[ikey];
		fits_write_key_str(fp, p->key, p->value, p->comment, &status);
	}

	return status;
}

static fitsfile *
create_arf_file(char *arf_file)
{
#define NCOLS	5
	static int tfields = NCOLS;
	static char extname[] = "SPECRESP";
	static char *ttype[NCOLS] = {
		"ENERG_LO", "ENERG_HI", "SPECRESP", "EFFAREA", "EXPOSURE"
	};
	static char *tform[NCOLS] = {
		"1E", "1E", "1E", "1E", "1E"
    };
	static char *tunit[NCOLS] = {
		"keV", "keV", "cm**2", "cm**2", ""
    };
#undef NCOLS
	static struct {
		char *key, *value, *comment;
	} *p, keys[] = {
 { "TELESCOP",	"SUZAKU",	"mission/satellite name" },
 { "INSTRUME",	"XRS",		"instrument/detector name" },
 { "ARFVERSN",	"1992a",	"OGIP classification of FITS format" },
 { "HDUCLASS",	"OGIP",		"format conforms to OGIP standard" },
 { "HDUCLAS1",	"RESPONSE",	"dataset relates to spectral response" },
 { "HDUVERS1",	"1.0.0",	"Version of family of formats" },
 { "HDUCLAS2",	"SPECRESP",	"dataset contains spectral response" },
 { "HDUVERS2",	"1.1.0",	"Version of format (OGIP memo CAL/GEN/92-002a)" }
};
	int ikey, nkey = sizeof(keys) / sizeof(*keys);

	fitsfile *fp;
	int status;

	status = 0;
	fits_create_file(&fp, arf_file, &status);
	if ( status ) {
		anl_msg_error("\
%s: creating ARF file '%s' failed\n", pname, arf_file);
		return NULL;
	}

	status = create_wmap(fp, com.teldef);
	if ( status ) {
		anl_msg_error("\
%s: creating WMAP failed for '%s'\n", pname, arf_file);
		return NULL;
	}

	fits_create_tbl(fp, BINARY_TBL,
		com.nrow, tfields, ttype, tform, tunit, extname, &status);

	keys[0].value = com.teldef->telescop;
	keys[1].value = com.teldef->instrume;

	for (ikey = 0; ikey < nkey; ikey++) {
		p = &keys[ikey];
		fits_write_key_str(fp, p->key, p->value, p->comment, &status);
	}
	if ( status ) {
		anl_msg_error("\
%s: header write error for '%s'\n", pname, arf_file);
		return NULL;
	}

	return fp;
}

void
XRSarfRoot_startup(int *status)
{
	com.teldef_file = com.o_teldef_file;
}

void
XRSarfRoot_com(int *status)
{
	static char *keytbl[] = {
		"SHOW",
		"TELESCOP",
		"INSTRUME",
		"TELDEF",
		"RMFFILE",
		"ARFFILE",
		"CLOBBER",
		"POSITION_TYPE",
		"PIXEL_SELECT",
		"EXIT"
	};
	static char *help[] = {
		"Show current settings",
		"Telescope name for CALDB access",
		"Instrument name for CALDB access",
		"XRS telescope definition file name",
		"XRS RMF file name",
		"Output ARF file name",
		"Overwrite output file if exists",
		"Select a type of the target position specification",
		"Specify the XRS pixels to use",
		"Exit from this menu"
	};
	static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);

	if ( *status ) {	/* HEADAS */
		char *k, buf[PIL_LINESIZE];

		*status = ANL_QUIT;
		if ( PILGetFname (k="telescop", com.telescop) ||
			 PILGetFname (k="instrume", com.instrume) ||
			 PILGetFname (k="teldef", com.teldef_file) ||
			 PILGetFname (k="rmffile", com.rmf_file) ||
			 PILGetFname (k="outfile", com.arf_file) ||
			 PILGetBool  (k="clobber", &com.clobber) ||
			 PILGetString(k="position_type", buf) ) {
			goto quit;
		}

		if ( 0 == CLstricmp("POINT_XRSXY", buf) ) {
			com.pos.type = PT_POINT_XRSXY;
			if ( PILGetReal(k="pos_xcen", &com.pos.xrs_xmm) ||
				 PILGetReal(k="pos_ycen", &com.pos.xrs_ymm) ) {
				goto quit;
			}
		} else {
			goto quit;
		}

		if ( PILGetString(k="pixel_select", com.pixel_text) ) {
			goto quit;
		}
		parse_pixel_text(com.pixel_text, com.pixel_select);

		*status = ANL_OK;;
		return;

	quit:
		anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
		*status = ANL_QUIT;
		return;
	}

	for (;;) {
		char *key;
		int ans[2];

		CMinquir(pname, nkey, keytbl, help, 1, ans);
		key = keytbl[ans[1]-1];
		if ( 0 == strcmp("SHOW", key) ) {
			show_parameter("%s:  *** show parameter ***");
		} else if ( 0 == strcmp("TELESCOP", key) ) {
			CLtxtrd(key, com.telescop, sizeof(com.telescop));
		} else if ( 0 == strcmp("INSTRUME", key) ) {
			CLtxtrd(key, com.instrume, sizeof(com.instrume));
		} else if ( 0 == strcmp("TELDEF", key) ) {
			CLtxtrd(key, com.teldef_file, sizeof(com.teldef_file));
		} else if ( 0 == strcmp("RMFFILE", key) ) {
			CLtxtrd(key, com.rmf_file, sizeof(com.rmf_file));
		} else if ( 0 == strcmp("ARFFILE", key) ) {
			CLtxtrd(key, com.arf_file, sizeof(com.arf_file));
		} else if ( 0 == strcmp("CLOBBER", key) ) {
			CLlogrd(key, &com.clobber);
		} else if ( 0 == strcmp("POSITION_TYPE", key) ) {
			static char *keytbl[] = {
				"POINT_XRSXY",
				"EXIT"
			};
			static char *help[] = {
  "Specify one target position in the XRS detector coordinates (mm)",
  "Exit from this menu"
			};
			static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
			char *key;
			int ans[2];

			CMinquir("********** Position Type **********",
					nkey, keytbl, help, 1, ans);
			key = keytbl[ans[1]-1];
			if ( 0 == strcmp("POINT_XRSXY", key) ) {
				com.pos.type = PT_POINT_XRSXY;
				CLfdprd("XRS_XMM", &com.pos.xrs_xmm);
				CLfdprd("XRS_YMM", &com.pos.xrs_ymm);
			} else if ( 0 == strcmp("EXIT", key) ) {
				/* do nothing */
			}
		} else if ( 0 == strcmp("PIXEL_SELECT", key) ) {
			CLtxtrd(key, com.pixel_text, sizeof(com.pixel_text));
			parse_pixel_text(com.pixel_text, com.pixel_select);
		} else if ( 0 == strcmp("EXIT", key) ) {
			break;
		}
	}

	*status = ANL_OK;
}

void
XRSarfRoot_init(int *status)
{
	char *arf_file = com.arf_file;
	CALDB_INFO caldb;

	if ( 0 == CLstricmp("CALDB", com.o_teldef_file) ) {
		aste_caldb_init(&caldb);
		caldb.telescop = com.telescop;
		caldb.instrume = com.instrume;
		caldb.codename = "TELDEF";
		aste_caldb_get(&caldb);
		if ( 0 != caldb.status || 0 == caldb.nfound ) {
			anl_msg_error("\
%s: no CALDB entry for '%s' (status=%d)\n",
				pname, caldb.codename, caldb.status);
			*status = ANL_QUIT;
			return;
		}
		if ( 1 != caldb.nfound ) {
			anl_msg_warning("\
%s: WARNING: multiple CALDB entry (nfound=%d) for '%s'\n",
				pname, caldb.nfound, caldb.codename);
		}
		com.teldef_file = caldb.filename;
	}

	EvsDef("XRSarfRoot:BEGIN");
	EvsDef("XRSarfRoot:ENTRY");
	EvsDef("XRSarfRoot:OK");

	BnkDef("ASTEARF:ROW", sizeof(com.irow));
	BnkDef("ASTEARF:ENERG_LO", sizeof(double));
	BnkDef("ASTEARF:ENERG_HI", sizeof(double));
	BnkDef("ASTEARF:SPECRESP", sizeof(double));
	BnkDef("ASTEARF:EFFAREA", sizeof(double));
	BnkDef("ASTEARF:EXPOSURE", sizeof(double));
	BnkDef("ASTEARF:FITS_PTR", sizeof(com.fp));
	BnkDef("ASTEARF:ARF_FILE", sizeof(arf_file));
	BnkDef("ASTEARF:TELDEF", sizeof(com.teldef));
	BnkDef("ASTEARF:POS:TYPE", sizeof(com.pos.type));
	BnkDef("ASTEARF:POS:XRS_XMM", sizeof(com.pos.xrs_xmm));
	BnkDef("ASTEARF:POS:XRS_YMM", sizeof(com.pos.xrs_ymm));
	BnkDef("ASTEARF:PIXEL_SELECT", sizeof(com.pixel_select));

	show_parameter("%s:  *** show parameter ***");

	anl_msg_info("\n\
%s: reading teldef file '%s'\n", pname, com.teldef_file);
	com.teldef = aste_coord_init(NULL, "XRS", com.teldef_file);
	if ( NULL == com.teldef ) {
		anl_msg_error("\
%s: XRS teldef file '%s' open failed\n", pname, com.teldef_file);
		*status = ANL_QUIT;
		return;
	}

	anl_msg_info("\
%s: reading RMF file '%s'\n", pname, com.rmf_file);
	com.irow = 0;
	com.nrow = read_energy_lo_hi(com.rmf_file);
	if ( com.nrow < 0 ) {
		*status = ANL_QUIT;
		return;
	}

	if ( 0 == CLstricmp("NONE", arf_file) ) {
		anl_msg_error("\
%s: output ARF name not specified\n", pname);
		*status = ANL_QUIT;
		return;
	}
	anl_msg_info("\
%s: creating ARF file '%s'\n", pname, arf_file);
	if ( com.clobber ) {
		unlink(arf_file);
	}
	com.fp = create_arf_file(arf_file);
	if ( NULL == com.fp ) {
		*status = ANL_QUIT;
		return;
	}

	BnkPut("ASTEARF:FITS_PTR", sizeof(com.fp), &com.fp);
	BnkPut("ASTEARF:ARF_FILE", sizeof(arf_file), &arf_file);
	BnkPut("ASTEARF:TELDEF", sizeof(com.teldef), &com.teldef);
	BnkPut("ASTEARF:POS:TYPE", sizeof(com.pos.type), &com.pos.type);
	BnkPut("ASTEARF:POS:XRS_XMM", sizeof(com.pos.xrs_xmm), &com.pos.xrs_xmm);
	BnkPut("ASTEARF:POS:XRS_YMM", sizeof(com.pos.xrs_ymm), &com.pos.xrs_ymm);
	BnkPut("ASTEARF:PIXEL_SELECT", sizeof(com.pixel_select), com.pixel_select);

	*status = write_fits_header(com.fp);
	if ( *status ) {
		anl_msg_error("\
%s: writing history failed for '%s'\n", pname, arf_file);
		*status = ANL_QUIT;
		return;
	}

	*status = ANL_OK;
}

void
XRSarfRoot_his(int *status)
{
	*status = ANL_OK;
}

void
XRSarfRoot_bgnrun(int *status)
{
	EvsSet("XRSarfRoot:BEGIN");

	*status = ANL_OK;
}

void
XRSarfRoot_ana(int nevent, int eventid, int *status)
{
	static double val = 1.0;

	EvsfSetM("XRSarfRoot:ENTRY");

	BnkfPutM("ASTEARF:ENERG_LO", sizeof(double), &com.energ_lo[com.irow]);
	BnkfPutM("ASTEARF:ENERG_HI", sizeof(double), &com.energ_hi[com.irow]);
	BnkfPutM("ASTEARF:SPECRESP", sizeof(val), &val);
	BnkfPutM("ASTEARF:EFFAREA",  sizeof(val), &val);
	BnkfPutM("ASTEARF:EXPOSURE", sizeof(val), &val);

	com.irow++;
	BnkfPutM("ASTEARF:ROW", sizeof(com.irow), &com.irow);

	EvsfSetM("XRSarfRoot:OK");
	if ( com.irow < com.nrow ) {
		*status = ANL_OK;
	} else {
		*status = ANL_ENDLOOP;
	}
}

void
XRSarfRoot_endrun(int *status)
{
	*status = ANL_OK;
}

void
XRSarfRoot_exit(int *status)
{
	*status = 0;
	fits_close_file(com.fp, status);
	if ( *status ) {
		anl_msg_error("\
%s: closing file failed for '%s'\n", pname, com.arf_file);
		*status = ANL_QUIT;
	}

	*status = ANL_OK;
}
