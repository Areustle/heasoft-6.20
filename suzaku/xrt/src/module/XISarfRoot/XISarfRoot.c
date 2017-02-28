/* $Id: XISarfRoot.c,v 1.4 2006/11/01 00:27:38 ishisaki Exp $ */
/****************************************************

 XISarfRoot.c

  2003-09-08	version 1.10	Y.ISHISAKI
	Made from XRSarfRoot.c version 1.10

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
#include "xisarf.h"

static char pname[] = "XISarfRoot";
char XISarfRoot_version[] = "version 1.4";

/* XISarfRoot parameters */
static struct {
	char telescop[PIL_LINESIZE];
	char instrume[PIL_LINESIZE];
	char *teldef_file, o_teldef_file[PIL_LINESIZE];
	char rmf_file[PIL_LINESIZE];
	char arf_file[PIL_LINESIZE];
	int clobber;
	struct {
		enum xis_pos_type type;
		double xis_xmm, xis_ymm;
	} pos;
	struct {
		enum xis_reg_type type;
		double xis_xmm, xis_ymm, xis_rmi, xis_rma;
	} reg;
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
		PT_POINT_XISXY_MM,		/* pos.type */
		0.0, 0.0				/* pos.xis_xmm, xis_ymm */
	},
	{
		RT_XISXYRmiRma_MM,		/* reg.type */
		0.0, 0.0, 0.0, 0.0		/* reg.xis_xmm, xis_ymm, xis_rmi, xis_rma */
	},
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
	case PT_POINT_XISXY_MM:
		MSG("%4s%-20s%s", "", "POSITION_TYPE", "POINT_XISXY_MM");
		MSG("%4s%-20s%.6f (mm)", "", "    XIS_XMM", com.pos.xis_xmm);
		MSG("%4s%-20s%.6f (mm)", "", "    XIS_YMM", com.pos.xis_ymm);
		break;
	default:
		MSG("%4s%-20s%s", "", "POSITION_TYPE", "(UNKNOWN)");
	}
	switch (com.reg.type) {
	case RT_XISXYRmiRma_MM:
		MSG("%4s%-20s%s", "", "REGION_TYPE", "XISXYRmiRma_MM");
		MSG("%4s%-20s%.6f (mm)", "", "    XIS_XMM", com.reg.xis_xmm);
		MSG("%4s%-20s%.6f (mm)", "", "    XIS_YMM", com.reg.xis_ymm);
		MSG("%4s%-20s%.6f (mm)", "", "    XIS_RMI", com.reg.xis_rmi);
		MSG("%4s%-20s%.6f (mm)", "", "    XIS_RMA", com.reg.xis_rma);
		break;
	default:
		MSG("%4s%-20s%s", "", "REGION_TYPE", "(UNKNOWN)");
	}
}

static int
write_fits_header(fitsfile *fp)
{
	char history[FLEN_FILENAME+FLEN_KEYWORD];
	int status = 0;

	sprintf(history, "%s %s", pname, XISarfRoot_version);
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
	case PT_POINT_XISXY_MM:
		sprintf(history, "  position_type=POINT_XISXY_MM");
		fits_write_history(fp, history, &status);
		sprintf(history, "  pos_xcen=%.6f (mm)", com.pos.xis_xmm);
		fits_write_history(fp, history, &status);
		sprintf(history, "  pos_ycen=%.6f (mm)", com.pos.xis_ymm);
		fits_write_history(fp, history, &status);
		break;
	}
	switch (com.reg.type) {
	case RT_XISXYRmiRma_MM:
		sprintf(history, "  region_type=XISXYRmiRma_MM");
		fits_write_history(fp, history, &status);
		sprintf(history, "  reg_xcen=%.6f (mm)", com.reg.xis_xmm);
		fits_write_history(fp, history, &status);
		sprintf(history, "  reg_ycen=%.6f (mm)", com.reg.xis_ymm);
		fits_write_history(fp, history, &status);
		sprintf(history, "  reg_rmin=%.6f (mm)", com.reg.xis_rmi);
		fits_write_history(fp, history, &status);
		sprintf(history, "  reg_rmax=%.6f (mm)", com.reg.xis_rma);
		fits_write_history(fp, history, &status);
		break;
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
%s: RMF file '%s' open failed\n", pname, rmf_file);
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
 { "INSTRUME",	"XIS",		"instrument/detector name" },
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
 { "INSTRUME",	"XIS",		"instrument/detector name" },
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
XISarfRoot_startup(int *status)
{
	com.teldef_file = com.o_teldef_file;
}

void
XISarfRoot_com(int *status)
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
		"REGION_TYPE",
		"EXIT"
	};
	static char *help[] = {
		"Show current settings",
		"Telescope name for CALDB access",
		"Instrument name for CALDB access",
		"XIS telescope definition file name",
		"XIS RMF file name",
		"Output ARF file name",
		"Overwrite output file if exists",
		"Select a type of the target position specification",
		"Select a type of the integration region specification",
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

		if ( 0 == CLstricmp("POINT_XISXY_MM", buf) ) {
			com.pos.type = PT_POINT_XISXY_MM;
			if ( PILGetReal(k="pos_xcen", &com.pos.xis_xmm) ||
				 PILGetReal(k="pos_ycen", &com.pos.xis_ymm) ) {
				goto quit;
			}
		} else {
			goto quit;
		}

		if ( PILGetString(k="region_type", buf) ) {
			goto quit;
		}
		if ( 0 == CLstricmp("XISXYRmiRma_MM", buf) ) {
			com.reg.type = RT_XISXYRmiRma_MM;
			if ( PILGetReal(k="reg_xcen", &com.reg.xis_xmm) ||
				 PILGetReal(k="reg_ycen", &com.reg.xis_ymm) ||
				 PILGetReal(k="reg_rmin", &com.reg.xis_rmi) ||
				 PILGetReal(k="reg_rmax", &com.reg.xis_rma) ) {
				goto quit;
			}
		} else {
			goto quit;
		}

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
				"POINT_XISXY_MM",
				"EXIT"
			};
			static char *help[] = {
  "Specify one target position in the XIS detector coordinates (mm)",
  "Exit from this menu"
			};
			static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
			char *key;
			int ans[2];

			CMinquir("********** Position Type **********",
					nkey, keytbl, help, 1, ans);
			key = keytbl[ans[1]-1];
			if ( 0 == strcmp("POINT_XISXY_MM", key) ) {
				com.pos.type = PT_POINT_XISXY_MM;
				CLfdprd("XIS_XMM", &com.pos.xis_xmm);
				CLfdprd("XIS_YMM", &com.pos.xis_ymm);
			} else if ( 0 == strcmp("EXIT", key) ) {
				/* do nothing */
			}
		} else if ( 0 == strcmp("REGION_TYPE", key) ) {
			static char *keytbl[] = {
				"XISXYRmiRma_MM",
				"EXIT"
			};
			static char *help[] = {
  "Specify X, Y, Rmi, Rma in the XIS detector coordinates (mm)",
  "Exit from this menu"
			};
			static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
			char *key;
			int ans[2];

			CMinquir("********** Region Type **********",
					nkey, keytbl, help, 1, ans);
			key = keytbl[ans[1]-1];
			if ( 0 == strcmp("XISXYRmiRma_MM", key) ) {
				com.pos.type = RT_XISXYRmiRma_MM;
				CLfdprd("XIS_XMM", &com.reg.xis_xmm);
				CLfdprd("XIS_YMM", &com.reg.xis_ymm);
				CLfdprd("XIS_Rmi", &com.reg.xis_rmi);
				CLfdprd("XIS_Rma", &com.reg.xis_rma);
			} else if ( 0 == strcmp("EXIT", key) ) {
				/* do nothing */
			}
		} else if ( 0 == strcmp("EXIT", key) ) {
			break;
		}
	}

	*status = ANL_OK;
}

void
XISarfRoot_init(int *status)
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

	EvsDef("XISarfRoot:BEGIN");
	EvsDef("XISarfRoot:ENTRY");
	EvsDef("XISarfRoot:OK");

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
	BnkDef("ASTEARF:POS:XIS_XMM", sizeof(com.pos.xis_xmm));
	BnkDef("ASTEARF:POS:XIS_YMM", sizeof(com.pos.xis_ymm));
	BnkDef("ASTEARF:REG:TYPE", sizeof(com.reg.type));
	BnkDef("ASTEARF:REG:XIS_XMM", sizeof(com.reg.xis_xmm));
	BnkDef("ASTEARF:REG:XIS_YMM", sizeof(com.reg.xis_ymm));
	BnkDef("ASTEARF:REG:XIS_RMI", sizeof(com.reg.xis_rmi));
	BnkDef("ASTEARF:REG:XIS_RMA", sizeof(com.reg.xis_rma));

	show_parameter("%s:  *** show parameter ***");

	anl_msg_info("\n\
%s: reading teldef file '%s'\n", pname, com.teldef_file);
	com.teldef = aste_coord_init(NULL, NULL, com.teldef_file);
	if ( NULL == com.teldef ) {
		anl_msg_error("\
%s: XIS teldef file '%s' open failed\n", pname, com.teldef_file);
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
	BnkPut("ASTEARF:POS:XIS_XMM", sizeof(com.pos.xis_xmm), &com.pos.xis_xmm);
	BnkPut("ASTEARF:POS:XIS_YMM", sizeof(com.pos.xis_ymm), &com.pos.xis_ymm);
	BnkPut("ASTEARF:REG:TYPE", sizeof(com.reg.type), &com.reg.type);
	BnkPut("ASTEARF:REG:XIS_XMM", sizeof(com.reg.xis_xmm), &com.reg.xis_xmm);
	BnkPut("ASTEARF:REG:XIS_YMM", sizeof(com.reg.xis_ymm), &com.reg.xis_ymm);
	BnkPut("ASTEARF:REG:XIS_RMI", sizeof(com.reg.xis_rmi), &com.reg.xis_rmi);
	BnkPut("ASTEARF:REG:XIS_RMA", sizeof(com.reg.xis_rma), &com.reg.xis_rma);

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
XISarfRoot_his(int *status)
{
	*status = ANL_OK;
}

void
XISarfRoot_bgnrun(int *status)
{
	EvsSet("XISarfRoot:BEGIN");

	*status = ANL_OK;
}

void
XISarfRoot_ana(int nevent, int eventid, int *status)
{
	static double val = 1.0;

	EvsfSetM("XISarfRoot:ENTRY");

	BnkfPutM("ASTEARF:ENERG_LO", sizeof(double), &com.energ_lo[com.irow]);
	BnkfPutM("ASTEARF:ENERG_HI", sizeof(double), &com.energ_hi[com.irow]);
	BnkfPutM("ASTEARF:SPECRESP", sizeof(val), &val);
	BnkfPutM("ASTEARF:EFFAREA",  sizeof(val), &val);
	BnkfPutM("ASTEARF:EXPOSURE", sizeof(val), &val);

	com.irow++;
	BnkfPutM("ASTEARF:ROW", sizeof(com.irow), &com.irow);

	EvsfSetM("XISarfRoot:OK");
	if ( com.irow < com.nrow ) {
		*status = ANL_OK;
	} else {
		*status = ANL_ENDLOOP;
	}
}

void
XISarfRoot_endrun(int *status)
{
	*status = ANL_OK;
}

void
XISarfRoot_exit(int *status)
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
