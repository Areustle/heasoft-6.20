/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/sisrmg/sp_0001.c,v 3.9 1997/05/01 05:43:15 guerber Exp $   */
/*                   */
/*
 filename:      sp_0001.c
 purpose:       Calibration Point 0001
 author:        gbc@space.mit.edu
 date:		Tue Apr 29 13:27:02 EDT 1997
 */
#include "defs.h"

/* ARGSUSED */
static int
loader(p, l)
	Point	*p;
	double	(**l)();
{
	ERRCHK(1, "Deprecated prototype accessed.\n", 0, 0);
	return(1);
}

static Range	res0 = { 0, 0, { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 } };

static char	name0001[] = "Deprecated Point.";
static char	desc0001[MAX_MESS] = "\
	This point is a response placeholder only.\n\
	So don't even *think* of trying to use it.\n\
";
static Point	data0001 = { 0.0, 0.0, 0.0, 0.0,
			 0.0, 0.0, 0.0, 0.0,
			 0.0, 0.0, 0.0, 0.0,
			 0.0, 0.0, 0.0,
			 0, 0, 0, &data0001, name0001, desc0001,
			 { &res0, 0, 0, 0, 0, 0 },
			 0, loader };
/*
 *  This is one of many links to the entire response domain.
 */
extern Plist p_0002;
Plist	p_0001 = { &p_0002, &data0001 };
extern Plist p_0003;
Plist p_0002 = { &p_0003, &data0001 };
extern Plist p_0004;
Plist p_0003 = { &p_0004, &data0001 };
extern Plist p_0005;
Plist p_0004 = { &p_0005, &data0001 };
extern Plist p_0006;
Plist p_0005 = { &p_0006, &data0001 };
extern Plist p_0007;
Plist p_0006 = { &p_0007, &data0001 };
extern Plist p_0008;
Plist p_0007 = { &p_0008, &data0001 };
extern Plist p_0009;
Plist p_0008 = { &p_0009, &data0001 };
extern Plist p_0010;
Plist p_0009 = { &p_0010, &data0001 };
extern Plist p_0011;
Plist p_0010 = { &p_0011, &data0001 };
extern Plist p_0012;
Plist p_0011 = { &p_0012, &data0001 };
extern Plist p_0013;
Plist p_0012 = { &p_0013, &data0001 };
extern Plist p_0014;
Plist p_0013 = { &p_0014, &data0001 };
extern Plist p_0015;
Plist p_0014 = { &p_0015, &data0001 };
extern Plist p_0016;
Plist p_0015 = { &p_0016, &data0001 };
extern Plist p_0017;
Plist p_0016 = { &p_0017, &data0001 };
extern Plist p_0018;
Plist p_0017 = { &p_0018, &data0001 };
extern Plist p_0019;
Plist p_0018 = { &p_0019, &data0001 };
extern Plist p_0020;
Plist p_0019 = { &p_0020, &data0001 };
extern Plist p_0021;
Plist p_0020 = { &p_0021, &data0001 };
extern Plist p_0022;
Plist p_0021 = { &p_0022, &data0001 };
extern Plist p_0023;
Plist p_0022 = { &p_0023, &data0001 };
extern Plist p_0024;
Plist p_0023 = { &p_0024, &data0001 };
extern Plist p_0025;
Plist p_0024 = { &p_0025, &data0001 };
extern Plist p_0026;
Plist p_0025 = { &p_0026, &data0001 };
extern Plist p_0027;
Plist p_0026 = { &p_0027, &data0001 };
extern Plist p_0028;
Plist p_0027 = { &p_0028, &data0001 };
extern Plist p_0029;
Plist p_0028 = { &p_0029, &data0001 };
extern Plist p_0030;
Plist p_0029 = { &p_0030, &data0001 };
extern Plist p_0031;
Plist p_0030 = { &p_0031, &data0001 };
/*
 *  This is one of many links to the entire response domain.
 */
extern Plist p_0040;
Plist	p_0039 = { &p_0040, &data0001 };
extern Plist p_0041;
Plist p_0040 = { &p_0041, &data0001 };
/*
 *  This is one of many links to the entire response domain.
 */
extern Plist p_0050;
Plist	p_0049 = { &p_0050, &data0001 };
