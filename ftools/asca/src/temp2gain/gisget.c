/*
 * gisget.c
 *
 *	get GIS information 	Y.Ishisaki
 *							ishisaki@miranda.phys.s.u-tokyo.ac.jp
 *
 * Version 1.0				92/04/06
 *      	.01				92/06/24 set_mpc() fixed
 *			.02				92/11/19 PH mode timing bit conversion fix
 */

#include <stdio.h>
#include <stdlib.h>
#include "gis.h"

#ifdef __alpha
#define	DWORD	int
#else
#define	DWORD	long
#endif

#define BITL	0x20
#define BITM	0x40
#define BITH	0x80
#define SFSEC(bit)	(((bit)==BITH)?2:(((bit)==BITM)?16:64))
#define FRTIM(bit)	(SFSEC(bit)*g1SEC/64)
#define sf_bit(sf)	((sf)[15][32])	/* Bit rate */
#define fr_FI(fr)	((fr)[3])
#undef max
#define max(a,b)	(((a)>(b))?(a):(b))
#undef min
#define min(a,b)	(((a)<(b))?(a):(b))

static void
set_stat(FR *frp, GISINFO *gi)
{
	int fi, frame;
	fi = fr_FI(*frp);
	frame = fi % SFFR;
	/* stat, cpu, gis[].hk, */
	switch (frame) {
	case 1:		/* GIS status 1 */
		((WD*)&gi->stat)[0] = (*frp)[32];
		break;
	case 9:		/* CPU SELECT */
		*(WD*)&gi->cpu = (*frp)[97];
		break;
	case 17:	/* GIS status 2 */
		((WD*)&gi->stat)[1] = (*frp)[32];
		break;
	case 33:	/* GIS status 3 */
		((WD*)&gi->stat)[2] = (*frp)[32];
		break;
	case 26:	/* HVL2 V monitor */
		gi->gis[gGIS2].hk.HVLV.raw = (*frp)[33];
		gi->gis[gGIS2].hk.HVLV.cvt = 1400.0 * (*frp)[33] / 255;
		break;
	case 27:	/* HVH2 V monitor */
		gi->gis[gGIS2].hk.HVHV.raw = (*frp)[33];
		gi->gis[gGIS2].hk.HVHV.cvt = -8000.0 * (*frp)[33] / 255;
		break;
	case 28:	/* HVH2 I monitor */
		gi->gis[gGIS2].hk.HVHI.raw = (*frp)[33];
		gi->gis[gGIS2].hk.HVHI.cvt = 1000.0 * (*frp)[33] / 255;
		break;
	case 29:	/* HVL3 V monitor */
		gi->gis[gGIS3].hk.HVLV.raw = (*frp)[33];
		gi->gis[gGIS3].hk.HVLV.cvt = 1400.0 * (*frp)[33] / 255;
		break;
	case 30:	/* HVH3 V monitor */
		gi->gis[gGIS3].hk.HVHV.raw = (*frp)[33];
		gi->gis[gGIS3].hk.HVHV.cvt = -8000.0 * (*frp)[33] / 255;
		break;
	case 31:	/* HVH3 I monitor */
		gi->gis[gGIS3].hk.HVHI.raw = (*frp)[33];
		gi->gis[gGIS3].hk.HVHI.cvt = 1000.0 * (*frp)[33] / 255;
		break;
	}
	switch (fi%(2*SFFR)) {
	case 64+42:	/* GIS2 temperature */
		gi->gis[gGIS2].hk.T.raw = (*frp)[33];
		gi->gis[gGIS2].hk.T.cvt = 0.5459 * (*frp)[33] - 47.50;
		break;
	case 64+44:	/* GIS3 temperature */
		gi->gis[gGIS3].hk.T.raw = (*frp)[33];
		gi->gis[gGIS3].hk.T.cvt = 0.5459 * (*frp)[33] - 47.50;
		break;
	}
	/* Block Command OS monitor */
	switch (frame&~1) {
	case 0:		/* GAIN CONT */
		*(WD*)&gi->gis[fi&1].gain = (*frp)[97];
		break;
	case 2:		/* RT LD */
		*(WD*)&gi->gis[fi&1].RTLD = (*frp)[97];
		break;
	case 4:		/* RT UD */
		*(WD*)&gi->gis[fi&1].RTUD = (*frp)[97];
		break;
	case 6:		/* HV CONT */
		*(WD*)&gi->gis[fi&1].HV = (*frp)[97];
		break;
	case 10:	/* SCALE DOWN */
		*(WD*)&gi->gis[fi&1].scale = (*frp)[97];
		break;
	case 12:
		*(WD*)&gi->gis[fi&1].tmm = (*frp)[97];
		break;
	}
	/* Block Table */
	if ( 14 <= frame ) {
		gi->gis[fi&1].tbl.b[25*((fi/SFFR)&1) + (frame-14)/2] = (*frp)[97];
#ifdef NEW_GIS_FORMAT
		if ( frame < 20 ) {
			gi->gis[fi&1].tbl.b[(frame-14)/2] = (*frp)[97];
		}
#endif
	}
}

static void
set_moni(SF *sfp, GISINFO *gi)
{
	static int scM[4] = { 0, 0, 3, 3 };
	static int scL[4] = { 0, 2, 0, 2 };
	gGISmoni *moni2, *moni3;
	int i, fi, n;
	long dt;
	moni2 = &gi->gis[gGIS2].moni;
	moni3 = &gi->gis[gGIS3].moni;
	dt = g1SEC * SFSEC(sf_bit(*sfp)) / 16;
	for (i = 0; i < 9; i++) {
		moni2->b[i].bits = moni3->b[i].bits = 8;
		moni2->b[i].scale = moni3->b[i].scale = 1;
		moni2->b[i].n = moni3->b[i].n = 16;
		moni2->b[i].dt = moni3->b[i].dt = dt;
	}
	moni2->LDhit.n = moni3->LDhit.n = 32;
	moni2->LDhit.dt = moni3->LDhit.dt = dt/2;
	if ( BITL == sf_bit(*sfp) ) {
		moni2->LDhit.scale <<= scL[gi->gis[gGIS2].scale.LDhit];
		moni3->LDhit.scale <<= scL[gi->gis[gGIS3].scale.LDhit];
		moni2->L1.scale <<= scL[gi->gis[gGIS2].scale.L1];
		moni3->L1.scale <<= scL[gi->gis[gGIS3].scale.L1];
	}
	if ( BITL == sf_bit(*sfp) || BITM == sf_bit(*sfp) ) {
		moni2->LDhit.scale <<= scM[gi->gis[gGIS2].scale.LDhit];
		moni3->LDhit.scale <<= scM[gi->gis[gGIS3].scale.LDhit];
		moni2->L1.scale <<= scM[gi->gis[gGIS2].scale.L1];
		moni3->L1.scale <<= scM[gi->gis[gGIS3].scale.L1];
	}
	if ( !gi->stat.MODEmpc && gi->stat.PCALon ) {
		for (i = 0; i < 9; i++) {
			moni2->b[i].n = moni3->b[i].n = 0;
		}
		moni2->L1.n = moni3->L1.n = 32;
		moni2->L1.dt = moni2->L1.dt = dt/2;
		moni2->L1.scale <<= 2;
		moni3->L1.scale <<= 2;
		moni2->L1.bits = 6;
		moni3->L1.bits = 6;
		for (i = 0; i < SFFR; i++) {
			FR *frp = &(*sfp)[i];
			fi = fr_FI(*frp);
			n = i / 2;
			switch ( fi & 1 ) {
			case 0:
				moni2->L1.count[n] = ((*frp)[66]&0x3f);
				break;
			case 1:
				moni3->L1.count[n] = ((*frp)[66]&0x3f);
				break;
			}
		}
		return;
	}
	for (i = 0; i < SFFR; i++) {
		FR *frp = &(*sfp)[i];
		fi = fr_FI(*frp);
		/* XY distribution */
		gi->gis[(fi/SFFR)&1].moni.XYdist[fi&1][(fi%SFFR)/2] = (*frp)[50];
		n = i / 4;
		switch ( fi & 3 ) {
		case 0:
			moni2->CPUin.count[n] = (*frp)[51];
			moni2->LDhit.count[2*n] = (*frp)[64];
			moni2->L2.count[n] = (*frp)[65];
			moni2->L1.count[n] = (*frp)[66];
			moni2->L0.count[n] = (*frp)[67];
			break;
		case 1:
			moni3->CPUin.count[n] = (*frp)[51];
			moni3->LDhit.count[2*n] = (*frp)[64];
			moni3->L2.count[n] = (*frp)[65];
			moni3->L1.count[n] = (*frp)[66];
			moni3->L0.count[n] = (*frp)[67];
			break;
		case 2:
			moni2->CPUout.count[n] = (*frp)[51];
			moni2->LDhit.count[2*n+1] = (*frp)[64];
			moni2->H2.count[n] = (*frp)[65];
			moni2->H1.count[n] = (*frp)[66];
			moni2->H0.count[n] = (*frp)[67];
			break;
		case 3:
			moni3->CPUout.count[n] = (*frp)[51];
			moni3->LDhit.count[2*n+1] = (*frp)[64];
			moni3->H2.count[n] = (*frp)[65];
			moni3->H1.count[n] = (*frp)[66];
			moni3->H0.count[n] = (*frp)[67];
			break;
		}
	}
}

static void
set_mpc(SF *sfp, GISINFO *gi)
{
	static gPHbits mpcbits = { 0, 0, 0, 0, 0, 0, 0L };
	static int PH[4] = { 0, 4, 6, 8 };
	static unsigned long dt[4] = { 1<<5, 1<<9, 1<<11, 1<<13 };
	gMPCdata *data[2];
	gMPCspec *spec[2];
	int i, j, k, n, m, frn, wdn;
	unsigned long t;
	i = gi->cpu.MPCPHbit;
	data[gGIS2] = &gi->gis[gGIS2].mpc;
	data[gGIS3] = &gi->gis[gGIS3].mpc;
	data[gGIS2]->bits = data[gGIS3]->bits = mpcbits;
	data[gGIS2]->bits.PH = data[gGIS3]->bits.PH = PH[i];
	m = 1 << PH[i];
	t = dt[i] * SFSEC(sf_bit(*sfp)) / 2;
	data[gGIS2]->bits.dt = data[gGIS3]->bits.dt = t;
	n = data[gGIS2]->nspec = data[gGIS3]->nspec = 1024 / m;
	spec[gGIS2] = data[gGIS2]->spec = (gMPCspec*)gi->work;
	spec[gGIS3] = data[gGIS3]->spec = (gMPCspec*)(gi->work+sizeof(gi->work)/2);
	for (i = 0; i < n; i++) {
		WD *p[2];
		spec[gGIS2][i].time = spec[gGIS3][i].time = i * t;
		p[gGIS2] = spec[gGIS2][i].PH = (WD*)(gi->gis[gGIS2].mpc.spec+n) + i*m;
		p[gGIS3] = spec[gGIS3][i].PH = (WD*)(gi->gis[gGIS3].mpc.spec+n) + i*m;
		for (j = 0; j < m; j++) {
			k = i*m + j;
			frn = (k/256)*16 + ((k/8)&15);	/* 1.01 92/06/24 Y.Ishisaki */
			wdn = (k&7)*16 + ((k/128)&1);
			p[gGIS2][j] = (*sfp)[frn][4+wdn];
			p[gGIS3][j] = (*sfp)[frn][6+wdn];
		}
	}
}

static void
set_pcal(SF *sfp, GISINFO *gi)
{
	static gPHbits pcalbits = { 10, 8, 8, 8, 8, 0, 0L };
	int i, j;
	unsigned long dt;
	gPHdata *data[2];
	data[gGIS2] = &gi->gis[gGIS2].ph;
	data[gGIS3] = &gi->gis[gGIS3].ph;
	data[gGIS2]->bits = data[gGIS3]->bits = pcalbits;
	dt = 1024 * SFSEC(sf_bit(*sfp)) / 2;
	data[gGIS2]->bits.dt = data[gGIS3]->bits.dt = dt;
	data[gGIS2]->event = (gPHevent*)gi->work;
	data[gGIS3]->event = (gPHevent*)(gi->work+sizeof(gi->work)/2);
	data[gGIS2]->nevents = data[gGIS3]->nevents = 0;
	for (i = 0; i < SFFR; i++) {
		FR *frp = &(*sfp)[i];
		int ph = ((*frp)[65]<<2) + ((*frp)[66]>>6);
		if ( ph ) {
			gGISID g23 = fr_FI(*frp) & 1;
			gPHevent *ev = data[g23]->event + data[g23]->nevents;
			ev->PH = ph;
			ev->XP = (*frp)[50];
			ev->YP = (*frp)[51];
			ev->RT = (*frp)[64];
			ev->SP = (*frp)[67];
			ev->TIM = 0L;
			ev->time = dt * (i/2);
			for (j = 0; j < 16; j++) {
				ev->anode[gX][j] = (*frp)[4 + (j/2)*16+(j&1)];
				ev->anode[gY][j] = (*frp)[6 + (j/2)*16+(j&1)];
			}
			data[g23]->nevents++;
		}
	}
}

static void
set_PHbits(WD tmm_w, gPHbits *bit_p, int sfsec)
{
	static gTMmode tmm = { 1, 3, 3, 1, 0 };
	static gPHbits bit = { 10, 8, 8, 5, 0, 0, 0 };
	static struct {
		int ph[2], xy[4], rt[4], sp[2];
	} bits = { { 8, 10 }, { 2, 4, 6, 8 }, { 0, 5, 6, 8 }, { 0, 8 } };
	if ( tmm_w != *(WD*)&tmm ) {
		int t = 31;
		*(WD*)&tmm = tmm_w;
		t -= ( bit.PH = bits.ph[tmm.PH] );
		t -= ( bit.XP = bits.xy[tmm.XP] );
		t -= ( bit.YP = bits.xy[tmm.YP] );
		t -= ( bit.RT = min(t, bits.rt[tmm.RT]) );
		t -= ( bit.SP = min(t, bits.sp[tmm.SP]) );
		bit.TIM = min(10, t);
	}
	bit.dt = (1L << (10 - bit.TIM)) * (sfsec/2);
	*bit_p = bit;
}

static void
set_PHdata(gGISID g23, SF *sfp, GISINFO *gi)
{
	int i, j;
	long t0 = ( fr_FI((*sfp)[0]) & 1 ) ? (-FRTIM(sf_bit(*sfp))) : 0L;
	gPHdata *ph = &gi->gis[g23].ph;
	gPHevent *ev = ph->event;
	ph->nevents = 0;
	for (i = 0; i < SFFR; i++) {
		int k = ph->nevents;
		FR *frp = &(*sfp)[i];
		long ts = ( fr_FI((*sfp)[i]) & 1 ) ? 512L : 0L;
		for (j = 0; j < 8; j++) {
			WD *p = &(*frp)[j*16+4];
			unsigned DWORD data = (p[0]<<8) | p[1];
			data <<= 16;
			data |= (unsigned)((p[2]<<8) | p[3]);
			if ( 0L == data || (data >> 31) != g23 ) continue;
			data <<= 1;
			ev->PH = data >> ( 32 - ph->bits.PH );
			data <<= ph->bits.PH;
			ev->XP = data >> ( 32 - ph->bits.XP );
			data <<= ph->bits.XP;
			ev->YP = data >> ( 32 - ph->bits.YP );
			data <<= ph->bits.YP;
			ev->RT = data >> ( 32 - ph->bits.RT );
			data <<= ph->bits.RT;
			ev->SP = data >> ( 32 - ph->bits.SP );
			data <<= ph->bits.SP;
			ev->TIM = data >> ( 32 - ph->bits.TIM );
			ev->time = t0 + ev->TIM * ph->bits.dt;
			if ( ts + 4*16*j + 4*4 <= (ev->TIM << (10 - ph->bits.TIM)) ) {
				ev->time -= 2 * FRTIM(sf_bit(*sfp));
			}
			if ( 0 == ph->bits.TIM ) {
				ev->time = (i*8.0 + j) * FRTIM(sf_bit(*sfp)) / 8.0;
			}
			ph->nevents++;
			ev++;
		}
		for (j = k+1; j < ph->nevents; j++) {
			if ( ph->event[j].time < ph->event[j-1].time ) {
				while ( k < j-- ) {
					ph->event[j].time -= 2 * FRTIM(sf_bit(*sfp));
				}
				break;
			}
		}
		if ( i & 1 ) t0 += 2 * FRTIM(sf_bit(*sfp));
	}
}

static void
set_ph(SF *sfp, GISINFO *gi)
{
	int sfsec = SFSEC(sf_bit(*sfp));
	set_PHbits(*(WD*)&gi->gis[gGIS2].tmm, &gi->gis[gGIS2].ph.bits, sfsec);
	set_PHbits(*(WD*)&gi->gis[gGIS3].tmm, &gi->gis[gGIS3].ph.bits, sfsec);
	gi->gis[gGIS2].ph.event = (gPHevent*)gi->work;
	set_PHdata(gGIS2, sfp, gi);
	gi->gis[gGIS3].ph.event =gi->gis[gGIS2].ph.event+gi->gis[gGIS2].ph.nevents;
	set_PHdata(gGIS3, sfp, gi);
}

void
GISget(SF *sfp, GISINFO *gi)
{
	int i;
	gi->gis[gGIS2].tbl.b = (DWORD*)&gi->gis[gGIS2].tbl.general;
	gi->gis[gGIS3].tbl.b = (DWORD*)&gi->gis[gGIS3].tbl.general;
	gi->gis[gGIS2].moni.b = &gi->gis[gGIS2].moni.CPUin;
	gi->gis[gGIS3].moni.b = &gi->gis[gGIS3].moni.CPUin;
	/* set unset values */
	gi->gis[gGIS2].ph.event = NULL;
	gi->gis[gGIS3].ph.event = NULL;
	gi->gis[gGIS2].mpc.spec = NULL;
	gi->gis[gGIS3].mpc.spec = NULL;
	gi->gis[gGIS2].hk.T.cvt = -9999;
	gi->gis[gGIS3].hk.T.cvt = -9999;
	for (i = 0; i < 50; i++) {
		gi->gis[gGIS2].tbl.b[i] = -1;
		gi->gis[gGIS3].tbl.b[i] = -1;
	}
	for (i = 0; i < 32; i++) {
		gi->gis[gGIS2].moni.XYdist[gX][i] = -1;
		gi->gis[gGIS2].moni.XYdist[gY][i] = -1;
		gi->gis[gGIS3].moni.XYdist[gX][i] = -1;
		gi->gis[gGIS3].moni.XYdist[gY][i] = -1;
	}
	/* set stat, cpu, gain, RTLD, RTUD, HV, scale, tmm, tbl, hk */
	for (i = 0; i < SFFR; i++) {
		set_stat(&(*sfp)[i], gi);
	}
	/* set moni */
	for (i = 0; i < SFFR; i++) {
		set_moni(sfp, gi);
	}
	/* set main data */
	if ( gi->stat.MODEmpc ) {
		set_mpc(sfp, gi);	/* MPC mode */
	} else if ( gi->stat.PCALon ) {
		set_pcal(sfp, gi);	/* PCAL mode */
	} else {
		set_ph(sfp, gi);	/* PH mode */
	}
}

/* only set gi->gis[g23].ph */
void
fastGISget(int do_gis2, int do_gis3, SF *sfp, GISINFO *gi)
{
	int sfsec = SFSEC(sf_bit(*sfp));
	gPHdata *g2 = &gi->gis[gGIS2].ph;
	gPHdata *g3 = &gi->gis[gGIS3].ph;
	g2->event = (gPHevent*)gi->work;
	if ( do_gis2 ) {
		set_PHbits((*sfp)[12][97], &g2->bits, sfsec);
		set_PHdata(gGIS2, sfp, gi);
	} else {
		g2->nevents = 0;
	}
	g3->event = g2->event + g2->nevents;
	if ( do_gis3 ) {
		set_PHbits((*sfp)[13][97], &g3->bits, sfsec);
		set_PHdata(gGIS3, sfp, gi);
	} else {
		g3->nevents = 0;
	}
}

static void
showGISstatus(FILE *fp, GISINFO *gi)
{
	static char *mOnOff[2] = { "OFF", "ON" };
	static char *mEnaDis[2] = { "DIS", "ENA" };
	static char *mNrmErr[2] = { "ERR", "NRM" };
	static char *mRunStp[2] = { "STP", "RUN" };
	static char *mPcalMes[2] = { "MES", "PCAL" };
	static char *mMpcPh[2] = { "PH", "MPC" };
	static char *mMemNrm[2] = { "NRM", "MEM" };
	static char *mErrOk[2] = { "OK", "ERR" };
	static char *mDisNul[2] = { "", "DIS" };
	static char *mScDn[4] = { "--", "-L", "M-", "ML" };
	int i;
	gGISstatus stat;
	gCPUselect cpu;
	gGISinfo *g2 = &gi->gis[gGIS2], *g3 = &gi->gis[gGIS3];
	stat = gi->stat;
	cpu = gi->cpu;
	fprintf(fp, "GISA BYPS GIS2 CPU2 C2OP GIS3 CPU3 C3OP\n");
	fprintf(fp, "%-4s %-4s %-4s %-4s %-4s %-4s %-4s %-4s\n\n",
			mOnOff[stat.GISAon],	mOnOff[stat.BYPSon],
			mOnOff[stat.GIS2on],	mRunStp[stat.CPU2run],
			mNrmErr[stat.CPU2norm],	mOnOff[stat.GIS3on],
			mRunStp[stat.CPU3run],	mNrmErr[stat.CPU3norm]);
	fprintf(fp, "RBM  RBFG RBFS PCAL MODE MEM  HAM  BC\n");
	fprintf(fp, "%-4s %-4s %-4s %-4s %-4s %-4s %-4s %-4s\n\n",
			mOnOff[stat.RBMon],		mOnOff[stat.RBFGGon],
			mOnOff[stat.RBFGSon],	mPcalMes[stat.PCALon],
			mMpcPh[stat.MODEmpc],	mMemNrm[stat.MEMCHKon],
			mErrOk[stat.HAMerr],	mEnaDis[stat.BCena]);
	fprintf(fp, "HVEN HVL2 HVH2 HVL3 HVH3 HVRD ADR2 ADR3    MPC PH bit\n");
	fprintf(fp, "%-4s %-4s %-4s %-4s %-4s %-4s %-4s %-4s        %c\n\n",
			mEnaDis[stat.GHVena],	mOnOff[stat.GHVL2on],
			mOnOff[stat.GHVH2on],	mOnOff[stat.GHVL3on],
			mOnOff[stat.GHVH3on],	mOnOff[stat.GHVREDon],
			mEnaDis[stat.ADR2ena],	mEnaDis[stat.ADR3ena],
			"0468"[cpu.MPCPHbit]);
	fprintf(fp, "CPU%c -> GIS2    CPU%c ->GIS3    SELF %sCONNECT\n\n",
			"23"[cpu.GIS2CPU], "32"[cpu.GIS3CPU], mDisNul[cpu.SelfConnect]);
	fprintf(fp, "           FGain  PH-LD  RT-LD  RT-UD  GHV-H  GHV-L  L1-SD  LD-SD\n");
	fprintf(fp, "GIS2/GIS3  %02x/%02x  %02x/%02x  %02x/%02x  %02x/%02x  %02x/%02x  %02x/%02x  %s/%s  %s/%s\n\n",
			g2->gain.FineGainCont,	g3->gain.FineGainCont,
			g2->gain.LD,			g3->gain.LD,
			g2->RTLD,				g3->RTLD,
			g2->RTUD,				g3->RTUD,
			g2->HV.GHVH,			g3->HV.GHVH,
			g2->HV.GHVL,			g3->HV.GHVL,
			mScDn[g2->scale.L1],	mScDn[g3->scale.L1],
			mScDn[g2->scale.LDhit],	mScDn[g3->scale.LDhit]);
	fprintf(fp, "     PH XP YP RT SP     HVLV       HVHV       HVHI       TEMP\n");
	for (i = 0; i < 2; i++) {
		gGISinfo *g = &gi->gis[i];
		fprintf(fp, "GIS%d %2d  %d  %d  %d  %d  %5.0lfV(%02x) %5.0lfV(%02x) %5.1lfuA(%02x) ",
				i+2,
				"\010\012"[g->tmm.PH], "\02\04\06\010"[g->tmm.XP],
				"\02\04\06\010"[g->tmm.YP], "\0\05\06\08"[g->tmm.RT],
				"\0\04"[g->tmm.SP],
				g->hk.HVLV.cvt, g->hk.HVLV.raw,
				g->hk.HVHV.cvt, g->hk.HVHV.raw,
				g->hk.HVHI.cvt, g->hk.HVHI.raw);
		if ( -9999 == g->hk.T.cvt ) {
			fprintf(fp, "-----C(--)\n");
		} else {
			fprintf(fp, "%5.1lfC(%02x)\n",  g->hk.T.cvt, g->hk.T.raw);
		}
	}
}

static void
showBlockTable(FILE *fp, GISINFO *gi)
{
	int i;
	for (i = 0; i < 50; i++) {
		if ( -1 != gi->gis[gGIS2].tbl.b[i] ) {
			fprintf(fp, "%2d %02x/", i, gi->gis[gGIS2].tbl.b[i]);
			if ( -1 != gi->gis[gGIS3].tbl.b[i] ) {
				fprintf(fp, "%02x", gi->gis[gGIS3].tbl.b[i]);
			} else {
				fprintf(fp, "--");
			}
			fputc((4 == i%5)?'\n':' ', fp);
		} else {
			if ( -1 != gi->gis[gGIS3].tbl.b[i] ) {
				fprintf(fp, "%2d --/%02x", i, gi->gis[gGIS3].tbl.b[i]);
				fputc((4 == i%5)?'\n':' ', fp);
			}
		}
	}
}	

static void
showGISmoni(FILE *fp, GISINFO *gi)
{
	int i, j;
	gGISinfo *g2 = &gi->gis[gGIS2], *g3 = &gi->gis[gGIS3];
	fprintf(fp, "       CPUin  out  LDhit   L2    H2    L1    H1    L0    H0    Xdist Ydist\n");
	fprintf(fp, "BITS  ");
	for (i = 0; i < 9; i++) {
		fprintf(fp, " %2d/%-2d", g2->moni.b[i].bits, g3->moni.b[i].bits);
	}
	fprintf(fp, "\nSCALE ");
	for (i = 0; i < 9; i++) {
		fprintf(fp, " %2d/%-2d", g2->moni.b[i].scale, g3->moni.b[i].scale);
	}
	for (i = 0; i < 32; i++) {
		fprintf(fp, "\n %2d   ", i);
		for (j = 0; j < 9; j++) {
			if ( 0 == g2->moni.b[j].n || (16 == g2->moni.b[j].n && i%2) ) {
				fprintf(fp, "   /");
			} else if ( 32 == g2->moni.b[j].n ) {
				fprintf(fp, " %02x/", g2->moni.b[j].count[i]);
			} else {
				fprintf(fp, " %02x/", g2->moni.b[j].count[i/2]);
			}
			if ( 0 == g3->moni.b[j].n || (16 == g3->moni.b[j].n && i%2) ) {
				fprintf(fp, "  ");
			} else if ( 32 == g3->moni.b[j].n ) {
				fprintf(fp, "%02x", g3->moni.b[j].count[i]);
			} else {
				fprintf(fp, "%02x", g3->moni.b[j].count[i/2]);
			}
		}
		fprintf(fp, "  ");
		for (j = 0; j < 2; j++) {
			if ( -1 == g2->moni.XYdist[j][i] ) fprintf(fp, " --/");
			else fprintf(fp, " %02x/", g2->moni.XYdist[j][i]);
			if ( -1 == g3->moni.XYdist[j][i] ) fprintf(fp, "--");
			else fprintf(fp, "%02x", g3->moni.XYdist[j][i]);
		}
	}
	fputc('\n', fp);
}	

static void
showMPC(FILE *fp, GISINFO *gi, gGISID g23)
{
	int i, j, n;
	unsigned long sum;
	gMPCdata *mpc = &gi->gis[g23].mpc;
	fprintf(fp, "NSPEC = %u, PHBIT = %d, dt = %8.2lf msec\n",
			mpc->nspec, mpc->bits.PH, 1000.0*mpc->bits.dt/g1SEC);
	n = 1 << mpc->bits.PH;
	fprintf(fp, "CH ");
	for (i = 0; i < mpc->nspec; i++) {
		fprintf(fp, "#%-3d", i);
	}
	fprintf(fp, " SUM\n");
	for (i = 0; i < n; i++) {
		sum = 0;
		fprintf(fp, "%-3d", i);
		for (j = 0; j < mpc->nspec; j++) {
			fprintf(fp, " %-3d", mpc->spec[j].PH[i]);
			sum += mpc->spec[j].PH[i];
		}
		fprintf(fp, " %d\n", sum);
	}
}

static void
showPH(FILE *fp, GISINFO *gi, gGISID g23)
{
	int i, j;
	gPHdata *ph = &gi->gis[g23].ph;
	fprintf(fp, "NEVENTS = %u, PH:%d XP:%d YP:%d RT:%d SP:%d TIM:%d, dt = %8.2lf msec\n",
			ph->nevents, ph->bits.PH, ph->bits.XP, ph->bits.YP,
			ph->bits.RT, ph->bits.SP, ph->bits.TIM,
			1000.0*ph->bits.dt/g1SEC);
	for (i = 0; i < ph->nevents; i++) {
		gPHevent *ev = &ph->event[i];
		fprintf(fp, "%03x %02x %02x %02x %02x %8.2lf msec",
				ev->PH, ev->XP, ev->YP, ev->RT, ev->SP, 1000.0*ev->time/g1SEC);
		if ( gi->stat.PCALon ) {
			for (j = 0; j < 16; j++) {
				fprintf(fp, " %02x", ev->anode[gX][j]);
			}
			fprintf(fp, "\n                             ");
			for (j = 0; j < 16; j++) {
				fprintf(fp, " %02x", ev->anode[gY][j]);
			}
		}
		fputc('\n', fp);
	}
}

void
showGISINFO(FILE *fp, GISINFO *gi)
{
	gGISID g23;
	fprintf(fp, "--- GIS status ---\n");
	showGISstatus(fp, gi);
	fprintf(fp, "\n--- Block Table ---\n");
	showBlockTable(fp, gi);
	fprintf(fp, "\n--- GIS MONITOR COUNT ---\n");
	showGISmoni(fp, gi);
	for (g23 = 0; g23 < 2; g23++) {
		if ( gi->stat.MODEmpc ) {
			fprintf(fp, "\n--- GIS%c MPC mode Data ---\n", g23+'2');
			showMPC(fp, gi, g23);
		} else {
			if ( gi->stat.PCALon ) {
				fprintf(fp, "\n--- GIS%c PCAL mode Data ---\n", g23+'2');
			} else {
				fprintf(fp, "\n--- GIS%c PH mode Data ---\n", g23+'2');
			}
			showPH(fp, gi, g23);
		}
	}
}

/* check frame indicater */
int
SFcheck(SF *sfp)
{
	int i, fi;
	fi = fr_FI((*sfp)[0]);
	if ( fi % SFFR ) return 1;
	if ( (*sfp)[0][32]&0x1f ) return 2;
	for (i = 1; i < SFFR; i++) {
		if ( fi + i != fr_FI((*sfp)[i]) ) return 3;
	}
	return 0;
}

/*#define TEST	/* test routine */

/*
#ifdef TEST
void
main(int argc, char **argv)
{
	static SF sf;
	static GISINFO gi;
	int n = 0;
	FILE *fp;
	if ( argc < 2 ) {
		fp = stdin;
	} else {
		fp = fopen(argv[1], "rb");
		if ( NULL == fp ) {
			fprintf(stderr, "%s: open failed\n", argv[1]);
			exit(1);
		}
	}
	while ( 1 == fread(&sf, sizeof(sf), 1, fp) ) {
		n++;
		GISget(&sf, &gi);
		if ( SFcheck(&sf) ) {
			fprintf(stderr, "SF#%d: wrong format\n", n);
		} else {
			showGISINFO(stdout, &gi);
		}
	}
	exit(0);
}
#endif
*/
