/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/sqaplot/compat.h,v 3.9 1997/10/27 22:49:34 peachey Exp $   */
/*                   */

/*
 filename:	compat.h
 purpose:	correct for unix/vms anomalies
 author:	Geoffrey B. Crew
 date:		May 1993
 */
#include <math.h>
#include <string.h>
#include <ctype.h>

/*
 * Parameter file interface routines for C using XPI
 * Note: these work for VAX and UNIX but not for IRAF.
 *
*/
# define FUCLPSR(N,P,S)         (void)cuclpsr(N,P,S)
# define FUCLPST(N,P,S)         (void)cuclpst(N,P,S)
# define FUCLGSB(N,P,S)         (void)cuclpsb(N,P,S)
# define FUCLGSI(N,P,S)         (void)cuclgsi(N,P,S)
# define FUCLGSR(N,P,S)         (void)cuclgsr(N,P,S)
/* # define FUCLGST(N,P,S)              (void)cuclgst(N,P,S) */
static void FUCLGST(N,P,S) char *N, *P; int *S; {
        (void)cuclgst(N,P,S);   /* now fortranify */
        for (N = P + MAX_FNAME - 1; *N == '\0'; N--) *N = ' ';
}

#ifdef vax
# include <file.h>
#else
# include <fcntl.h>
#endif  vax

/*
 *  Six-letter equivalents for external names, and
 *  other global external compatibility issues.  The
 *  sizeof's are due to the F77 string calling convention.
 */
#ifdef unix
# define PGBEGIN(I,S,H,V)	(void)pgbegin_(I,S,H,V,\
					sizeof(S))
# define PGPAGE			(void)pgpage_
# define PGIDEN			(void)pgiden_
# define PGVPORT(L,R,T,B)	(void)pgvport_(L,R,T,B)
# define PGVSTAND		(void)pgvstand_
# define PGWINDOW(L,R,T,B)	(void)pgwindow_(L,R,T,B)
# define PGWNAD(L,R,T,B)	(void)pgwnad_(L,R,T,B)
# define PGBOX(S,R,I,T,Q,J)	(void)pgbox_(S,R,I,T,Q,J,\
					sizeof(S), sizeof(T))
# define PGGRAY(A,I,J,I1,I2,J1,J2,F,G,T)	\
				(void)pggray_(A,I,J,I1,I2,J1,J2,F,G,T)
# define PGCONT(A,I,J,I1,I2,J1,J2,F,G,T)	\
				(void)pgcons_(A,I,J,I1,I2,J1,J2,F,G,T)
# define PGBIN(N,X,D,C)		(void)pgbin_(N,X,D,C)
# define PGLINE(N,X,Y)		(void)pgline_(N,X,Y)
# define PGQPOS(X,Y)		(void)pgqpos_(X,Y)
# define PGMOVE(X,Y)		(void)pgmove_(X,Y)
# define PGDRAW(X,Y)		(void)pgdraw_(X,Y)
# define PGSLW(L)		(void)pgslw_(L)
# define PGSLS(L)		(void)pgsls_(L)
# define PGSCI(L)		(void)pgsci_(L)
# define PGMTXT(S,D,C,F,T)	(void)pgmtxt_(S,D,C,F,T,\
					sizeof(S), strlen(T))
# define PGLAB(X,Y,T)		(void)pglab_(X,Y,T,\
					strlen(X),strlen(Y),strlen(T))
# define PGSCH(F)		(void)pgsch_(F)
# define PGEND			(void)pgend_

# define FFCPARS(I,N,E,S)	(void)fcpars_(I,N,E,S,\
					sizeof(I),sizeof(N))
# define FCECHO(M)		(void)fcecho_(M,\
					sizeof(M))
# define FFTOPEN(F,N,M,B,S)	(void)ftopen_(F,N,M,B,S,\
					sizeof(N))
# define FFTCOPY(I,O,M,S)	(void)ftcopy_(I,O,M,S\
					)
# define FFTMAHD(F,E,H,S)	(void)ftmahd_(F,E,H,S\
					)
# define FFTGCNO(F,E,N,M,S)	(void)ftgcno_(F,E,N,M,S,\
					70 /*sizeof(N)*/)
# define FFTPHIS(F,C,S)		(void)ftphis_(F,C,S,\
					sizeof(C))
# define FFTPCOM(F,C,S)		(void)ftpcom_(F,C,S,\
					sizeof(C))
# define FFTPREC(F,V,S)		(void)ftprec_(F,V,S,\
					sizeof(V))
# define FFTGREC(F,N,V,S)	(void)ftgrec_(F,N,V,S,\
					sizeof(V))
# define FFTGHSP(F,N,K,S)	(void)ftghsp_(F,N,K,S\
					)
# define FFTGKEY(F,K,V,C,S)	(void)ftgkey_(F,K,V,C,S,\
					sizeof(K),sizeof(V),sizeof(C))
# define FFTPKYS(F,K,V,C,S)	(void)ftpkys_(F,K,V,C,S,\
					sizeof(K),sizeof(V),sizeof(C))
# define FFTPKYJ(F,K,V,C,S)	(void)ftpkyj_(F,K,V,C,S,\
					sizeof(K),sizeof(C))
# define FFTMKYJ(F,K,V,C,S)	(void)ftmkyj_(F,K,V,C,S,\
					sizeof(K),sizeof(C))
# define FFTPKYD(F,K,V,D,C,S)	(void)ftpkyd_(F,K,V,D,C,S,\
					sizeof(K),sizeof(C))
# define FFTPKYE(F,K,V,D,C,S)	(void)ftpkye_(F,K,V,D,C,S,\
					sizeof(K),sizeof(C))
# define FFTCLOS(F,S)		(void)ftclos_(F,S\
					)
# define FFTINIT(F,N,M,S)	(void)ftinit_(F,N,M,S,\
					sizeof(N))
# define FFTPHPR(F,L,B,A,X,P,G,E,S)	\
				(void)ftphpr_(F,L,B,A,X,P,G,E,S\
					)
# define FFTPDEF(F,B,A,X,P,G,S)	(void)ftpdef_(F,B,A,X,P,G,S\
					)
# define FFTCRHD(F,S)		(void)ftcrhd_(F,S\
					)
# define FFTPHBN(F,R,D,T,M,U,E,P,S)	\
				(void)ftphbn_(F,R,D,\
					T[0],M[0],U[0],E,P,S,\
					8,8,8,sizeof(E))
# define FFTBDEF(F,N,M,P,R,S)	(void)ftbdef_(F,N,M[0],P,R,S,\
					8)
# define FFTPCLB(F,C,R,T,N,A,S)	(void)ftpclb_(F,C,R,T,N,A,S\
					)
# define FFTPCLD(F,C,R,T,N,A,S)	(void)ftpcld_(F,C,R,T,N,A,S\
					)
# define FFTPCLI(F,C,R,T,N,A,S)	(void)ftpcli_(F,C,R,T,N,A,S\
					)
# define FFTPCLE(F,C,R,T,N,A,S)	(void)ftpcle_(F,C,R,T,N,A,S\
					)
# define FFTGCVB(F,C,R,T,N,L,A,Y,S)	\
				(void)ftgcvb_(F,C,R,T,N,L,A,Y,S)
# define FFTGCVI(F,C,R,T,N,L,A,Y,S)	\
				(void)ftgcvi_(F,C,R,T,N,L,A,Y,S)
# define FFTGCVD(F,C,R,T,N,L,A,Y,S)	\
				(void)ftgcvd_(F,C,R,T,N,L,A,Y,S)
# define FFTGCVE(F,C,R,T,N,L,A,Y,S)	\
				(void)ftgcve_(F,C,R,T,N,L,A,Y,S)
# define FFTGDES(F,C,R,N,O,S)	(void)ftgdes_(F,C,R,N,O,S\
					)
# define FFTDDEF(F,O,S)		(void)ftddef_(F,O,S\
					)
# define FFTPSCL(F,B,Z,S)	(void)ftpscl_(F,B,Z,S\
					)
# define FFTPNUL(F,Z,S)		(void)ftpnul_(F,Z,S\
					)
#endif unix

#ifdef vms
# define PGBEGIN(I,S,H,V)	(void)pgbegin(I,S,H,V,\
					sizeof(S))
# define PGPAGE			(void)pgpage
# define PGIDEN			(void)pgiden
# define PGVPORT(L,R,T,B)	(void)pgvport(L,R,T,B)
# define PGVSTAND		(void)pgvstand
# define PGWINDOW(L,R,T,B)	(void)pgwindow(L,R,T,B)
# define PGWNAD(L,R,T,B)	(void)pgwnad(L,R,T,B)
# define PGBOX(S,R,I,T,Q,J)	(void)pgbox(S,R,I,T,Q,J,\
					sizeof(S), sizeof(T))
# define PGSCH(F)		(void)pgsch(F)
# define PGGRAY(A,I,J,I1,I2,J1,J2,F,G,T)	\
				(void)pggray(A,I,J,I1,I2,J1,J2,F,G,T)
# define PGCONT(A,I,J,I1,I2,J1,J2,F,G,T)	\
				(void)pgcons(A,I,J,I1,I2,J1,J2,F,G,T)
# define PGBIN(N,X,D,C)		(void)pgbin(N,X,D,C)
# define PGLINE(N,X,Y)		(void)pgline(N,X,Y)
# define PGQPOS(X,Y)		(void)pgqpos(X,Y)
# define PGMOVE(X,Y)		(void)pgmove(X,Y)
# define PGDRAW(X,Y)		(void)pgdraw(X,Y)
# define PGSLW(L)		(void)pgslw(L)
# define PGSLS(L)		(void)pgsls(L)
# define PGSCI(L)		(void)pgsci(L)
# define PGMTXT(S,D,C,F,T)	(void)pgmtxt(S,D,C,F,T,\
					sizeof(S), strlen(T))
# define PGLAB(X,Y,T)		(void)pglab(X,Y,T,\
					strlen(X),strlen(Y),strlen(T))
# define PGEND			(void)pgend
# define FUCLGSB(N,P,S)		(void)uclgsb(N,P,S,\
					sizeof(N))
# define FUCLGSR(N,P,S)		(void)uclgsr(N,P,S,\
					sizeof(N))
# define FUCLGSI(N,P,S)		(void)uclgsi(N,P,S,\
					sizeof(N))
# define FUCLGST(N,P,S)		(void)uclgst(N,P,S,\
					sizeof(N),MAX_FNAME)
# define FFCPARS(I,N,E,S)	(void)fcpars(I,N,E,S,\
					sizeof(I),sizeof(N))
# define FCECHO(M)		(void)fcecho(M,\
					sizeof(M))
# define FFTOPEN(F,N,M,B,S)	(void)ftopen(F,N,M,B,S,\
					sizeof(N))
# define FFTCOPY(I,O,M,S)	(void)ftcopy(I,O,M,S\
					)
# define FFTMAHD(F,E,H,S)	(void)ftmahd(F,E,H,S\
					)
# define FFTGCNO(F,E,N,M,S)	(void)ftgcno(F,E,N,M,S,\
					70 /*sizeof(N)*/)
# define FFTPHIS(F,C,S)		(void)ftphis(F,C,S,\
					sizeof(C))
# define FFTPREC(F,V,S)		(void)ftprec(F,V,S,\
					sizeof(V))
# define FFTGREC(F,N,V,S)	(void)ftgrec(F,N,V,S,\
					sizeof(V))
# define FFTGHSP(F,N,K,S)	(void)ftghsp(F,N,K,S\
					)
# define FFTPCOM(F,C,S)		(void)ftpcom(F,C,S,\
					sizeof(C))
# define FFTGKEY(F,K,V,C,S)	(void)ftgkey(F,K,V,C,S,\
					sizeof(K),sizeof(V),sizeof(C))
# define FFTPKYS(F,K,V,C,S)	(void)ftpkys(F,K,V,C,S,\
					sizeof(K),sizeof(V),sizeof(C))
# define FFTPKYJ(F,K,V,C,S)	(void)ftpkyj(F,K,V,C,S,\
					sizeof(K),sizeof(C))
# define FFTMKYJ(F,K,V,C,S)	(void)ftmkyj(F,K,V,C,S,\
					sizeof(K),sizeof(C))
# define FFTPKYD(F,K,V,D,C,S)	(void)ftpkyd(F,K,V,D,C,S,\
					sizeof(K),sizeof(C))
# define FFTPKYE(F,K,V,D,C,S)	(void)ftpkye(F,K,V,D,C,S,\
					sizeof(K),sizeof(C))
# define FFTCLOS(F,S)		(void)ftclos(F,S\
					)
# define FFTINIT(F,N,M,S)	(void)ftinit(F,N,M,S,\
					sizeof(N))
# define FFTPHPR(F,L,B,A,X,P,G,E,S)	\
				(void)ftphpr(F,L,B,A,X,P,G,E,S\
					)
# define FFTPDEF(F,B,A,X,P,G,S)	(void)ftpdef(F,B,A,X,P,G,S\
					)
# define FFTCRHD(F,S)		(void)ftcrhd(F,S\
					)
# define FFTPHBN(F,R,D,T,M,U,E,P,S)	\
				(void)ftphbn(F,R,D,\
					T[0],M[0],U[0],E,P,S,\
					8,8,8,sizeof(E))
# define FFTBDEF(F,N,M,P,R,S)	(void)ftbdef(F,N,M[0],P,R,S,\
					8)
# define FFTPCLB(F,C,R,T,N,A,S)	(void)ftpclb(F,C,R,T,N,A,S\
					)
# define FFTPCLD(F,C,R,T,N,A,S)	(void)ftpcld(F,C,R,T,N,A,S\
					)
# define FFTPCLI(F,C,R,T,N,A,S)	(void)ftpcli(F,C,R,T,N,A,S\
					)
# define FFTPCLE(F,C,R,T,N,A,S)	(void)ftpcle(F,C,R,T,N,A,S\
					)
# define FFTGCVB(F,C,R,T,N,L,A,Y,S)	\
				(void)ftgcvb(F,C,R,T,N,L,A,Y,S)
# define FFTGCVI(F,C,R,T,N,L,A,Y,S)	\
				(void)ftgcvi(F,C,R,T,N,L,A,Y,S)
# define FFTGCVD(F,C,R,T,N,L,A,Y,S)	\
				(void)ftgcvd(F,C,R,T,N,L,A,Y,S)
# define FFTGCVE(F,C,R,T,N,L,A,Y,S)	\
				(void)ftgcve(F,C,R,T,N,L,A,Y,S)
# define FFTGDES(F,C,R,N,O,S)	(void)ftgdes(F,C,R,N,O,S\
					)
# define FFTDDEF(F,O,S)		(void)ftddef(F,O,S\
					)
# define FFTPSCL(F,B,Z,S)	(void)ftpscl(F,B,Z,S\
					)
# define FFTPNUL(F,Z,S)		(void)ftpnul(F,Z,S\
					)
/* compensate for holes in vms <math.h> */
# define M_2_SQRTPI	1.12837916709551257390
# define M_SQRT2	1.41421356237309504880

/* and missing libc routine */
extern int	bzero();

#endif vms

#ifdef SISDEBUG
#	include <stdio.h>
#	define ERRCK2(C,M,V,A)	do if(C){		\
			(void)fprintf(stderr,(M),(V));	\
			A;				\
		} while(0)
#else
#	ifdef lint
#		define sprintf	bsd_sprintf
#	endif
#	define ERRCK2(C,M,V,A)	do if(C){		\
			static char	mess[80];	\
			(void)sprintf(mess,(M),(V));	\
			errck2(mess);			\
			A;				\
		} while(0)
	extern void	errck2();
#endif SISDEBUG

/*
 *  Bit mask for parsing CCDLST[01] HK variable.
 */
#define CCD_BITS	0x03
