/*
  AT&T ver1.00  12/15/92  I.Hatsukade &  K.Takagishi
  AT&T ver1.1   93.04.26  N.Kawai
  	calc target position in optical axis coordinate.  
	Optical axis correction matrix is specified by a environtal parameter
	"xrtmatrix". If not set, default file "xrtrev.tran" is used.
  AT&T ver1.2   93.07.07  C.Otani
        Filename which includes Transform-matrix is read
	from a file (adFileList) whose name is specified by the environment
	variable "ASTDFIL".
	A bug included in "target" command was fixed.
  AT&T ver1.31   93.08.07  C.Otani
        Added formats of command-input for "time" and Euler Angle.
	See help for "time" and "euler" or "ea".
  AT&T ver1.31k   93.11.07 K.Hayashida
        sun_d_cone, sun_d_pahse was added to adust the definition 
        of sun-cone & sun-phase In ASTRO-D QL 
  AT&T ver1.32    96.10.09 C.Otani
        Added command line options "-e", "-t" and "-q"
  AT&T ver1.33    96.12.24 T.Kotani
        fixed a bug in the TARGET process. R.A.>=23h0m0s is now acceptable.
  AT&T ver1.4   1999-12-29 R.Fujimoto
        Y2K modification, added ascatime.
	Chaned the name time and ctime to attTime and attCTime.
  AT&T ver1.41  2000-01-04 R.Fujimoto
        Reflected KSC's latest version (1.32 and 1.33)
  AT&T ver1.42  2005-07-21 R.Fujimoto
        Revision of attmsg.h for compiling on Linux gcc-3.3 
  AT&T ver2.0   2005-07-21 R.Fujimoto
        Astro-E2 version
*/
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <atFunctions.h>
#include <atError.h>
#include <aste_time.h>		/* added by R.Fujimoto */
#include <curses.h>
#include "attmsgs.h"

#include "aeFilenameGet.c"   /*   added by C.Otani */

#define  MJD2000    51545.0
#define  MJD1950    33282.0
#define  SOL        2.99793458e10
#define  XRT_MATRIX  "xrtrev.tran"

#define	NAXIS	7
#define	NTRGT	6
#define NNAME   20
typedef struct {
    char   name[NNAME];	 /* target name */
    double az,el;	 /* satellite coodinate (deg) */
    double x,y;	 	/* satellite coodinate (arcmin) */
    double ra,dec;	 /* R.A. Dec (deg) 2000 */
    double gl,gb;	 /* galactic coodinate (deg) */
}   Direction;

Direction   sat_axis[NAXIS]={
	" +X",	 0.0,   0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
	" +Y",  90.0,   0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
	" +Z",	 0.0,  90.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
	" -X", 180.0,   0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
	" -Y", -90.0,   0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
	" -Z",   0.0, -90.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
	"XRT",   0.0,  90.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
};

Direction   sun_d={
        "sun",  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
};
double  sun_angle;
/* added by K.Hayashida 1993.11.7 */
double sun_d_phase=0.0;
double sun_d_cone=0.0;

int ntarget=0;
Direction   targets[NTRGT]={
        "   ",  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
        "   ",  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
        "   ",  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
        "   ",  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
        "   ",  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
        "   ",  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
};

AtRotMat    matrix;
AtEulerAng  euler={ 0.0, 0.0, 0.0 };
AtQuat      qpara;

AtTime      attTime={ 2005, 7, 10, 3, 30, 0, 0.0 };
char        attCTime[27];
double      mjd;
double      astroe_t;
double      geocen_t;
double      barycen_t;

int         orbit_mode=0;
char*       orbit_file;
char*       orbit_env="orbit";

AtRotMat    xrtRM={1.,0.,0., 0.,1.,0., 0.,0.,1.};

/*   changed by C.Otani   */
char        xrtmat_file[500];
/*
char*       xrtmat_file;
char*       xrtmat_env="xrtmatrix";
*/

char	lineBuffer[81];

#define	ESC 0x1b
#define CR  0x0d
#define DEL 0x7f
#define BS  0x08
#define TOP     0
#define BOTTOM 22

#define MAX_ARG  10
#define MAX_ARGL 80

int cmd_get( char ** );
void display();
void angle( int, char ** );
void convert( int, char ** );
void help( int, char ** );
void set_euler( int, char ** );
void set_qpara( int, char ** );
void set_time( int,  char ** );
void set_target( int, char ** );
void rot_sat( int, char ** );
void err_msg( int, char ** );
void cal_direction();
void cal_sun();
void cal_time();
int  read_pv( char *, char *, AtVect );
void rot_mat( int, double, AtRotMat );
void ad2lb( double, double, double *, double *);
void lb2ad( double, double, double *, double *);
char *strupr(char * );

int
main(int argc0, char *argv0[])
{
  char *argv[MAX_ARG];
  int  i,argc;
  FILE *fp;
  double r;
  AtRotMat rm;

/* comamnd line treatment by C.Otani 96/10/9 */
  if ( argc0 > 1 ){
    i = 1;
	while ( argc0 > i ){
	  if      ( strcmp(argv0[i],"-e") == 0 ){
	    euler.phi   = atof(argv0[++i])*DEG2RAD;
		euler.theta = atof(argv0[++i])*DEG2RAD;
		euler.psi   = atof(argv0[++i])*DEG2RAD;
	  }
	  else if ( strcmp(argv0[i],"-q") == 0 ){
	    qpara[0] = atof(argv0[++i]);
	    qpara[1] = atof(argv0[++i]);
	    qpara[2] = atof(argv0[++i]);
	    qpara[3] = atof(argv0[++i]);
		atQuatToRM(qpara,rm);
		atRMToEuler(rm,&euler);
	  }
	  else if ( strcmp(argv0[i],"-t") == 0 ){
	    attTime.yr = atoi(argv0[++i]);
		if ( attTime.yr <= 10 ) {
		  attTime.yr += 2000;
		} else if ( attTime.yr < 100 ) {
		  attTime.yr += 1900;
		}
		attTime.mo = atoi(argv0[++i]);
		attTime.dy = atoi(argv0[++i]);
		attTime.hr = atoi(argv0[++i]);
		attTime.mn = atoi(argv0[++i]);
		attTime.sc = atoi(argv0[++i]);
		attTime.ms = 0.00;
	  }
	  else {
		fprintf(stderr,"Usage: att [-e phi theta psi]\n");
		fprintf(stderr,"           [-t YY MM DD hh mm ss]\n");
		fprintf(stderr,"           [-q qp1 qp2 qp3 qp4]\n");
		exit(-1);
	  }
	  i++;
    }
  }

/* allocate arguments */
  for( i=0; i<MAX_ARG; i++){
    argv[i]=( char *) malloc( MAX_ARGL );
    if ( argv[i] == NULL) {
      printf( "%s", err_memory ); 
      exit(1);
    }
  }

/* get orbit file name from enviroment variable */
  orbit_file=getenv( orbit_env );
  if ( orbit_file != NULL ) {
    if ( ( fp=fopen( orbit_file, "r" ) ) != NULL ) {
      orbit_mode=1;
      fclose( fp );
    }
  }

/* get XRT trans matrix file name from enviroment variable   N.K. 93.04.25*/
/*   revised by C.Otani 93.07.07 */
/*
    xrtmat_file=getenv( xrtmat_env );
    if ( xrtmat_file == NULL ) xrtmat_file = XRT_MATRIX;
*/
  if ( aeFilenameGet("alignment","xrt","matrix",xrtmat_file) != 0 ){
    sprintf(xrtmat_file,XRT_MATRIX);
  }
  if ( ( fp=fopen( xrtmat_file, "r" ) ) != NULL ) {
    for (i=0; i<9; i++) {
      do {
	fgets(lineBuffer, 80, fp);
      } while(sscanf(lineBuffer, "%lf", &xrtRM[0][0]+i) !=1);
    }
    fclose( fp );
    atVectToPolDeg(xrtRM[2],
		   &r, &sat_axis[NAXIS-1].az, &sat_axis[NAXIS-1].el);
  }

/* initialize screen */
  initscr();
  noecho();
  crmode();
  nonl();

/* initialize euler angle and time */
  atEulerToRM( &euler, matrix );
  atRMToQuat( matrix, qpara );
  cal_direction();
  cal_time();
  cal_sun();
  display();



/* command mode */
  while( 1 ) {
    argc = cmd_get( argv );
    if     ( argc == -1 ) {
      display();
    }
    else if ( strncmp( argv[0], "ANGLE", 1 ) == 0 ) {
      angle( argc, argv );
    }
    else if ( strncmp( argv[0], "CONVERT", 1 ) == 0 ) {
      convert( argc, argv );
    }
    else if ( strncmp( argv[0], "HELP", 1 ) == 0 ) {
      move( TOP,0 );
      clrtobot();
      help( argc, argv );
      refresh();
    }
    else if ( strncmp( argv[0], "END", 2 )== 0 ) {
      break;
    }
    else if ( strncmp( argv[0], "EULER", 2 ) == 0 ||
	      strncmp( argv[0], "EA",    2 ) == 0 ) {
      set_euler( argc, argv );
    }
    else if( strncmp( argv[0], "EXIT", 2 ) == 0 ) {
      break;
    }
    else if ( strncmp( argv[0], "QPARA", 2 )== 0 ) {
      set_qpara( argc, argv );
    }
    else if ( strncmp( argv[0], "QUIT", 2 ) == 0 ) {
      break;
    }
    else if ( strncmp( argv[0], "ROTATE", 1 ) == 0 ) {
      rot_sat( argc, argv );
    }
    else if( strncmp( argv[0], "TIME", 2 ) == 0 ) {
      set_time( argc, argv );
    }
    else if( strncmp( argv[0], "TARGET", 2 ) == 0 ) {
      set_target( argc, argv );
    }
  }

/* terminate program */
  endwin();
  printf("\natt-program normal end \n");
  return 0;
}

int cmd_get( char *argv[] )
{
  char chr,cmd[256],*chp,argc;
  int  i,n;

  move( BOTTOM,0 );
  clrtobot();
  addstr( prompt);
  refresh();

  chp=cmd;

  n=0;

  while((chr=getch()) != CR ) {
    if(chr== DEL || chr ==BS) {
      if(n>0){
 	n--;
 	chp--;
      }
      move(BOTTOM,strlen( prompt )+n);
      delch();
    }
    else if ( (chr>=32) && (chr<=126)) {
      move(BOTTOM,strlen( prompt )+n);
      addch(chr);
      n++;
      *chp++=chr;
    }
    refresh();
  }
  *chp = 0x00;
  strupr(cmd);

  for( i=0; i<MAX_ARG; i++) {
    *argv[i]=0x00;
  }
  argc=sscanf( cmd, "%s %s %s %s %s %s %s",
	      argv[0],argv[1],argv[2],argv[3],argv[4],argv[5],argv[6]);
  return(argc);
}

/*  display  */
void display( ) {
  int i;

  move(TOP,0);
  clrtobot();
  printw("%s", version );

  move(1,0);  printw("------------  Attitude  ------------");
  move(2,0);  printw("Qparam  %8.6f", qpara[0]);
  move(3,0);  printw("        %8.6f", qpara[1]);
  move(4,0);  printw("        %8.6f", qpara[2]);
  move(5,0);  printw("        %8.6f", qpara[3]);
  move(6,0);  printw("Eul ( %9.5f %9.5f %9.5f )",
          euler.phi*RAD2DEG,euler.theta*RAD2DEG,euler.psi*RAD2DEG);

  move(0,40);  printw("------------    Axis    ------------");
  move(1,40);  printw("      alpha   delta       l       b");
  for( i=0; i<NAXIS; i++) {
    move(2+i,40);  printw("%3s (%7.3f,%7.3f) (%7.3f,%7.3f)",
			  sat_axis[i].name,
			  sat_axis[i].ra,sat_axis[i].dec,
			  sat_axis[i].gl,sat_axis[i].gb );
  }

  /* modified by R.Fujimoto for YYYY and ascatime. */
  move(7,0);  printw("------------    Time    ------------");
  move(8,0);  printw("UT      :%27s", attCTime );
  move(9,0);  printw("MJD     : %16.6lf", mjd );
  /*  move(10,0);  printw("ADT     : %16.6lf", astroe_t );*/
  move(10,0);  printw("ASTETIME: %16.6lf", astroe_t );
  if ( orbit_mode == 1 ) {
    move(11,0);  printw("GeoCen  : %12.6f", geocen_t);
    move(12,0);  printw("BaryCen : %12.6f", barycen_t);
  }

  move( 9,40); printw("------------    Sun     ------------");
  move(10,40); printw("Cone =%7.3f Phase =%7.3f", sun_d_cone, sun_d_phase );
  move(11,40); printw("      alpha   delta     azi     elv");
  move(12,40); printw("    (%7.3f,%7.3f) (%7.3f,%7.3f)",
                          sun_d.ra, sun_d.dec, sun_d.az, sun_d.el );

  move(13,0);  printw("----------------------------------  Targets   ----------------------------------");
  move(14,0);  printw("   Name            azi  distance   x(') y(')   alpha   delta       l       b");
  for( i=0; i<ntarget; i++){
    move(15+i,0);  printw("%2d %-13s (%6.2f,%7.3f/%5.1f,%5.1f) (%7.3f,%7.3f)(%7.3f,%7.3f)",
			  i+1,  targets[i].name,
			  targets[i].az,90.0-targets[i].el,
			  targets[i].x,targets[i].y,
			  targets[i].ra,targets[i].dec,
			  targets[i].gl,targets[i].gb );
  }
  refresh();
}

void angle( int argc, char *argv[] )
{
  int i,err_cnt;
  AtVect  x,y,z,w;
  double       r,angle,ra,dec;

  err_cnt=0;

  move(TOP,0);
  clrtobot();

  err_cnt += read_pv( argv[1], argv[2], x );
  err_cnt += read_pv( argv[3], argv[4], y );

  if ( err_cnt == 0 ) {
    atAngDistance( x, y, &angle );
    atVectProd( x, y, z);
    atInvVect( z, w );

    move(1,0);  printw( "  Angle between two vector ");
    atVectToPolDeg( x, &r, &ra, &dec );
    move(3,0);  printw( "    vect 1  ( %7.3lf %7.3lf)", ra ,dec );
    atVectToPolDeg( y, &r, &ra, &dec );
    move(4,0);  printw( "    vect 2  ( %7.3lf %7.3lf)", ra, dec );
    move(6,0);  printw( "    angle= %7.3lf", angle*RAD2DEG );
 
    move(8,0);  printw( "  Vertical Vector" );
    atVectToPolDeg( z, &r, &ra, &dec );
    move(9,0);  printw( "    vect A  ( %7.3lf %7.3lf)", ra, dec );
    atVectToPolDeg( w, &r, &ra, &dec );
    move(10,0);  printw( "    vect B  ( %7.3lf %7.3lf)", ra, dec );
  }
  else {
    err_msg(argc, argv);
  }
  refresh();
}

void convert( int argc, char *argv[] )
{
  int          mode, err_cnt;
  AtVect       x,r2000,r1950;
  double  ra1,ra2,dec1,dec2,gl,gb,alpha,delta,r;

  err_cnt = 0;
  move(TOP,0);
  clrtobot();

  if ( argc == 3 ) {
    err_cnt += read_pv( argv[1],argv[2], x);
    mode=1;
  }
  else if ( argc == 4 ) {
    if ( strncmp( argv[1], "G", 1 ) == 0 ) { 
      mode=3;
    }
    else if ( strncmp( argv[1], "E1", 2) == 0 ) {
      mode=2;
    }
    else if ( strncmp( argv[1], "E2", 2) == 0 ) {
      mode=1;
    }
    else {
      err_cnt++;
      mode=0;
    }
    err_cnt += read_pv( argv[2], argv[3], x );
  }
  else {
    err_cnt++;
  }

  if ( err_cnt == 0 ) {
    atVectToPolDeg(x, &r, &alpha, &delta);
    if ( mode == 1 ) {
      ra2=alpha;
      dec2=delta;
      atPrecession( MJD2000, x, MJD1950, r1950 ); 
      atVectToPolDeg( r1950, &r, &ra1, &dec1 );
      ad2lb( ra2, dec2, &gl, &gb );
    }
    else if ( mode == 2 ) {
      ra1=alpha;
      dec1=delta;
      atPrecession( MJD1950, x, MJD2000, r2000 );
      atVectToPolDeg( r2000, &r, &ra2, &dec2 );
      ad2lb( ra2, dec2, &gl, &gb );
    }
    else if ( mode == 3 ) {
      gl=alpha;
      gb=delta;
      lb2ad( gl, gb, &ra2, &dec2 );
      atPolDegToVect( 1.0, ra2, dec2, r2000 );
      atPrecession( MJD2000, r2000, MJD1950, r1950 );
      atVectToPolDeg( r1950, &r, &ra1, &dec1 );
    }

    move(0,0); printw( "  Coordinate convert ");
    move(2,0); printw( "  input vector :");
    if ( mode == 1 )   printw( "  Equatorial Coordinate (Epoch 2000)" );
    if ( mode == 2 )   printw( "  Equatorial Coordinate (Epoch 1950)" );
    if ( mode == 3 )   printw( "  Galactic Coordinate" );
    move(3,0);  printw( "  (  %10.5lf   %10.5lf )", alpha, delta );

    move(5,0);  printw( "  Equatorial Coordinate (Epoch 1950)" );
    move(6,0);  printw( "  (  %10.5lf   %10.5lf )", ra1, dec1 );
    move(8,0);  printw( "  Equatorial Coordinate (Epoch 2000)" );
    move(9,0);  printw( "  (  %10.5lf   %10.5lf )", ra2, dec2 );
    move(11,0); printw("   Galactic Coordinate");
    move(12,0); printw( "  (  %10.5lf   %10.5lf )", gl, gb );
  }
  else {
    err_msg( argc, argv);
  }
  refresh();
}

void help( int argc, char *argv[] )
{ 
  if ( argc != 2 ) {
    printw("%s",help_all);
  }
  else if ( strncmp( argv[1], "ANGLE", 1)==0) {
    printw("%s",help_angle);
  }
  else if ( strncmp( argv[1], "CONVERT", 1)==0) {
    printw("%s",help_convert);
  }
  else if ( strncmp( argv[1], "END", 2)==0) {
    printw("%s", help_end);
  }
  else if ( strncmp( argv[1], "EULER", 2)==0 ||
	    strncmp( argv[1], "EA",    2)==0) {
    printw("%s", help_euler);
  }
  else if ( strncmp( argv[1], "HELP", 1)==0) {
    printw("%s", help_help);
  }
  else if ( strncmp( argv[1], "QPARA", 1)==0) {
    printw("%s", help_qpara);
  }
  else if ( strncmp( argv[1], "ROTATE", 1 ) == 0 ) {
    printw("%s", help_rotate);
  }
  else if ( strncmp( argv[1], "TARGET", 2)==0) {
    printw("%s", help_target);
  }
  else if ( strncmp( argv[1], "TIME", 2)==0) {
    printw("%s", help_time);
  }
}

void set_euler( int argc, char *argv[] )
{
  int i, errcnt;
  double eu[3];
  
  errcnt=0;
  if ( argc == 4 ) {
    for( i=0; i<3; i++) {
      eu[i]=atof( argv[i+1] );
      if ( ( eu[i] < -360.0) || ( eu[i] > 360.0 ) ) errcnt++;
    }
  } else {
    errcnt++;
  }
  if ( errcnt == 0 ) {
    euler.phi=eu[0]*DEG2RAD;
    euler.theta=eu[1]*DEG2RAD;
    euler.psi=eu[2]*DEG2RAD;
    atEulerToRM( &euler, matrix );
    atRMToQuat( matrix, qpara );

    cal_direction();
    cal_sun();
    display();
  }
  else {
    err_msg( argc, argv);
  }
  refresh();
}

void set_qpara( int argc, char *argv[] )
{
  int i, err_cnt;
  double q[4];
  
  err_cnt=0;
  if ( argc == 5 ) {
    for( i=0; i<4; i++) {
      q[i]=atof( argv[i+1] );
      if ( ( q[i] < -1.0) || ( q[i] > 1.0 ) ) err_cnt++;
    }
  } else {
    err_cnt++;
  }
  if ( err_cnt == 0 ) {
    for( i=0; i<4; i++ ) {
      qpara[i]=q[i];
    }
    atQuatToRM( qpara, matrix );
    atRMToEuler( matrix, &euler );
    atRMToQuat( matrix, qpara );
    cal_direction();
    cal_sun();
    display();
  }
  else {
    err_msg( argc, argv);
  }
  refresh();
}

void set_time( int argc, char *argv[] )
{
  int     i,err_cnt;
  int     yr,mo,dy,hr,mn;
  double  sc;


  err_cnt=0;

  if ( argc == 3 ) {
    if ( sscanf( argv[1],"%d/%d/%d",&yr,&mo,&dy) !=3 ) err_cnt++;
    /* modified by R.Fujimoto 1999-12-29 */
    /*    if (( yr < 0 ) || (yr > 99 )) err_cnt++;*/
    if (( yr < 0 )) err_cnt++;
    if (( mo < 1 ) || (mo > 12 )) err_cnt++;
    if (( dy < 1 ) || (dy > 31 )) err_cnt++;
    if ( sscanf( argv[2],"%d:%d:%lf",&hr,&mn,&sc ) !=3 ) err_cnt++; 
    if (( hr < 0 ) || (hr > 23 )) err_cnt++;
    if (( mn < 0 ) || (mn > 59 )) err_cnt++;
    if (( sc < 0.0) || ( sc >=60 )) err_cnt++;
  }
  else if ( argc == 7 ){
    /* modified by R.Fujimoto 1999-12-29 */
    /*    if ( ( yr = atoi(argv[1]) ) < 0   || yr > 99 )   err_cnt++;*/
    if ( ( yr = atoi(argv[1]) ) < 0 )   err_cnt++;
    if ( ( mo = atoi(argv[2]) ) < 1   || mo > 12 )   err_cnt++;
    if ( ( dy = atoi(argv[3]) ) < 1   || dy > 31 )   err_cnt++;
    if ( ( hr = atoi(argv[4]) ) < 0   || hr > 23 )   err_cnt++;
    if ( ( mn = atoi(argv[5]) ) < 0   || mn > 59 )   err_cnt++;
    if ( ( sc = atof(argv[6]) ) < 0.0 || sc >=60.0 ) err_cnt++;
  }
  else {
    err_cnt++;
  }
  if ( err_cnt == 0 ) {
    /* modified by R.Fujimoto 1999-12-29 */
    if (yr <= 10) {
      attTime.yr = yr + 2000;
    } else if (yr < 100) {
      attTime.yr = yr + 1900;
    } else {
      attTime.yr = yr;
    }
    attTime.mo=mo;
    attTime.dy=dy;
    attTime.hr=hr;
    attTime.mn=mn;
    attTime.sc= (int) sc;
    attTime.ms= (float)( sc - attTime.sc )*1000.0;
    cal_time();
    cal_sun();
    display();
  }
  else {
    err_msg( argc, argv);
  }
  refresh();
}

void set_target( int argc, char *argv[] )
{
  int err_cnt,mode,id,i;
  AtVect  x,y;
  double  r,ra,dec,gl,gb;

  mode=0;
  id=0;
  err_cnt=0;

  if ( ( argc < 4 ) || ( argc > 6 ) ) err_cnt++;

  for( i=1; i< argc-3; i++){
    if ( strncmp( argv[i], "G",  1 ) == 0 ) mode=2;
    else if ( strncmp( argv[i], "E1", 2 ) == 0 ) mode=1;
    else if ( strncmp( argv[i], "E2", 2 ) == 0 ) mode=0;
    else if ( strcmp( argv[i], "1"  ) == 0 ) id=1;
    else if ( strcmp( argv[i], "2"  ) == 0 ) id=2;
    else if ( strcmp( argv[i], "3"  ) == 0 ) id=3;
    else if ( strcmp( argv[i], "4"  ) == 0 ) id=4;
    else if ( strcmp( argv[i], "5"  ) == 0 ) id=5;
    else if ( strcmp( argv[i], "6"  ) == 0 ) id=6;
    else err_cnt++;
  }
  if ( argc > 1 ){   /*   if-sentense is added by C.Otani */
    err_cnt += read_pv( argv[argc-2],argv[argc-1], x );
  }

  if ( err_cnt == 0 ) { 

    if ( mode == 0 ) {
      atVectToPolDeg( x, &r, &ra, &dec );
      ad2lb( ra, dec, &gl, &gb );
    }
    else if ( mode == 1 ) {
      atPrecession(MJD1950, x, MJD2000, y);
      atVectToPolDeg( y, &r, &ra, &dec );
      ad2lb( ra, dec, &gl, &gb );
    }
    else if ( mode == 2 ) {
      atVectToPolDeg( x, &r, &gl, &gb );
      lb2ad( gl, gb, &ra, &dec );
    }

    if ( id == 0 ) {
      if ( ntarget < NTRGT ) {
	ntarget++;
	id=ntarget;
      }
      else {
	id=NTRGT;
      }
    }
    if ( id > ntarget ) ntarget=id;

    strncpy( targets[id-1].name, argv[argc-3], NNAME );
    targets[id-1].ra=ra;
    targets[id-1].dec=dec;
    targets[id-1].gl=gl;
    targets[id-1].gb=gb;

    cal_direction();
    display();

  }
  else {
    err_msg( argc, argv );
  }
  refresh();
}

void rot_sat( int argc, char *argv[] )
{
  int     err_cnt,axis;
  double  angle;

  axis=0;
  err_cnt=0;


  if ( argc == 3 ) {
    if ( strncmp( argv[1], "X", 1 ) == 0 ) {
      axis=0;
    }
    else if ( strncmp( argv[1], "Y", 1 ) == 0 ) {
      axis=1;
    }
    else if ( strncmp( argv[1], "Z", 1 ) == 0 ) {
      axis=2;
    }
    else {
      err_cnt++;
    }
    angle=atof( argv[2] );
    if (( angle < -360.0 ) || ( angle > 360.0 )) err_cnt++;    
  }
  else {
    err_cnt++;
  }

  if ( err_cnt == 0 ) {
    rot_mat( axis, angle, matrix );
    atRMToEuler(matrix, &euler);
    atRMToQuat( matrix, qpara );
    cal_direction();
    cal_sun();
    display();
  }
  else {
    err_msg(argc, argv);
  }
  refresh();
}

void err_msg(int argc, char * argv[] )
{
  int i;

  move( TOP,0 );
  clrtobot();
  printw( "Syntax Error >> ");
  for(i=0; i< argc; i++) {
    printw("%s ", argv[i] );
  }
  argc=2;
  strcpy(argv[1], argv[0]);
  help( argc, argv );
}

void cal_direction()
{
  int i;
  AtVect x,y;
  double r;
  AtRotMat  rmatrix;

  atInvRotMat( matrix, rmatrix );

  for(i=0;i<NAXIS;i++) {
    atPolDegToVect( 1.0, sat_axis[i].az, sat_axis[i].el, x );
    atRotVect( rmatrix, x, y);
    atVectToPolDeg( y, &r, &sat_axis[i].ra, &sat_axis[i].dec );
    ad2lb( sat_axis[i].ra, sat_axis[i].dec,
	  &sat_axis[i].gl, &sat_axis[i].gb );
  }
  for(i=0;i<ntarget;i++) {
    atPolDegToVect( 1.0, targets[i].ra, targets[i].dec, x );
    atRotVect( matrix, x, y);
    atRotVect( xrtRM, y, x);	/* XRT optical axis  93.04.25 N.K. */
    atVectToPolDeg( x, &r, &targets[i].az, &targets[i].el );
    targets[i].x = x[0] * RAD2DEG * 60.;
    targets[i].y = x[1] * RAD2DEG * 60.;
  }
}

void cal_sun()
{
  AtVect x,y,z,yy;
  double r;
/* added by K.Hayashida 1993.11.7 */
  AtRotMat zax2yaxRM;
  AtVect xaxis={1.0,0.0,0.0};

  atSun( mjd, x );
  atVectToPolDeg( x, &r, &sun_d.ra,  &sun_d.dec );
  ad2lb( sun_d.ra, sun_d.dec, &sun_d.gl, &sun_d.gb );
  atRotVect( matrix, x, y );
  atVectToPolDeg( y, &r, &sun_d.az, &sun_d.el );
  atPolDegToVect( 1.0, sat_axis[1].az, sat_axis[1].el, z );
  atAngDistance( y, z, &r ); 
  sun_angle=r*RAD2DEG;

/* added by K.Hayashida 1993.11.7 
   sun_d_cone ... angle between the sun and the sat. Y-axis  
   sun_d_phase ... phase angle of the sun around Y-axis from Y-Z plane */
  atSetRotMat(xaxis, -0.5*(PI), zax2yaxRM);
  atRotVect( zax2yaxRM, y, yy );
  atVectToPolDeg( yy, &r, &sun_d_phase, &sun_d_cone );
  sun_d_phase = sun_d_phase + 90.0;
  if( sun_d_phase > 360.0) { sun_d_phase = sun_d_phase - 360.0; }
  sun_d_cone = 90.0- sun_d_cone;


}

void cal_time()
{
  AtVect x,y;

  atCTime2( &attTime, attCTime );
  atMJulian( &attTime, &mjd );
/*  atTDT( mjd, &astroe_t );  */
  astroe_t = mjd2aste(mjd);	/* added by R.Fujimoto */
  if ( orbit_mode == 1 ) {
    atSatPos( mjd, x );
/*    atBarycentric( mjd, x, y );  
    geocen_t=
    barycen_t=
*/
  }
}

int read_pv( char *str1, char *str2, AtVect x )
{
  int              i,err_cnt;
  AtRightAscension ra;
  AtDeclination    dec;
  int              dummy;
  double           alpha,delta;

  err_cnt=0;

  if ( strchr(str1,'H') == NULL ) {
    alpha=atof( str1 );
    if ( ( alpha <= -360.0 ) || ( alpha >= 360.0 ) ) err_cnt++;
  } else {
    i=sscanf( str1, "%uH%uM%lf", &ra.hour, &ra.min, &ra.sec );
    if ( i != 3 ) err_cnt++;
    if ( ( ra.hour < 0 ) || ( ra.hour >=24 ) ) err_cnt++;
    if ( ( ra.min < 0 ) || ( ra.min >= 60 ) ) err_cnt++;
    if ( ( ra.sec < 0 ) || ( ra.sec >=60 ) ) err_cnt++;
    alpha= atRAToRadian( ra ) * RAD2DEG;
  }

  if ( strchr(str2, 'D') == NULL ) {
    delta=atof( str2 );
    if ( ( delta < -90.0) || ( delta > 90.0 ) ) err_cnt++;
  } else {
/*    i=sscanf( str2, "%dD%uM%lf", &dec.deg, &dec.min, &dec.sec );   */
    i=sscanf( str2, "%dD%uM%lf", &dummy, &dec.min, &dec.sec );
    if ( i != 3 ) err_cnt++;
/*    if ( ( dec.deg < -90 ) || ( dec.deg > 90 ) ) err_cnt++;   */
    if ( ( dummy < -90 ) || ( dummy > 90 ) ) err_cnt++;
    if (strchr(str2, '-') != NULL) dec.sign = -1;
    else dec.sign =1;
    dec.deg = abs(dummy);
    if ( ( dec.min < 0 ) || ( dec.min >= 60 ) ) err_cnt++;
    if ( ( dec.sec < 0 ) || ( dec.sec >=60 ) ) err_cnt++;
    delta= atDecToRadian( dec ) * RAD2DEG;
  }
  
  atPolDegToVect( 1.0, alpha, delta, x );

  return( err_cnt );
}

char *strupr(char *cp)
{
  int n,i,m;
  char *cpp;

  n = strlen(cp);
  cpp=cp;

  m=0;
  while( *cpp == ' ' ) {
    m++;
    cpp++;
  }

  cpp=cp;
  for(i=m; i<n; i++){
    *cpp = toupper(*(cpp+m));
    cpp++;
  }
  return(cp);
}

void rot_mat( int axis, double angle, AtRotMat matrix  )
{
  int        i,j,n;
  AtRotMat   mat1,mat2;
  AtVect     xyz[3]={  1.0, 0.0, 0.0,
		       0.0, 1.0, 0.0,
		       0.0, 0.0, 1.0 };
  angle=angle*DEG2RAD;
  atSetRotMat( xyz[axis], angle, mat1 );
  for( i=0; i<3; i++){
    for( j=0; j<3; j++){
      mat2[i][j]=mat1[i][0]*matrix[0][j]
	+mat1[i][1]*matrix[1][j]+mat1[i][2]*matrix[2][j];
    }
  }
  for( i=0; i<3; i++){
    for( j=0; j<3; j++){
      matrix[i][j]=mat2[i][j];
    }
  }
}

/* convert R.A. Dec. (2000) coodinate to galactic coodinate */
void ad2lb( double ra, double dec, double *gl, double *gb )
{
    AtPolarVect x,y;
    AtEulerAng eu= { (  192.85*DEG2RAD ),
                     (  62.86666666666667*DEG2RAD ),
                     (  57.06666666666667*DEG2RAD ) };

    x.r=1.0;
    x.lon=ra*DEG2RAD;
    x.lat=dec*DEG2RAD;
    atRotPVect( &eu, &x, &y );
    *gl=y.lon*RAD2DEG;
    *gb=y.lat*RAD2DEG;
}
/* convert galactic coodinate to R.A. Dec. (2000) coodinate */
void lb2ad( double gl, double gb, double *ra, double *dec )
{
    AtPolarVect x,y;
    AtEulerAng eu={ ( -57.06666666666667*DEG2RAD ),
                    ( -62.86666666666667*DEG2RAD ),
                    (-192.85*DEG2RAD ) };

    x.r=1.0;
    x.lon=gl*DEG2RAD;
    x.lat=gb*DEG2RAD;
    atRotPVect( &eu, &x, &y );
    *ra=y.lon*RAD2DEG;
    *dec=y.lat*RAD2DEG;
}












