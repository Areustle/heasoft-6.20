/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/ofaintdfe/oreadTempPara.c,v 3.8 2000/09/14 14:34:57 peachey Exp $   */
/*                   */
/*
   readTempPara5.41.c
*/

int readTempPara( tblfile )
char *tblfile ;
{
  FILE *fp;
  char str[4096];
  int i, j;
  int sensor, CCD;
  double time0;


/*   Open parameter table file   */
  if ( ( fp = fopen(tblfile,"r") ) == NULL ){
    fprintf(stderr,"readTempPara5.41: Parameter File Open Error (%s) !!!\n",
	    tblfile);
    return -1;
  }


/*   Reading data   */
  i = j = 0;
  while ( fgets(str,4096,fp) != NULL ){
    if ( strncmp(str,"!",1) ){
      if ( sscanf(str,"%d%d%lf%lf%lf%lf%lf%lf%lf%lf",&sensor,&CCD,
		  &tf0[i][j], &Tf[i][j],  &ss1[i][j], &ss0[i][j],
		  &QQ1[i][j], &QQ0[i][j], &qq1[i][j], &qq0[i][j]) == 10 ){
	if ( sensor != i ){
	  fprintf(stderr,"readTempPara5.41: Invalid Detector Number (%d).\n",sensor);
	  return -1;
	}
	if ( !( j==0 && CCD==1 ) && !( j==1 && CCD==2 ) && !(j==2 && CCD==4 ) ){
	  fprintf(stderr,"readTempPara5.41: \"%d-CCD\" mode is invalid. Check the Parameter Table.\n",CCD);
	  return -1;
	}
	if ( ++j >= 3 ){
	  i++;
	  j=0;
	}
	if ( i>=2 )   break;
      }
      else if ( sscanf(str,"%lf",&time0) == 1 ){
	ASCA_t0 = time0;
      }
      else{
	fprintf(stderr,"readTempPara5.41: Invalid Format in the Table File (%s).\n",tblfile);
	return -1;
      }
    }
  }
  fclose(fp);


/*   Debug   */
/*
  fprintf(stderr,"time = %le\n",ASCA_t0);
  for (i=0;i<2;i++){
    for (j=0;j<3;j++){
      fprintf(stderr,"%d %d  %.4le %.4le   %.4le %.4lf   %.4le %7.4lf   %.4le %.5lf\n",i, j,
	      tf0[i][j], Tf[i][j],  ss1[i][j], ss0[i][j],
	      QQ1[i][j], QQ0[i][j], qq1[i][j], qq0[i][j]);
    }
  }
*/

  return 0;
}
