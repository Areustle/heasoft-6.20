/* 
   Function:     memsort()
   Description:  build an index to be used in sorting a FITS table
   Author/Date:  J.K.Blackburn
   Modification: Peter D. Wilson/Nov.97
                  Added multi-column sorting and unique (row purging) option
                 Peter D. Wilson/Aug.98
                  Crash caused by heap sorting table with one row fixed
                 Peter D Wilson/Jan 99
                  Crash caused by bad FitsStrBufLen value for ASCII columns fxd

   Usage: memsort(iu,ou,width,repeat,nrows,ncols,cols,dtype,method,
                  ascend,ld2mem,unique,ftstatus)

   Arguments:    iu       - input unit number
                 ou       - output unit number
                 width    - row width in characters
                 repeat   - width of a character string
                 nrows    - number of rows in table
                 ncols    - number of columns to sort in table
                 cols     - column numbers of columns to sort
                 dtype    - column data types
                 method   - sorting method to apply
                 ascend   - sort in ascending or descending order
                 ld2mem   - load entire table into memory for sorting?
                 unique   - purge rows with duplicate keys?
                 ftstatus - status of FITSIO calls that are used
*/

#include <stdio.h>
#include <math.h>
#include "memsort.h"

void memsort(iu,ou,width,repeat,nrows,ncols,cols,dtype,method,
             ascend,ld2mem,unique,ftstatus,modflag)
int iu,ou,width,repeat[],nrows,ncols,cols[],dtype[]
   ,ascend,ld2mem,unique,*ftstatus,modflag[];
char *method;
{
 int anyf,j,r,row,mem_error,col=0;
 char error_msg[80];
 char **ptr;
 double *cpx;
 int *tbl_index;
 int a;
 column dataset[999];
 void sort_logical(),sort_string(),sort_integer(),sort_double(),sort_complex();
 void sort_table(),DoSort(),FindMax();
 mem_error = myFALSE;

 tbl_index = (int*)malloc(nrows*sizeof(int));
 if (tbl_index != NULL)
   {
    for( row=0; row<nrows; row++) tbl_index[row] = row;
    for( col=0; col<ncols && !(mem_error || *ftstatus); col++ ) {
       anyf = myFALSE;
       dataset[col].dtype = dtype[col];
       switch (dtype[col])
       {
        case FITS_LOGICAL:
             if( ColAlloc( nrows, sizeof(int), dataset+col ) )
                FCGCL(iu,cols[col],1,1,nrows,dataset[col].data.iptr,ftstatus);
             else
                mem_error=myTRUE;
             break;
        case FITS_ASCII:
             ptr = (char**)malloc(nrows*sizeof(char*));
             r = repeat[col]+1;
             if( ColAlloc( nrows, r*sizeof(char), dataset+col ) )
               {
		int FitsStrBufLen=repeat[col];
                ptr[0] = (char*)dataset[col].data.sptr;
                dataset[col].data.sptr = ptr;
                for( row=0; row<nrows; row++ )
                   {
                    dataset[col].data.sptr[row] = dataset[col].data.sptr[0]
		                                  +r*row;
                    FCGCFS(iu,cols[col],row+1,1,1,dataset[col].data.sptr[row],
                           dataset[col].undef_flags+row,&anyf,ftstatus);
                   }
               }
             else
               {
                mem_error = myTRUE;
                free(ptr);
               }
             break;
        case FITS_BIT:
        case FITS_BYTE:
        case FITS_SHORT:
        case FITS_LONG:
             if( ColAlloc( nrows, sizeof(int), dataset+col ) )
                FCGCFJ(iu,cols[col],1,1,nrows,dataset[col].data.iptr,
                       dataset[col].undef_flags,&anyf,ftstatus);
             else
                mem_error = myTRUE;
             break;
        case FITS_SINGLE:
        case FITS_DOUBLE:
                if( ColAlloc( nrows, sizeof(double), dataset+col ) )
                   FCGCFD(iu,cols[col],1,1,nrows,dataset[col].data.dptr,
                          dataset[col].undef_flags,&anyf,ftstatus);
                else
                   mem_error = myTRUE;
             break;
        case FITS_COMPLEX:
        case FITS_DCOMPLEX:
                if( ColAlloc( nrows*2, sizeof(double), dataset+col ) )
                  {
                     cpx = (double *)dataset[col].data.cptr;
                     FCGCFD(iu,cols[col],1,1,nrows*2,cpx,
                            dataset[col].undef_flags,&anyf,ftstatus);
                     if( anyf )
			for( j=0;j<nrows;j++ )
			   if( dataset[col].undef_flags[j+j]
			       || dataset[col].undef_flags[j+j+1] )
			      dataset[col].undef_flags[j]=1;
                  }
                else
                  mem_error = myTRUE;
             break;
       default:
             strcpy(error_msg,"Unsupported column type.");
             Fcerr(error_msg);
             *ftstatus=1000;
             break;
       }
      }
   }
 else /* failed to allocate memory for table index */
    mem_error = myTRUE;
 if (mem_error)
   {
    strcpy(error_msg,"Unable to allocate memory for data columns");
    Fcerr(error_msg);
    *ftstatus = 1000;
   }
 else if ( *ftstatus )
   {
      if( *ftstatus!=1000 ) {
	 strcpy(error_msg,"FITSIO error reading columns into memory");
	 Fcerr(error_msg);
	 Fcerrm(*ftstatus);
      }
   }
 else
   
    FindMax(nrows,dataset,ncols,modflag);
    DoSort(0,nrows-1,dataset,ncols,tbl_index,method,unique,modflag);

 while( col-- ) { /*  Free up memory of only those columns allocated  */
    if( dtype[col]==FITS_ASCII ) free( dataset[col].data.sptr[0] );
    free( dataset[col].data.iptr );
    free( dataset[col].undef_flags );
 }
 if ( *ftstatus == 0 && !mem_error )
    sort_table(iu,ou,nrows,width,ascend,ld2mem,unique,tbl_index,ftstatus);
 free(tbl_index);
}
FCALLSCSUB14(memsort,MEMSORT,memsort,INT,INT,INT,INTV,INT,INT,INTV,INTV,STRING,LOGICAL,LOGICAL,LOGICAL,PINT,INTV)

int ColAlloc(int nrows, int size, column *col)
{
   col->data.iptr = (int *)malloc(nrows*size);
   col->undef_flags = (int *)calloc(nrows,sizeof(int));
   if( col->data.iptr==NULL || col->undef_flags==NULL ) {
      free(col->data.iptr);
      free(col->undef_flags);
      return(0);
   }
   return(1);
}

void FindMax(int nrows, column * col, int ncols,  int * modflag)
{
	int i,j,k,tlmin,tlmax;
	for (i=0; i<ncols;i++) {
	tlmax=-1;
        tlmin=1;
	switch(col->dtype)
	{
	case FITS_BIT:
        case FITS_BYTE:
        case FITS_SHORT:
        case FITS_LONG:
	if(modflag[i] > 0) {
                j=0;
		while(j<nrows) {
		if(!col->undef_flags[j]) {
			tlmax=col->data.iptr[j];
			tlmin=col->data.iptr[j];
			break;
		}
		j++;
		}
		
		for(k=j+1;k<nrows;k++) {
		if(!col->undef_flags[j]) {
		if(col->data.iptr[k] > tlmax) tlmax=col->data.iptr[k];
		if(col->data.iptr[k] < tlmin) tlmin=col->data.iptr[k];
		}
		}
	modflag[i]=tlmax-tlmin+1;
	}
	break;
	}
	col++;
	}
}

			



	

void DoSort( int start, int end, column *col, int ncols, int *tbl_index,
             char *method, int unique, int* modflag)
{
   int i,j,r;
   void sort_logical(),sort_string(),sort_integer(),sort_double(),
      sort_complex();


   switch( col->dtype )
       {
        case FITS_LOGICAL:
             sort_logical( end-start+1, method, col, tbl_index+start );
             for( i=start; i<end; i=j+1 ) {
                j=i;
                while( col->data.iptr[ tbl_index[i] ]
                       == col->data.iptr[ tbl_index[j+1] ] )
                   if( ++j==end ) break;
                if( j>i ) {
                   if( ncols>1 ) DoSort( i, j, col+1, ncols-1,
                                         tbl_index, method, unique,modflag+1 );
                   else if( unique )
                      for( r=i+1; r<=j; r++ ) tbl_index[r]=-1;
                }
             }
             break;
        case FITS_ASCII:
             sort_string( end-start+1, method, col, tbl_index+start );
             for( i=start; i<end; i=j+1 ) {
                j=i;
                while( !chrcmp( col->data.sptr[ tbl_index[i] ],
                                col->data.sptr[ tbl_index[j+1] ],
                                col->undef_flags[ tbl_index[i] ],
                                col->undef_flags[ tbl_index[j+1] ])  )
                   if( ++j==end ) break;
                if( j>i ) {
                   if( ncols>1 ) DoSort( i, j, col+1, ncols-1,
                                         tbl_index, method, unique,modflag+1 );
                   else if( unique )
                      for( r=i+1; r<=j; r++ ) tbl_index[r]=-1;
                }
             }
             break;
        case FITS_BIT:
        case FITS_BYTE:
        case FITS_SHORT:
        case FITS_LONG:
             sort_integer( end-start+1, method, col, tbl_index+start, *modflag);
             for( i=start; i<end; i=j+1 ) {
                j=i;
                while( !intcmp( col->data.iptr[ tbl_index[i] ],
                                col->data.iptr[ tbl_index[j+1] ],
                                col->undef_flags[ tbl_index[i] ],
                                col->undef_flags[ tbl_index[j+1] ],*modflag)  )
                   if( ++j==end ) break;
                if( j>i ) {
                   if( ncols>1 ) DoSort( i, j, col+1, ncols-1,
                                         tbl_index, method, unique,modflag+1 );
                   else if( unique )
                      for( r=i+1; r<=j; r++ ) tbl_index[r]=-1;
                }
             }
             break;
        case FITS_SINGLE:
        case FITS_DOUBLE:
             sort_double( end-start+1, method, col, tbl_index+start );
             for( i=start; i<end; i=j+1 ) {
                j=i;
                while( !dblcmp( col->data.dptr[ tbl_index[i] ],
                                col->data.dptr[ tbl_index[j+1] ],
                                col->undef_flags[ tbl_index[i] ],
                                col->undef_flags[ tbl_index[j+1] ])  )
                   if( ++j==end ) break;
                if( j>i ) {
                   if( ncols>1 ) DoSort( i, j, col+1, ncols-1,
                                         tbl_index, method, unique,modflag+1 );
                   else if( unique ) {
                      for( r=i+1; r<=j; r++ ) tbl_index[r]=-1;
                   }
                }
             }
             break;
        case FITS_COMPLEX:
        case FITS_DCOMPLEX:
             sort_complex( end-start+1, method, col, tbl_index+start );
             for( i=start; i<end; i=j+1 ) {
                j=i;
                while( !cpxcmp( col->data.cptr[ tbl_index[i] ],
                                col->data.cptr[ tbl_index[j+1] ],
                                col->undef_flags[ tbl_index[i] ],
                                col->undef_flags[ tbl_index[j+1] ])  )
                   if( ++j==end ) break;
                if( j>i ) {
                   if( ncols>1 ) DoSort( i, j, col+1, ncols-1,
                                         tbl_index, method, unique,modflag+1 );
                   else if( unique )
                      for( r=i+1; r<=j; r++ ) tbl_index[r]=-1;
                }
             }
             break;
       }
}      

void sort_table(iunit,ounit,nrows,width,ascend,ldflag,unique,tbl_index,status)
int iunit,ounit,nrows,width,ascend,ldflag,unique,*tbl_index,*status;
{
 int i,j,k,size;
 char *buffer;
 void move_row();

 if( ldflag ) {
    size = nrows*width;
    buffer = (char*) malloc( size*sizeof(char) );
    if( buffer==NULL )
       ldflag = 0;
    else {
       FCGTBB(iunit,1,1,size,(unsigned char *)buffer,status);
       iunit = 0;
    }
 }
 if( !ldflag )
    buffer = (char*) malloc(width*sizeof(char));
 k = 1;
 if (ascend)
   {
    for (i = 0; i < nrows; i++)
       {
        j = tbl_index[i]+1;
        if( j ) {
           move_row(j,k,iunit,ounit,width,buffer,status);
           k++;
        }
       }
   }
 else
   {
    for (i = nrows-1; i >= 0; i--)
       {
        j = tbl_index[i]+1;
        if( j )
          {
           move_row(j,k,iunit,ounit,width,buffer,status);
           k++;
          }
       }
   }
 if( k<=nrows ) {
    FCFLUS(ounit,status);
    FCDROW(ounit,k,nrows-k+1,status);
 }
 free(buffer);
}

void move_row(grow,prow,iunit,ounit,width,buffer,status)
int grow,prow,iunit,ounit,width,*status;
char *buffer;
{
 int i;
 char error_msg[80];

 if( grow==prow ) return;  /*  Extension already has exact copy  */
			   /*  of original row                   */
 if( iunit ) 
    FCGTBB(iunit,grow,1,width,(unsigned char *)buffer,status);
 else
    buffer += (grow-1)*width;
 FCPTBB(ounit,prow,1,width,(unsigned char *)buffer,status);
 if ( *status != 0 )
   {
    strcpy(error_msg,"FITSIO error moving sorted row");
    Fcerr(error_msg);
   }
}

/* ------------------------------------------------- */
/* Sorting routines for all the supported data types */
/* ------------------------------------------------- */

/* these routines are for logical sorting */

void sort_logical(nrows,method,col,tbl_index)
int nrows,*tbl_index;
char *method;
column *col;
{
 void sort_logical_by_shell(),sort_logical_by_heap(),
      sort_logical_by_insert();

 if (strcmp(method,"SHELL") == 0)
   sort_logical_by_shell(nrows,col,tbl_index);
 if (strcmp(method,"HEAP") == 0)
   sort_logical_by_heap(nrows,col,tbl_index);
 if (strcmp(method,"INSERT") == 0)
   sort_logical_by_insert(nrows,col,tbl_index);
}

void sort_logical_by_shell(n,col,tbl_index)
int n,*tbl_index;
column *col;
{
 double aln2i = 1.442695022;
 double tiny = 1.0e-5;
 int nn,m,j,lognb2;
 int lla,iib;
 int *lgc_array,*idx;

 lgc_array = (int*)col->data.iptr;
 lognb2 = (int)(log((double)n) * aln2i + tiny);
 m = n;
 for (nn = 0; nn < lognb2; nn++)
    {
     m >>= 1;
     for (j = m; j < n; j++)
        {
         idx = tbl_index + (j - m);
         iib = tbl_index[j];
         lla = lgc_array[iib];
         while (idx >= tbl_index && lgc_array[*idx] > lla)
              {
               idx[m] = *idx;
               idx -= m;
              }
         idx[m] = iib;
        }
    }
}

void sort_logical_by_heap(n,col,tbl_index)
int n,*tbl_index;
column *col;
{
 int l,i,j,ir;
 int lla, iib;
 int *lgc_array;

 lgc_array = (int*)col->data.iptr;
 l = (n >> 1) + 1;
 ir = n;
 for (;;)
    {
     if (l > 1)
       {
        l--;
        iib = tbl_index[l-1];
        lla = lgc_array[iib];
       }
     else
       {
        iib = tbl_index[ir-1];
        lla = lgc_array[iib];
        tbl_index[ir-1] = tbl_index[0];
        if ( --ir <= 1 )
          {
           tbl_index[0] = iib;
           return;
          }
       }
     i = l;
     j = l << 1;
     while ( j <= ir )
          {
           if ( j < ir &&
                lgc_array[ tbl_index[j] ] > lgc_array[ tbl_index[j-1] ])
              ++j;
           if ( lgc_array[ tbl_index[j-1] ] > lla )
             {
              tbl_index[i-1] = tbl_index[j-1];
              j += (i=j);
             }
           else
             j = ir + 1;
          }
     tbl_index[i-1] = iib;
    }
}

void sort_logical_by_insert(n,col,tbl_index)
int n,*tbl_index;
column *col;
{
 int j;
 int lla,iib;
 int *lgc_array,*idx;

 lgc_array = (int*)col->data.iptr;
 for (j = 1; j < n; j++)
    {
     iib = tbl_index[j];
     lla = lgc_array[iib];
     idx = tbl_index + (j - 1);
     while ( idx >= tbl_index && lgc_array[*idx] > lla )
          {
           idx[1] = *idx;
           idx--;
          }
     idx[1] = iib;
    }
}

/* end of the logical sorting routines */
/* ---------------------------------------------------------- */
/* these routines are for string sorting */

void sort_string(nrows,method,col,tbl_index)
int nrows,*tbl_index;
char *method;
column *col;
{
 void sort_string_by_shell(),sort_string_by_heap(),
      sort_string_by_insert();

 if (strcmp(method,"SHELL") == 0)
   sort_string_by_shell(nrows,col,tbl_index);
 if (strcmp(method,"HEAP") == 0)
   sort_string_by_heap(nrows,col,tbl_index);
 if (strcmp(method,"INSERT") == 0)
   sort_string_by_insert(nrows,col,tbl_index);

}

void sort_string_by_shell(n,col,tbl_index)
int n,*tbl_index;
column *col;
{
 double aln2i = 1.442695022;
 double tiny = 1.0e-5;
 int nn,m,j,lognb2;
 char *cca;
 int iib,iiu;
 char **chr_array;
 int *undefined,*idx;
 int chrcmp();

 chr_array = (char**)col->data.sptr;
 undefined = (int*)col->undef_flags;
 lognb2 = (int)(log((double)n) * aln2i + tiny);
 m = n;
 for (nn = 0; nn < lognb2; nn++)
    {
     m >>= 1;
     for (j = m; j < n; j++)
        {
         idx = tbl_index + (j - m);
         iib = tbl_index[j];
         cca = chr_array[iib];
         iiu = undefined[iib];
         while (idx >= tbl_index && 
                chrcmp(chr_array[*idx],cca,undefined[*idx],iiu)>0)
              {
               idx[m] = *idx;
               idx -= m;
              }
         idx[m] = iib;
        }
    }
}

void sort_string_by_heap(n,col,tbl_index)
int n,*tbl_index;
column *col;
{
 int l,i,j,ir;
 char *cca;
 int iib, iiu;
 char **chr_array;
 int *undefined;
 int chrcmp();

 chr_array = (char**)col->data.sptr;
 undefined = (int*)col->undef_flags;
 l = (n >> 1) + 1;
 ir = n;
 for (;;)
    {
     if (l > 1)
       {
        l--;
        iib = tbl_index[l-1];
        cca = chr_array[iib];
        iiu = undefined[iib];
       }
     else
       {
        iib = tbl_index[ir-1];
        cca = chr_array[iib];
        iiu = undefined[iib];
        tbl_index[ir-1] = tbl_index[0];
        if ( --ir <= 1 )
          {
           tbl_index[0] = iib;
           return;
          }
       }
     i = l;
     j = l << 1;
     while ( j <= ir )
          {
           if ( j < ir && 
                chrcmp(chr_array[ tbl_index[j] ],chr_array[ tbl_index[j-1] ],
                       undefined[ tbl_index[j] ],undefined[ tbl_index[j-1] ])>0)
              ++j;
           if ( chrcmp(chr_array[ tbl_index[j-1] ],cca,
                       undefined[ tbl_index[j-1] ],iiu) > 0 )
             {
              tbl_index[i-1] = tbl_index[j-1];
              j += (i=j);
             }
           else
             j = ir + 1;
          }
     tbl_index[i-1] = iib;
    }
}

void sort_string_by_insert(n,col,tbl_index)
int n,*tbl_index;
column *col;
{
 int j;
 char *cca; 
 int iib,iiu;
 char **chr_array;
 int *undefined,*idx;
 int dblcmp();

 chr_array = (char**)col->data.sptr;
 undefined = (int*)col->undef_flags;
 for (j = 1; j < n; j++)
    {
     iib = tbl_index[j];
     cca = chr_array[iib];
     iiu = undefined[iib];
     idx = tbl_index + (j - 1);
     while ( idx >= tbl_index 
             && chrcmp(chr_array[*idx],cca,undefined[*idx],iiu)>0 )
          {
           idx[1] = *idx;
           idx--;
          }
     idx[1] = iib;
    }
}

int chrcmp(val_1,val_2,nul_1,nul_2)
char *val_1, *val_2;
int nul_1, nul_2;
{
 int greater_than = 0;

 if ( nul_1 && (!nul_2) )
   greater_than = -1;
 else if ( (!nul_1) && nul_2 )
    greater_than = 1;
 else if ( (!nul_1) && (!nul_2) )
    greater_than = strcmp(val_1, val_2);
 return (greater_than);
}

/* end of the string sorting routines */
/* ---------------------------------------------------------- */
/* these routines are for integer sorting */

void sort_integer(nrows,method,col,tbl_index,mod)
int nrows,*tbl_index;
char *method;
column *col;
int mod;
{
 void sort_integer_by_shell(),sort_integer_by_heap(),
      sort_integer_by_insert();

 if (strcmp(method,"SHELL") == 0)
   sort_integer_by_shell(nrows,col,tbl_index,mod);
 if (strcmp(method,"HEAP") == 0)
   sort_integer_by_heap(nrows,col,tbl_index,mod);
 if (strcmp(method,"INSERT") == 0)
   sort_integer_by_insert(nrows,col,tbl_index,mod);

}

void sort_integer_by_shell(n,col,tbl_index,mod)
int n,*tbl_index;
column *col;
int mod;
{
 double aln2i = 1.442695022;
 double tiny = 1.0e-5;
 int nn,m,j,lognb2;
 int iia,iib,iiu;
 int *int_array,*undefined,*idx;
 int intcmp();

 int_array = (int*)col->data.iptr;
 undefined = (int*)col->undef_flags;
 lognb2 = (int)(log((double)n) * aln2i + tiny);
 m = n;
 for (nn = 0; nn < lognb2; nn++)
    {
     m >>= 1;
     for (j = m; j < n; j++)
        {
         idx = tbl_index + (j - m);
         iib = tbl_index[j];
         iia = int_array[iib];
         iiu = undefined[iib];
         while (idx >= tbl_index && 
                intcmp(int_array[*idx],iia,undefined[*idx],iiu,mod)>0 )
              {
               idx[m] = *idx;
               idx -= m;
              }
         idx[m] = iib;
        }
    }
}

void sort_integer_by_heap(n,col,tbl_index,mod)
int n,*tbl_index;
column *col;
int mod;
{
 int l,i,j,ir;
 int iia, iib, iiu;
 int *int_array,*undefined;
 int intcmp();

 int_array = (int*)col->data.iptr;
 undefined = (int*)col->undef_flags;
 l = (n >> 1) + 1;
 ir = n;
 for (;;)
    {
     if (l > 1)
       {
        l--;
        iib = tbl_index[l-1];
        iia = int_array[iib];
        iiu = undefined[iib];
       }
     else
       {
        iib = tbl_index[ir-1];
        iia = int_array[iib];
        iiu = undefined[iib];
        tbl_index[ir-1] = tbl_index[0];
        if ( --ir <= 1 )
          {
           tbl_index[0] = iib;
           return;
          }
       }
     i = l;
     j = l << 1;
     while ( j <= ir )
          {
           if ( j < ir && 
                intcmp(int_array[ tbl_index[j] ],int_array[ tbl_index[j-1] ],
                       undefined[ tbl_index[j] ],undefined[ tbl_index[j-1] ],mod)>0)
              ++j;
           if ( intcmp(int_array[ tbl_index[j-1] ],iia,
                       undefined[ tbl_index[j-1] ],iiu,mod) > 0 )
             {
              tbl_index[i-1] = tbl_index[j-1];
              j += (i=j);
             }
           else
             j = ir + 1;
          }
     tbl_index[i-1] = iib;
    }
}

void sort_integer_by_insert(n,col,tbl_index,mod)
int n,*tbl_index;
column *col;
int mod;
{
 int j;
 int iia,iib,iiu;
 int *int_array,*undefined,*idx;
 int intcmp();

 int_array = (int*)col->data.iptr;
 undefined = (int*)col->undef_flags;
 for (j = 1; j < n; j++)
    {
     iib = tbl_index[j];
     iia = int_array[iib];
     iiu = undefined[iib];
     idx = tbl_index + (j - 1);
     while ( idx >= tbl_index
             && intcmp(int_array[*idx],iia,undefined[*idx],iiu,mod)>0 )
          {
           idx[1] = *idx;
           idx--;
          }
     idx[1] = iib;
    }
}

int intcmp(val_1,val_2,nul_1,nul_2,mod)
int val_1,val_2,nul_1,nul_2;
int mod;
{
 int greater_than = 0;

 if ( nul_1 && (!nul_2) )
   greater_than = -1;
 else if ( (!nul_1) && nul_2 )
    greater_than = 1;
 else if ( (!nul_1) && (!nul_2) ) {
   if(mod < 0) {
    if ( val_1 > val_2 ) greater_than = 1;
    else if( val_1<val_2 ) greater_than = -1;
   }
   else {
    if( val_1 - val_2 > mod/2 ) val_2 +=mod;
    if(val_2 -val_1 > mod/2 ) val_1 +=mod;
    return intcmp(val_1,val_2,nul_1,nul_2,-1);
   }
 }
 return (greater_than);
}

/* end of the integer sorting routines */
/* ---------------------------------------------------------- */
/* these routines are for double sorting */

void sort_double(nrows,method,col,tbl_index)
int nrows,*tbl_index;
char *method;
column *col;
{
 void sort_double_by_shell(),sort_double_by_heap(),
      sort_double_by_insert();

 if (strcmp(method,"SHELL") == 0)
   sort_double_by_shell(nrows,col,tbl_index);
 if (strcmp(method,"HEAP") == 0)
   sort_double_by_heap(nrows,col,tbl_index);
 if (strcmp(method,"INSERT") == 0)
   sort_double_by_insert(nrows,col,tbl_index);

}

void sort_double_by_shell(n,col,tbl_index)
int n,*tbl_index;
column *col;
{
 double aln2i = 1.442695022;
 double tiny = 1.0e-5;
 int nn,m,j,lognb2;
 double dda;
 int iib,iiu;
 double *dbl_array;
 int *undefined,*idx;
 int dblcmp();

 dbl_array = (double*)col->data.dptr;
 undefined = (int*)col->undef_flags;
 lognb2 = (int)(log((double)n) * aln2i + tiny);
 m = n;
 for (nn = 0; nn < lognb2; nn++)
    {
     m >>= 1;
     for (j = m; j < n; j++)
        {
         idx = tbl_index + (j - m);
         iib = tbl_index[j];
         dda = dbl_array[iib];
         iiu = undefined[iib];
         while (idx >= tbl_index && 
                dblcmp(dbl_array[*idx],dda,undefined[*idx],iiu)>0 )
              {
               idx[m] = *idx;
               idx -= m;
              }
         idx[m] = iib;
        }
    }
}

void sort_double_by_heap(n,col,tbl_index)
int n,*tbl_index;
column *col;
{
 int l,i,j,ir;
 double dda;
 int iib, iiu;
 double *dbl_array;
 int *undefined;
 int dblcmp();

 dbl_array = (double*)col->data.dptr;
 undefined = (int*)col->undef_flags;
 l = (n >> 1) + 1;
 ir = n;
 for (;;)
    {
     if (l > 1)
       {
        l--;
        iib = tbl_index[l-1];
        dda = dbl_array[iib];
        iiu = undefined[iib];
       }
     else
       {
        iib = tbl_index[ir-1];
        dda = dbl_array[iib];
        iiu = undefined[iib];
        tbl_index[ir-1] = tbl_index[0];
        if ( --ir <= 1 )
          {
           tbl_index[0] = iib;
           return;
          }
       }
     i = l;
     j = l << 1;
     while ( j <= ir )
          {
           if ( j < ir && 
                dblcmp(dbl_array[ tbl_index[j] ],dbl_array[ tbl_index[j-1] ],
                       undefined[ tbl_index[j] ],undefined[ tbl_index[j-1] ])>0)
              ++j;
           if ( dblcmp(dbl_array[ tbl_index[j-1] ],dda,
                       undefined[ tbl_index[j-1] ],iiu) > 0 )
             {
              tbl_index[i-1] = tbl_index[j-1];
              j += (i=j);
             }
           else
             j = ir + 1;
          }
     tbl_index[i-1] = iib;
    }
}

void sort_double_by_insert(n,col,tbl_index)
int n,*tbl_index;
column *col;
{
 int j;
 double dda; 
 int iib,iiu;
 double *dbl_array;
 int *undefined,*idx;
 int dblcmp();

 dbl_array = (double*)col->data.dptr;
 undefined = (int*)col->undef_flags;
 for (j = 1; j < n; j++)
    {
     iib = tbl_index[j];
     dda = dbl_array[iib];
     iiu = undefined[iib];
     idx = tbl_index + (j - 1);
     while ( idx >= tbl_index
             && dblcmp(dbl_array[*idx],dda,undefined[*idx],iiu)>0 )
          {
           idx[1] = *idx;
           idx--;
          }
     idx[1] = iib;
    }
}

int dblcmp(val_1,val_2,nul_1,nul_2)
double val_1,val_2;
int nul_1,nul_2;
{
 int greater_than = 0;

 if ( nul_1 && (!nul_2) )
   greater_than = -1;
 else if ( (!nul_1) && nul_2 )
    greater_than = 1;
 else if ( (!nul_1) && (!nul_2) ) {
    if ( val_1 > val_2 ) greater_than = 1;
    else if( val_1<val_2 ) greater_than = -1;
 }
 return (greater_than);
}

/* end of the double sorting routines */
/* ---------------------------------------------------------- */
/* these routines are for complex sorting */

void sort_complex(nrows,method,col,tbl_index)
int nrows,*tbl_index;
char *method;
column *col;
{
 void sort_complex_by_shell(),sort_complex_by_heap(),
      sort_complex_by_insert();

 if (strcmp(method,"SHELL") == 0)
   sort_complex_by_shell(nrows,col,tbl_index);
 if (strcmp(method,"HEAP") == 0)
   sort_complex_by_heap(nrows,col,tbl_index);
 if (strcmp(method,"INSERT") == 0)
   sort_complex_by_insert(nrows,col,tbl_index);

}

void sort_complex_by_shell(n,col,tbl_index)
int n,*tbl_index;
column *col;
{
 double aln2i = 1.442695022;
 double tiny = 1.0e-5;
 int nn,m,j,lognb2;
 complex cca;
 int iib,iiu;
 complex *cpx_array;
 int *undefined,*idx;
 int cpxcmp();

 cpx_array = (complex*)col->data.cptr;
 undefined = (int*)col->undef_flags;
 lognb2 = (int)(log((double)n) * aln2i + tiny);
 m = n;
 for (nn = 0; nn < lognb2; nn++)
    {
     m >>= 1;
     for (j = m; j < n; j++)
        {
         idx = tbl_index + (j - m);
         iib = tbl_index[j];
         cca.real = cpx_array[iib].real;
         cca.imag = cpx_array[iib].imag;
         iiu = undefined[iib];
         while (idx >= tbl_index && 
                cpxcmp(cpx_array[*idx],cca,undefined[*idx],iiu)>0 )
              {
               idx[m] = *idx;
               idx -= m;
              }
         idx[m] = iib;
        }
    }
}

void sort_complex_by_heap(n,col,tbl_index)
int n,*tbl_index;
column *col;
{
 int l,i,j,ir;
 complex cca;
 int iib, iiu;
 complex *cpx_array;
 int *undefined;
 int cpxcmp();

 cpx_array = (complex*)col->data.cptr;
 undefined = (int*)col->undef_flags;
 l = (n >> 1) + 1;
 ir = n;
 for (;;)
    {
     if (l > 1)
       {
        l--;
        iib = tbl_index[l-1];
        cca.real = cpx_array[iib].real;
        cca.imag = cpx_array[iib].imag;
        iiu = undefined[iib];
       }
     else
       {
        iib = tbl_index[ir-1];
        cca.real = cpx_array[iib].real;
        cca.imag = cpx_array[iib].imag;
        iiu = undefined[iib];
        tbl_index[ir-1] = tbl_index[0];
        if ( --ir <= 1 )
          {
           tbl_index[0] = iib;
           return;
          }
       }
     i = l;
     j = l << 1;
     while ( j <= ir )
          {
           if ( j < ir && 
                cpxcmp(cpx_array[ tbl_index[j] ],cpx_array[ tbl_index[j-1] ],
                       undefined[ tbl_index[j] ],undefined[ tbl_index[j-1] ])>0)
              ++j;
           if ( cpxcmp(cpx_array[ tbl_index[j-1] ],cca,
                       undefined[ tbl_index[j-1] ],iiu) > 0 )
             {
              tbl_index[i-1] = tbl_index[j-1];
              j += (i=j);
             }
           else
             j = ir + 1;
          }
     tbl_index[i-1] = iib;
    }
}

void sort_complex_by_insert(n,col,tbl_index)
int n,*tbl_index;
column *col;
{
 int j;
 complex cca; 
 int iib,iiu;
 complex *cpx_array;
 int *undefined,*idx;
 int cpxcmp();

 cpx_array = (complex*)col->data.cptr;
 undefined = (int*)col->undef_flags;
 for (j = 1; j < n; j++)
    {
     iib = tbl_index[j];
     cca.real = cpx_array[iib].real;
     cca.imag = cpx_array[iib].imag;
     iiu = undefined[iib];
     idx = tbl_index + (j - 1);
     while ( idx >= tbl_index 
	     && cpxcmp(cpx_array[*idx],cca,undefined[*idx],iiu)>0 )
          {
           idx[1] = *idx;
           idx--;
          }
     idx[1] = iib;
    }
}

int cpxcmp(val_1,val_2,nul_1,nul_2)
complex val_1,val_2;
int nul_1,nul_2;
{
 int greater_than = 0;

 if ( nul_1 && (!nul_2) )
   greater_than = -1;
 else if ( (!nul_1) && nul_2 )
    greater_than = 1;
 else if ( (!nul_1) && (!nul_2) ) {
         if( val_1.real>val_2.real ) greater_than = 1;
    else if( val_1.real<val_2.real ) greater_than =-1;
    else if( val_1.imag>val_2.imag ) greater_than = 1;
    else if( val_1.imag<val_2.imag ) greater_than =-1;
 }
 return (greater_than);
}

/* end of the complex sorting routines */
