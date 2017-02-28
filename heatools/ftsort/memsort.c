#include <stdio.h>
#include <math.h>
#include "fitsio.h"
#include "ftsort.h"

int col_cmp (column *col, 
             int row1, 
             int row2)
/*
compare two items on the same column
*/
{
   
    int greater_than = 0;

    if ( col->undef_flags[row1] && !(col->undef_flags[row2]) ) 
    {
        greater_than = -1;
    }
    else if ( !(col->undef_flags[row1]) && col->undef_flags[row2] ) 
    {
        greater_than =1;
    }
    else if ( !(col->undef_flags[row1]) && !(col->undef_flags[row2]) ) 
    {
        switch(col->dtype) 
        {
            case TBIT: 
            case TBYTE:
            case TUSHORT:
            case TSHORT:
            case TINT:
                 if ( col->data.iptr[row1] > col->data.iptr[row2] ) greater_than =1;
                 else if ( col->data.iptr[row1] < col->data.iptr[row2] ) greater_than =-1;
             break;
             case TSTRING:
                 greater_than = strcmp ( col->data.sptr[row1],col->data.sptr[row2] );
             break;
             case TLONG: 
             case TFLOAT:
             case TDOUBLE:
                 if( col->data.dptr[row1] > col->data.dptr[row2] )
                           greater_than =1;
                 else if( col->data.dptr[row1] < col->data.dptr[row2] )
                           greater_than =-1;
             break;
             case TCOMPLEX:
             case TDBLCOMPLEX:
                 if( col->data.cptr[row1].real > col->data.cptr[row2].real ) greater_than =1;
                 else if( col->data.cptr[row1].real < col->data.cptr[row2].real ) greater_than =-1;
                 else if( col->data.cptr[row1].imag > col->data.cptr[row2].imag ) greater_than =1;
                 else if( col->data.cptr[row1].imag < col->data.cptr[row2].imag ) greater_than =-1;
             break;
        }
    }
    return (greater_than);
}  

void adjust_heap( int* tbl_indx, /* IO - index of table which represents a 
                                              binary tree struture */
                  int root,     /* I  - root of tbl_indx */
                  int n,        /* I  - Number of nodes */ 
                  column* col,  /* I  - Array of column data */
                  int ascend)
/*
Adjust the binary tree with root "root" to satisy the heap property. 
Assume the left and rigth subtrees of "root" already satisfy heap property. 
*/
{

    int j;
    int idx, idx1, root_idx;
    int greater_than;

    root_idx= tbl_indx[root];
  
    for (j =2*(root+1)-1; j<n; j=(j+1)*2-1) 
    { 
        /* first find max of left and right child */

        idx=tbl_indx[j];

        if( j < n-1) 
        {
            idx=tbl_indx[j];
            idx1=tbl_indx[j+1];
            greater_than = col_cmp(col,idx,idx1);
            if(!ascend) greater_than = -greater_than;

            if( greater_than < 0 ) 
            {
                j++;
                idx=idx1;
            }
        }

        /* Compare the max child with root, 
           if it is larger than root, then stop */ 
        greater_than = col_cmp(col,idx,root_idx);
        if(!ascend) greater_than = -greater_than;

       /* move the child to the root*/
       if( greater_than < 0) break;
       tbl_indx[(j+1)/2-1]= tbl_indx[j];
    }
    tbl_indx[(j+1)/2-1] =root_idx;
}


void heap_sort (int * tbl_indx, /* IO - Table index to be sorted */ 
                const int n,    /* I  - Number of row            */ 
                column* col,    /* I  - Column */
                int ascend)   /* I  - ascend sorting */
/*
Sort the table "tbl_indx" by using heap. 
*/
{

    int i;
    int t;
  
    /* convert tbl_index into a heap */
    for ( i = n/2-1 ; i>=0; i--) 
    {
        adjust_heap(tbl_indx,i,n,col,ascend);
    }

  
    /* sort tbl_index */
    for ( i = n-2; i >=0; i--) 
    {
         t = tbl_indx[i+1];
         tbl_indx[i+1] = tbl_indx[0];
         tbl_indx[0] = t;
         adjust_heap(tbl_indx,0,i+1,col,ascend); 
    }
} 

void insert_sort (int* tbl_indx,  /* IO - Table index */
                  int n,          /* I  - Number of rows of table */ 
                  column* col,    /* I  - Column */
                  int ascend)     /* I  - Ascend sorting */
/*
Sort tbl_indx by using insert method.
*/
{
    int j;
    int iib;
    int greater_than;
    int *idx;

    for (j=1; j < n; j++) 
    {
       iib = tbl_indx[j];
       idx = tbl_indx + (j -1);
       while (idx >= tbl_indx ) 
       {
            greater_than =col_cmp(col, *idx,iib);
            if(!ascend) greater_than = -greater_than;

            if( greater_than  <=0 ) break;
            idx[1] = *idx;
            idx--;
        } 
        idx[1] = iib;
    }
}

void shell_sort(int * tbl_index, 
                int n,
                column *col, 
                int ascend)
{
     double aln2i = 1.442695022;
     double tiny = 1.0e-5;
     int nn,m,j,lognb2;
     int iib,*idx;
     
     lognb2 = (int)(log((double)n) * aln2i + tiny);

     m = n;
     for (nn = 0; nn < lognb2; nn++)
     {
         m >>= 1;
         for (j = m; j < n; j++)
         {
             idx = tbl_index + (j - m);
             iib = tbl_index[j];
             if( !ascend ) 
             {
             	while (idx >= tbl_index && col_cmp(col,*idx,iib) < 0 )
             	{
                	  idx[m] = *idx;
                  	idx -= m;
             	}
             }
             else 
             {
             	while (idx >= tbl_index && col_cmp(col,*idx,iib) > 0 )
             	{
                	  idx[m] = *idx;
                  	idx -= m;
             	}
             }
                 
            idx[m] = iib;
          }
      }
}


void do_memsort(int start, int end, int* tbl_index, column * col, int *ascend,
                 int ncols, char* method, int unique)
{
      int col_cmp(column*,int,int);
      void heap_sort(int*,const int,column*,int ascend);
      void insert_sort(int*,int,column*,int ascend);
      void shell_sort(int*,int,column*,int ascend);
      int i,j,r;

      if(strcmp(method,"heap")==0)
      {
            heap_sort(tbl_index+start,end-start,col,*ascend);
      }
      else if(strcmp(method,"insert")==0)
      {
            insert_sort(tbl_index+start,end-start,col,*ascend);
      }
      else if(strcmp(method,"shell")==0 )
      {
            shell_sort(tbl_index+start,end-start,col,*ascend);
      }

      /* sorting next column recursively */
      for ( i= start; i<end; i=j)
      {
            j=i;
            
            while( j < end && col_cmp(col,tbl_index[i],tbl_index[j])==0) 
               j++;

            if( j>i+1 ) 
            {
                   if( ncols>1 ) 
                   {
                         do_memsort( i, j, tbl_index,col+1,ascend+1, ncols-1,
                                          method,unique );
                   }
                   else if(unique)
                   {
                         for( r=i+1; r<j; r++ ) 
                         {
                             tbl_index[r]=-1;
                         }
                   }
            }
      }

}


int col_alloc(int nrows, int size, column *col);

int read_table( fitsfile *infptr,int nrows,int  ncols,long* repeat, int *dtype,int* colnum, int
** tbl_index, column **data, int *status)
{
    long row,col;
    char ** ptr;
    long r;
    int mem_error=0;
    int anynull;

    *tbl_index =(int*) malloc(nrows*sizeof(int));

    *data =(column*) malloc(ncols*sizeof(column));

    if( *tbl_index !=NULL && *data !=NULL)
    {
       for (row=0; row<nrows; row++) *(*tbl_index+row)=row;

       for (col=0; col<ncols && !(mem_error || *status); col++) 
       {
           (*data+col)->dtype = dtype[col];

           switch(dtype[col])
           {
               case TBIT:
               case TBYTE:
               case TSHORT:
               case TINT:
                    if(col_alloc(nrows, sizeof(int), *data+col ) )
                    {
                        fits_read_colnull(infptr,TINT,colnum[col],1,1,nrows,
                        (*data+col)->data.iptr,(*data+col)->undef_flags,&anynull,status);
                    }
                    else
                    {
                         mem_error =1;
                    }
                                          
                    
                break;
                case TSTRING:
                    ptr =(char**) malloc (nrows*sizeof(char*));
                    r = repeat[col]+1;
                    if  (col_alloc(nrows, (repeat[col]+1)*sizeof(char), *data+col ) )
                    {
                        ptr[0] = (char*)(*data+col)->data.sptr;

                        (*data+col)->data.sptr = ptr;

                        for( row=0; row<nrows; row++ )
                        {
                             (*data+col)->data.sptr[row] = (*data+col)->data.sptr[0]
                                                 +r*row;
                        }
                           fits_read_colnull(infptr,TSTRING,colnum[col],1,1,nrows,
                                (*data+col)->data.sptr,(*data+col)->undef_flags,&anynull,status);
                    }
                    else
                    {
                         mem_error = 1;
                         free(ptr);
                    }
                  break;
                  case TLONG:
                  case TFLOAT:
                  case TDOUBLE:
                       if( col_alloc( nrows, sizeof(double), *data+col ) ) 
                       {
                           fits_read_colnull(infptr,TDOUBLE,colnum[col],1,1,nrows,
                                (*data+col)->data.dptr,(*data+col)->undef_flags,&anynull,status);
                       }
                       else
                       {
                            mem_error = 1;
                       }
                  break;
                  case TCOMPLEX:
                  case TDBLCOMPLEX:
                        if( col_alloc( nrows*2, sizeof(double), *data+col ) )
                        {
                           fits_read_col(infptr,TDBLCOMPLEX,colnum[col],1,1,nrows,
                                (*data+col)->data.cptr,(*data+col)->undef_flags,&anynull,status);
                        }
                        else
                        {
                           mem_error = 1;
                        }
                   break;


           }

        }
    }
    else 
    {
       mem_error =1;
    }

   
  
    return (mem_error || *status);
}

int col_alloc(int nrows, int size, column *col)
{
     col->data.iptr = (int *)malloc(nrows*size);
     col->undef_flags = (char *)calloc(nrows,sizeof(char));
     if( col->data.iptr==NULL || col->undef_flags==NULL )
     {
         free(col->data.iptr);
         free(col->undef_flags);
         return(0);
     }
     return(1);
}


void write_table(fitsfile * infptr,
                fitsfile *outfptr,
                int nrows,
                int ldflag,
                int *tbl_index,
                int *status)
{
     int i, j, k, size;
     char *buffer;
     long width;
     void move_row();
     fits_read_key_lng(infptr,"NAXIS1",&width,NULL,status);
     if( ldflag )
     {
         size = nrows*width;
         buffer = (char*) malloc( size*sizeof(char) );
         if( buffer==NULL )
            ldflag = 0;
         else
         {
           ffgtbb(infptr,1,1,size,(unsigned char *)buffer,status);
            infptr = NULL;
         }
     }
    if( !ldflag )
        buffer = (char*) malloc(width*sizeof(char));
    k = 1;

   for (i = 0; i < nrows; i++)
   {
        j = tbl_index[i]+1;
        if( j ) {
             move_row(j,k,infptr,outfptr,width,buffer,status);
             k++;
        }
   }

  if( k<=nrows )
  {
      ffflus(outfptr,status);
      ffdrow(outfptr,k,nrows-k+1,status);
  }
  free(buffer);
}
void move_row(int grow,
              int prow,
              fitsfile * intfptr,
              fitsfile * outfptr,
              int width,
              char * buffer,
              int * status)
{

   if( grow==prow ) return;  /*  Extension already has exact copy  */
                           /*  of original row                   */
   if( intfptr != NULL )
      ffgtbb(intfptr,grow,1,width,(unsigned char *)buffer,status);
   else
      buffer += (grow-1)*width;
      ffptbb(outfptr,prow,1,width,(unsigned char *)buffer,status);
   if ( *status != 0 )
   {
      fprintf(stderr,"FITSIO error moving sorted row\n");
   }
}


void free_mem(int ncols, column* data)
{

    while (ncols--)
    {
       if( (data+ncols)->dtype ==TSTRING ) free( (data+ncols)->data.sptr[0]);
       free( (data+ncols)->data.iptr );
       free( (data+ncols)->undef_flags );
    }
}
    
