/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 22:49:53 1999 by E. Miyata
 *	version 1.8  2006.08.24		Y.ISHISAKI
 *              COLUMN_INF moved from xisread.h
 *		add index to COLUMN_INF
 */
typedef struct {
  /* bank */
  char *name;
  int size;
  int index;

  /* fits */
  int datatype;
  int nelem;
  int colnum;
} COLUMN_INF;

/*************************************
	fits �� column list ��Ĥ��롣
*************************************/
int list_up_column (fitsfile *fitsd, char *head, COLUMN_INF **column_inf,
		    int *column_num, int *max_value_size);
/*************************************
	fits �� ����������ɤ�Ǥ��� bank �˥��ԡ����롣
*************************************/
int fits2bank (fitsfile *fitsd, COLUMN_INF *column_inf, int column_num,
	       long irow, int max_value_size);
/*************************************
	bank �ξ�����ɤ�Ǥ��� fits �˽񤭽Ф���
*************************************/
int bank2fits (fitsfile *fitsd, COLUMN_INF *column_inf,
	      int column_num, long irow, int max_value_size);
/*************************************
	fits �Υ�����ɾ�����ɤ�Ǥ��� bank �˥��ԡ����롣
	primary header �ΤȤ��� skip_flag=ANL_FALSE �Ȥ���
	���Ƥ� keywords �� bnk �����
*************************************/
int fits2bank_keyword (fitsfile *fitsd, int skip_flag);
