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
	fits の column list をつくる。
*************************************/
int list_up_column (fitsfile *fitsd, char *head, COLUMN_INF **column_inf,
		    int *column_num, int *max_value_size);
/*************************************
	fits の コラム情報を読んできて bank にコピーする。
*************************************/
int fits2bank (fitsfile *fitsd, COLUMN_INF *column_inf, int column_num,
	       long irow, int max_value_size);
/*************************************
	bank の情報を読んできて fits に書き出す。
*************************************/
int bank2fits (fitsfile *fitsd, COLUMN_INF *column_inf,
	      int column_num, long irow, int max_value_size);
/*************************************
	fits のキーワード情報を読んできて bank にコピーする。
	primary header のときは skip_flag=ANL_FALSE として
	全ての keywords を bnk に落す。
*************************************/
int fits2bank_keyword (fitsfile *fitsd, int skip_flag);
