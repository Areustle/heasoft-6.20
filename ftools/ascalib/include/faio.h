/************************************************************************/
/*	faio.h Ver1.0
/*		1993/04/24	by R.Fujimoto
/************************************************************************/

#ifndef _FAIO_H_
#define _FAIO_H_

#define RECORDSIZE 67
#define packedRECORDSIZE 16
#define BUFS 100

#define NORMAL_END		0
#define NOT_INTERPOLATED	1
#define OPEN_ERR		91
#define CLOSE_ERR		92
#define READ_ERR		93
#define SEEK_ERR		94
#define MALLOC_ERR		95
#define ILLEGAL_INPUT		999
#define NO_KEYWORD		998

typedef struct {
	double adstime;
	double qparam[4];
	double qparamerr[3];
	unsigned char sensor[3];
} ADS_DATA;

typedef struct {
  int adstime, eulang[3];
} packedADS_DATA;

/************************************************************************/
/*  faOpen
/*	機能	姿勢FRFを開く
/*	戻り値
/*		NORMAL_END	正常終了
/*		SEEK_ERR	fseek失敗
/*		READ_ERR	fread失敗
/*		NO_KEYWOD	初めまたは終わりの時刻読み出し失敗
/************************************************************************/
int faOpen(
	char filename[] /*姿勢FRFの名前*/
);

/************************************************************************/
/*  faClose
/*	機能	姿勢FRFを閉じる
/*	戻り値
/*		NORMAL_END	正常終了
/*		CLOSE_ERR      	fclose失敗
/************************************************************************/
int faClose(void);

/************************************************************************/
/*  faHeader
/*	機能	FITSヘッダーを読み出す
/*	戻り値
/*		NORMAL_END	正常終了
/*		ILLEGAL_INPUT	入力キーワードがおかしい
/************************************************************************/
int faHeader(
	char keyword[],	/* input FITSキーワード */
	char value[]	/* output キーワードの値 */
);

/************************************************************************/
/*  faReadADStime
/*	現在位置の時刻を読み出し、ファイルポインタを次の位置に進める
/*	戻り値
/*		あすか時刻、問題が生じると負の値を返す
/************************************************************************/
double faReadADStime(void);

/************************************************************************/
/*  faSearchADStime
/*	与えられたあすか時刻のデータにファイルポインタを移動させる
/*	戻り値
/*		NORMAL_END	正常終了
/*		ILLEGAL_INPUT	指定された時刻が含まれていない
/*		SEEK_ERR	fseek失敗（ファイルの終わり）
/************************************************************************/
int faSearchADStime(
	double ascatime	/* input あすか時刻 */
);

/************************************************************************/
/*  faReadADSdata
/*	現在位置の姿勢データを読み出し、ADS_DATA構造体に入れて返す
/*	戻り値
/*		NORMAL_END	正常終了
/*		MALLOC_ERR	メモリ割り当て失敗
/*		READ_ERR	fread失敗
/************************************************************************/
int faReadADSdata(
	ADS_DATA *adsdata	/* output 姿勢決定結果 */
);

/************************************************************************/
/*  faIntrpolQparam
/*    	任意の時刻のQパラメタを内挿によって求める
/*	戻り値
/*		NORMAL_END	正常終了
/*		NOT_INTERPOLATED
/*			一方に近いもしくは前後が同じ値であったので
/*			内挿しなかった
/************************************************************************/
int faIntrpolQparam(
	double t0,		/* input 前の時刻 */
	double qparam0[4],	/* input 前のQパラメタ */
	double t1,		/* input 終わりの時刻 */
	double qparam1[4],	/* input 終わりのQパラメタ */
	double t,		/* input 推定する時刻 */
	double qparam[4]	/* output 内挿して求めたQパラメタ */
);

/************************************************************************/
/*  faAttitude
/*	任意の時刻のQパラメタとその前後の姿勢決定結果を返す
/*	戻り値
/*		NORMAL_END	正常終了
/*		NOT_INTERPOLATED
/*			一方に近いもしくは前後が同じ値であったので
/*			内挿しなかった
/*		ILLEGAL_INPUT	指定された時刻が含まれていない
/*		MALLOC_ERR	メモリ割り当て失敗
/*		SEEK_ERR	fseek失敗（ファイルの終わり）
/*		READ_ERR	fread失敗
/************************************************************************/
int faAttitude(
	double ascatime,		/* input あすか時刻 */
	ADS_DATA *adsdata0,	/* output 前の姿勢決定結果 */
	ADS_DATA *adsdata1,	/* output 後の姿勢決定結果 */
	double qparam[4]		/* output 内挿Qパラメタ */
);

/************************************************************************/
/*  faQparam
/*	任意の時刻のQパラメタを返す
/*	戻り値
/*		NORMAL_END	正常終了
/*		NOT_INTERPOLATED
/*			一方に近いもしくは前後が同じ値であったので
/*			内挿しなかった
/*		ILLEGAL_INPUT	指定された時刻が含まれていない
/*		MALLOC_ERR	メモリ割り当て失敗
/*		SEEK_ERR	fseek失敗（ファイルの終わり）
/*		READ_ERR	fread失敗
/************************************************************************/
int faQparam(
	double ascatime, 	/* input あすか時刻 */
	double qparam[4]	/* output 内挿Qパラメタ */
);

/************************************************************************/
/*  ascaQqparam
/*	任意の時刻のQパラメタを返す。姿勢ファイルは FADIR から探す。
/*	戻り値
/*		NORMAL_END	正常終了
/*		NOT_INTERPOLATED
/*			一方に近いもしくは前後が同じ値であったので
/*			内挿しなかった
/*		OPEN_ERR	対応する時間の姿勢ファイルがなかった
/************************************************************************/
int ascaQparam(
	double ascatime, 	/* input あすか時刻 */
	double qparam[4]	/* output 内挿Qパラメタ */
);

#endif	/* _FAIO_H_ */
