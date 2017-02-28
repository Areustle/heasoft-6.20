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
/*	��ǽ	����FRF�򳫤�
/*	�����
/*		NORMAL_END	���ｪλ
/*		SEEK_ERR	fseek����
/*		READ_ERR	fread����
/*		NO_KEYWOD	���ޤ��Ͻ����λ����ɤ߽Ф�����
/************************************************************************/
int faOpen(
	char filename[] /*����FRF��̾��*/
);

/************************************************************************/
/*  faClose
/*	��ǽ	����FRF���Ĥ���
/*	�����
/*		NORMAL_END	���ｪλ
/*		CLOSE_ERR      	fclose����
/************************************************************************/
int faClose(void);

/************************************************************************/
/*  faHeader
/*	��ǽ	FITS�إå������ɤ߽Ф�
/*	�����
/*		NORMAL_END	���ｪλ
/*		ILLEGAL_INPUT	���ϥ�����ɤ���������
/************************************************************************/
int faHeader(
	char keyword[],	/* input FITS������� */
	char value[]	/* output ������ɤ��� */
);

/************************************************************************/
/*  faReadADStime
/*	���߰��֤λ�����ɤ߽Ф����ե�����ݥ��󥿤򼡤ΰ��֤˿ʤ��
/*	�����
/*		������������꤬�����������ͤ��֤�
/************************************************************************/
double faReadADStime(void);

/************************************************************************/
/*  faSearchADStime
/*	Ϳ����줿����������Υǡ����˥ե�����ݥ��󥿤��ư������
/*	�����
/*		NORMAL_END	���ｪλ
/*		ILLEGAL_INPUT	���ꤵ�줿���郎�ޤޤ�Ƥ��ʤ�
/*		SEEK_ERR	fseek���ԡʥե�����ν�����
/************************************************************************/
int faSearchADStime(
	double ascatime	/* input ���������� */
);

/************************************************************************/
/*  faReadADSdata
/*	���߰��֤λ����ǡ������ɤ߽Ф���ADS_DATA��¤�Τ�������֤�
/*	�����
/*		NORMAL_END	���ｪλ
/*		MALLOC_ERR	���������Ƽ���
/*		READ_ERR	fread����
/************************************************************************/
int faReadADSdata(
	ADS_DATA *adsdata	/* output ���������� */
);

/************************************************************************/
/*  faIntrpolQparam
/*    	Ǥ�դλ����Q�ѥ�᥿�����ޤˤ�äƵ���
/*	�����
/*		NORMAL_END	���ｪλ
/*		NOT_INTERPOLATED
/*			�����˶ᤤ�⤷�������夬Ʊ���ͤǤ��ä��Τ�
/*			���ޤ��ʤ��ä�
/************************************************************************/
int faIntrpolQparam(
	double t0,		/* input ���λ��� */
	double qparam0[4],	/* input ����Q�ѥ�᥿ */
	double t1,		/* input �����λ��� */
	double qparam1[4],	/* input ������Q�ѥ�᥿ */
	double t,		/* input ���ꤹ����� */
	double qparam[4]	/* output ���ޤ��Ƶ�᤿Q�ѥ�᥿ */
);

/************************************************************************/
/*  faAttitude
/*	Ǥ�դλ����Q�ѥ�᥿�Ȥ�������λ��������̤��֤�
/*	�����
/*		NORMAL_END	���ｪλ
/*		NOT_INTERPOLATED
/*			�����˶ᤤ�⤷�������夬Ʊ���ͤǤ��ä��Τ�
/*			���ޤ��ʤ��ä�
/*		ILLEGAL_INPUT	���ꤵ�줿���郎�ޤޤ�Ƥ��ʤ�
/*		MALLOC_ERR	���������Ƽ���
/*		SEEK_ERR	fseek���ԡʥե�����ν�����
/*		READ_ERR	fread����
/************************************************************************/
int faAttitude(
	double ascatime,		/* input ���������� */
	ADS_DATA *adsdata0,	/* output ���λ��������� */
	ADS_DATA *adsdata1,	/* output ��λ��������� */
	double qparam[4]		/* output ����Q�ѥ�᥿ */
);

/************************************************************************/
/*  faQparam
/*	Ǥ�դλ����Q�ѥ�᥿���֤�
/*	�����
/*		NORMAL_END	���ｪλ
/*		NOT_INTERPOLATED
/*			�����˶ᤤ�⤷�������夬Ʊ���ͤǤ��ä��Τ�
/*			���ޤ��ʤ��ä�
/*		ILLEGAL_INPUT	���ꤵ�줿���郎�ޤޤ�Ƥ��ʤ�
/*		MALLOC_ERR	���������Ƽ���
/*		SEEK_ERR	fseek���ԡʥե�����ν�����
/*		READ_ERR	fread����
/************************************************************************/
int faQparam(
	double ascatime, 	/* input ���������� */
	double qparam[4]	/* output ����Q�ѥ�᥿ */
);

/************************************************************************/
/*  ascaQqparam
/*	Ǥ�դλ����Q�ѥ�᥿���֤��������ե������ FADIR ����õ����
/*	�����
/*		NORMAL_END	���ｪλ
/*		NOT_INTERPOLATED
/*			�����˶ᤤ�⤷�������夬Ʊ���ͤǤ��ä��Τ�
/*			���ޤ��ʤ��ä�
/*		OPEN_ERR	�б�������֤λ����ե����뤬�ʤ��ä�
/************************************************************************/
int ascaQparam(
	double ascatime, 	/* input ���������� */
	double qparam[4]	/* output ����Q�ѥ�᥿ */
);

#endif	/* _FAIO_H_ */
