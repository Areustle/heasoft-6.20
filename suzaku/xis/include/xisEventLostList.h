/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 21:38:23 1999 by E. Miyata*/
/**************************************

  xisEventLostList.h
     96/09/09 created by T. Tsuru

     XISframeMake ����ή��Ƥ��� eventList, lostArea ��
     ptr, size, num �� 1�Ĥι�¤�Τ�����ƻȤ��䤹������

     Ver 0.03 (980728) E. Miyata
		lost area �ι�¤�Τ� num �� segment ʬ�������ݤ���
     Ver 0.02 (961204) E. Miyata
		��ʣ OK !
     Ver 0.01 (960909) T. Tsuru

**************************************/

#ifndef _XIS_EVENT_LOST_LIST_
#define _XIS_EVENT_LOST_LIST_

typedef struct {
  char *ptr;
  int size;
  int num;
} XISeventDataS;

typedef struct {
  XISlostArea *ptr;
  int size;
  int num[XIStotalSegNo];
} XISlostAreaS;

#endif
