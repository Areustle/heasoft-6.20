/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 22:49:56 1999 by E. Miyata*/
/*
*
*     XISgradeUtil.c
*        ・5x5 pixelを使って、grade判定を行ないます。
*	・Grade2-6は、まわりのpixelにsplit thresholdを越える
*	  ものがあれば捨てる -> Grade 10
*	・田の字の角が一番大きい場合も捨てる -> Grade 11
*	・Grade7でも、3x3におさまるものは、Grade9とする(X線イベントとはしない)
*
*       input:  *handle[25] pointer array to pulse height(interger)
*               split       split threshold
*       output: *sumph      total ph
*               *type       grade
*               *above      numbers of pixels summed in *sumph
*        X-->
*          PH-09 PH-10 PH-11 PH-12 PH-13
*          PH-14 PH-01 PH-02 PH-03 PH-15
*          PH-16 PH-04 PH-E  PH-05 PH-17
*          PH-18 PH-06 PH-07 PH-08 PH-19
*          PH-20 PH-21 PH-22 PH-23 PH-24     P3.2-100.24
*
*     H.Murakami
*	  99/01/09 ver 0.1 classify3i.cを修正、5x5で対応させる
*                           まわりのpixelは斜めも見て判断
*	  99/03/03 ver 1.0 2x2と対応させるため、まわりのpixelは隣だけ見る
*
*	1999.07.25	ver1.1	Emi Miyata
*		support 5x5, 3x3, 2x2 mode
*		change *handle[] -> *handle
*
*	2004.12.21	ver1.2	Hiroshi Murakami
*		Grade7,9で角のpixelのPHを足さないバグを修正
*
*	2005.06.21	ver1.3	Hiroshi Nakajima
*               eliminate some sources of compile warnings..
*
*	2005.12.25	ver1.5	Hiroshi Nakajima
*               Correct the bit order for XISgrade2Outergrade in 3x3 mode
*
*	2007.04.22	ver3.0	Y.ISHISAKI
*               change int -> double handle, split, sum_ph
*
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "xisTelemFormat.h"
#include "xisGradeUtil.h"
#include "anl.h"

static int lookup[256] = {
  0,1,2,5,1,1,5,7,3,5,8,6,3,5,7,7,
  4,4,8,7,5,5,6,7,7,7,7,7,7,7,7,7,
  1,1,2,5,1,1,5,7,5,7,7,7,5,7,7,7,
  4,4,8,7,5,5,6,7,7,7,7,7,7,7,7,7,
  2,2,7,7,2,2,7,7,8,7,7,7,8,7,7,7,
  8,8,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  5,5,7,7,5,5,7,7,6,7,7,7,6,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  1,1,2,5,1,1,5,7,3,5,8,6,3,5,7,7,
  5,5,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  1,1,2,5,1,1,5,7,5,7,7,7,5,7,7,7,
  5,5,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  5,5,7,7,5,5,7,7,7,7,7,7,7,7,7,7,
  6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
};

static int cnv[16] = {
  15,14,13,12,11,0,10,1,9,2,8,3,4,5,6,7
};

/* 外縁部を並びかえ
15 14 13 12 11
 0          10
 1           9
 2           8
 3  4  5  6  7
*/

/* 3x3のうち、スプリット閾値を越えたピクセルの外側をマスクとする */
static int
mkgrademask(int XISgrade)
{
  static int mark[9] = {
    0x0000, 0x4001, 0x2000, 0x1400,
    0x0002, 0x0200, 0x0014, 0x0020, 0x0140
  };
  static int crosspix[4] = {
    2,4,5,7
  };

  int i,ipix;
  int outermask;

  outermask = 0;
  for (i = 0; i < 4; i++) {
    ipix = crosspix[i];
    if ( (XISgrade & (1 << (ipix-1))) != 0 ) {
      outermask = outermask | mark[ipix];
    }
  }

  /* corner pixelの取り扱い  */
  if ( (XISgrade & (0x000B)) == 0x000B ) outermask = outermask | mark[1];
  if ( (XISgrade & (0x0016)) == 0x0016 ) outermask = outermask | mark[3];
  if ( (XISgrade & (0x0068)) == 0x0068 ) outermask = outermask | mark[6];
  if ( (XISgrade & (0x00D0)) == 0x00D0 ) outermask = outermask | mark[8];

  return outermask;
}

static int
XISgrade2ASCAgrade(int xisGrade)
{
  int ascaGrade;
  int maskGrade;

  maskGrade = xisGrade & 0x00FF;  /* 3x3 pixelしかみない */
  ascaGrade = lookup[maskGrade];  /* 256のパターンを参照 */
  return ascaGrade;
}

/*  外の16ピクセルを、順番を変えて16ビット整数に */
static int
XISgrade2Outergrade(int xisGrade)
{
  int i;
  int outerGrade;
  int maskGrade;

  maskGrade = xisGrade >> 8;  /* 3x3 pixel以外しかみない */
  outerGrade = 0;
  for (i=0; i< 16; i++ ) {
    if ( (maskGrade & (0x0001 << i)) != 0 ) {
      outerGrade = outerGrade | (1 << cnv[i]);
    }
  }

  /* *outerGrade = maskGrade; **/
  return outerGrade;
}

/*
*       Sum PHs except for corner pixels
*               In the case of grade6, add a corner pixel
*/
static double
sum5x5(double split, int *above, double *handle, int type)
{
  int i;
  double sumph;

  *above = 1;
  sumph = handle[0];
  for (i = 2; i < 8; i++) {	/* sum ph over split_thres. */
    if ( handle[i] >= split ) {
      /* judge pixels over split_threshold */
      switch (i) {
      case 2: case 4: case 5: case 7:
	sumph += handle[i];
	(*above)++;
	break;
      default:
	break;
      }
    }
  }

  if ( 6==type || 7==type || 8==type ) {
    if ( handle[2] >= split && handle[4] >= split ) {
      (*above)++;
      sumph += handle[1];
    }
    if ( handle[2] >= split && handle[5] >= split ) {
      (*above)++;
      sumph += handle[3];
    }
    if ( handle[4] >= split && handle[7] >= split ) {
      (*above)++;
      sumph += handle[6];
    }
    if ( handle[5] >= split && handle[7] >= split ) {
      (*above)++;
      sumph += handle[8];
    }
  }

  return sumph;
}

/* 5x5 mode */
int
classify_5x5(double *handle, double split, double *sumph, int *type, int *above)
{
  int i, grade=0, innerGrade, ascaGrade, outerGrade, mask;
/* grade -- 5x5のパターン
   innerGrade -- 3x3のパターン
   ascaGrade -- 3x3でつけたASCA Grade
   outerGrade -- まわり16pixelのパターン
   mask -- まわり16pixelのマスク
*/

  for (i = 1; i < XISoneEventPixelTotNo5x5; i++) {
    if ( handle[i] >= split ) {
      grade = grade | (1 << (i-1));
    }
  }

  /* 3x3 でASCAのグレードをつける */
  ascaGrade = XISgrade2ASCAgrade(grade);

  /* Grade 6 (square) で、角が一番大きい場合は捨てる (pile up event) */
  innerGrade = grade & 0x00FF;
  if ( ascaGrade == 6 ) {
    switch (innerGrade){
    case 0x0b: case 0x8b:
      if ( handle[1] > handle[2] && handle[1] > handle[4] ) {
	ascaGrade = 11;
      }
      break;
    case 0x16: case 0x36:
      if ( handle[3] > handle[2] && handle[3] > handle[5] ) {
	ascaGrade = 11;
      }
      break;
    case 0x68: case 0x6c:
      if ( handle[6] > handle[4] && handle[6] > handle[7] ) {
	ascaGrade = 11;
      }
      break;
    case 0xd0: case 0xd1:
      if ( handle[8] > handle[5] && handle[8] > handle[7] ) {
	ascaGrade = 11;
      }
      break;
    default:
      ascaGrade = 6;
      break;
    }
  }
  /*  printf("%d\n",ascaGrade);*/

  /*  外の16ピクセルを、順番を変えて16ビット整数に */
  outerGrade = XISgrade2Outergrade(grade);
  /*  printf("%d\n",outerGrade);*/

  /* 3x3のパターンに応じて、まわり16ピクセルのマスクを作成 */
  mask = mkgrademask(grade);
  /*mask = 0;*/
  /*  printf("%d\n",mask);*/

  /* まわりのマスク内にスプリット閾値を越えるものがあるか？ */
  /* 3x3のGradeごとに処理 */
  switch (ascaGrade) {
  case 0: case 1:
    /* ピクセルレベルをたし合わせる */
    *sumph = sum5x5(split, above, handle, ascaGrade);
/*    printf("%d\n",*sumph); */
    /* Gradeはそのまま */
    *type = ascaGrade;
    break;
  case 2: case 3: case 4: case 5: case 6: case 8:
    if ( (outerGrade & mask) == 0 ) {
      *sumph = sum5x5(split, above, handle, ascaGrade);
      *type = ascaGrade;
      /* set the grade of L-shaped event to 6 */
      if( ascaGrade == 8 ) *type = 6;
    } else {
      *sumph = sum5x5(split, above, handle, ascaGrade);
      *type = 10;
    }
    break;
  case 7:
    if ( (outerGrade & mask) == 0 ) {
      *sumph = sum5x5(split, above, handle, ascaGrade);
      *type = 9;
    } else {
      *sumph = sum5x5(split, above, handle, ascaGrade);
      *type = 7;
    }
    break;
  case 11:
    *sumph = sum5x5(split, above, handle, ascaGrade);
    *type = 11;
    break;
  default:
    *type = 7;
    break;
  }
  return ANL_TRUE;
}

/* 3x3 mode */
int
classify_3x3(double *handle, int p_outer_most, int sum_outer_most,
	     double split, double *sumph, int *type, int *above)
{
  int i, grade=0, innerGrade, ascaGrade, outerGrade, mask, outerPattern;
/* grade -- 5x5のパターン
   innerGrade -- 3x3のパターン
   ascaGrade -- 3x3でつけたASCA Grade
   outerPattern -- まわり16pixelのパターン
   mask -- まわり16pixelのマスク
   outerGrade -- outerPatternをmaskとおなじbitじゅんにならべたもの
*/

  for (i = 1; i < XISoneEventPixelTotNo3x3; i++) {
    if ( handle[i] >= split ) {
      grade = grade | (1 << (i-1));
    }
  }

  /* 3x3 でASCAのグレードをつける */
  ascaGrade = XISgrade2ASCAgrade(grade);

  /* Grade 6 (square) で、角が一番大きい場合は捨てる (pile up event) */
  innerGrade = grade & 0x00FF;
  if ( ascaGrade == 6 ) {
    switch (innerGrade) {
    case 0x0b: case 0x8b:
      if ( handle[1] > handle[2] && handle[1] > handle[4] ) {
	ascaGrade = 11;
      }
      break;
    case 0x16: case 0x36:
      if ( handle[3] > handle[2] && handle[3] > handle[5] ) {
	ascaGrade = 11;
      }
      break;
    case 0x68: case 0x6c:
      if ( handle[6] > handle[4] && handle[6] > handle[7] ) {
	ascaGrade = 11;
      }
      break;
    case 0xd0: case 0xd1:
      if ( handle[8] > handle[5] && handle[8] > handle[7] ) {
	ascaGrade = 11;
      }
      break;
    default:
      ascaGrade = 6;
      break;
    }
  }

  outerPattern = 0;
  for (i = 1; i < 17; i++) {
    if ( (p_outer_most & (1 << (16-i))) != 0 ) {
      outerPattern = outerPattern | (0x0001 << (i-1));
    }
  }
  /*  外の16ピクセルを、順番を変えて16ビット整数に */
  outerGrade = XISgrade2Outergrade(outerPattern<<8);

  /* 3x3のパターンに応じて、まわり16ピクセルのマスクを作成 */
  mask = mkgrademask(grade);

  /* まわりのマスク内にスプリット閾値を越えるものがあるか？ */
  /* 3x3のGradeごとに処理 */
  switch (ascaGrade) {
  case 0: case 1:
    /* ピクセルレベルをたし合わせる */
    *sumph = sum5x5(split, above, handle, ascaGrade);
/*    printf("%d\n",*sumph); */
    /* Gradeはそのまま */
    *type = ascaGrade;
    break;
  case 2: case 3: case 4: case 5: case 6: case 8:
    if ( (outerGrade & mask) == 0 ) {
      *sumph = sum5x5(split, above, handle, ascaGrade);
      *type = ascaGrade;
      /* set the grade of L-shaped event to 6 */
      if( ascaGrade == 8 ) *type = 6;
    } else {
      *sumph = sum5x5(split, above, handle, ascaGrade);
      *type = 10;
    }
    break;
  case 7:
    if ( (outerGrade & mask) == 0 ) {
      *sumph = sum5x5(split, above, handle, ascaGrade);
      *type = 9;
    } else {
      *sumph = sum5x5(split, above, handle, ascaGrade);
      *type = 7;
    }
    break;
  case 11:
    *sumph = sum5x5(split, above, handle, ascaGrade);
    *type =11;
    break;
  default:
    *type = 7;
    break;
  }

  return ANL_TRUE;
}

static int
classify_2x2_outer(int pos2x2, int p_adj, int grade)
{
  int p_adjs[8], i;

  for (i = 0; i < 8; i++) {
    p_adjs[i] = (p_adj>>(7-i)) & 0x01;
  }

  switch (pos2x2) {
  case 0:
    grade = p_adjs[0] ? (grade | 0x01 << 9) : grade;
    grade = p_adjs[1] ? (grade | 0x01 << 10) : grade;
    grade = p_adjs[2] ? (grade | 0x01 << 13) : grade;
    grade = p_adjs[3] ? (grade | 0x01 << 2) : grade;
    grade = p_adjs[4] ? (grade | 0x01 << 15) : grade;
    grade = p_adjs[5] ? (grade | 0x01 << 4) : grade;
    grade = p_adjs[6] ? (grade | 0x01 << 5) : grade;
    grade = p_adjs[7] ? (grade | 0x01 << 6) : grade;
    grade = (p_adjs[0] & p_adjs[2]) ? (grade | 0x01 << 8) : grade;
    grade = (p_adjs[1] & p_adjs[3]) ? (grade | 0x01 << 11) : grade;
    grade = (p_adjs[4] & p_adjs[6]) ? (grade | 0x01 << 17) : grade;
    grade = (p_adjs[5] & p_adjs[7]) ? (grade | 0x01 << 7) : grade;
    break;
  case 1:
    grade = p_adjs[0] ? (grade | 0x01 << 10) : grade;
    grade = p_adjs[1] ? (grade | 0x01 << 11) : grade;
    grade = p_adjs[2] ? (grade | 0x01 << 0) : grade;
    grade = p_adjs[3] ? (grade | 0x01 << 14) : grade;
    grade = p_adjs[4] ? (grade | 0x01 << 3) : grade;
    grade = p_adjs[5] ? (grade | 0x01 << 16) : grade;
    grade = p_adjs[6] ? (grade | 0x01 << 6) : grade;
    grade = p_adjs[7] ? (grade | 0x01 << 7) : grade;
    grade = (p_adjs[0] & p_adjs[2]) ? (grade | 0x01 << 9) : grade;
    grade = (p_adjs[1] & p_adjs[3]) ? (grade | 0x01 << 12) : grade;
    grade = (p_adjs[4] & p_adjs[6]) ? (grade | 0x01 << 5) : grade;
    grade = (p_adjs[5] & p_adjs[7]) ? (grade | 0x01 << 18) : grade;
    break;
  case 2:
    grade = p_adjs[0] ? (grade | 0x01 << 0) : grade;
    grade = p_adjs[1] ? (grade | 0x01 << 1) : grade;
    grade = p_adjs[2] ? (grade | 0x01 << 15) : grade;
    grade = p_adjs[3] ? (grade | 0x01 << 4) : grade;
    grade = p_adjs[4] ? (grade | 0x01 << 17) : grade;
    grade = p_adjs[5] ? (grade | 0x01 << 7) : grade;
    grade = p_adjs[6] ? (grade | 0x01 << 20) : grade;
    grade = p_adjs[7] ? (grade | 0x01 << 21) : grade;
    grade = (p_adjs[0] & p_adjs[2]) ? (grade | 0x01 << 13) : grade;
    grade = (p_adjs[1] & p_adjs[3]) ? (grade | 0x01 << 2) : grade;
    grade = (p_adjs[4] & p_adjs[6]) ? (grade | 0x01 << 19) : grade;
    grade = (p_adjs[5] & p_adjs[7]) ? (grade | 0x01 << 22) : grade;
    break;
  case 3:
    grade = p_adjs[0] ? (grade | 0x01 << 1) : grade;
    grade = p_adjs[1] ? (grade | 0x01 << 2) : grade;
    grade = p_adjs[2] ? (grade | 0x01 << 3) : grade;
    grade = p_adjs[3] ? (grade | 0x01 << 16) : grade;
    grade = p_adjs[4] ? (grade | 0x01 << 5) : grade;
    grade = p_adjs[5] ? (grade | 0x01 << 18) : grade;
    grade = p_adjs[6] ? (grade | 0x01 << 21) : grade;
    grade = p_adjs[7] ? (grade | 0x01 << 22) : grade;
    grade = (p_adjs[0] & p_adjs[2]) ? (grade | 0x01 << 0) : grade;
    grade = (p_adjs[1] & p_adjs[3]) ? (grade | 0x01 << 14) : grade;
    grade = (p_adjs[4] & p_adjs[6]) ? (grade | 0x01 << 20) : grade;
    grade = (p_adjs[5] & p_adjs[7]) ? (grade | 0x01 << 23) : grade;
    break;
  default:
    break;
  }

  return grade;
}

static void
swap_2x2ph(double *handle, double *swapped, int pos2x2)
{
  int i;

  swapped[0] = handle[0];
  for (i = 1; i < XISoneEventPixelTotNo5x5; i++) {
    swapped[i] = 0;
  }

  switch (pos2x2) {
  case 0:
    swapped[1] = handle[1];
    swapped[2] = handle[2];
    swapped[4] = handle[3];
    break;
  case 1:
    swapped[2] = handle[1];
    swapped[3] = handle[2];
    swapped[5] = handle[3];
    break;
  case 2:
    swapped[4] = handle[1];
    swapped[6] = handle[2];
    swapped[7] = handle[3];
    break;
  case 3:
    swapped[5] = handle[1];
    swapped[7] = handle[2];
    swapped[8] = handle[3];
    break;
  default:
    break;
  }
}

/* 2x2 mode */
int
classify_2x2(double *handle, int pos2x2, int p_adj,
	     double split, double *sumph, int *type, int *above)
{
  int i, grade, innerGrade, ascaGrade, outerGrade, mask;
  double swap_pha[XISoneEventPixelTotNo5x5];
/* grade -- 5x5のパターン
   innerGrade -- 3x3のパターン
   ascaGrade -- 3x3でつけたASCA Grade
   outerGrade -- まわり16pixelのパターン
   mask -- まわり16pixelのマスク
*/

  /* swap ph order */
  swap_2x2ph(handle, swap_pha, pos2x2);

  grade=0;
  /* grade with 5x5 */
  for (i = 1; i < XISoneEventPixelTotNo5x5; i++) {
    if ( swap_pha[i] >= split ) {
      grade = grade | (0x01 << (i-1));
    }
  }

  /* classify 2x2 */
  grade = classify_2x2_outer(pos2x2, p_adj, grade);

  /* grade 2 ascagrade */
  ascaGrade = XISgrade2ASCAgrade(grade);

  /* Grade 6 (square) で、角が一番大きい場合は捨てる (pile up event) */
  innerGrade = grade & 0x00FF;
  if ( ascaGrade == 6 ) {
    switch (innerGrade) {
    case 0x0b: case 0x8b:
      if ( swap_pha[1] > swap_pha[2] && swap_pha[1] > swap_pha[4] ) {
	ascaGrade = 11;
      }
      break;
    case 0x16: case 0x36:
      if ( swap_pha[3] > swap_pha[2] && swap_pha[3] > swap_pha[5] ) {
	ascaGrade = 11;
      }
      break;
    case 0x68: case 0x6c:
      if ( swap_pha[6] > swap_pha[4] && swap_pha[6] > swap_pha[7] ) {
	ascaGrade = 11;
      }
      break;
    case 0xd0: case 0xd1:
      if ( swap_pha[8] > swap_pha[5] && swap_pha[8] > swap_pha[7] ) {
	ascaGrade = 11;
      }
      break;
    default:
      ascaGrade = 6;
      break;
    }
  }
  /*  printf("%d\n",ascaGrade);*/

  /*  外の16ピクセルを、順番を変えて16ビット整数に */
  outerGrade = XISgrade2Outergrade(grade);
  /*  printf("%d\n",outerGrade);*/

  /* 3x3のパターンに応じて、まわり16ピクセルのマスクを作成 */
  mask = mkgrademask(grade);
  /*mask = 0;*/
  /*  printf("%d\n",mask);*/

  /* まわりのマスク内にスプリット閾値を越えるものがあるか？ */
  /* 3x3のGradeごとに処理 */
  switch (ascaGrade) {
  case 0: case 1:
    /* ピクセルレベルをたし合わせる */
    *sumph = sum5x5(split, above, swap_pha, ascaGrade);
/*    printf("%d\n",*sumph); */
    /* Gradeはそのまま */
    *type = ascaGrade;
    break;
  case 2: case 3: case 4: case 5: case 6: case 8:
    if ( (outerGrade & mask) == 0 ) {
      *sumph = sum5x5(split, above, swap_pha, ascaGrade);
      *type = ascaGrade;
      /* set the grade of L-shaped event to 6 */
      if( ascaGrade == 8 ) *type = 6;
    } else {
      *sumph = sum5x5(split, above, swap_pha, ascaGrade);
      *type = 10;
    }
    break;
  case 7:
    if ( (outerGrade & mask) == 0 ) {
      *sumph = sum5x5(split, above, swap_pha, ascaGrade);
      *type = 9;
    } else {
      *sumph = sum5x5(split, above, swap_pha, ascaGrade);
      *type = 7;
    }
    break;
  case 11:
    *sumph = sum5x5(split, above, swap_pha, ascaGrade);
    *type =11;
    break;
  default:
    *type = 7;
    break;
  }

  return ANL_TRUE;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:8 ***
;;; c-indent-level:2  ***
;;; End: ***
*/
