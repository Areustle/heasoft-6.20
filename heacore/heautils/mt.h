#ifndef MT_RAND_H
#define MT_RAND_H

/* This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.  You should have received
   a copy of the GNU General Public License along with this program;
   if not, write to the Free Foundation, Inc., 59 Temple Place, Suite
   330, Boston, MA 02111-1307 USA

   Original implementation was copyright (C) 1997 Makoto Matsumoto and
   Takuji Nishimura. Coded by Takuji Nishimura, considering the
   suggestions by Topher Cooper and Marc Rieffel in July-Aug. 1997, "A
   C-program for MT19937: Integer version (1998/4/6)"

   This implementation copyright (C) 1998 Brian Gough. I reorganized
   the code to use the module framework of GSL.  The license on this
   implementation was changed from LGPL to GPL, following paragraph 3
   of the LGPL, version 2.

   Update:

   The seeding procedure has been updated to match the 10/99 release
   of MT19937.

   Update:

   The seeding procedure has been updated again to match the 2002
   release of MT19937

   The original code included the comment: "When you use this, send an
   email to: matumoto@math.keio.ac.jp with an appropriate reference to
   your work".

   Makoto Matsumoto has a web page with more information about the
   generator, http://www.math.keio.ac.jp/~matumoto/emt.html. 

   The paper below has details of the algorithm.

   From: Makoto Matsumoto and Takuji Nishimura, "Mersenne Twister: A
   623-dimensionally equidistributerd uniform pseudorandom number
   generator". ACM Transactions on Modeling and Computer Simulation,
   Vol. 8, No. 1 (Jan. 1998), Pages 3-30

   You can obtain the paper directly from Makoto Matsumoto's web page.

   The period of this generator is 2^{19937} - 1.

*/


#define MT_N 624   /* Period parameters */
#define MT_M 397

typedef struct mt_state_t
  {
    unsigned long mt[MT_N];
    int mti;
  }
mt_state_t;


unsigned long int mt_get (mt_state_t *state);
double mt_get_double (mt_state_t *state);
void mt_set (mt_state_t *state, unsigned long int s);


#endif
