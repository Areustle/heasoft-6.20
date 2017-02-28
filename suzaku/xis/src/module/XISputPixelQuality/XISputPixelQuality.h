/* 32 bits Pixel Quality Code

*** LSB is defined as B0 and MSB is B31!
*** Be careful that "fdump" displays the
*** MSB in the leftmost. The MSB appears
*** in the 1st column of a "fv" Vector Table.
*** Example:
*** ---------------------------------
***  MSB                          LSB
*** ---------------------------------
***   3         2         1
***  10987654321098765432109876543210  B0 - B31 definition
*** ---------------------------------
***           1         2         3
***  12345678901234567890123456789012 "fv" convention
*** ---------------------------------
*** b00000000000100000000000000000000: BAD CTI column
*** b00000000010000000000000000000000: flickering pixels
*** b00000000010100000000000000000000: both of above
 --------------------------------------


B0
B1  edge of area-dsicri area
B2  pixels two pixels apart from the segment boundary
B3  pixels two pixels apart from the preceding pixels in a bad column

B4  pixels two pixels apart from bad CTE columns
B5  pixels two pixels apart from hot pixels
B6  pixels two pixels apart from flickering pixels
B7  reserved

B8  pixels one pixel apart from the segment boundary
B9  reserved
B10 pixels one pixel apart from the preceding pixels in a bad column
B11 preceding pixels in a bad column

B12 pixels one pixel apart from bad CTE columns
B13 pixels one pixel apart from hot pixels
B14 pixels one pixel apart from flickering pixels
B15 reserved

B16 inside the calibration mask area
B17 segment boundary
B18 reserved
B19 reserved

B20 bad CTE columns
B21 hot pixels
B22 flickering pixels
B23 reserved

B24 outside of aread-discri area
B25 other kinds of bad pixels

*/

#define NBITS_BCCODE               32

#define BIT_AREADISCRI_EDGE         1
#define BIT_2PIX_FROM_SEGBOUNDARY   2
#define BIT_2PIX_FROM_PRECEDING     3

#define BIT_2PIX_FROM_BADCTE        4
#define BIT_2PIX_FROM_HOT           5
#define BIT_2PIX_FROM_FLICKERING    6

#define BIT_1PIX_FROM_SEGBOUNDARY   8
#define BIT_1PIX_FROM_PRECEDING     10
#define BIT_PRECEDING               11

#define BIT_1PIX_FROM_BADCTE        12
#define BIT_1PIX_FROM_HOT           13
#define BIT_1PIX_FROM_FLICKERING    14

#define BIT_CALMASK                 16
#define BIT_SEGBOUNDARY             17

#define BIT_BADCTE        20
#define BIT_HOT           21
#define BIT_FLICKERING    22

#define BIT_OUTSIDE_AREADISCRI      24
#define BIT_OTHER_BAD               25
