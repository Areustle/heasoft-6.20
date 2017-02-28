/*********************************************
 * AEpacketNova.h
 *
 *
 *
 *
 *  Ver 0.00 (1998/08/27)   T. Tsuru
 *     Initial Version
 *  Ver 0.01 (1999/08/16)   M. Ozaki
 *     Add AePacketInfo
 *********************************************/

#define NOVA_AE_PACKET  "/tmp/aePacket.nova"
#define MAX_PACKET_SIZE (65536+6)

typedef struct {
  int  aeflag;
  double aetime;
  int  apid;
  int  pksize;
  unsigned char packet[MAX_PACKET_SIZE];
} AePacket;

typedef struct {
  int  aeflag;
  double aetime;
  int  apid;
  int  pksize;
} AePacketInfo;
