/*
 *  err_xwrite.c 
 *
 *  Implement ast error output as xwrite
 */

#include "../include/xcommon.h"

static int astchat = 10;

void astPutErr_( int status, const char *message ) {
   (void) cxwrite(message, astchat);
}

void setastchat(int chat_level) {
   astchat = chat_level;
}

int getastchat() {
   return astchat;
}
