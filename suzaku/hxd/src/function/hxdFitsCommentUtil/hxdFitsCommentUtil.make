hxdFitsCommentUtil_DIR = $(HXD_FUNCTION_DIR)/hxdFitsCommentUtil/0.0.7

hxdFitsCommentUtil = hxdFitsCommentUtil.o

hxdFitsCommentUtil.o: $(hxdFitsCommentUtil_DIR)/hxdFitsCommentUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdFitsCommentUtil_DIR)/hxdFitsCommentUtil.c
