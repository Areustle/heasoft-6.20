c
c
c	Include File for FORTRAN I/O Status Errors
c       $Id: ftnerr.h,v 3.1 2002/04/16 20:32:12 irby Exp $
c       $Log: ftnerr.h,v $
c       Revision 3.1  2002/04/16 20:32:12  irby
c       Additions to libgro - previously these codes existed in the following
c       libraries:
c
c         libsenstv
c         libsysutil
c         libutil
c         libftio
c
c Revision 1.1  1996/02/20  20:53:00  programs
c Initial revision
c
c
c	These values are "common" for most UNIX FORTRAN 
c	implementations. 
c
c	"man 3f perror" should list them on your system
c
c	The FOLLOWING appear to be "most" common

	integer*4 ftn_format_error
	integer*4 ftn_illegal_unit
	integer*4 ftn_no_format_io
	integer*4 ftn_no_unform_io
	integer*4 ftn_no_direct_io
	integer*4 ftn_no_seqntl_io
	integer*4 ftn_no_backspace
	integer*4 ftn_off_beg_recd
	integer*4 ftn_no_file_stat
	integer*4 ftn_repeat_count 
	integer*4 ftn_off_end_recd
	integer*4 ftn_trunc_failed
	integer*4 ftn_bad_list_inp
	integer*4 ftn_no_free_spce
	integer*4 ftn_not_connectd
	integer*4 ftn_unexpct_char
	integer*4 ftn_blnk_logical
	integer*4 ftn_newfile_exst
	integer*4 ftn_oldfile_gone
	integer*4 ftn_system_error
	integer*4 ftn_require_seek
	integer*4 ftn_bad_argument
	integer*4 ftn_negtv_repeat 
	integer*4 ftn_bad_unit_opr

	integer*4 ftn_eof
	integer*4 ftn_ok

	parameter ( ftn_ok  =  0 )
	parameter ( ftn_eof = -1 )

	parameter ( ftn_format_error = 100 )
	parameter ( ftn_illegal_unit = 101 )
	parameter ( ftn_no_format_io = 102 )
	parameter ( ftn_no_unform_io = 103 )
	parameter ( ftn_no_direct_io = 104 )
	parameter ( ftn_no_seqntl_io = 105 )
	parameter ( ftn_no_backspace = 106 )
	parameter ( ftn_off_beg_recd = 107 )
	parameter ( ftn_no_file_stat = 108 )
	parameter ( ftn_repeat_count = 109 )
	parameter ( ftn_off_end_recd = 110 )
	parameter ( ftn_trunc_failed = 111 )
	parameter ( ftn_bad_list_inp = 112 )
	parameter ( ftn_no_free_spce = 113 )
	parameter ( ftn_not_connectd = 114 )
	parameter ( ftn_unexpct_char = 115 )
	parameter ( ftn_blnk_logical = 116 )
	parameter ( ftn_newfile_exst = 117 )
	parameter ( ftn_oldfile_gone = 118 )
	parameter ( ftn_system_error = 119 )
	parameter ( ftn_require_seek = 120 )
	parameter ( ftn_bad_argument = 121 )
	parameter ( ftn_negtv_repeat = 122 )
	parameter ( ftn_bad_unit_opr = 123 )


c	SUN OS system error codes
c
c       100  ``error in format''
c       101  ``illegal unit number''
c       102  ``formatted io not allowed''
c       103  ``unformatted io not allowed''
c       104  ``direct io not allowed''
c       105  ``sequential io not allowed''
c       106  ``can't backspace file''
c       107  ``off beginning of record''
c       108  ``can't stat file''
c       109  ``no * after repeat count''
c       110  ``off end of record''
c       111  ``truncation failed''
c       112  ``incomprehensible list input''
c       113  ``out of free space''
c       114  ``unit not connected''
c       115  ``read unexpected character''
c       116  ``blank logical input field''
c       117  ``'new' file exists''
c       118  ``can't find 'old' file''
c       119  ``unknown system error''
c       120  ``requires seek ability''
c       121  ``illegal argument''
c       122  ``negative repeat count''
c       123  ``illegal operation for unit''
c       124  ``too many files open - no free descriptors''
c       125  ``attempted operation on unit that's not open''
c       126  ``illegal input for namelist''
c       127  ``error in FILEOPT parameter''
c
c
c	Hewlett Packard OS system error codes
c
c
c       107  ``off beginning of record''
c       108  ``can't stat file''
c       109  ``no * after repeat count''
c       110  ``off end of record''
c       111  ``truncation failed''
c       112  ``incomprehensible list input''
c       113  ``out of free space''
c       114  ``unit not connected''
c       115  ``invalid data for integer format term''
c       116  ``invalid data for logical format term''
c       117  ``'new' file exists''
c       118  ``can't find 'old' file''
c       119  ``opening too many files or unknown system error''
c       120  ``requires seek ability''
c       121  ``illegal argument''
c       122  ``negative repeat count''
c       123  ``illegal operation for unit''
c       124  ``invalid data for d, e, f, or g format term''
c       125  ``illegal input for namelist''
c
c
c	CONVEX OS system error codes
c
c       100    error in format
c       101    illegal unit number
c       102    formatted I/O not allowed
c       103    unformatted I/O not allowed
c       104    direct I/O not allowed
c       105    sequential I/O not allowed
c       106    can't backspace file
c       107    off beginning of record
c       108    can't stat file
c       109    no * after repeat count
c       110    off end of record
c       111    truncation failed
c       112    incomprehensible list input
c       113    out of free space
c       114    unit not connected
c       115    read unexpected character
c       116    blank logical input field
c       117    "new" file exists
c       118    can't find "old" file
c       119    unknown system error
c       120    requires seek ability
c       121    illegal argument
c       122    negative repeat count
c       123    illegal operation for unit
c       124    new record not allowed
c       125    numeric keyword variable overflowed
c       126    record number exceeds maximum records
c       127    file is read-only
c       128    variable record format not allowed
c       129    invalid record length
c       130    exceeds maximum number of open files
c       131    data type size too small for real
c       132    infinite loop in format
c       133    illegal recordtype
c       134    attempt to read nonexistent record in direct access file
c       135    attempt to reopen file with different unit number
c       136    incompatible format code and I/O list item type
c       137    unknown record length
c       138    asynchronous I/O not allowed
c       139    synchronous I/O not allowed
c       140    incompatible format structure
c       141    namelist error
c       142    apparent recursive logical name definition
c       143    recursive input/output operation
c       144    out of free space, possibly due to unformatted I/O on a
c              formatted file
c       145    Error in conversion of string to numeric
c       146    binary I/O data format conversion routine returned error
c       147    partial record I/O not supported (BUFFERIN/BUFFEROUT)

