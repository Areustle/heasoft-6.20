#!/usr/local/bin/tclsh 
# The initialization script. It executes  before 
# initializing the xselect.
#
# You have define four channels 
# xsl_std_channame: stdout and stderr output channel
# xsl_log_channame: log message output channel
# xsl_prompt_channame: command prommpt output channel
# xsl_cmd_channame:  cmd/parameter  input channel
#
set auto_path [linsert $auto_path 0 $env(FTOOLS)/lib/txselect ]
package require xsl_help 1.0

puts "Welcome to the Tcl-Xselect"
set xsl_log_channame [open xselect.log w]
set xsl_std_channame stdout
set xsl_prompt_channame stdout
set xsl_cmd_channame stdin 
#set fplot_window "xterm -title fplot -geometry 50x15+10+10 -e "
set fplot_window ""
set fplot_isbkg ""
set using_fv "no"

set ftool_out_buffer 0 

help_manual
