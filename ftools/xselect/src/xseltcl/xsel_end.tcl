# The termination script. You need close all the channels
# you have opened. 
if {$xsl_log_channame != "stdout"} { 
    close $xsl_log_channame
} 
if {$xsl_std_channame != "stdout"} { 
    close $xsl_std_channame
} 
if {$xsl_prompt_channame != "stdout"} { 
    close $xsl_prompt_channame
} 
if {$xsl_cmd_channame != "stdin"} { 
    close $xsl_cmd_channame
}
file delete xselcmd.tmp
